package quickjs.objmodel

import quickjs.value.JSValue
import quickjs.runtime.JSContext

import scala.collection.mutable

/** JavaScript Object representation.
  *
  * Design goals:
  *   - Fast property access
  *   - Support for prototype chains
  *   - Property descriptors and attributes
  */
final class JSObject private (
    private var properties: mutable.LinkedHashMap[String, JSValue],
    private var propertyAttributes: mutable.LinkedHashMap[
      String,
      JSObject.PropertyAttributes
    ],
    private var prototype: JSObject | Null,
    private var extensible: Boolean,
    private var symbolProperties: mutable.LinkedHashMap[Int, JSValue] =
      mutable.LinkedHashMap.empty,
    private var symbolPropertyAttributes: mutable.LinkedHashMap[
      Int,
      JSObject.PropertyAttributes
    ] = mutable.LinkedHashMap.empty
) {
  import JSObject.JSObjectFlags

  // Object flags (bitfield for compactness)
  private var flags: Int = 0

  // Non-strict simple-parameter `arguments` objects keep selected indexed
  // properties aliased to the corresponding local VarRef. QuickJS C stores
  // these as JS_CLASS_MAPPED_ARGUMENTS array entries backed by JSVarRef.
  private val mappedArgumentRefs: mutable.HashMap[String, JSValue.VarRef] =
    mutable.HashMap.empty

  def mapArgumentProperty(key: String, ref: JSValue.VarRef): Unit =
    mappedArgumentRefs(key) = ref

  def mappedArgumentRef(key: String): Option[JSValue.VarRef] =
    mappedArgumentRefs.get(key)

  def isExtensible: Boolean = extensible
  def isFrozen: Boolean = (flags & JSObjectFlags.Frozen) != 0
  def isSealed: Boolean = (flags & JSObjectFlags.Sealed) != 0

  def getPrototype: JSObject | Null = prototype
  def setPrototype(proto: JSObject | Null): Unit =
    if !hasImmutablePrototype then prototype = proto

  def hasPrototype(target: JSObject): Boolean = {
    var current = prototype
    while current != null do {
      if current.eq(target) then return true
      current = current.getPrototype
    }
    false
  }

  def hasImmutablePrototype: Boolean =
    (flags & JSObjectFlags.ImmutablePrototype) != 0

  // Property operations
  def getOwnProperty(key: String)(using ctx: JSContext): Option[JSValue] =
    mappedArgumentRefs.get(key).map(_.get).orElse(properties.get(key))

  /** Get own property without requiring a JSContext (for error formatting,
    * etc.).
    */
  def getOwnPropertyRaw(key: String): Option[JSValue] =
    mappedArgumentRefs.get(key).map(_.get).orElse(properties.get(key))

  def getOwnPropertyDescriptor(
      key: String
  )(using ctx: JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
    properties.get(key).map { storedValue =>
      val value = mappedArgumentRefs.get(key).map(_.get).getOrElse(storedValue)
      val attrs = propertyAttributes.getOrElse(
        key,
        JSObject.PropertyAttributes(enumerable = true)
      )
      (value, attrs)
    }

  def getPropertyDescriptor(key: String)(using
      ctx: JSContext
  ): Option[(JSValue, JSObject.PropertyAttributes)] =
    getOwnPropertyDescriptor(key) match {
      case some @ Some(_) => some
      case None           =>
        prototype match {
          case null  => None
          case proto => proto.getPropertyDescriptor(key)
        }
    }

  def getPropertyDescriptorWithOwner(key: String)(using
      ctx: JSContext
  ): Option[(JSObject, JSValue, JSObject.PropertyAttributes)] =
    getOwnPropertyDescriptor(key) match {
      case Some((value, attrs)) => Some((this, value, attrs))
      case None                 =>
        prototype match {
          case null  => None
          case proto => proto.getPropertyDescriptorWithOwner(key)
        }
    }

  def get(key: String)(using ctx: JSContext): JSValue =
    mappedArgumentRefs.get(key).map(_.get).orElse(properties.get(key)) match {
      case Some(value) => value
      case None        =>
        // Look in prototype chain
        prototype match {
          case null  => JSValue.Undefined
          case proto => proto.get(key)
        }
    }

  def set(key: String, value: JSValue)(using ctx: JSContext): Boolean =
    propertyAttributes.get(key) match {
      case Some(attrs)
          if attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined =>
        // Accessors are handled by caller
        true
      case Some(attrs) if !attrs.writable =>
        false
      case _ =>
        if !isExtensible && !properties.contains(key) then false
        else {
          properties(key) = value
          mappedArgumentRefs.get(key).foreach(_.set(value))
          if !propertyAttributes.contains(key) then
            propertyAttributes(key) =
              JSObject.PropertyAttributes(enumerable = true)
          true
        }
    }

  def hasProperty(key: String)(using ctx: JSContext): Boolean =
    properties
      .contains(key) || (prototype != null && prototype.hasProperty(key))

  def deleteProperty(key: String)(using ctx: JSContext): Boolean =
    propertyAttributes.get(key) match {
      case Some(attrs) if !attrs.configurable => false
      case _                                  =>
        // Note: isExtensible only affects adding new properties, not deleting existing ones
        properties.remove(key)
        propertyAttributes.remove(key)
        mappedArgumentRefs.remove(key)
        true
    }

  def defineProperty(
      key: String,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean = true,
      configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    defineDataProperty(
      key,
      Some(value),
      Some(enumerable),
      Some(writable),
      Some(configurable)
    )

  def defineDataProperty(
      key: String,
      value: Option[JSValue],
      enumerable: Option[Boolean],
      writable: Option[Boolean],
      configurable: Option[Boolean]
  )(using ctx: JSContext): Boolean = {
    val succeeded =
      if !isExtensible && !properties.contains(key) then false
      else
      propertyAttributes.get(key) match {
        case None =>
          properties(key) = value.getOrElse(JSValue.Undefined)
          propertyAttributes(key) = JSObject.PropertyAttributes(
            enumerable = enumerable.getOrElse(false),
            writable = writable.getOrElse(false),
            configurable = configurable.getOrElse(false)
          )
          true
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          val hasDataFields = value.isDefined || writable.isDefined

          if !existing.configurable then {
            if configurable.contains(true) then false
            else if enumerable.exists(_ != existing.enumerable) then false
            else if existingIsAccessor && hasDataFields then false
            else if !existingIsAccessor && !existing.writable && writable.contains(true)
            then false
            else if !existingIsAccessor && !existing.writable && value.exists(v =>
                properties.get(key).exists(_ != v)
              )
            then false
            else {
              if !existingIsAccessor then value.foreach(v => properties(key) = v)
              propertyAttributes(key) = existing.copy(
                enumerable = enumerable.getOrElse(existing.enumerable),
                writable =
                  if existingIsAccessor then existing.writable
                  else writable.getOrElse(existing.writable),
                configurable = existing.configurable
              )
              true
            }
          }
          else {
            val newEnumerable = enumerable.getOrElse(existing.enumerable)
            val newConfigurable = configurable.getOrElse(existing.configurable)
            if existingIsAccessor && hasDataFields then {
              properties(key) = value.getOrElse(JSValue.Undefined)
              propertyAttributes(key) = JSObject.PropertyAttributes(
                enumerable = newEnumerable,
                writable = writable.getOrElse(false),
                configurable = newConfigurable
              )
            }
            else {
              if value.isDefined then properties(key) = value.get
              propertyAttributes(key) = existing.copy(
                enumerable = newEnumerable,
                writable =
                  if existingIsAccessor then existing.writable
                  else writable.getOrElse(existing.writable),
                configurable = newConfigurable
              )
            }
            true
          }
      }
    if succeeded then {
      value.foreach { newValue =>
        mappedArgumentRefs.get(key).foreach(_.set(newValue))
      }
      if writable.contains(false) then {
        // Arguments exotic [[DefineOwnProperty]] snapshots the current
        // parameter value into the ordinary data slot before severing the
        // mapping when [[Writable]] becomes false.
        mappedArgumentRefs.remove(key).foreach { ref =>
          properties(key) = ref.get
        }
      }
    }
    succeeded
  }

  def defineAccessorProperty(
      key: String,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    defineAccessorPropertyDetailed(
      key,
      getter,
      setter,
      hasGetter = getter.isDefined,
      hasSetter = setter.isDefined,
      enumerable = Some(enumerable),
      configurable = Some(configurable)
    )

  def defineAccessorPropertyDetailed(
      key: String,
      getter: Option[JSValue],
      setter: Option[JSValue],
      hasGetter: Boolean,
      hasSetter: Boolean,
      enumerable: Option[Boolean],
      configurable: Option[Boolean]
  )(using ctx: JSContext): Boolean = {
    val succeeded =
      if !isExtensible && !properties.contains(key) then false
      else
      propertyAttributes.get(key) match {
        case None =>
          properties(key) = JSValue.Undefined
          propertyAttributes(key) = JSObject.PropertyAttributes(
            enumerable = enumerable.getOrElse(false),
            writable = false,
            configurable = configurable.getOrElse(false),
            getter = if hasGetter then getter else None,
            setter = if hasSetter then setter else None,
            isAccessor = true
          )
          true
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined

          if !existing.configurable then {
            if configurable.contains(true) then false
            else if enumerable.exists(_ != existing.enumerable) then false
            else if !existingIsAccessor then false
            else if hasGetter && getter != existing.getter then false
            else if hasSetter && setter != existing.setter then false
            else true
          }
          else {
            val mergedGetter =
              if hasGetter then getter
              else if existingIsAccessor then existing.getter
              else None
            val mergedSetter =
              if hasSetter then setter
              else if existingIsAccessor then existing.setter
              else None
            val newEnumerable = enumerable.getOrElse(existing.enumerable)
            val newConfigurable = configurable.getOrElse(existing.configurable)
            properties(key) = JSValue.Undefined
            propertyAttributes(key) = JSObject.PropertyAttributes(
              enumerable = newEnumerable,
              writable = false,
              configurable = newConfigurable,
              getter = mergedGetter,
              setter = mergedSetter,
              isAccessor = true
            )
            true
          }
      }
    if succeeded then mappedArgumentRefs.remove(key)
    succeeded
  }

  def getPropertyAttributes(key: String): Option[JSObject.PropertyAttributes] =
    propertyAttributes.get(key)

  /** Own string keys in ECMAScript [[OwnPropertyKeys]] order: array indices in
    * ascending numeric order, followed by the remaining strings in creation
    * order.
    */
  def getAllOwnStringPropertyKeys(): Array[String] =
    JSObject.orderStringPropertyKeys(propertyAttributes.keysIterator)

  /** Enumerable own string keys in ECMAScript [[OwnPropertyKeys]] order. */
  def getEnumerableOwnStringPropertyKeys(): Array[String] =
    JSObject.orderStringPropertyKeys(
      propertyAttributes.iterator.collect {
        case (key, attrs) if attrs.enumerable => key
      }
    )

  // Own enumerable property keys (ordered string keys first, then symbols)
  def getOwnPropertyKeys(): Array[String] = {
    val stringKeys = getEnumerableOwnStringPropertyKeys()
    val symbolKeys = symbolPropertyAttributes.collect {
      case (id, attrs) if attrs.enumerable => s"@@symbol:$id"
    }.toArray
    stringKeys ++ symbolKeys
  }

  /** Get symbol property ids for enumerable symbol keys. */
  def getOwnSymbolPropertyIds(): Array[Int] =
    symbolPropertyAttributes.collect {
      case (id, attrs) if attrs.enumerable => id
    }.toArray

  /** Get all own symbol property ids, including non-enumerable properties. */
  def getAllOwnSymbolPropertyIds(): Array[Int] =
    symbolPropertyAttributes.keys.toArray

  /** Check if a key string is an encoded symbol key and extract its id. */
  def isEncodedSymbolKey(key: String): Option[Int] =
    if key.startsWith("@@symbol:") then
      try Some(key.substring(9).toInt)
      catch case _ => None
    else None

  // Get all properties as map (for pretty printing)
  def getAllProperties: Map[String, JSValue] = Map.from(properties)

  /** All own string keys in property creation order. */
  def getAllOwnPropertyKeys(): Vector[String] = properties.keysIterator.toVector

  // Get property count
  def getPropertyCount: Int = properties.size + symbolProperties.size

  // =========================================================================
  // Symbol-keyed property operations (keyed by symbol id: Int)
  // =========================================================================

  /** Get an own symbol-keyed property value by symbol id. */
  def getOwnSymbolProperty(symbolId: Int)(using
      ctx: JSContext
  ): Option[JSValue] =
    symbolProperties.get(symbolId)

  /** Get own symbol-keyed property descriptor by symbol id. */
  def getOwnSymbolPropertyDescriptor(
      symbolId: Int
  )(using ctx: JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
    symbolProperties.get(symbolId).map { value =>
      val attrs = symbolPropertyAttributes.getOrElse(
        symbolId,
        JSObject.PropertyAttributes(enumerable = true)
      )
      (value, attrs)
    }

  /** Get symbol-keyed property descriptor with owner (walks prototype chain).
    */
  def getSymbolPropertyDescriptorWithOwner(symbolId: Int)(using
      ctx: JSContext
  ): Option[(JSObject, JSValue, JSObject.PropertyAttributes)] =
    getOwnSymbolPropertyDescriptor(symbolId) match {
      case Some((value, attrs)) => Some((this, value, attrs))
      case None                 =>
        prototype match {
          case null  => None
          case proto => proto.getSymbolPropertyDescriptorWithOwner(symbolId)
        }
    }

  /** Get symbol-keyed property value (walks prototype chain). */
  def getSymbol(symbolId: Int)(using ctx: JSContext): JSValue =
    symbolProperties.get(symbolId) match {
      case Some(value) => value
      case None        =>
        prototype match {
          case null  => JSValue.Undefined
          case proto => proto.getSymbol(symbolId)
        }
    }

  /** Set a symbol-keyed property value. Returns true on success. */
  def setSymbol(symbolId: Int, value: JSValue)(using ctx: JSContext): Boolean =
    symbolPropertyAttributes.get(symbolId) match {
      case Some(attrs)
          if attrs.isAccessor || attrs.getter.isDefined || attrs.setter.isDefined =>
        true
      case Some(attrs) if !attrs.writable => false
      case _                              =>
        if !isExtensible && !symbolProperties.contains(symbolId) then false
        else {
          symbolProperties(symbolId) = value
          if !symbolPropertyAttributes.contains(symbolId) then
            symbolPropertyAttributes(symbolId) =
              JSObject.PropertyAttributes(enumerable = true)
          true
        }
    }

  /** Check if this object has a symbol-keyed property (own or inherited). */
  def hasSymbolProperty(symbolId: Int)(using ctx: JSContext): Boolean =
    symbolProperties.contains(symbolId) || (prototype != null && prototype
      .hasSymbolProperty(symbolId))

  /** Delete a symbol-keyed property. Returns true if deleted. */
  def deleteSymbolProperty(symbolId: Int)(using ctx: JSContext): Boolean =
    symbolPropertyAttributes.get(symbolId) match {
      case Some(attrs) if !attrs.configurable => false
      case _                                  =>
        symbolProperties.remove(symbolId)
        symbolPropertyAttributes.remove(symbolId)
        true
    }

  /** Define a symbol-keyed data property with attributes. */
  def defineSymbolProperty(
      symbolId: Int,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean = true,
      configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    defineSymbolDataProperty(
      symbolId,
      Some(value),
      Some(enumerable),
      Some(writable),
      Some(configurable)
    )

  def defineSymbolDataProperty(
      symbolId: Int,
      value: Option[JSValue],
      enumerable: Option[Boolean],
      writable: Option[Boolean],
      configurable: Option[Boolean]
  )(using ctx: JSContext): Boolean =
    if !isExtensible && !symbolProperties.contains(symbolId) then false
    else
      symbolPropertyAttributes.get(symbolId) match {
        case None =>
          symbolProperties(symbolId) = value.getOrElse(JSValue.Undefined)
          symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
            enumerable = enumerable.getOrElse(false),
            writable = writable.getOrElse(false),
            configurable = configurable.getOrElse(false)
          )
          true
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          val hasDataFields = value.isDefined || writable.isDefined

          if !existing.configurable then {
            if configurable.contains(true) then false
            else if enumerable.exists(_ != existing.enumerable) then false
            else if existingIsAccessor && hasDataFields then false
            else if !existingIsAccessor && !existing.writable && writable.contains(true)
            then false
            else if !existingIsAccessor && !existing.writable && value.exists(v =>
                symbolProperties.get(symbolId).exists(_ != v)
              )
            then false
            else {
              if !existingIsAccessor then value.foreach(v => symbolProperties(symbolId) = v)
              symbolPropertyAttributes(symbolId) = existing.copy(
                enumerable = enumerable.getOrElse(existing.enumerable),
                writable =
                  if existingIsAccessor then existing.writable
                  else writable.getOrElse(existing.writable),
                configurable = existing.configurable
              )
              true
            }
          }
          else {
            val newEnumerable = enumerable.getOrElse(existing.enumerable)
            val newConfigurable = configurable.getOrElse(existing.configurable)
            if existingIsAccessor && hasDataFields then {
              symbolProperties(symbolId) = value.getOrElse(JSValue.Undefined)
              symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
                enumerable = newEnumerable,
                writable = writable.getOrElse(false),
                configurable = newConfigurable
              )
            }
            else {
              if value.isDefined then symbolProperties(symbolId) = value.get
              symbolPropertyAttributes(symbolId) = existing.copy(
                enumerable = newEnumerable,
                writable =
                  if existingIsAccessor then existing.writable
                  else writable.getOrElse(existing.writable),
                configurable = newConfigurable
              )
            }
            true
          }
      }

  /** Define a symbol-keyed accessor property (getter/setter). */
  def defineSymbolAccessorProperty(
      symbolId: Int,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    defineSymbolAccessorPropertyDetailed(
      symbolId,
      getter,
      setter,
      hasGetter = getter.isDefined,
      hasSetter = setter.isDefined,
      enumerable = Some(enumerable),
      configurable = Some(configurable)
    )

  def defineSymbolAccessorPropertyDetailed(
      symbolId: Int,
      getter: Option[JSValue],
      setter: Option[JSValue],
      hasGetter: Boolean,
      hasSetter: Boolean,
      enumerable: Option[Boolean],
      configurable: Option[Boolean]
  )(using ctx: JSContext): Boolean =
    if !isExtensible && !symbolProperties.contains(symbolId) then false
    else
      symbolPropertyAttributes.get(symbolId) match {
        case None =>
          symbolProperties(symbolId) = JSValue.Undefined
          symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
            enumerable = enumerable.getOrElse(false),
            writable = false,
            configurable = configurable.getOrElse(false),
            getter = if hasGetter then getter else None,
            setter = if hasSetter then setter else None,
            isAccessor = true
          )
          true
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined

          if !existing.configurable then {
            if configurable.contains(true) then false
            else if enumerable.exists(_ != existing.enumerable) then false
            else if !existingIsAccessor then false
            else if hasGetter && getter != existing.getter then false
            else if hasSetter && setter != existing.setter then false
            else true
          }
          else {
            val mergedGetter =
              if hasGetter then getter
              else if existingIsAccessor then existing.getter
              else None
            val mergedSetter =
              if hasSetter then setter
              else if existingIsAccessor then existing.setter
              else None
            symbolProperties(symbolId) = JSValue.Undefined
            symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
              enumerable = enumerable.getOrElse(existing.enumerable),
              writable = false,
              configurable = configurable.getOrElse(existing.configurable),
              getter = mergedGetter,
              setter = mergedSetter,
              isAccessor = true
            )
            true
          }
      }

  // Type checking
  def isArray: Boolean = (flags & JSObjectFlags.Array) != 0
  def isFunction: Boolean = (flags & JSObjectFlags.Function) != 0
  def isArguments: Boolean = (flags & JSObjectFlags.Arguments) != 0
  def isConstructor: Boolean = (flags & JSObjectFlags.Constructor) != 0

  // Freeze/seal/preventExtensions operations
  def freeze()(using ctx: JSContext): Unit = {
    // Make all string properties non-writable and non-configurable
    for (key, attrs) <- propertyAttributes do
      if attrs.getter.isEmpty && attrs.setter.isEmpty then
        propertyAttributes(key) =
          attrs.copy(writable = false, configurable = false)
      else propertyAttributes(key) = attrs.copy(configurable = false)
    // Make all symbol properties non-writable and non-configurable
    for (id, attrs) <- symbolPropertyAttributes do
      if attrs.getter.isEmpty && attrs.setter.isEmpty then
        symbolPropertyAttributes(id) =
          attrs.copy(writable = false, configurable = false)
      else symbolPropertyAttributes(id) = attrs.copy(configurable = false)
    // Set frozen flag and prevent extensions
    flags |= JSObjectFlags.Frozen | JSObjectFlags.Sealed
    extensible = false
  }

  def seal()(using ctx: JSContext): Unit = {
    // Make all string properties non-configurable (but keep writable as-is)
    for (key, attrs) <- propertyAttributes do
      propertyAttributes(key) = attrs.copy(configurable = false)
    // Make all symbol properties non-configurable
    for (id, attrs) <- symbolPropertyAttributes do
      symbolPropertyAttributes(id) = attrs.copy(configurable = false)
    // Set sealed flag and prevent extensions
    flags |= JSObjectFlags.Sealed
    extensible = false
  }

  def preventExtensions(): Unit =
    extensible = false

  // Check if object is truly frozen (all properties non-writable, non-configurable)
  def checkFrozen()(using ctx: JSContext): Boolean =
    if extensible then false
    else
      propertyAttributes.forall { case (_, attrs) =>
        !attrs.configurable && (attrs.getter.isDefined || attrs.setter.isDefined || !attrs.writable)
      } &&
      symbolPropertyAttributes.forall { case (_, attrs) =>
        !attrs.configurable && (attrs.getter.isDefined || attrs.setter.isDefined || !attrs.writable)
      }

  // Check if object is truly sealed (all properties non-configurable)
  def checkSealed()(using ctx: JSContext): Boolean =
    if extensible then false
    else
      propertyAttributes.forall { case (_, attrs) => !attrs.configurable } &&
      symbolPropertyAttributes.forall { case (_, attrs) => !attrs.configurable }

  // Internal helpers
  private[objmodel] def setArrayFlag(): Unit = flags |= JSObjectFlags.Array
  private[objmodel] def setFunctionFlag(): Unit =
    flags |= JSObjectFlags.Function

  /** Set a property directly without JSContext (for initialization). */
  private[quickjs] def initProperty(
      key: String,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean,
      configurable: Boolean
  ): Unit = {
    properties(key) = value
    propertyAttributes(key) = JSObject.PropertyAttributes(
      enumerable = enumerable,
      writable = writable,
      configurable = configurable
    )
  }

  /** Set an accessor property directly without JSContext (for initialization). */
  private[quickjs] def initAccessorProperty(
      key: String,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean
  ): Unit = {
    properties(key) = JSValue.Undefined
    propertyAttributes(key) = JSObject.PropertyAttributes(
      enumerable = enumerable,
      writable = false,
      configurable = configurable,
      getter = getter,
      setter = setter
    )
  }

  /** Set a symbol-keyed property directly without JSContext (for
    * initialization).
    */
  private[quickjs] def initSymbolProperty(
      symbolId: Int,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean,
      configurable: Boolean
  ): Unit = {
    symbolProperties(symbolId) = value
    symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
      enumerable = enumerable,
      writable = writable,
      configurable = configurable
    )
  }

  /** Set a symbol-keyed accessor property directly without JSContext. */
  private[quickjs] def initSymbolAccessorProperty(
      symbolId: Int,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean
  ): Unit = {
    symbolProperties(symbolId) = JSValue.Undefined
    symbolPropertyAttributes(symbolId) = JSObject.PropertyAttributes(
      enumerable = enumerable,
      writable = false,
      configurable = configurable,
      getter = getter,
      setter = setter
    )
  }

  def markAsArray(): Unit = flags |= JSObjectFlags.Array
}

object JSObject {
  private val MaxArrayIndex = 4294967294L

  /** ES array-index recognition used by ordinary [[OwnPropertyKeys]].
    * 2^32 - 1 is deliberately not an array index.
    */
  private[objmodel] def arrayIndex(key: String): Option[Long] =
    if key.isEmpty || (key.length > 1 && key.charAt(0) == '0') ||
        key.length > 10 || !key.forall(_.isDigit)
    then None
    else
      try
        val value = key.toLong
        if value <= MaxArrayIndex && value.toString == key then Some(value)
        else None
      catch case _: NumberFormatException => None

  private[objmodel] def orderStringPropertyKeys(
      keys: Iterator[String]
  ): Array[String] =
    val indexed = mutable.ArrayBuffer.empty[(Long, String)]
    val ordinary = mutable.ArrayBuffer.empty[String]
    keys.foreach { key =>
      arrayIndex(key) match
        case Some(index) => indexed += ((index, key))
        case None        => ordinary += key
    }
    indexed.sortInPlaceBy(_._1)
    (indexed.iterator.map(_._2) ++ ordinary.iterator).toArray

  def apply(
      prototype: JSObject | Null = null,
      extensible: Boolean = true
  ): JSObject =
    new JSObject(
      properties = mutable.LinkedHashMap.empty,
      propertyAttributes = mutable.LinkedHashMap.empty,
      prototype = prototype,
      extensible = extensible
    )

  // Standard object creation
  def create(proto: JSObject | Null)(using ctx: JSContext): JSObject =
    JSObject(prototype = proto, extensible = true)

  def createOrdinary()(using ctx: JSContext): JSObject =
    JSObject(prototype = ctx.objectPrototype, extensible = true)

  /** Object flag bit constants.
    *
    * Flags are stored as a bitfield for compactness:
    *   - Bit 0 (0x01): ImmutablePrototype - prototype cannot be changed
    *   - Bit 1 (0x02): Sealed - no new properties can be added
    *   - Bit 2 (0x04): Frozen - object is immutable (sealed + non-writable)
    *   - Bit 3 (0x08): Constructor - object is a constructor
    *   - Bit 4 (0x10): Array - object is an array
    *   - Bit 5 (0x20): Function - object is a function
    *   - Bit 6 (0x40): Arguments - object is arguments object
    */
  object JSObjectFlags {
    val ImmutablePrototype: Int = 0x01
    val Sealed: Int = 0x02
    val Frozen: Int = 0x04
    val Constructor: Int = 0x08
    val Array: Int = 0x10
    val Function: Int = 0x20
    val Arguments: Int = 0x40
  }

  final case class PropertyAttributes(
      enumerable: Boolean,
      writable: Boolean = true,
      configurable: Boolean = true,
      getter: Option[JSValue] = None,
      setter: Option[JSValue] = None,
      isAccessor: Boolean = false
  )
}
