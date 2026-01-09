package quickjs.objmodel

import quickjs.value.JSValue
import quickjs.runtime.JSContext

import scala.collection.mutable

/** JavaScript Object representation.
  *
  * Design goals:
  * - Fast property access
  * - Support for prototype chains
  * - Property descriptors and attributes
  */
final class JSObject private (
  private var properties: mutable.LinkedHashMap[String, JSValue],
  private var propertyAttributes: mutable.LinkedHashMap[String, JSObject.PropertyAttributes],
  private var prototype: JSObject | Null,
  private var extensible: Boolean
):
  import JSObject.JSObjectFlags

  // Object flags (bitfield for compactness)
  private var flags: Int = 0

  def isExtensible: Boolean = extensible
  def isFrozen: Boolean = (flags & JSObjectFlags.Frozen) != 0
  def isSealed: Boolean = (flags & JSObjectFlags.Sealed) != 0

  def getPrototype: JSObject | Null = prototype
  def setPrototype(proto: JSObject | Null): Unit =
    if !hasImmutablePrototype then prototype = proto

  def hasPrototype(target: JSObject): Boolean =
    var current = prototype
    while current != null do
      if current.eq(target) then return true
      current = current.getPrototype
    false

  def hasImmutablePrototype: Boolean = (flags & JSObjectFlags.ImmutablePrototype) != 0

  // Property operations
  def getOwnProperty(key: String)(using ctx: JSContext): Option[JSValue] =
    properties.get(key)

  def getOwnPropertyDescriptor(key: String)(using ctx: JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
    properties.get(key).map { value =>
      val attrs = propertyAttributes.getOrElse(key, JSObject.PropertyAttributes(enumerable = true))
      (value, attrs)
    }

  def getPropertyDescriptor(key: String)(using ctx: JSContext): Option[(JSValue, JSObject.PropertyAttributes)] =
    getOwnPropertyDescriptor(key) match
      case some @ Some(_) => some
      case None =>
        prototype match
          case null => None
          case proto => proto.getPropertyDescriptor(key)

  def getPropertyDescriptorWithOwner(key: String)(using ctx: JSContext): Option[(JSObject, JSValue, JSObject.PropertyAttributes)] =
    getOwnPropertyDescriptor(key) match
      case Some((value, attrs)) => Some((this, value, attrs))
      case None =>
        prototype match
          case null => None
          case proto => proto.getPropertyDescriptorWithOwner(key)

  def get(key: String)(using ctx: JSContext): JSValue =
    properties.get(key) match
      case Some(value) => value
      case None =>
        // Look in prototype chain
        prototype match
          case null => JSValue.Undefined
          case proto => proto.get(key)

  def set(key: String, value: JSValue)(using ctx: JSContext): Boolean =
    propertyAttributes.get(key) match
      case Some(attrs) if attrs.getter.isDefined || attrs.setter.isDefined =>
        // Accessors are handled by caller
        true
      case Some(attrs) if !attrs.writable =>
        false
      case _ =>
        if !isExtensible && !properties.contains(key) then false
        else
          properties(key) = value
          if !propertyAttributes.contains(key) then
            propertyAttributes(key) = JSObject.PropertyAttributes(enumerable = true)
          true

  def hasProperty(key: String)(using ctx: JSContext): Boolean =
    properties.contains(key) || (prototype != null && prototype.hasProperty(key))

  def deleteProperty(key: String)(using ctx: JSContext): Boolean =
    propertyAttributes.get(key) match
      case Some(attrs) if !attrs.configurable => false
      case _ =>
        // Note: isExtensible only affects adding new properties, not deleting existing ones
        properties.remove(key)
        propertyAttributes.remove(key)
        true

  def defineProperty(
    key: String,
    value: JSValue,
    enumerable: Boolean,
    writable: Boolean = true,
    configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    if !isExtensible && !properties.contains(key) then false
    else
      propertyAttributes.get(key) match
        case Some(existing) if !existing.configurable =>
          if existing.enumerable != enumerable then false
          else if existing.getter.isDefined || existing.setter.isDefined then
            false
          else if !existing.writable && (writable || properties.get(key).exists(_ != value)) then
            false
          else
            properties(key) = value
            propertyAttributes(key) = existing.copy(writable = writable)
            true
        case _ =>
          properties(key) = value
          propertyAttributes(key) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = writable,
            configurable = configurable
          )
          true

  def defineAccessorProperty(
    key: String,
    getter: Option[JSValue],
    setter: Option[JSValue],
    enumerable: Boolean,
    configurable: Boolean = true
  )(using ctx: JSContext): Boolean =
    if !isExtensible && !properties.contains(key) then false
    else
      propertyAttributes.get(key) match
        case Some(existing) if !existing.configurable =>
          if existing.enumerable != enumerable then false
          else if existing.getter != getter || existing.setter != setter then false
          else true
        case _ =>
          properties(key) = JSValue.Undefined
          propertyAttributes(key) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = false,
            configurable = configurable,
            getter = getter,
            setter = setter
          )
          true

  def getPropertyAttributes(key: String): Option[JSObject.PropertyAttributes] =
    propertyAttributes.get(key)

  // Own enumerable property keys
  def getOwnPropertyKeys(): Array[String] =
    propertyAttributes.collect { case (key, attrs) if attrs.enumerable => key }.toArray

  // Get all properties as map (for pretty printing)
  def getAllProperties: Map[String, JSValue] = Map.from(properties)

  // Get property count
  def getPropertyCount: Int = properties.size

  // Type checking
  def isArray: Boolean = (flags & JSObjectFlags.Array) != 0
  def isFunction: Boolean = (flags & JSObjectFlags.Function) != 0
  def isArguments: Boolean = (flags & JSObjectFlags.Arguments) != 0
  def isConstructor: Boolean = (flags & JSObjectFlags.Constructor) != 0

  // Freeze/seal/preventExtensions operations
  def freeze()(using ctx: JSContext): Unit =
    // Make all properties non-writable and non-configurable
    for (key, attrs) <- propertyAttributes do
      if attrs.getter.isEmpty && attrs.setter.isEmpty then
        propertyAttributes(key) = attrs.copy(writable = false, configurable = false)
      else
        propertyAttributes(key) = attrs.copy(configurable = false)
    // Set frozen flag and prevent extensions
    flags |= JSObjectFlags.Frozen | JSObjectFlags.Sealed
    extensible = false

  def seal()(using ctx: JSContext): Unit =
    // Make all properties non-configurable (but keep writable as-is)
    for (key, attrs) <- propertyAttributes do
      propertyAttributes(key) = attrs.copy(configurable = false)
    // Set sealed flag and prevent extensions
    flags |= JSObjectFlags.Sealed
    extensible = false

  def preventExtensions(): Unit =
    extensible = false

  // Check if object is truly frozen (all properties non-writable, non-configurable)
  def checkFrozen()(using ctx: JSContext): Boolean =
    if extensible then false
    else
      propertyAttributes.forall { case (_, attrs) =>
        !attrs.configurable && (attrs.getter.isDefined || attrs.setter.isDefined || !attrs.writable)
      }

  // Check if object is truly sealed (all properties non-configurable)
  def checkSealed()(using ctx: JSContext): Boolean =
    if extensible then false
    else
      propertyAttributes.forall { case (_, attrs) => !attrs.configurable }

  // Internal helpers
  private[objmodel] def setArrayFlag(): Unit = flags |= JSObjectFlags.Array
  private[objmodel] def setFunctionFlag(): Unit = flags |= JSObjectFlags.Function

object JSObject:
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
    * - Bit 0 (0x01): ImmutablePrototype - prototype cannot be changed
    * - Bit 1 (0x02): Sealed - no new properties can be added
    * - Bit 2 (0x04): Frozen - object is immutable (sealed + non-writable)
    * - Bit 3 (0x08): Constructor - object is a constructor
    * - Bit 4 (0x10): Array - object is an array
    * - Bit 5 (0x20): Function - object is a function
    * - Bit 6 (0x40): Arguments - object is arguments object
    */
  object JSObjectFlags:
    val ImmutablePrototype: Int = 0x01
    val Sealed: Int = 0x02
    val Frozen: Int = 0x04
    val Constructor: Int = 0x08
    val Array: Int = 0x10
    val Function: Int = 0x20
    val Arguments: Int = 0x40

  final case class PropertyAttributes(
    enumerable: Boolean,
    writable: Boolean = true,
    configurable: Boolean = true,
    getter: Option[JSValue] = None,
    setter: Option[JSValue] = None
  )
