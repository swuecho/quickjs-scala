package quickjs.objmodel

import scala.collection.mutable
import quickjs.value.JSValue

/** JavaScript Array implementation.
  *
  * Design:
  *   - Arrays are objects with special indexed properties
  *   - Elements stored in a mutable ArrayBuffer
  *   - length property is special (auto-updated)
  *   - Supports sparse arrays
  *   - Supports property attributes (for Object.defineProperty on indices)
  */
final class JSArray(
    private val elements: mutable.ArrayBuffer[JSValue],
    private val properties: mutable.LinkedHashMap[String, JSValue],
    var length: Int = 0,
    var isExtensible: Boolean = true
) {
  // QuickJS keeps a fast dense representation only while it is economical,
  // then falls back to ordinary indexed properties. Never allocate up to an
  // attacker/test-controlled index.
  private val sparseElements: mutable.HashMap[Long, JSValue] =
    mutable.HashMap.empty
  private val presentIndices: mutable.HashSet[Long] = mutable.HashSet.empty
  // Presence bits for dense slots; `presentIndices` only tracks sparse ones.
  private val densePresent: mutable.BitSet = mutable.BitSet.empty
  private val symbolProperties: mutable.LinkedHashMap[Int, JSValue] =
    mutable.LinkedHashMap.empty
  private val propertyAttributes
      : mutable.LinkedHashMap[String, JSObject.PropertyAttributes] =
    mutable.LinkedHashMap.empty
  private val MaxDenseIndex = 1024 * 1024
  // Maximum gap between the dense end and a new index that is still stored
  // densely. Larger gaps use the sparse map, so a single large-index write is
  // O(1) memory instead of materializing the whole range.
  private val MaxDenseGap = 4096
  // `new Array(n)` pre-allocates at most this many dense slots; larger arrays
  // stay sparse (their length is still n).
  private val MaxPreallocated = 65536
  private var lengthWritable: Boolean = true
  private var logicalLength: Long = length.toLong
  // None denotes the realm's intrinsic Array.prototype. Arrays are modeled
  // separately from JSObject, so keep an explicit slot for SetPrototypeOf.
  private var prototypeOverride: Option[JSValue] = None

  def getPrototypeOverride: Option[JSValue] = prototypeOverride
  def setPrototypeOverride(value: JSValue): Unit = prototypeOverride = Some(value)

  private def updateLength(newLength: Long): Unit = {
    logicalLength = math.max(0L, math.min(4294967295L, newLength))
    length = math.min(logicalLength, Int.MaxValue.toLong).toInt
  }

  private def readElement(index: Long): JSValue =
    // Sparse entries can shadow dense slots when an element was first stored
    // far beyond the dense end and later the dense buffer grew past it.
    if sparseElements.nonEmpty then
      sparseElements.get(index) match {
        case Some(value) => value
        case None =>
          if index >= 0 && index < elements.length then elements(index.toInt)
          else JSValue.Undefined
      }
    else if index >= 0 && index < elements.length then elements(index.toInt)
    else JSValue.Undefined

  private def writeElement(index: Long, value: JSValue): Unit =
    // Dense storage is only used when the index is close to the current dense
    // end. A single `arr[999999] = x` on an empty array must not materialize a
    // million-slot backing array; far indices go to the sparse map instead.
    // Sequential writes (push, fill loops) stay dense because the gap is 1.
    val currentDenseEnd = elements.length.toLong
    if index >= 0 && index <= MaxDenseIndex &&
        index <= currentDenseEnd + MaxDenseGap
    then {
      val denseIndex = index.toInt
      if denseIndex >= elements.length then {
        elements.sizeHint(denseIndex + 1)
        while elements.length <= denseIndex do elements += JSValue.Undefined
      }
      elements(denseIndex) = value
      sparseElements.remove(index)
      // Dense presence is tracked with a compact bitset instead of a HashSet
      // entry plus a boxed Long per element.
      densePresent += denseIndex
      presentIndices.remove(index)
    }
    else {
      sparseElements(index) = value
      presentIndices += index
    }

  // Property attributes for array indices (used by Object.defineProperty)
  private val indexAttributes
      : mutable.LinkedHashMap[Long, JSObject.PropertyAttributes] =
    mutable.LinkedHashMap.empty

  def getOwnProperty(key: String): Option[JSValue] =
    properties.get(key)

  def getOwnPropertyDescriptor(
      key: String
  ): Option[(JSValue, JSObject.PropertyAttributes)] =
    properties.get(key).map { value =>
      value -> propertyAttributes.getOrElse(
        key,
        JSObject.PropertyAttributes(enumerable = true)
      )
    }

  /** Get own property descriptor for an index (supports getters/setters). */
  def getOwnIndexDescriptor(
      index: Long
  ): Option[(JSValue, JSObject.PropertyAttributes)] =
    indexAttributes.get(index) match {
      case Some(attrs) => Some((readElement(index), attrs))
      case None if hasIndex(index) =>
        Some(
          readElement(index) -> JSObject.PropertyAttributes(
            enumerable = true,
            writable = true,
            configurable = true
          )
        )
      case None => None
    }

  /** Define a property on an array index with attributes. */
  def defineIndexProperty(
      index: Long,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean = true,
      configurable: Boolean = true
  ): Boolean =
    if index >= logicalLength && !lengthWritable then false
    else if !isExtensible && !hasIndex(index) then false
    else
      indexAttributes.get(index) match {
        case Some(existing) if !existing.configurable =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          val currentValue =
            readElement(index)
          if configurable then false
          else if enumerable != existing.enumerable then false
          else if existingIsAccessor then false
          else if !existing.writable && writable then false
          else if !existing.writable && value != currentValue then false
          else {
            writeElement(index, value)
            indexAttributes(index) = existing.copy(
              writable = if existing.writable then writable else existing.writable,
              configurable = false
            )
            true
          }
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          if existingIsAccessor then {
            writeElement(index, value)
            indexAttributes(index) = JSObject.PropertyAttributes(
              enumerable = enumerable,
              writable = writable,
              configurable = configurable
            )
          }
          else {
            writeElement(index, value)
            indexAttributes(index) = existing.copy(
              enumerable = enumerable,
              writable = writable,
              configurable = configurable,
              getter = None,
              setter = None,
              isAccessor = false
            )
          }
          if index >= logicalLength then updateLength(index + 1)
          true
        case None =>
          writeElement(index, value)
          indexAttributes(index) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = writable,
            configurable = configurable
          )
          if index >= logicalLength then updateLength(index + 1)
          true
      }

  /** Define an accessor property on an array index. */
  def defineIndexAccessor(
      index: Long,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean = true
  ): Boolean =
    if index >= logicalLength && !lengthWritable then false
    else if !isExtensible && !hasIndex(index) then false
    else
      indexAttributes.get(index) match {
        case Some(existing) if !existing.configurable =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          if configurable then false
          else if enumerable != existing.enumerable then false
          else if !existingIsAccessor then false
          else if getter != existing.getter then false
          else if setter != existing.setter then false
          else true
        case Some(existing) =>
          val existingIsAccessor =
            existing.isAccessor || existing.getter.isDefined || existing.setter.isDefined
          if existingIsAccessor then {
            indexAttributes(index) = existing.copy(
              enumerable = enumerable,
              configurable = configurable,
              getter = getter,
              setter = setter,
              isAccessor = true
            )
          }
          else {
            writeElement(index, JSValue.Undefined)
            indexAttributes(index) = JSObject.PropertyAttributes(
              enumerable = enumerable,
              writable = false,
              configurable = configurable,
              getter = getter,
              setter = setter,
              isAccessor = true
            )
          }
          if index >= logicalLength then updateLength(index + 1)
          true
        case None =>
          writeElement(index, JSValue.Undefined)
          indexAttributes(index) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = false,
            configurable = configurable,
            getter = getter,
            setter = setter,
            isAccessor = true
          )
          if index >= logicalLength then updateLength(index + 1)
          true
      }

  def hasIndex(index: Long): Boolean =
    index >= 0 &&
      (presentIndices.contains(index) ||
        (index <= Int.MaxValue && densePresent.contains(index.toInt)) ||
        indexAttributes.contains(index))

  def getOwnIndexKeys: Vector[Long] =
    (densePresent.iterator.map(_.toLong) ++
      presentIndices.iterator ++
      indexAttributes.keysIterator).toSet.toVector.sorted

  def setProperty(key: String, value: JSValue): Unit =
    propertyAttributes.get(key) match {
      case Some(attrs) if attrs.isAccessor => ()
      case Some(attrs) if !attrs.writable  => ()
      case _ =>
        properties(key) = value
        if !propertyAttributes.contains(key) then
          propertyAttributes(key) = JSObject.PropertyAttributes(enumerable = true)
    }

  def defineNamedDataProperty(
      key: String,
      value: Option[JSValue],
      enumerable: Option[Boolean],
      writable: Option[Boolean],
      configurable: Option[Boolean]
  ): Boolean =
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
          val existingIsAccessor = existing.isAccessor ||
            existing.getter.isDefined || existing.setter.isDefined
          val hasDataFields = value.isDefined || writable.isDefined
          if !existing.configurable then {
            if configurable.contains(true) ||
                enumerable.exists(_ != existing.enumerable) ||
                (existingIsAccessor && hasDataFields) ||
                (!existingIsAccessor && !existing.writable && writable.contains(true)) ||
                (!existingIsAccessor && !existing.writable &&
                  value.exists(_ != properties(key)))
            then false
            else {
              if !existingIsAccessor && existing.writable then
                value.foreach(properties(key) = _)
              if !existingIsAccessor && writable.contains(false) then
                propertyAttributes(key) = existing.copy(writable = false)
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
              value.foreach(properties(key) = _)
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

  def defineNamedAccessorProperty(
      key: String,
      getter: Option[JSValue],
      setter: Option[JSValue],
      hasGetter: Boolean,
      hasSetter: Boolean,
      enumerable: Option[Boolean],
      configurable: Option[Boolean]
  ): Boolean =
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
          val existingIsAccessor = existing.isAccessor ||
            existing.getter.isDefined || existing.setter.isDefined
          if !existing.configurable then {
            if configurable.contains(true) ||
                enumerable.exists(_ != existing.enumerable) ||
                !existingIsAccessor ||
                (hasGetter && getter != existing.getter) ||
                (hasSetter && setter != existing.setter)
            then false
            else true
          }
          else {
            properties(key) = JSValue.Undefined
            propertyAttributes(key) = JSObject.PropertyAttributes(
              enumerable = enumerable.getOrElse(existing.enumerable),
              writable = false,
              configurable = configurable.getOrElse(existing.configurable),
              getter = if hasGetter then getter else if existingIsAccessor then existing.getter else None,
              setter = if hasSetter then setter else if existingIsAccessor then existing.setter else None,
              isAccessor = true
            )
            true
          }
      }

  def deleteIndex(index: Long): Boolean =
    getOwnIndexDescriptor(index) match {
      case Some((_, attrs)) if !attrs.configurable => false
      case _ =>
        if index >= 0 && index < elements.length then
          elements(index.toInt) = JSValue.Undefined
        sparseElements.remove(index)
        presentIndices.remove(index)
        if index <= Int.MaxValue then densePresent -= index.toInt
        indexAttributes.remove(index)
        true
    }

  def deleteProperty(key: String): Boolean =
    propertyAttributes.get(key) match {
      case Some(attrs) if !attrs.configurable => false
      case _ =>
        properties.remove(key)
        propertyAttributes.remove(key)
        true
    }

  def getOwnPropertyKeys: Array[String] =
    properties.keys.toArray

  def getEnumerableOwnPropertyKeys: Array[String] =
    properties.keysIterator
      .filter(key => propertyAttributes.get(key).forall(_.enumerable))
      .toArray

  def getOwnSymbol(symbolId: Int): Option[JSValue] =
    symbolProperties.get(symbolId)

  def setSymbol(symbolId: Int, value: JSValue): Unit =
    symbolProperties(symbolId) = value

  def getAllOwnSymbolPropertyIds: Array[Int] =
    symbolProperties.keysIterator.toArray

  private def truncateLength(newLength: Long): Unit = {
    val normalized = math.max(0L, newLength)
    if normalized < elements.length then {
      val denseLength = normalized.toInt
      // Remove index attributes for truncated indices
      indexAttributes.keysIterator
        .filter(_ >= normalized)
        .toList
        .foreach(indexAttributes.remove)
      sparseElements.keysIterator
        .filter(_ >= normalized)
        .toList
        .foreach(sparseElements.remove)
      presentIndices.filterInPlace(_ < normalized)
      if normalized <= Int.MaxValue then
        densePresent.filterInPlace(_ < normalized.toInt)
      elements.remove(denseLength, elements.length - denseLength)
    }
    else {
      indexAttributes.keysIterator
        .filter(_ >= normalized)
        .toList
        .foreach(indexAttributes.remove)
      sparseElements.keysIterator
        .filter(_ >= normalized)
        .toList
        .foreach(sparseElements.remove)
      presentIndices.filterInPlace(_ < normalized)
      if normalized <= Int.MaxValue then
        densePresent.filterInPlace(_ < normalized.toInt)
    }
    updateLength(normalized)
  }

  /** Ordinary assignment to Array length. */
  def setLength(newLength: Int): Boolean =
    setLength(newLength.toLong)

  def setLength(newLength: Long): Boolean =
    defineLength(Some(newLength), writable = None)

  def isLengthWritable: Boolean = lengthWritable

  def freeze(): Unit = {
    isExtensible = false
    lengthWritable = false
    getOwnIndexKeys.foreach { index =>
      val attrs = indexAttributes.getOrElse(
        index,
        JSObject.PropertyAttributes(enumerable = true)
      )
      indexAttributes(index) = attrs.copy(
        writable = if attrs.isAccessor then attrs.writable else false,
        configurable = false
      )
    }
    propertyAttributes.keys.toList.foreach { key =>
      val attrs = propertyAttributes(key)
      propertyAttributes(key) = attrs.copy(
        writable = if attrs.isAccessor then attrs.writable else false,
        configurable = false
      )
    }
  }

  def seal(): Unit = {
    isExtensible = false
    getOwnIndexKeys.foreach { index =>
      val attrs = indexAttributes.getOrElse(
        index,
        JSObject.PropertyAttributes(enumerable = true)
      )
      indexAttributes(index) = attrs.copy(configurable = false)
    }
    propertyAttributes.keys.toList.foreach { key =>
      propertyAttributes(key) = propertyAttributes(key).copy(configurable = false)
    }
  }

  def checkFrozen(): Boolean =
    !isExtensible && !lengthWritable &&
      getOwnIndexKeys.forall(index => {
        val attrs = indexAttributes.getOrElse(
          index,
          JSObject.PropertyAttributes(enumerable = true)
        )
        !attrs.configurable && (attrs.isAccessor || !attrs.writable)
      }) && propertyAttributes.values.forall(attrs =>
        !attrs.configurable && (attrs.isAccessor || !attrs.writable)
      )

  def checkSealed(): Boolean =
    !isExtensible && getOwnIndexKeys.forall(index =>
      !indexAttributes.getOrElse(
        index,
        JSObject.PropertyAttributes(enumerable = true)
      ).configurable
    ) && propertyAttributes.values.forall(!_.configurable)

  /** ArraySetLength for lengths representable by this implementation.
    * Returns false when a non-configurable element prevents shrinking or the
    * length property is non-writable.
    */
  def defineLength(
      newLength: Option[Long],
      writable: Option[Boolean]
  ): Boolean = {
    val requested = newLength.getOrElse(logicalLength)
    if !lengthWritable && requested != logicalLength then false
    else if !lengthWritable && writable.contains(true) then false
    else {
      var succeeded = true
      if requested < logicalLength then {
        val blocker = indexAttributes.iterator
          .collect { case (index, attrs) if index >= requested && !attrs.configurable => index }
          .maxOption
        blocker match {
          case Some(index) =>
            truncateLength(index + 1)
            succeeded = false
          case None => truncateLength(requested)
        }
      }
      else if requested > logicalLength then updateLength(requested)
      if writable.contains(false) then lengthWritable = false
      succeeded
    }
  }

  /** Get element at index, invoking getter if present. */
  def get(index: Int): JSValue = get(index.toLong)

  def get(index: Long): JSValue =
    indexAttributes.get(index) match {
      case Some(attrs) if attrs.getter.isDefined =>
        // Accessor property — can't invoke getter here (no JSContext)
        // Return the stored value; getter invocation handled by getProperty helpers
        readElement(index)
      case _ =>
        readElement(index)
    }

  /** Get element at index WITHOUT getter invocation (raw access). */
  def getRaw(index: Long): JSValue =
    readElement(index)

  /** Check if an index has an accessor (getter/setter). */
  def hasIndexAccessor(index: Long): Boolean =
    indexAttributes
      .get(index)
      .exists(a => a.getter.isDefined || a.setter.isDefined)

  /** Get the property attributes for an index. */
  def getIndexAttributes(index: Long): Option[JSObject.PropertyAttributes] =
    indexAttributes.get(index)

  /** Set element at index */
  def set(index: Long, value: JSValue): Unit =
    // Check for accessor setter
    indexAttributes.get(index) match {
      case Some(attrs) if attrs.setter.isDefined =>
        // Accessor with setter — can't invoke here, but store for later
        // Actual setter invocation handled by setProperty helpers
        writeElement(index, value)
      case Some(attrs) if !attrs.writable =>
        // Non-writable — silently ignore (strict mode would throw)
        ()
      case _ =>
        writeElement(index, value)
        // Update length if needed
        if index >= logicalLength then updateLength(index + 1)
    }

  /** Push element to end of array */
  def push(value: JSValue): Int = {
    writeElement(logicalLength, value)
    if logicalLength < 4294967295L then updateLength(logicalLength + 1)
    length
  }

  /** Pop element from end of array */
  def pop(): JSValue =
    if logicalLength == 0 then JSValue.Undefined
    else {
      val lastIndex = logicalLength - 1
      val result = readElement(lastIndex)
      if lastIndex < elements.length then
        elements.remove(lastIndex.toInt, elements.length - lastIndex.toInt)
      sparseElements.remove(lastIndex)
      presentIndices.remove(lastIndex)
      if lastIndex <= Int.MaxValue then densePresent -= lastIndex.toInt
      indexAttributes.remove(lastIndex)
      updateLength(lastIndex)
      result
    }

  /** Splice array in place and return removed elements as a new array. */
  def splice(start: Int, deleteCount: Int, items: Seq[JSValue]): JSArray = {
    val actualStart =
      if start < 0 then math.max(length + start, 0)
      else math.min(start, length)
    val actualDelete = math.max(0, math.min(deleteCount, length - actualStart))

    val removed = JSArray.empty()
    var i = 0
    while i < actualDelete do {
      removed.push(elements(actualStart + i))
      i += 1
    }

    if actualDelete > 0 then elements.remove(actualStart, actualDelete)
    if items.nonEmpty then elements.insertAll(actualStart, items)

    updateLength(elements.length.toLong)
    removed
  }

  /** Convert to string (JSON-like) */
  override def toString: String = {
    val contents = elements.map(_.toString).mkString(", ")
    s"[$contents]"
  }

  /** Get all elements (for pretty printing) */
  def getElements: IndexedSeq[JSValue] = elements.toIndexedSeq

  /** Get array length */
  def getLength: Int = length

  def getLengthLong: Long = logicalLength

  def getLengthValue: JSValue = JSValue.fromDouble(logicalLength.toDouble)

  def getProperty(key: String): Option[JSValue] =
    properties.get(key)
}

object JSArray {

  /** Create an empty array */
  def empty(): JSArray =
    new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, 0)

  /** Create an array with initial size */
  def apply(size: Int): JSArray = {
    val arr =
      new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, size)
    // `new Array(n)` only pre-allocates a bounded number of dense slots; larger
    // lengths stay sparse (holes), with `length` still equal to n.
    if size > 0 && size <= arr.MaxPreallocated then {
      arr.elements.sizeHint(size)
      for _ <- 0 until size do arr.elements += JSValue.Undefined
    }
    arr
  }
}
