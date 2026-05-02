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
):
  // Property attributes for array indices (used by Object.defineProperty)
  private val indexAttributes
      : mutable.LinkedHashMap[Int, JSObject.PropertyAttributes] =
    mutable.LinkedHashMap.empty

  def getOwnProperty(key: String): Option[JSValue] =
    properties.get(key)

  /** Get own property descriptor for an index (supports getters/setters). */
  def getOwnIndexDescriptor(
      index: Int
  ): Option[(JSValue, JSObject.PropertyAttributes)] =
    indexAttributes.get(index).map { attrs =>
      val value =
        if index < elements.length then elements(index) else JSValue.Undefined
      (value, attrs)
    }

  /** Define a property on an array index with attributes. */
  def defineIndexProperty(
      index: Int,
      value: JSValue,
      enumerable: Boolean,
      writable: Boolean = true,
      configurable: Boolean = true
  ): Boolean =
    if !isExtensible && !indexAttributes.contains(index) then false
    else
      indexAttributes.get(index) match
        case Some(existing) if !existing.configurable => false
        case _                                        =>
          // Ensure element slot exists
          if index >= elements.length then
            elements.sizeHint(index + 1)
            while elements.length <= index do elements += JSValue.Undefined
          elements(index) = value
          indexAttributes(index) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = writable,
            configurable = configurable
          )
          if index >= length then length = index + 1
          true

  /** Define an accessor property on an array index. */
  def defineIndexAccessor(
      index: Int,
      getter: Option[JSValue],
      setter: Option[JSValue],
      enumerable: Boolean,
      configurable: Boolean = true
  ): Boolean =
    if !isExtensible && !indexAttributes.contains(index) then false
    else
      indexAttributes.get(index) match
        case Some(existing) if !existing.configurable => false
        case _                                        =>
          // Ensure element slot exists (store Undefined for accessor)
          if index >= elements.length then
            elements.sizeHint(index + 1)
            while elements.length <= index do elements += JSValue.Undefined
          elements(index) = JSValue.Undefined
          indexAttributes(index) = JSObject.PropertyAttributes(
            enumerable = enumerable,
            writable = false,
            configurable = configurable,
            getter = getter,
            setter = setter
          )
          if index >= length then length = index + 1
          true

  def hasIndex(index: Int): Boolean =
    index >= 0 && index < elements.length

  def setProperty(key: String, value: JSValue): Unit =
    properties(key) = value

  def getOwnPropertyKeys: Array[String] =
    properties.keys.toArray

  def setLength(newLength: Int): Unit =
    val normalized = math.max(0, newLength)
    if normalized < elements.length then
      // Remove index attributes for truncated indices
      indexAttributes.keysIterator
        .filter(_ >= normalized)
        .toList
        .foreach(indexAttributes.remove)
      elements.remove(normalized, elements.length - normalized)
    else if normalized > elements.length then
      elements.sizeHint(normalized)
      while elements.length < normalized do elements += JSValue.Undefined
    length = normalized

  /** Get element at index, invoking getter if present. */
  def get(index: Int): JSValue =
    indexAttributes.get(index) match
      case Some(attrs) if attrs.getter.isDefined =>
        // Accessor property — can't invoke getter here (no JSContext)
        // Return the stored value; getter invocation handled by getProperty helpers
        if index < elements.length then elements(index) else JSValue.Undefined
      case _ =>
        if index >= 0 && index < elements.length then elements(index)
        else JSValue.Undefined

  /** Get element at index WITHOUT getter invocation (raw access). */
  def getRaw(index: Int): JSValue =
    if index >= 0 && index < elements.length then elements(index)
    else JSValue.Undefined

  /** Check if an index has an accessor (getter/setter). */
  def hasIndexAccessor(index: Int): Boolean =
    indexAttributes
      .get(index)
      .exists(a => a.getter.isDefined || a.setter.isDefined)

  /** Get the property attributes for an index. */
  def getIndexAttributes(index: Int): Option[JSObject.PropertyAttributes] =
    indexAttributes.get(index)

  /** Set element at index */
  def set(index: Int, value: JSValue): Unit =
    // Check for accessor setter
    indexAttributes.get(index) match
      case Some(attrs) if attrs.setter.isDefined =>
        // Accessor with setter — can't invoke here, but store for later
        // Actual setter invocation handled by setProperty helpers
        if index >= elements.length then
          elements.sizeHint(index + 1)
          while elements.length <= index do elements += JSValue.Undefined
        elements(index) = value
      case Some(attrs) if !attrs.writable =>
        // Non-writable — silently ignore (strict mode would throw)
        ()
      case _ =>
        // Extend array if needed
        if index >= elements.length then
          elements.sizeHint(index + 1)
          while elements.length <= index do elements += JSValue.Undefined
        elements(index) = value
        // Update length if needed
        if index >= length then length = index + 1

  /** Push element to end of array */
  def push(value: JSValue): Int =
    elements += value
    length = elements.length
    length

  /** Pop element from end of array */
  def pop(): JSValue =
    if elements.isEmpty then JSValue.Undefined
    else
      val result = elements.last
      elements.remove(elements.length - 1)
      length = elements.length
      result

  /** Splice array in place and return removed elements as a new array. */
  def splice(start: Int, deleteCount: Int, items: Seq[JSValue]): JSArray =
    val actualStart =
      if start < 0 then math.max(length + start, 0)
      else math.min(start, length)
    val actualDelete = math.max(0, math.min(deleteCount, length - actualStart))

    val removed = JSArray.empty()
    var i = 0
    while i < actualDelete do
      removed.push(elements(actualStart + i))
      i += 1

    if actualDelete > 0 then elements.remove(actualStart, actualDelete)
    if items.nonEmpty then elements.insertAll(actualStart, items)

    length = elements.length
    removed

  /** Convert to string (JSON-like) */
  override def toString: String =
    val contents = elements.map(_.toString).mkString(", ")
    s"[$contents]"

  /** Get all elements (for pretty printing) */
  def getElements: IndexedSeq[JSValue] = elements.toIndexedSeq

  /** Get array length */
  def getLength: Int = elements.length

  def getProperty(key: String): Option[JSValue] =
    properties.get(key)

object JSArray:
  /** Create an empty array */
  def empty(): JSArray =
    new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, 0)

  /** Create an array with initial size */
  def apply(size: Int): JSArray =
    val arr =
      new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, size)
    arr.elements.sizeHint(size)
    for i <- 0 until size do arr.elements += JSValue.Undefined
    arr
