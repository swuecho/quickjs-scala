package quickjs.objmodel

import scala.collection.mutable
import quickjs.value.JSValue

/** JavaScript Array implementation.
 *
 * Design:
 * - Arrays are objects with special indexed properties
 * - Elements stored in a mutable ArrayBuffer
 * - length property is special (auto-updated)
 * - Supports sparse arrays
 */
final class JSArray(
  private val elements: mutable.ArrayBuffer[JSValue],
  private val properties: mutable.LinkedHashMap[String, JSValue],
  var length: Int = 0,
  var isExtensible: Boolean = true
):
  def getOwnProperty(key: String): Option[JSValue] =
    properties.get(key)

  def hasIndex(index: Int): Boolean =
    index >= 0 && index < elements.length

  def setProperty(key: String, value: JSValue): Unit =
    properties(key) = value

  def getOwnPropertyKeys: Array[String] =
    properties.keys.toArray

  def setLength(newLength: Int): Unit =
    val normalized = math.max(0, newLength)
    if normalized < elements.length then
      elements.remove(normalized, elements.length - normalized)
    else if normalized > elements.length then
      elements.sizeHint(normalized)
      while elements.length < normalized do
        elements += JSValue.Undefined
    length = normalized

  /** Get element at index */
  def get(index: Int): JSValue =
    if index >= 0 && index < elements.length then
      elements(index)
    else
      JSValue.Undefined

  /** Set element at index */
  def set(index: Int, value: JSValue): Unit =
    // Extend array if needed
    if index >= elements.length then
      elements.sizeHint(index + 1)
      while elements.length <= index do
        elements += JSValue.Undefined

    elements(index) = value

    // Update length if needed
    if index >= length then
      length = index + 1

  /** Push element to end of array */
  def push(value: JSValue): Int =
    elements += value
    length = elements.length
    length

  /** Pop element from end of array */
  def pop(): JSValue =
    if elements.isEmpty then
      JSValue.Undefined
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

    if actualDelete > 0 then
      elements.remove(actualStart, actualDelete)
    if items.nonEmpty then
      elements.insertAll(actualStart, items)

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
  def empty(): JSArray = new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, 0)

  /** Create an array with initial size */
  def apply(size: Int): JSArray =
    val arr = new JSArray(mutable.ArrayBuffer.empty, mutable.LinkedHashMap.empty, size)
    arr.elements.sizeHint(size)
    for i <- 0 until size do
      arr.elements += JSValue.Undefined
    arr
