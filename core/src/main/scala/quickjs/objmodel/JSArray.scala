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
  var length: Int = 0
):
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

  /** Convert to string (JSON-like) */
  override def toString: String =
    val contents = elements.map(_.toString).mkString(", ")
    s"[$contents]"

object JSArray:
  /** Create an empty array */
  def empty(): JSArray = new JSArray(mutable.ArrayBuffer.empty, 0)

  /** Create an array with initial size */
  def apply(size: Int): JSArray =
    val arr = new JSArray(mutable.ArrayBuffer.empty, size)
    arr.elements.sizeHint(size)
    for i <- 0 until size do
      arr.elements += JSValue.Undefined
    arr
