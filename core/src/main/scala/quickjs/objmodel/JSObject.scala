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
  private var properties: mutable.HashMap[String, JSValue],
  private var propertyAttributes: mutable.HashMap[String, JSObject.PropertyAttributes],
  private var prototype: JSObject | Null,
  private var extensible: Boolean
):
  // Object flags (bitfield for compactness)
  private var flags: Int = 0

  def isExtensible: Boolean = extensible
  def isFrozen: Boolean = (flags & 0x04) != 0
  def isSealed: Boolean = (flags & 0x02) != 0

  def getPrototype: JSObject | Null = prototype
  def setPrototype(proto: JSObject | Null): Unit =
    if !hasImmutablePrototype then prototype = proto

  def hasImmutablePrototype: Boolean = (flags & 0x01) != 0

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

  def get(key: String)(using ctx: JSContext): JSValue =
    properties.get(key) match
      case Some(value) => value
      case None =>
        // Look in prototype chain
        prototype match
          case null => JSValue.Undefined
          case proto => proto.get(key)

  def set(key: String, value: JSValue)(using ctx: JSContext): Boolean =
    if !isExtensible && !properties.contains(key) then false
    else
      properties(key) = value
      if !propertyAttributes.contains(key) then
        propertyAttributes(key) = JSObject.PropertyAttributes(enumerable = true)
      true

  def hasProperty(key: String)(using ctx: JSContext): Boolean =
    properties.contains(key) || (prototype != null && prototype.hasProperty(key))

  def deleteProperty(key: String)(using ctx: JSContext): Boolean =
    if !isExtensible && properties.contains(key) then false
    else
      properties.remove(key)
      propertyAttributes.remove(key)
      true

  def defineProperty(key: String, value: JSValue, enumerable: Boolean)(using ctx: JSContext): Boolean =
    if !isExtensible && !properties.contains(key) then false
    else
      properties(key) = value
      propertyAttributes(key) = JSObject.PropertyAttributes(enumerable = enumerable)
      true

  def defineAccessorProperty(
    key: String,
    getter: Option[JSValue],
    setter: Option[JSValue],
    enumerable: Boolean
  )(using ctx: JSContext): Boolean =
    if !isExtensible && !properties.contains(key) then false
    else
      properties(key) = JSValue.Undefined
      propertyAttributes(key) = JSObject.PropertyAttributes(enumerable = enumerable, getter = getter, setter = setter)
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
  def isArray: Boolean = (flags & 0x10) != 0
  def isFunction: Boolean = (flags & 0x20) != 0
  def isArguments: Boolean = (flags & 0x40) != 0
  def isConstructor: Boolean = (flags & 0x08) != 0

  // Internal helpers
  private[objmodel] def setArrayFlag(): Unit = flags |= 0x10
  private[objmodel] def setFunctionFlag(): Unit = flags |= 0x20

object JSObject:
  def apply(
    prototype: JSObject | Null = null,
    extensible: Boolean = true
  ): JSObject =
    new JSObject(
      properties = mutable.HashMap.empty,
      propertyAttributes = mutable.HashMap.empty,
      prototype = prototype,
      extensible = extensible
    )

  // Standard object creation
  def create(proto: JSObject | Null)(using ctx: JSContext): JSObject =
    JSObject(prototype = proto, extensible = true)

  def createOrdinary()(using ctx: JSContext): JSObject =
    JSObject(prototype = ctx.objectPrototype, extensible = true)

  final case class PropertyAttributes(
    enumerable: Boolean,
    getter: Option[JSValue] = None,
    setter: Option[JSValue] = None
  )
