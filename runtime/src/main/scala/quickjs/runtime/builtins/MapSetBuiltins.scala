package quickjs.runtime.builtins

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers.callFunctionWithThis
import scala.collection.mutable

/** Map, Set, WeakMap, WeakSet built-ins with internal storage classes. */
object MapSetBuiltins {
  import quickjs.objmodel.JSObject

  // ============================================================
  // Iterator Protocol Helper
  // ============================================================

  /** Maximum safe iterations (loop guard to prevent infinite iteration).
    * ECMAScript spec uses 2^53-1.
    */
  private final val MAX_SAFE_ITERATIONS = 9007199254740991L

  /** Get the @@iterator symbol id from the Symbol constructor. */
  private def getIteratorSymbolId(using ctx: JSContext): Int =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get("iterator")(using ctx) match {
          case JSValue.Symbol(id) => id
          case _ => ctx.throwTypeError("Symbol.iterator not available")
        }
      case _ => ctx.throwTypeError("Symbol not available")
    }

  /** Call iterator.return() if it exists (IteratorClose). */
  private def iteratorClose(iterator: JSValue)(using ctx: JSContext): Unit =
    try {
      val returnMethod = iterator match {
        case JSValue.Object(obj) =>
          obj.get("return")(using ctx)
        case _ => JSValue.Undefined
      }
      if returnMethod != JSValue.Undefined then
        callFunctionWithThis(returnMethod, iterator, Array.empty)
    }
    catch {
      case _: Exception => // Suppress errors from return() per spec
    }

  /** Call a named method on an object, throw TypeError if not found. */
  private def getMethod(obj: JSValue, name: String)(using
      ctx: JSContext
  ): JSValue =
    obj match {
      case JSValue.Object(o) =>
        val m = o.get(name)(using ctx)
        if m == JSValue.Undefined then
          ctx.throwTypeError(s"$name is not a function")
        m
      case _ => ctx.throwTypeError(s"Cannot read properties of non-object")
    }

  /** ES [[Get]](O, P) — property lookup with getter invocation. Walks the
    * prototype chain and invokes getters if present.
    */
  private def getProperty(obj: JSValue, key: String)(using
      ctx: JSContext
  ): JSValue =
    obj match {
      case JSValue.Object(o)       => getPropertyFromObject(o, key)
      case JSValue.JSArrayVal(arr) =>
        // Check if key is an array index with accessor
        if isArrayIndexKey(key) then {
          val idx = key.toInt
          arr.getIndexAttributes(idx) match {
            case Some(attrs) if attrs.getter.isDefined =>
              // Accessor property — invoke the getter with the array as this
              callFunctionWithThis(
                attrs.getter.get,
                JSValue.JSArrayVal(arr),
                Array.empty
              )
            case Some(_) =>
              // Data property with attributes
              if idx < arr.getLength then arr.getRaw(idx) else JSValue.Undefined
            case None =>
              // Plain element
              if idx < arr.getLength then arr.getRaw(idx) else JSValue.Undefined
          }
        }
        else
          // Named property: check array's own properties first, then Array.prototype
          arr.getOwnProperty(key) match {
            case Some(value) => value
            case None        => getPropertyFromObject(ctx.arrayPrototype, key)
          }
      case JSValue.Native(nf: quickjs.value.NativeFunction) =>
        getPropertyFromObject(nf.funcObj, key)
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        getPropertyFromObject(nc.funcObj, key)
      case _ => JSValue.Undefined
    }

  private def getPropertyFromObject(o: JSObject, key: String)(using
      ctx: JSContext
  ): JSValue =
    o.getPropertyDescriptorWithOwner(key) match {
      case Some((owner, value, attrs)) =>
        attrs.getter match {
          case Some(getter) =>
            // Accessor property — invoke the getter
            callFunctionWithThis(getter, JSValue.Object(owner), Array.empty)
          case None => value
        }
      case None => JSValue.Undefined
    }

  /** Check if a string key is an array index (non-negative integer). */
  private def isArrayIndexKey(key: String): Boolean =
    key.nonEmpty && key
      .forall(_.isDigit) && (key.length == 1 || key.charAt(0) != '0')

  /** Iterate using the ES iterator protocol, calling adder for each item.
    * Implements IteratorClose on error and loop guard. Falls back to direct
    * array iteration for JSArrayVal.
    *
    * @param iterable
    *   the iterable to iterate over
    * @param thisObj
    *   the map/set object (this for the adder)
    * @param adder
    *   the prototype adder method (Map.prototype.set, WeakMap.prototype.set,
    *   WeakSet.prototype.add)
    * @param isMap
    *   true for Map/WeakMap (expects [key, value] entries), false for WeakSet
    *   (expects single values)
    */
  private def iterateWithAdder(
      iterable: JSValue,
      thisObj: JSValue,
      adder: JSValue,
      isMap: Boolean
  )(using ctx: JSContext): Unit =
    // Handle JSArrayVal directly (Array.prototype[@@iterator] not yet wired up)
    iterable match {
      case JSValue.JSArrayVal(arr) =>
        var i = 0
        while i < arr.getLength do {
          val item = arr.get(i)
          if isMap then {
            // Map/WeakMap: entry must be an object with 0/1 keys
            if !item.isObject then
              ctx.throwTypeError("Iterator value is not an entry object")
            val key = item match {
              case JSValue.Object(entryObj)     => entryObj.get("0")(using ctx)
              case JSValue.JSArrayVal(entryArr) =>
                if entryArr.getLength > 0 then entryArr.get(0)
                else JSValue.Undefined
              case _ => JSValue.Undefined
            }
            val value = item match {
              case JSValue.Object(entryObj)     => entryObj.get("1")(using ctx)
              case JSValue.JSArrayVal(entryArr) =>
                if entryArr.getLength > 1 then entryArr.get(1)
                else JSValue.Undefined
              case _ => JSValue.Undefined
            }
            callFunctionWithThis(adder, thisObj, Array(key, value))
          }
          else
            // WeakSet: item is the value to add
            callFunctionWithThis(adder, thisObj, Array(item))
          i += 1
        }

      case _ =>
        // Use ES iterator protocol for objects
        val symId = getIteratorSymbolId
        val iteratorMethod = iterable match {
          case JSValue.Object(o) => o.getSymbol(symId)
          case _                 => JSValue.Undefined
        }

        if iteratorMethod == JSValue.Undefined then
          ctx.throwTypeError("iterable is not iterable")

        // 2. Get iterator by calling @@iterator
        val iterator =
          callFunctionWithThis(iteratorMethod, iterable, Array.empty)

        // 3. Loop
        var loopCount = 0L
        try {
          while loopCount < MAX_SAFE_ITERATIONS do {
            // Call next()
            val nextMethod = getMethod(iterator, "next")
            val nextResult =
              callFunctionWithThis(nextMethod, iterator, Array.empty)

            // Check done (use getProperty to invoke getters)
            val done = getProperty(nextResult, "done").toBoolean

            if done then return

            // Get value (use getProperty to invoke getters)
            val item = getProperty(nextResult, "value")

            if isMap then {
              // For Map/WeakMap: item must be an object with "0" and "1" keys (key-value pair)
              if !item.isObject then
                // Throw TypeError — catch clause will call IteratorClose
                ctx.throwTypeError("Iterator value is not an entry object")

              // Use getProperty to properly invoke getters on key/value
              val key = getProperty(item, "0")
              val value = getProperty(item, "1")
              callFunctionWithThis(adder, thisObj, Array(key, value))
            }
            else
              // For WeakSet: item is the value to add directly
              callFunctionWithThis(adder, thisObj, Array(item))

            loopCount += 1
          }

          // Loop guard exceeded
          iteratorClose(iterator)
          ctx.throwError("RangeError", "Maximum iteration count exceeded")
        }
        catch {
          case e: Exception =>
            iteratorClose(iterator)
            throw e
        }
    }

  // ============================================================
  // Map Implementation
  // ============================================================

  /** Internal storage class for Map - uses AnyRef wrapper for proper key
    * comparison
    */
  private final class JSMapStorage {
    private val storage = mutable.LinkedHashMap.empty[MapKey, JSValue]

    def get(key: JSValue): Option[JSValue] = storage.get(MapKey(key))
    def set(key: JSValue, value: JSValue): Unit =
      storage.update(MapKey(key), value)
    def has(key: JSValue): Boolean = storage.contains(MapKey(key))
    def delete(key: JSValue): Boolean = {
      val k = MapKey(key)
      if storage.contains(k) then {
        storage.remove(k)
        true
      }
      else false
    }
    def clear(): Unit = storage.clear()
    def size: Int = storage.size
    def entries: Iterator[(JSValue, JSValue)] = storage.iterator.map {
      case (k, v) => (k.value, v)
    }
    def keys: Iterator[JSValue] = storage.keysIterator.map(_.value)
    def values: Iterator[JSValue] = storage.valuesIterator
  }

  /** Wrapper for Map keys that implements SameValueZero comparison */
  private final case class MapKey(value: JSValue) {
    override def hashCode(): Int = value match {
      case JSValue.Float64(d) if d.isNaN => 0 // All NaN values hash the same
      case JSValue.Float64(0.0)          => 0 // +0 and -0 hash the same
      case JSValue.Int32(0)              => 0
      case JSValue.Float64(d)            => java.lang.Double.hashCode(d)
      case JSValue.Int32(i)              =>
        java.lang.Double.hashCode(i.toDouble)
      case JSValue.Object(obj)           => System.identityHashCode(obj)
      case JSValue.JSArrayVal(arr)       => System.identityHashCode(arr)
      case f: JSValue.Function           => System.identityHashCode(f)
      case JSValue.Native(n)             => System.identityHashCode(n)
      case _                             => value.hashCode()
    }

    override def equals(other: Any): Boolean = other match {
      case MapKey(otherValue) => sameValueZero(value, otherValue)
      case _                  => false
    }

    private def sameValueZero(a: JSValue, b: JSValue): Boolean = (a, b) match {
      case (JSValue.Float64(x), JSValue.Float64(y)) if x.isNaN && y.isNaN =>
        true
      case (JSValue.Float64(x), JSValue.Float64(y))       => x == y
      case (JSValue.Int32(x), JSValue.Int32(y))           => x == y
      case (JSValue.Int32(x), JSValue.Float64(y))         => x.toDouble == y
      case (JSValue.Float64(x), JSValue.Int32(y))         => x == y.toDouble
      case (JSValue.Object(x), JSValue.Object(y))         => x eq y
      case (JSValue.JSArrayVal(x), JSValue.JSArrayVal(y)) => x eq y
      case (x: JSValue.Function, y: JSValue.Function)     => x eq y
      case (JSValue.Native(x), JSValue.Native(y))         => x eq y
      case _                                              => a == b
    }
  }

  private def getMapStorage(obj: JSObject)(using
      ctx: JSContext
  ): Option[JSMapStorage] =
    obj.getOwnProperty("__mapStorage") match {
      case Some(JSValue.Native(storage: JSMapStorage)) => Some(storage)
      case _                                           => None
    }

  /** Get a well-known symbol from the Symbol constructor */
  private def getWellKnownSymbol(name: String)(using
      ctx: quickjs.runtime.JSContext
  ): JSValue =
    ctx.global.get("Symbol") match {
      case JSValue.Native(nc: quickjs.value.NativeConstructor) =>
        nc.funcObj.get(name)(using ctx)
      case _ => JSValue.Undefined
    }

  private def initializeMap(ctx: JSContext): Unit = {
    given JSContext = ctx
    val symToStringTag = getWellKnownSymbol("toStringTag")
    val symSpecies = getWellKnownSymbol("species")

    val mapConstructor = quickjs.value.NativeConstructor(
      name = "Map",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Map requires 'new'")
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = JSObject(prototype = ctx.mapPrototype, extensible = true)
        val storage = new JSMapStorage()
        obj.defineProperty(
          "__mapStorage",
          JSValue.Native(storage),
          enumerable = false,
          writable = false,
          configurable = false
        )

        // If iterable argument is provided and not null/undefined, iterate using @@iterator protocol
        if args.nonEmpty && args(0) != JSValue.Null && args(
            0
          ) != JSValue.Undefined
        then {
          // Get the adder from Map.prototype using proper [[Get]] (invokes getters)
          val adder = getProperty(JSValue.Object(obj), "set")
          // Check IsCallable
          adder match {
            case _: (JSValue.Function | JSValue.Native) => // callable
            case _ => ctx.throwTypeError("set is not a function")
          }

          iterateWithAdder(args(0), JSValue.Object(obj), adder, isMap = true)
        }

        JSValue.Object(obj)
      ,
      prototype = ctx.mapPrototype
    )
    BuiltinHelpers.initConstructor(mapConstructor, length = 0)
    ctx.global.defineProperty(
      "Map",
      JSValue.Native(mapConstructor),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "constructor",
      JSValue.Native(mapConstructor),
      enumerable = false
    )

    // Map.prototype.get(key)
    val mapGet = NativeFunction(
      name = "get",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                storage.get(key).getOrElse(JSValue.Undefined)
              case None =>
                ctx.throwTypeError("get method called on non-Map object")
            }
          case _ => ctx.throwTypeError("get method called on non-Map object")
        }
    )

    // Map.prototype.set(key, value)
    val mapSet = NativeFunction(
      name = "set",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                val value =
                  if args.length > 2 then args(2) else JSValue.Undefined
                storage.set(key, value)
                JSValue.Object(obj)
              case None =>
                ctx.throwTypeError("set method called on non-Map object")
            }
          case _ => ctx.throwTypeError("set method called on non-Map object")
        }
    )

    // Map.prototype.has(key)
    val mapHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.has(key))
              case None =>
                ctx.throwTypeError("has method called on non-Map object")
            }
          case _ => ctx.throwTypeError("has method called on non-Map object")
        }
    )

    // Map.prototype.delete(key)
    val mapDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                val key = if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.delete(key))
              case None =>
                ctx.throwTypeError("delete method called on non-Map object")
            }
          case _ => ctx.throwTypeError("delete method called on non-Map object")
        }
    )

    // Map.prototype.clear()
    val mapClear = NativeFunction(
      name = "clear",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                storage.clear()
                JSValue.Undefined
              case None =>
                ctx.throwTypeError("clear method called on non-Map object")
            }
          case _ => ctx.throwTypeError("clear method called on non-Map object")
        }
    )

    // Map.prototype.size (getter)
    val mapSizeGetter = NativeFunction(
      name = "get size",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) => JSValue.fromInt(storage.size)
              case None          =>
                ctx.throwTypeError("size getter called on non-Map object")
            }
          case _ => ctx.throwTypeError("size getter called on non-Map object")
        }
    )

    // Map.prototype.forEach(callback, thisArg)
    val mapForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getMapStorage(obj) match {
              case Some(storage) =>
                val callback =
                  if args.length > 1 then args(1) else JSValue.Undefined
                val thisArg =
                  if args.length > 2 then args(2) else JSValue.Undefined
                storage.entries.foreach { case (key, value) =>
                  callFunctionWithThis(
                    callback,
                    thisArg,
                    Array(value, key, JSValue.Object(obj))
                  )
                }
                JSValue.Undefined
              case None =>
                ctx.throwTypeError("forEach method called on non-Map object")
            }
          case _ =>
            ctx.throwTypeError("forEach method called on non-Map object")
        }
    )

    // Map.prototype.keys()
    val mapIteratorPrototype =
      createIteratorPrototype(ctx, "Map Iterator", mapIteratorNext())

    val mapKeys = NativeFunction(
      name = "keys",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) if getMapStorage(obj).isDefined =>
            createMapIterator(JSValue.Object(obj), "key", mapIteratorPrototype)
          case _ => ctx.throwTypeError("keys method called on non-Map object")
        }
    )

    // Map.prototype.values()
    val mapValues = NativeFunction(
      name = "values",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) if getMapStorage(obj).isDefined =>
            createMapIterator(
              JSValue.Object(obj),
              "value",
              mapIteratorPrototype
            )
          case _ => ctx.throwTypeError("values method called on non-Map object")
        }
    )

    // Map.prototype.entries()
    val mapEntries = NativeFunction(
      name = "entries",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) if getMapStorage(obj).isDefined =>
            createMapIterator(
              JSValue.Object(obj),
              "entry",
              mapIteratorPrototype
            )
          case _ => ctx.throwTypeError("entries method called on non-Map object")
        }
    )

    ctx.mapPrototype.defineProperty(
      "get",
      JSValue.Native(mapGet),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "set",
      JSValue.Native(mapSet),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "has",
      JSValue.Native(mapHas),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "delete",
      JSValue.Native(mapDelete),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "clear",
      JSValue.Native(mapClear),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "forEach",
      JSValue.Native(mapForEach),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "keys",
      JSValue.Native(mapKeys),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "values",
      JSValue.Native(mapValues),
      enumerable = false
    )
    ctx.mapPrototype.defineProperty(
      "entries",
      JSValue.Native(mapEntries),
      enumerable = false
    )
    ctx.mapPrototype.defineAccessorProperty(
      "size",
      getter = Some(JSValue.Native(mapSizeGetter)),
      setter = None,
      enumerable = false,
      configurable = true
    )

    // Symbol.toStringTag = "Map"
    symToStringTag match {
      case sym: JSValue.Symbol =>
        ctx.mapPrototype.initSymbolProperty(
          sym.value,
          JSValue.fromString("Map"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

    // Symbol.iterator = Map.prototype.entries
    val mapIteratorSym = getWellKnownSymbol("iterator")
    mapIteratorSym match {
      case sym: JSValue.Symbol =>
        ctx.mapPrototype.initSymbolProperty(
          sym.value,
          JSValue.Native(mapEntries),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }

    // Symbol.species getter returning this
    symSpecies match {
      case sym: JSValue.Symbol =>
        val speciesGetter = NativeFunction(
          name = "get [Symbol.species]",
          length = 0,
          impl = (args, ctx) => args(0)
        )
        mapConstructor.funcObj.defineSymbolAccessorProperty(
          sym.value,
          getter = Some(JSValue.Native(speciesGetter)),
          setter = None,
          enumerable = false,
          configurable = true
        )
      case _ => ()
    }
  }

  // ============================================================
  // WeakMap Implementation
  // ============================================================

  /** Internal storage class for WeakMap - uses WeakHashMap with object identity
    */
  private final class JSWeakMapStorage {
    private val storage = java.util.WeakHashMap[WeakObjectKey, JSValue]()
    private val symbolStorage = mutable.HashMap.empty[Int, JSValue]

    def get(key: JSValue): Option[JSValue] = key match {
      case JSValue.Object(obj) =>
        Option(storage.get(WeakObjectKey(obj)))
      case JSValue.JSArrayVal(arr) =>
        Option(storage.get(WeakObjectKey(arr)))
      case f: JSValue.Function =>
        Option(storage.get(WeakObjectKey(f)))
      case JSValue.Native(n) =>
        Option(storage.get(WeakObjectKey(n)))
      case JSValue.Symbol(id) => symbolStorage.get(id)
      case _ => None
    }

    def set(key: JSValue, value: JSValue): Boolean =
      key match {
        case JSValue.Object(obj) =>
          storage.put(WeakObjectKey(obj), value)
          true
        case JSValue.JSArrayVal(arr) =>
          storage.put(WeakObjectKey(arr), value)
          true
        case f: JSValue.Function =>
          storage.put(WeakObjectKey(f), value)
          true
        case JSValue.Native(n) =>
          storage.put(WeakObjectKey(n), value)
          true
        case JSValue.Symbol(id) =>
          symbolStorage(id) = value
          true
        case _ => false
      }

    def has(key: JSValue): Boolean =
      key match {
        case JSValue.Object(obj)     => storage.containsKey(WeakObjectKey(obj))
        case JSValue.JSArrayVal(arr) => storage.containsKey(WeakObjectKey(arr))
        case f: JSValue.Function     => storage.containsKey(WeakObjectKey(f))
        case JSValue.Native(n)       => storage.containsKey(WeakObjectKey(n))
        case JSValue.Symbol(id)      => symbolStorage.contains(id)
        case _                       => false
      }

    def delete(key: JSValue): Boolean =
      key match {
        case JSValue.Object(obj) =>
          storage.remove(WeakObjectKey(obj)) != null
        case JSValue.JSArrayVal(arr) =>
          storage.remove(WeakObjectKey(arr)) != null
        case f: JSValue.Function =>
          storage.remove(WeakObjectKey(f)) != null
        case JSValue.Native(n) =>
          storage.remove(WeakObjectKey(n)) != null
        case JSValue.Symbol(id) =>
          symbolStorage.remove(id).isDefined
        case _ => false
      }
  }

  /** Wrapper for weak references that uses object identity */
  private final class WeakObjectKey(val obj: AnyRef) {
    override def hashCode(): Int = System.identityHashCode(obj)
    override def equals(other: Any): Boolean = other match {
      case that: WeakObjectKey => this.obj eq that.obj
      case _                   => false
    }
  }

  private def getWeakMapStorage(obj: JSObject)(using
      ctx: JSContext
  ): Option[JSWeakMapStorage] =
    obj.getOwnProperty("__weakMapStorage") match {
      case Some(JSValue.Native(storage: JSWeakMapStorage)) => Some(storage)
      case _                                               => None
    }

  private def initializeWeakMap(ctx: JSContext): Unit = {
    given JSContext = ctx
    val symToStringTag = getWellKnownSymbol("toStringTag")

    val weakMapConstructor = quickjs.value.NativeConstructor(
      name = "WeakMap",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor WeakMap requires 'new'")
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = JSObject(prototype = ctx.weakMapPrototype, extensible = true)
        val storage = new JSWeakMapStorage()
        obj.defineProperty(
          "__weakMapStorage",
          JSValue.Native(storage),
          enumerable = false,
          writable = false,
          configurable = false
        )

        // If iterable argument is provided and not null/undefined, iterate using @@iterator protocol
        if args.nonEmpty && args(0) != JSValue.Null && args(
            0
          ) != JSValue.Undefined
        then {
          // Get the adder from WeakMap.prototype using proper [[Get]] (invokes getters)
          val adder = getProperty(JSValue.Object(obj), "set")
          // Check IsCallable
          adder match {
            case _: (JSValue.Function | JSValue.Native) => // callable
            case _ => ctx.throwTypeError("set is not a function")
          }

          iterateWithAdder(args(0), JSValue.Object(obj), adder, isMap = true)
        }

        JSValue.Object(obj)
      ,
      prototype = ctx.weakMapPrototype
    )
    BuiltinHelpers.initConstructor(weakMapConstructor, length = 0)
    ctx.global.defineProperty(
      "WeakMap",
      JSValue.Native(weakMapConstructor),
      enumerable = false
    )
    ctx.weakMapPrototype.defineProperty(
      "constructor",
      JSValue.Native(weakMapConstructor),
      enumerable = false
    )

    // WeakMap.prototype.get(key)
    val weakMapGet = NativeFunction(
      name = "get",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match {
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                storage.get(key).getOrElse(JSValue.Undefined)
              case None =>
                ctx.throwTypeError("get called on incompatible WeakMap")
            }
          case _ => ctx.throwTypeError("get called on incompatible object")
        }
    )
    ctx.weakMapPrototype.defineProperty(
      "get",
      JSValue.Native(weakMapGet),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // WeakMap.prototype.set(key, value)
    val weakMapSet = NativeFunction(
      name = "set",
      length = 2,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match {
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                val value = args.lift(2).getOrElse(JSValue.Undefined)
                if storage.set(key, value) then args.head
                else ctx.throwTypeError("Invalid value used as weak map key")
              case None =>
                ctx.throwTypeError("set called on incompatible WeakMap")
            }
          case _ => ctx.throwTypeError("set called on incompatible object")
        }
    )
    ctx.weakMapPrototype.defineProperty(
      "set",
      JSValue.Native(weakMapSet),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // WeakMap.prototype.has(key)
    val weakMapHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match {
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.has(key))
              case None =>
                ctx.throwTypeError("has called on incompatible WeakMap")
            }
          case _ => ctx.throwTypeError("has called on incompatible object")
        }
    )
    ctx.weakMapPrototype.defineProperty(
      "has",
      JSValue.Native(weakMapHas),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // WeakMap.prototype.delete(key)
    val weakMapDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakMapStorage(obj) match {
              case Some(storage) =>
                val key = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.delete(key))
              case None =>
                ctx.throwTypeError("delete called on incompatible WeakMap")
            }
          case _ => ctx.throwTypeError("delete called on incompatible object")
        }
    )
    ctx.weakMapPrototype.defineProperty(
      "delete",
      JSValue.Native(weakMapDelete),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // Symbol.toStringTag = "WeakMap"
    symToStringTag match {
      case sym: JSValue.Symbol =>
        ctx.weakMapPrototype.initSymbolProperty(
          sym.value,
          JSValue.fromString("WeakMap"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }
  }

  // ============================================================
  // Set Implementation
  // ============================================================

  /** Internal storage class for Set */
  private final class JSSetStorage {
    private val storage = mutable.LinkedHashSet.empty[MapKey]

    def add(value: JSValue): Unit = storage.add(MapKey(value))
    def has(value: JSValue): Boolean = storage.contains(MapKey(value))
    def delete(value: JSValue): Boolean = storage.remove(MapKey(value))
    def clear(): Unit = storage.clear()
    def size: Int = storage.size
    def values: Iterator[JSValue] = storage.iterator.map(_.value)
  }

  private def getSetStorage(obj: JSObject)(using
      ctx: JSContext
  ): Option[JSSetStorage] =
    obj.getOwnProperty("__setStorage") match {
      case Some(JSValue.Native(storage: JSSetStorage)) => Some(storage)
      case _                                           => None
    }

  // ============================================================
  // Iterator objects
  // ============================================================

  private final class MapIteratorSnapshot(
      val entries: Vector[(JSValue, JSValue)],
      val size: Int
  )
  private final class SetIteratorSnapshot(
      val values: Vector[JSValue],
      val size: Int
  )

  /** `{ value, done }` result object for `next()`. */
  private def iteratorResult(value: JSValue, done: Boolean)(using
      ctx: JSContext
  ): JSValue = {
    val obj = JSObject(prototype = ctx.objectPrototype)
    obj.defineProperty(
      "value",
      value,
      enumerable = true,
      writable = true,
      configurable = true
    )
    obj.defineProperty(
      "done",
      JSValue.Bool(done),
      enumerable = true,
      writable = true,
      configurable = true
    )
    JSValue.Object(obj)
  }

  /** Build a `%MapIteratorPrototype%` / `%SetIteratorPrototype%` with `next`,
    * `@@iterator` and `@@toStringTag`.
    */
  private def createIteratorPrototype(
      ctx: JSContext,
      tag: String,
      next: NativeFunction
  ): JSObject = {
    given JSContext = ctx
    val proto = JSObject(prototype = ctx.objectPrototype)
    proto.defineProperty("next", JSValue.Native(next), enumerable = false)
    getWellKnownSymbol("iterator") match {
      case sym: JSValue.Symbol =>
        val self = NativeFunction(
          name = "[Symbol.iterator]",
          length = 0,
          impl = (args, _) =>
            args.headOption.getOrElse(JSValue.Undefined)
        )
        proto.initSymbolProperty(
          sym.value,
          JSValue.Native(self),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }
    getWellKnownSymbol("toStringTag") match {
      case sym: JSValue.Symbol =>
        proto.initSymbolProperty(
          sym.value,
          JSValue.fromString(tag),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }
    proto
  }

  private def createMapIterator(
      target: JSValue,
      kind: String,
      proto: JSObject
  )(using ctx: JSContext): JSValue = {
    val it = JSObject(prototype = proto)
    it.initProperty(
      "__mapIteratorTarget",
      target,
      enumerable = false,
      writable = false,
      configurable = false
    )
    it.initProperty(
      "__mapIteratorKind",
      JSValue.fromString(kind),
      enumerable = false,
      writable = false,
      configurable = false
    )
    it.initProperty(
      "__mapIteratorIndex",
      JSValue.Int32(0),
      enumerable = false,
      writable = true,
      configurable = false
    )
    JSValue.Object(it)
  }

  private def createSetIterator(
      target: JSValue,
      kind: String,
      proto: JSObject
  )(using ctx: JSContext): JSValue = {
    val it = JSObject(prototype = proto)
    it.initProperty(
      "__setIteratorTarget",
      target,
      enumerable = false,
      writable = false,
      configurable = false
    )
    it.initProperty(
      "__setIteratorKind",
      JSValue.fromString(kind),
      enumerable = false,
      writable = false,
      configurable = false
    )
    it.initProperty(
      "__setIteratorIndex",
      JSValue.Int32(0),
      enumerable = false,
      writable = true,
      configurable = false
    )
    JSValue.Object(it)
  }

  private def mapIteratorNext(): NativeFunction =
    NativeFunction(
      name = "next",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(it))
              if it.getOwnProperty("__mapIteratorTarget").isDefined =>
            val target = it.getOwnProperty("__mapIteratorTarget")
            val kind =
              it.getOwnProperty("__mapIteratorKind")
                .map(_.toString)
                .getOrElse("value")
            val index = it
              .getOwnProperty("__mapIteratorIndex")
              .map(_.toNumber.toInt)
              .getOrElse(0)
            val storage = target.flatMap {
              case JSValue.Object(o) => getMapStorage(o)
              case _                 => None
            }
            storage match {
              case Some(s) =>
                val entries =
                  it.getOwnProperty("__mapIteratorSnapshot") match {
                    case Some(JSValue.Native(snap: MapIteratorSnapshot))
                        if snap.size == s.size =>
                      snap.entries
                    case _ =>
                      val v = s.entries.toVector
                      it.initProperty(
                        "__mapIteratorSnapshot",
                        JSValue.Native(new MapIteratorSnapshot(v, s.size)),
                        enumerable = false,
                        writable = true,
                        configurable = false
                      )
                      v
                  }
                if index >= entries.length then
                  iteratorResult(JSValue.Undefined, done = true)
                else {
                  it.set("__mapIteratorIndex", JSValue.fromInt(index + 1))
                  val (k, v) = entries(index)
                  val value = kind match {
                    case "key" => k
                    case "entry" =>
                      val pair = quickjs.objmodel.JSArray.empty()
                      pair.push(k)
                      pair.push(v)
                      JSValue.JSArrayVal(pair)
                    case _ => v
                  }
                  iteratorResult(value, done = false)
                }
              case None => iteratorResult(JSValue.Undefined, done = true)
            }
          case _ =>
            ctx.throwTypeError(
              "Map Iterator.prototype.next called on incompatible receiver"
            )
        }
    )

  private def setIteratorNext(): NativeFunction =
    NativeFunction(
      name = "next",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(it))
              if it.getOwnProperty("__setIteratorTarget").isDefined =>
            val target = it.getOwnProperty("__setIteratorTarget")
            val kind =
              it.getOwnProperty("__setIteratorKind")
                .map(_.toString)
                .getOrElse("value")
            val index = it
              .getOwnProperty("__setIteratorIndex")
              .map(_.toNumber.toInt)
              .getOrElse(0)
            val storage = target.flatMap {
              case JSValue.Object(o) => getSetStorage(o)
              case _                 => None
            }
            storage match {
              case Some(s) =>
                val values = it.getOwnProperty("__setIteratorSnapshot") match {
                  case Some(JSValue.Native(snap: SetIteratorSnapshot))
                      if snap.size == s.size =>
                    snap.values
                  case _ =>
                    val v = s.values.toVector
                    it.initProperty(
                      "__setIteratorSnapshot",
                      JSValue.Native(new SetIteratorSnapshot(v, s.size)),
                      enumerable = false,
                      writable = true,
                      configurable = false
                    )
                    v
                }
                if index >= values.length then
                  iteratorResult(JSValue.Undefined, done = true)
                else {
                  it.set("__setIteratorIndex", JSValue.fromInt(index + 1))
                  val v = values(index)
                  val value = kind match {
                    case "entry" =>
                      val pair = quickjs.objmodel.JSArray.empty()
                      pair.push(v)
                      pair.push(v)
                      JSValue.JSArrayVal(pair)
                    case _ => v
                  }
                  iteratorResult(value, done = false)
                }
              case None => iteratorResult(JSValue.Undefined, done = true)
            }
          case _ =>
            ctx.throwTypeError(
              "Set Iterator.prototype.next called on incompatible receiver"
            )
        }
    )

  private def initializeSet(ctx: JSContext): Unit = {
    given JSContext = ctx
    val symToStringTag = getWellKnownSymbol("toStringTag")
    val symSpecies = getWellKnownSymbol("species")

    val setConstructor = quickjs.value.NativeConstructor(
      name = "Set",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor Set requires 'new'")
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = JSObject(prototype = ctx.setPrototype, extensible = true)
        val storage = new JSSetStorage()
        obj.defineProperty(
          "__setStorage",
          JSValue.Native(storage),
          enumerable = false,
          writable = false,
          configurable = false
        )

        if args.nonEmpty && args(0) != JSValue.Null && args(
            0
          ) != JSValue.Undefined
        then
          args(0) match {
            case JSValue.JSArrayVal(arr) =>
              var i = 0
              while i < arr.getLength do {
                storage.add(arr.get(i))
                i += 1
              }
            case JSValue.JSStr(str) =>
              var i = 0
              while i < str.length do {
                storage.add(JSValue.fromString(str.charAt(i).toString))
                i += 1
              }
            case JSValue.Object(iterObj) =>
              val len = iterObj.get("length").toNumber.toInt
              var i = 0
              while i < len do {
                storage.add(iterObj.get(i.toString))
                i += 1
              }
            case _ => ()
          }

        JSValue.Object(obj)
      ,
      prototype = ctx.setPrototype
    )
    BuiltinHelpers.initConstructor(setConstructor, length = 0)
    ctx.global.defineProperty(
      "Set",
      JSValue.Native(setConstructor),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "constructor",
      JSValue.Native(setConstructor),
      enumerable = false
    )

    // Set.prototype.add(value)
    val setAdd = NativeFunction(
      name = "add",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) =>
                val value =
                  if args.length > 1 then args(1) else JSValue.Undefined
                storage.add(value)
                JSValue.Object(obj)
              case None =>
                ctx.throwTypeError("add method called on non-Set object")
            }
          case _ => ctx.throwTypeError("add method called on non-Set object")
        }
    )

    // Set.prototype.has(value)
    val setHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) =>
                val value =
                  if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.has(value))
              case None =>
                ctx.throwTypeError("has method called on non-Set object")
            }
          case _ => ctx.throwTypeError("has method called on non-Set object")
        }
    )

    // Set.prototype.delete(value)
    val setDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) =>
                val value =
                  if args.length > 1 then args(1) else JSValue.Undefined
                JSValue.Bool(storage.delete(value))
              case None =>
                ctx.throwTypeError("delete method called on non-Set object")
            }
          case _ => ctx.throwTypeError("delete method called on non-Set object")
        }
    )

    // Set.prototype.clear()
    val setClear = NativeFunction(
      name = "clear",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) =>
                storage.clear()
                JSValue.Undefined
              case None =>
                ctx.throwTypeError("clear method called on non-Set object")
            }
          case _ => ctx.throwTypeError("clear method called on non-Set object")
        }
    )

    // Set.prototype.size (getter)
    val setSizeGetter = NativeFunction(
      name = "get size",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) => JSValue.fromInt(storage.size)
              case None          =>
                ctx.throwTypeError("size getter called on non-Set object")
            }
          case _ => ctx.throwTypeError("size getter called on non-Set object")
        }
    )

    // Set.prototype.forEach(callback, thisArg)
    val setForEach = NativeFunction(
      name = "forEach",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getSetStorage(obj) match {
              case Some(storage) =>
                val callback =
                  if args.length > 1 then args(1) else JSValue.Undefined
                val thisArg =
                  if args.length > 2 then args(2) else JSValue.Undefined
                storage.values.foreach { value =>
                  callFunctionWithThis(
                    callback,
                    thisArg,
                    Array(value, value, JSValue.Object(obj))
                  )
                }
                JSValue.Undefined
              case None =>
                ctx.throwTypeError("forEach method called on non-Set object")
            }
          case _ =>
            ctx.throwTypeError("forEach method called on non-Set object")
        }
    )

    // Set.prototype.values() - also aliased as keys()
    val setIteratorPrototype =
      createIteratorPrototype(ctx, "Set Iterator", setIteratorNext())

    val setValues = NativeFunction(
      name = "values",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) if getSetStorage(obj).isDefined =>
            createSetIterator(
              JSValue.Object(obj),
              "value",
              setIteratorPrototype
            )
          case _ => ctx.throwTypeError("values method called on non-Set object")
        }
    )

    // Set.prototype.entries()
    val setEntries = NativeFunction(
      name = "entries",
      length = 0,
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) if getSetStorage(obj).isDefined =>
            createSetIterator(
              JSValue.Object(obj),
              "entry",
              setIteratorPrototype
            )
          case _ => ctx.throwTypeError("entries method called on non-Set object")
        }
    )

    ctx.setPrototype.defineProperty(
      "add",
      JSValue.Native(setAdd),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "has",
      JSValue.Native(setHas),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "delete",
      JSValue.Native(setDelete),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "clear",
      JSValue.Native(setClear),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "forEach",
      JSValue.Native(setForEach),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "values",
      JSValue.Native(setValues),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "keys",
      JSValue.Native(setValues),
      enumerable = false
    )
    ctx.setPrototype.defineProperty(
      "entries",
      JSValue.Native(setEntries),
      enumerable = false
    )
    ctx.setPrototype.defineAccessorProperty(
      "size",
      getter = Some(JSValue.Native(setSizeGetter)),
      setter = None,
      enumerable = false,
      configurable = true
    )

    // Symbol.toStringTag = "Set"
    symToStringTag match {
      case sym: JSValue.Symbol =>
        ctx.setPrototype.initSymbolProperty(
          sym.value,
          JSValue.fromString("Set"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }

    // Symbol.iterator = Set.prototype.values
    val setIteratorSym = getWellKnownSymbol("iterator")
    setIteratorSym match {
      case sym: JSValue.Symbol =>
        ctx.setPrototype.initSymbolProperty(
          sym.value,
          JSValue.Native(setValues),
          enumerable = false,
          writable = true,
          configurable = true
        )
      case _ => ()
    }

    // Symbol.species getter returning this
    symSpecies match {
      case sym: JSValue.Symbol =>
        val speciesGetter = NativeFunction(
          name = "get [Symbol.species]",
          length = 0,
          impl = (args, ctx) => args(0)
        )
        setConstructor.funcObj.defineSymbolAccessorProperty(
          sym.value,
          getter = Some(JSValue.Native(speciesGetter)),
          setter = None,
          enumerable = false,
          configurable = true
        )
      case _ => ()
    }
  }

  // ============================================================
  // WeakSet Implementation
  // ============================================================

  /** Internal storage class for WeakSet - uses WeakHashMap */
  private final class JSWeakSetStorage {
    private val storage =
      java.util.WeakHashMap[WeakObjectKey, java.lang.Boolean]()

    def add(value: JSValue): Boolean =
      value match {
        case JSValue.Object(obj) =>
          storage.put(WeakObjectKey(obj), java.lang.Boolean.TRUE)
          true
        case JSValue.JSArrayVal(arr) =>
          storage.put(WeakObjectKey(arr), java.lang.Boolean.TRUE)
          true
        case f: JSValue.Function =>
          storage.put(WeakObjectKey(f), java.lang.Boolean.TRUE)
          true
        case JSValue.Native(n) =>
          storage.put(WeakObjectKey(n), java.lang.Boolean.TRUE)
          true
        case _ => false
      }

    def has(value: JSValue): Boolean =
      value match {
        case JSValue.Object(obj)     => storage.containsKey(WeakObjectKey(obj))
        case JSValue.JSArrayVal(arr) => storage.containsKey(WeakObjectKey(arr))
        case f: JSValue.Function     => storage.containsKey(WeakObjectKey(f))
        case JSValue.Native(n)       => storage.containsKey(WeakObjectKey(n))
        case _                       => false
      }

    def delete(value: JSValue): Boolean =
      value match {
        case JSValue.Object(obj) =>
          storage.remove(WeakObjectKey(obj)) != null
        case JSValue.JSArrayVal(arr) =>
          storage.remove(WeakObjectKey(arr)) != null
        case f: JSValue.Function =>
          storage.remove(WeakObjectKey(f)) != null
        case JSValue.Native(n) =>
          storage.remove(WeakObjectKey(n)) != null
        case _ => false
      }
  }

  private def getWeakSetStorage(obj: JSObject)(using
      ctx: JSContext
  ): Option[JSWeakSetStorage] =
    obj.getOwnProperty("__weakSetStorage") match {
      case Some(JSValue.Native(storage: JSWeakSetStorage)) => Some(storage)
      case _                                               => None
    }

  private def initializeWeakSet(ctx: JSContext): Unit = {
    given JSContext = ctx
    val symToStringTag = getWellKnownSymbol("toStringTag")

    val weakSetConstructor = quickjs.value.NativeConstructor(
      name = "WeakSet",
      callImpl = (args, ctx) =>
        given JSContext = ctx
        ctx.throwTypeError("Constructor WeakSet requires 'new'")
      ,
      constructImpl = (args, ctx) =>
        given JSContext = ctx
        val obj = JSObject(prototype = ctx.weakSetPrototype, extensible = true)
        val storage = new JSWeakSetStorage()
        obj.defineProperty(
          "__weakSetStorage",
          JSValue.Native(storage),
          enumerable = false,
          writable = false,
          configurable = false
        )

        // If iterable argument is provided and not null/undefined, iterate using @@iterator protocol
        if args.nonEmpty && args(0) != JSValue.Null && args(
            0
          ) != JSValue.Undefined
        then {
          // Get the adder from WeakSet.prototype using proper [[Get]] (invokes getters)
          val adder = getProperty(JSValue.Object(obj), "add")
          // Check IsCallable
          adder match {
            case _: (JSValue.Function | JSValue.Native) => // callable
            case _ => ctx.throwTypeError("add is not a function")
          }

          iterateWithAdder(args(0), JSValue.Object(obj), adder, isMap = false)
        }

        JSValue.Object(obj)
      ,
      prototype = ctx.weakSetPrototype
    )
    BuiltinHelpers.initConstructor(weakSetConstructor, length = 0)
    ctx.global.defineProperty(
      "WeakSet",
      JSValue.Native(weakSetConstructor),
      enumerable = false
    )
    ctx.weakSetPrototype.defineProperty(
      "constructor",
      JSValue.Native(weakSetConstructor),
      enumerable = false
    )

    // WeakSet.prototype.add(value)
    val weakSetAdd = NativeFunction(
      name = "add",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match {
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                if storage.add(value) then args.head
                else ctx.throwTypeError("Invalid value used in weak set")
              case None =>
                ctx.throwTypeError("add called on incompatible WeakSet")
            }
          case _ => ctx.throwTypeError("add called on incompatible object")
        }
    )
    ctx.weakSetPrototype.defineProperty(
      "add",
      JSValue.Native(weakSetAdd),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // WeakSet.prototype.has(value)
    val weakSetHas = NativeFunction(
      name = "has",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match {
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.has(value))
              case None =>
                ctx.throwTypeError("has called on incompatible WeakSet")
            }
          case _ => ctx.throwTypeError("has called on incompatible object")
        }
    )
    ctx.weakSetPrototype.defineProperty(
      "has",
      JSValue.Native(weakSetHas),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // WeakSet.prototype.delete(value)
    val weakSetDelete = NativeFunction(
      name = "delete",
      impl = (args, ctx) =>
        given JSContext = ctx
        args.headOption match {
          case Some(JSValue.Object(obj)) =>
            getWeakSetStorage(obj) match {
              case Some(storage) =>
                val value = args.lift(1).getOrElse(JSValue.Undefined)
                JSValue.Bool(storage.delete(value))
              case None =>
                ctx.throwTypeError("delete called on incompatible WeakSet")
            }
          case _ => ctx.throwTypeError("delete called on incompatible object")
        }
    )
    ctx.weakSetPrototype.defineProperty(
      "delete",
      JSValue.Native(weakSetDelete),
      enumerable = false,
      writable = true,
      configurable = true
    )

    // Symbol.toStringTag = "WeakSet"
    symToStringTag match {
      case sym: JSValue.Symbol =>
        ctx.weakSetPrototype.initSymbolProperty(
          sym.value,
          JSValue.fromString("WeakSet"),
          enumerable = false,
          writable = false,
          configurable = true
        )
      case _ => ()
    }
  }

  // Public initialize method that calls all sub-initializers
  def initialize(ctx: JSContext): Unit = {
    initializeMap(ctx)
    initializeWeakMap(ctx)
    initializeSet(ctx)
    initializeWeakSet(ctx)
  }
}
