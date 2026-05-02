package quickjs.atom

import scala.collection.mutable

/** Atom (string interning) system.
  *
  * Atoms are used to efficiently store and compare strings. Each unique string
  * is assigned a unique integer ID.
  */
final class JSAtomTable:
  private val stringToAtom: mutable.HashMap[String, Int] = mutable.HashMap.empty
  private val atomToString: mutable.ArrayBuffer[String] =
    mutable.ArrayBuffer.empty
  private var nextAtom: Int = 1

  /** Get or create an atom for a string */
  def atom(str: String): Int =
    stringToAtom.get(str) match
      case Some(id) => id
      case None     =>
        val id = nextAtom
        nextAtom += 1
        stringToAtom(str) = id
        // Resize buffer if needed
        while atomToString.length <= id do atomToString += null
        atomToString(id) = str
        id

  /** Get the string for an atom */
  def string(atom: Int): String =
    if atom > 0 && atom < atomToString.length then atomToString(atom)
    else throw new IllegalArgumentException(s"Invalid atom ID: $atom")

  /** Check if an atom exists */
  def contains(str: String): Boolean = stringToAtom.contains(str)

  /** Get the number of atoms */
  def size: Int = stringToAtom.size

object JSAtomTable:
  /** Well-known atoms */
  val Empty: Int = 0

  val Null: Int = 1
  val Undefined: Int = 2
  val True: Int = 3
  val False: Int = 4
  val This: Int = 5

  val Length: Int = 6
  val Prototype: Int = 7
  val Constructor: Int = 8
  val ToString: Int = 9
  val ValueOf: Int = 10

  // Initialize well-known atoms
  def initialize(): JSAtomTable =
    val table = new JSAtomTable
    table.atom("null")
    table.atom("undefined")
    table.atom("true")
    table.atom("false")
    table.atom("this")
    table.atom("length")
    table.atom("prototype")
    table.atom("constructor")
    table.atom("toString")
    table.atom("valueOf")
    table
