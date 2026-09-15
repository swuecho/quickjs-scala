package quickjs.node

import quickjs.value.JSValue

import java.nio.file.Path

/** Mutable process-wide state shared by the Node compatibility modules. */
final class NodeState(val options: NodeOptions) {
  private var currentCwd: Path = options.cwd

  /** `process.cwd()`. */
  def cwd: Path = currentCwd

  /** `process.chdir()`; only affects paths resolved through this state. */
  def chdir(path: Path): Unit = currentCwd = path.toAbsolutePath.normalize

  /** `process.exitCode`. */
  var exitCode: Int = 0

  /** Exit listeners registered via `process.on('exit', fn)`. */
  val exitListeners: scala.collection.mutable.ArrayBuffer[JSValue] =
    scala.collection.mutable.ArrayBuffer.empty

  var argv: Vector[String] = options.argv
}
