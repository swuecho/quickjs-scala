package quickjs.stdlib

import munit.FunSuite

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Tests for the test262 harness runner itself (enumeration, harness
  * injection) rather than the JavaScript engine.
  */
class Test262RunnerTest extends FunSuite:

  private def withTempDir[A](body: Path => A): A =
    val dir = Files.createTempDirectory("quickjs-test262-runner")
    try body(dir)
    finally {
      val paths = Files.walk(dir).iterator().asScala.toList
      paths.sortBy(_.toString.length).reverse.foreach(Files.deleteIfExists(_))
    }

  private def write(path: Path, content: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, content)

  test("enumerateTests skips _FIXTURE.js support files") {
    withTempDir { dir =>
      write(dir.resolve("real-test.js"), "// test")
      write(dir.resolve("dep_FIXTURE.js"), "// fixture")
      write(dir.resolve("nested/other_FIXTURE.js"), "// fixture")
      write(dir.resolve("nested/another-test.js"), "// test")
      write(dir.resolve("not-a-test.txt"), "// not js")

      val names = Test262Runner
        .enumerateTests(dir.toString)
        .map(p => dir.relativize(Path.of(p)).toString)
        .sorted
      assertEquals(
        names,
        List("nested/another-test.js", "real-test.js")
      )
    }
  }
