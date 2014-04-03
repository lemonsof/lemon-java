package lemon.compilers

import org.scalatest.FunSuite
import java.io.File
import lemon.compilers.backend.{JavaCloneCodeGen, JavaBackendLogger, JavaCodeGen}

class CompileServiceTest extends FunSuite {
  test("test compile builtin types"){
    val target = new File("./__gen").getAbsoluteFile
    target.mkdirs()

    JavaCodeGen.gen(target,CompileService.builtinTypes.values)

    val xml = <root><message id="12"></message></root>

    println((xml \ "message").filter(node => node.attribute("id").exists(title => title.text == "12")))
  }
}
