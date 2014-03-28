package lemon.compilers

import org.scalatest.FunSuite
import java.io.File
import lemon.compilers.backend.{JavaSerializableCodeGen, JavaBackendLogger, JavaCodeGen}

class CompileServiceTest extends FunSuite {
  test("test compile builtin types"){
    val target = new File("./__gen").getAbsoluteFile
    target.mkdirs()

    val codeGen =
      new JavaCodeGen(target,CompileService.builtinTypes.values)
        with JavaSerializableCodeGen
        with JavaBackendLogger

    codeGen.gen()
  }
}
