package lemon.compilers

import org.scalatest.FunSuite

class CompileServiceTest extends FunSuite {
  test("test compile builtin types"){
    CompileService.builtinTypes
  }
}
