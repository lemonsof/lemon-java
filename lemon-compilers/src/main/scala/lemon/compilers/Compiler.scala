package lemon.compilers

import java.io.File
import lemon.compilers.frontend.{Script, Linker, IL}

private object BuiltinCompiler {
  def compile():Map[String,IL] = {

    val directory =  this.getClass.getClassLoader.getResource("__lemon_builtin")

    if(directory == null){
      return Map()
    }

    val files = new File(directory.getFile).listFiles

    Linker.link(files.map {
      file=> new Script(file)
    })
  }
}

object CompileService {

  lazy val builtinTypes = BuiltinCompiler.compile()

  def compile(files :Array[File]) : Map[String,IL] = {
    Linker.link(files.map { file=> new Script(file) }) ++ builtinTypes
  }
}
