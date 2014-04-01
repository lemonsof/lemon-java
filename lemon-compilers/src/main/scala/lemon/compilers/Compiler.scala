package lemon.compilers

import java.io.File
import lemon.compilers.frontend.{Script, Linker}
import lemon.messages.reflect.IR

private object BuiltinCompiler {
  def compile():Map[String,IR] = {

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

  def compile(files :Array[File]) : Map[String,IR] = {
    Linker.link(files.map { file=> new Script(file) }) ++ builtinTypes
  }
}
