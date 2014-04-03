package lemon.compilers.backend

import com.sun.codemodel.{JDefinedClass, JCodeModel}
import lemon.messages.reflect.Message_

trait JavaReaderCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: Message_): JDefinedClass = {

    val model = super.omit(message)

    model
  }
}
