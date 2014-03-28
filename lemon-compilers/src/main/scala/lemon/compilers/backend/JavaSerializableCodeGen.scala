package lemon.compilers.backend

import lemon.compilers.frontend.MessageIL
import com.sun.codemodel.{JExpr, JMod, JCodeModel, JDefinedClass}

trait JavaSerializableCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: MessageIL): JDefinedClass = {

    val model = super.omit(message)

    model._implements(classOf[lemon.messages.Serializable])

    createReadMethod(message,model)
  }

  private  def createReadMethod(message:MessageIL,model:JDefinedClass):JDefinedClass = {
    val readMethod = model.method(JMod.PUBLIC,codeModel.VOID,"read")

    val param = readMethod.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.Reader]),"reader")

    message.extend.foreach {
      extend =>
        val invoke = readMethod.body().invoke(JExpr._super(),"read")
        invoke.arg(param.invoke("readSuper"))
    }

    message.fi

    model
  }

}
