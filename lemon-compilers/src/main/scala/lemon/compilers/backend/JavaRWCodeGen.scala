package lemon.compilers.backend

import lemon.compilers.frontend._
import com.sun.codemodel._
import lemon.compilers.frontend.FieldIL
import lemon.compilers.frontend.MessageIL

trait JavaRWCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: MessageIL): JDefinedClass = {

    val model = super.omit(message)

    model._implements(classOf[lemon.messages.Serializable])

    createWriteMethod(message,model)
  }


  private  def createWriteMethod(message:MessageIL,model:JDefinedClass):JDefinedClass = {
    val method = model.method(JMod.PUBLIC,codeModel.VOID,"write")

    val writer = method.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.MessageWriter]),"writer")

    val resolver = method.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.MetadataResolver]),"resolver")

    method.body()
      .invoke(writer,"beginWrite")
      .arg(JExpr.lit(message.name))
      .arg(resolver.invoke("resolve").arg(message.Name))

    message.extend.foreach{
      extend =>
        val body = method.body()
        body.invoke(JExpr._super(),"write").arg(JExpr
          .invoke(writer,"createMessageWriter")
          .arg(JExpr._null())
          .arg(JExpr.lit(0)))
    }

    message.fields.foreach{
      field => writeField(field,writer,resolver,method,model)
    }


    method.body()
      .invoke(writer,"endWrite")

    model
  }

  def writeField(field:FieldIL,writer: JVar,resolver:JVar,method:JMethod,model:JDefinedClass) {
    val invoke = field.fieldType match {
      case VarIL(length,signed) =>
        method.body()
          .invoke(writer,"writeVar")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))
      case FixedIL(length,signed) =>
        method.body()
          .invoke(writer,"writeFixed")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))

      case string:StringIL =>
        method.body()
          .invoke(writer,"writeString")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))

      case FloatIL(length) =>
        length match{
          case 4 =>
            method.body()
              .invoke(writer,"writeFloat")
              .arg(JExpr.lit(field.name))
              .arg(JExpr.lit(field.id))
          case 8 =>
            method.body()
              .invoke(writer,"writeDouble")
              .arg(JExpr.lit(field.name))
              .arg(JExpr.lit(field.id))
        }

      case ListIL(valType) =>
        method.body().forEach(getJType(valType),"current",JExpr.ref(field.name))
        method.body().invoke(writer,"createListWriter")
      case ArrayIL(valType,length) =>
        method.body().forEach(getJType(valType),"current",JExpr.ref(field.name))
        method.body().invoke(writer,"createArrayWriter")
      case MapIL(keyType,valType) =>
        method.body().invoke(writer,"createMapWriter")
    }

    invoke
      .arg(JExpr.ref(field.name))
      .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))
  }
}
