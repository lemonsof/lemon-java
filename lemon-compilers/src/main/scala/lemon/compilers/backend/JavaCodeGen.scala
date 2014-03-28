package lemon.compilers.backend
import java.io.File
import lemon.compilers.frontend._
import com.sun.codemodel._
import lemon.compilers.frontend.MessageIL
import java.util

class JavaCodeGen(output:File,types:Iterable[IL]) extends JavaBackend{

  private val _codeModel = new JCodeModel

  override protected  def codeModel = _codeModel

  def gen():Iterable[File] = {
    val files = types.map {
      current =>
        gen(current)
        new File(output.toString + "/" + current.Name.replace('.','/') + ".java")
    }
    codeModel.build(output)
    files
  }

  private def gen(current : IL){
    current match {
      case message:MessageIL => omit(message)
      case _ =>
    }
  }

  private def getJType(current:IL):JType = {
    current match {
      case ArrayIL(valType,length) =>
        getJType(valType).array()
      case ListIL(valType) =>
        codeModel.ref(classOf[util.List[_]]).narrow(getJType(valType))
      case SetIL(valType) =>
        codeModel.ref(classOf[util.Set[_]]).narrow(getJType(valType))
      case MapIL(keyType,valType) =>
        codeModel.ref(classOf[util.Map[_,_]]).narrow(getJType(keyType)).narrow(getJType(valType))
      case _:StringIL =>
        codeModel.ref(classOf[String])
      case VarIL(length,signed) => length match {
        case 1 => codeModel.ref(classOf[Byte])
        case 2 => codeModel.ref(classOf[Short])
        case 4 => codeModel.ref(classOf[Int])
        case 8 => codeModel.ref(classOf[Long])
      }
      case FixedIL(length,_) => length match {
        case 1 => codeModel.ref(classOf[Byte])
        case 2 => codeModel.ref(classOf[Short])
        case 4 => codeModel.ref(classOf[Int])
        case 8 => codeModel.ref(classOf[Long])
      }
      case FloatIL(length) => length match {
        case 4 => codeModel.ref(classOf[Float])
        case 8 => codeModel.ref(classOf[Double])
      }
      case ReferenceIL(name) => codeModel.ref(name)
    }
  }

  override protected def omit(message: MessageIL): JDefinedClass = {
    val model = codeModel._class(message.Name)

    message.extend.foreach{
      extend => model._extends(codeModel.ref(extend.Name))
    }
    message.attributes.foreach{
      attribute => omit(message,attribute,model)
    }

    message.fields.foreach {
      field => omit(message,field,model)
    }

    model
  }

  override protected def omit(
                               message: MessageIL,
                               attribute: AttributeIL,
                               model :JDefinedClass): Option[JAnnotationUse] = None

  override protected def omit(message: MessageIL, field: FieldIL, model: JDefinedClass): JFieldVar = {
    val jType = getJType(field.fieldType)
    val methodName = field.name.charAt(0).toUpper + field.name.substring(1)
    val filedModel = field.fieldType match {
      case ArrayIL(valType,length) =>
        val fieldModel = model.field(JMod.PRIVATE|JMod.FINAL,jType,field.name)
        fieldModel.init(JExpr.newArray(getJType(valType),length))
        val getMethod = model.method(JMod.PUBLIC,jType,"get" + methodName)
        getMethod.body()._return(JExpr._this().ref(fieldModel))
        fieldModel
      case _ =>
        val fieldModel = model.field(JMod.PRIVATE,jType,field.name)
        val getMethod = model.method(JMod.PUBLIC,jType,"get" + methodName)
        getMethod.body()._return(JExpr._this().ref(fieldModel))
        val setMethod = model.method(JMod.PUBLIC,jType,"set" + methodName)
        val setParam = setMethod.param(JMod.FINAL,jType,field.name)
        setMethod.body().assign(JExpr._this().ref(fieldModel),setParam)
        fieldModel
    }

    field.attributes.foreach{
      attribute => omit(message,field,attribute,model,filedModel)
    }

    filedModel
  }

  override protected def omit(
                               message: MessageIL,
                               field: FieldIL,
                               attribute: AttributeIL,
                               model: JDefinedClass,
                               fieldModel: JFieldVar): Option[JAnnotationUse] = None
}
