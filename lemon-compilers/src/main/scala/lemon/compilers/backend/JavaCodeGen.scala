package lemon.compilers.backend
import java.io.File
import com.sun.codemodel._
import lemon.messages.reflect._
import lemon.compilers.CompileService

class JavaCodeGen(output:File,types:Iterable[IR]) extends JavaBackend {

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

  private def gen(current : IR){
    current match {
      case message:Message_ => omit(message)
      case enum:Enum_ => omit(enum)
      case _ =>
    }
  }

  override protected def omit(enum: Enum_): JDefinedClass = {
    val model = codeModel._class(enum.name,ClassType.ENUM)

    model.field(JMod.PRIVATE,classOf[Long],"value")

    val constructor = model.constructor(JMod.PRIVATE)

    val param = constructor.param(JMod.FINAL,classOf[Long],"value")

    constructor.body().assign(JExpr._this().ref("value"),param)

    model.method(JMod.PUBLIC,classOf[Long],"getValue").body()._return(JExpr._this().ref("value"))

    enum.attributes.foreach{
      attribute => omit(enum,attribute,model)
    }


    enum.fields.foreach {
      field => omit(enum,field,model)
    }

    model
  }


  override protected def omit(enum:Enum_,field:(Long,String),model :JDefinedClass):JEnumConstant = {
    model.enumConstant(field._2).arg(JExpr.lit(field._1))
  }

  override protected def omit(message: Message_): JDefinedClass = {
    val model = codeModel._class(message.Name)

    model._implements(classOf[lemon.messages.io.PortableMessage])

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
                               message: Message_,
                               attribute: Attribute_,
                               model :JDefinedClass): Option[JAnnotationUse] = None

  override protected def omit(message: Message_, field: Field_, model: JDefinedClass): JFieldVar = {
    val jType = getJType(field.fieldType)
    val methodName = field.name.charAt(0).toUpper + field.name.substring(1)
    val filedModel = field.fieldType match {
      case Array_(valType,length) =>
        val fieldModel = model.field(JMod.PRIVATE,jType,field.name)
        fieldModel.init(JExpr.newArray(getJType(valType),length))
        val getMethod = model.method(JMod.PUBLIC,jType,"get" + methodName)
        getMethod.body()._return(JExpr._this().ref(fieldModel))
        fieldModel
      case _ =>
        val fieldModel = model.field(JMod.PRIVATE,jType,field.name)
        val getMethod = model.method(JMod.PUBLIC,jType,"get" + methodName)
        getMethod.body()._return(JExpr._this().ref(fieldModel))
        val setMethod = model.method(JMod.PUBLIC,codeModel.VOID,"set" + methodName)
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
                               message: Message_,
                               field: Field_,
                               attribute: Attribute_,
                               model: JDefinedClass,
                               fieldModel: JFieldVar): Option[JAnnotationUse] = None

  override protected def omit(enum: Enum_, attribute: Attribute_, model: JDefinedClass): Option[JAnnotationUse] = None
}


object JavaCodeGen {
  def gen(output:File,types:Iterable[IR]){
    val codeGen = new JavaCodeGen(output,types) with JavaCloneCodeGen with JavaWriterCodeGen with JavaReaderCodeGen

    codeGen.gen()
  }
}