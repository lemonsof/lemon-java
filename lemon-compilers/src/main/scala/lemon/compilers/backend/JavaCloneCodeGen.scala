package lemon.compilers.backend
import com.sun.codemodel._
import lemon.messages.reflect._

trait JavaCloneCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: Message_): JDefinedClass = {

    val model = super.omit(message)

    cloneMethod(model,message)

    model
  }


  override def clone(): AnyRef = super.clone()

  private def cloneMethod(model:JDefinedClass,message: Message_) {
    val cloneMethod = model.method(JMod.PUBLIC,model,"clone")
    val target = cloneMethod.param(JMod.FINAL,model,"target")
    val block = cloneMethod.body()

    message.extend.foreach{
      extend =>  block.invoke(JExpr._super(),"clone").arg(target)
    }

    message.fields.foreach{
      field =>
        block.assign(target.ref(field.name),clone(field.fieldType,JExpr._this().ref(field.name),block,field.name))
    }

    block._return(target)
  }

  private def clone(targetType:Type_,source:JExpression,block:JBlock,tag:String,stack:Int = 0):JExpression = {
    targetType match {
      case ref:Ref_ =>
        cloneMessage(ref,source)
      case array:Array_ =>
        cloneArray(array,source,block,tag,stack)
      case list:List_ =>
        cloneList(list,source,block,tag,stack)
      case set:Set_ =>
        cloneSet(set,source,block,tag,stack)
      case map:Map_ =>
        cloneMap(map,source,block,tag,stack)
      case _ => source
    }
  }

  private def cloneMessage(ref:Ref_,source:JExpression):JExpression = {
    source.invoke("clone").arg(JExpr._new(getJType(ref)))
  }

  private def cloneArray(array:Array_,source:JExpression,block:JBlock,tag:String,stack:Int = 0):JExpression = {
    val target = block.decl(getJType(array),tag + stack,JExpr.newArray(getJType(array.valType),array.length))

    block.add(codeModel.ref("java.lang.System")
      .staticInvoke("arraycopy")
      .arg(source)
      .arg(JExpr.lit(0))
      .arg(target)
      .arg(JExpr.lit(0))
      .arg(JExpr.lit(array.length)))

      target
  }

  private def cloneList(list:List_,source:JExpression,block:JBlock,tag:String,stack:Int = 0):JExpression = {
    val target = block.decl(getJType(list),tag + stack,JExpr._new(codeModel.ref(classOf[java.util.ArrayList[_]]).narrow(getJType(list.valType))))

    val forEach = block.forEach(getJType(list.valType),tag + stack +"_",source)

    val item = clone(list.valType,forEach.`var`(),forEach.body(),tag,stack +1)

    forEach.body().invoke(target,"add").arg(item)

    target
  }

  private def cloneSet(set:Set_,source:JExpression,block:JBlock,tag:String,stack:Int = 0):JExpression = {
    val target = block.decl(getJType(set),tag + stack,JExpr._new(codeModel.ref(classOf[java.util.HashSet[_]]).narrow(getJType(set.valType))))

    val forEach = block.forEach(getJType(set.valType),tag + stack +"_",source)

    val item = clone(set.valType,forEach.`var`(),forEach.body(),tag,stack +1)

    forEach.body().invoke(target,"add").arg(item)

    target
  }

  private def cloneMap(map:Map_,source:JExpression,block:JBlock,tag:String,stack:Int = 0):JExpression = {
    val target = block.decl(
      getJType(map),
      tag + stack,
      JExpr._new(codeModel.ref(classOf[java.util.HashMap[_,_]])
        .narrow(getJType(map.keyType))
        .narrow(getJType(map.valType))))

    val forEach = block.forEach(
      codeModel.ref(classOf[java.util.Map.Entry[_,_]])
      .narrow(getJType(map.keyType))
      .narrow(getJType(map.valType)),tag + stack +"_",source.invoke("entrySet"))

    val key = clone(map.keyType,forEach.`var`().invoke("getKey"),forEach.body(),tag,stack +1)
    val value = clone(map.valType,forEach.`var`().invoke("getValue"),forEach.body(),tag,stack +1)

    forEach.body().invoke(target,"put").arg(key).arg(value)

    target
  }
}
