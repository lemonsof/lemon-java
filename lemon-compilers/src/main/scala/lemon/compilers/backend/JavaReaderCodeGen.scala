package lemon.compilers.backend

import com.sun.codemodel._
import lemon.messages.reflect._
import lemon.messages.ConstraintException
import lemon.messages.reflect.Boolean_
import lemon.messages.reflect.Array_
import lemon.messages.reflect.Fixed_
import lemon.messages.reflect.Var_
import lemon.messages.reflect.Message_
import lemon.messages.reflect.Set_
import lemon.messages.reflect.Ref_
import lemon.messages.reflect.List_
import lemon.messages.reflect.Field_
import lemon.messages.io.SeqReader

trait JavaReaderCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: Message_): JDefinedClass = {

    val model = super.omit(message)
    readMethod(model,message)
    model
  }

  private def readMethod(model:JDefinedClass,message: Message_) {
    val readerMethod = model.method(JMod.PUBLIC,codeModel.VOID,"read")
    readerMethod.annotate(classOf[Override])
    readerMethod._throws(classOf[Exception])
    val reader = readerMethod.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.io.Reader]),"read")
    val block = readerMethod.body()

    message.extend.foreach{
      extend =>

        block.invoke(JExpr._super(),"read").arg(reader.invoke("readSupper"))
    }

    message.fields.foreach{
      field =>
        if(!field.required){
          val _try = block._try()
          _try._catch(codeModel.ref(classOf[ConstraintException])).param("ignored")
          readField(field,JExpr._this().ref(field.name),_try.body(),reader)
        } else {
          readField(field,JExpr._this().ref(field.name),block,reader)
        }
    }
  }

  private def readField(field:Field_,target:JAssignmentTarget,block:JBlock,reader:JExpression){
    field.fieldType match {
      case ref:Ref_ =>
        block.assign(target,JExpr._new(getJType(field.fieldType)))
        block.invoke(target,"read")
          .arg(
            block.invoke(reader,"readMessage")
              .arg(JExpr.lit(field.name))
              .arg(JExpr.lit(field.id)))
      case string:String_ =>
        block.assign(target,block.invoke(reader,"readString")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id)))

      case Var_(length,signed) =>
        block.assign(target, JExpr.cast(getJType(field.fieldType),block.invoke(reader,"readVar")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))))
      case Fixed_(length,signed) =>
        block.assign(target, JExpr.cast(getJType(field.fieldType),block.invoke(reader,"readFixed")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))))
      case _:Boolean_ =>
        block.assign(target, block.invoke(reader,"readBoolean")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id)))

      case Array_(valType,length) =>
        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          field.name + "Reader",
          reader.invoke("readArray")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(JExpr.lit(length)))

        readArray(length,valType,seqReader,target,block)

      case List_(valType) =>
        val listType = codeModel.ref(classOf[java.util.ArrayList[_]]).narrow(getJType(valType))

        block.assign(target,JExpr._new(listType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          field.name + "Reader",
          reader.invoke("readList")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id)))

        readSeq(valType,seqReader,target,block)
      case Set_(valType) =>

        val setType = codeModel.ref(classOf[java.util.HashSet[_]]).narrow(getJType(valType))

        block.assign(target,JExpr._new(setType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          field.name + "Reader",
          reader.invoke("readSet")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id)))

        readSeq(valType,seqReader,target,block)
      case Map_(keyType,valType) =>

        val mapType = codeModel
          .ref(classOf[java.util.HashMap[_,_]])
          .narrow(getJType(keyType))
          .narrow(getJType(valType))

        block.assign(target,JExpr._new(mapType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          field.name + "Reader",
          reader.invoke("readMap")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id)))

        readMap(keyType,valType,seqReader,target,block)
      case _ =>
    }
  }

  private def read(valType:Type_,reader:JExpression,block:JBlock,stack:Int):JExpression = {
    valType match {
      case ref:Ref_ =>
        val message = block.decl(getJType(valType),"message" + stack,JExpr._new(getJType(valType)))

        val messageReader = block.decl(
          codeModel.ref(classOf[lemon.messages.io.Reader]),
          "messageReader" + stack,
          reader.invoke("readMessage"))

        block.invoke(message,"read").arg(messageReader)

        message
      case Var_(length,signed) =>
        JExpr.cast(getJType(valType),reader.invoke("readVar").arg(JExpr.lit(length)).arg(JExpr.lit(signed)))
      case Fixed_(length,signed) =>
        JExpr.cast(getJType(valType),reader.invoke("readFixed").arg(JExpr.lit(length)).arg(JExpr.lit(signed)))
      case _:String_ =>
        reader.invoke("readString")
      case _:Boolean_ =>
        reader.invoke("readBoolean")

      case List_(childValType) =>

        val listType = codeModel.ref(classOf[java.util.ArrayList[_]]).narrow(getJType(childValType))

        val list = block.decl(listType,"list" + stack,JExpr._new(listType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          "reader" + stack,
          reader.invoke("readList"))

        readSeq(childValType,seqReader,list,block,stack + 1)

        list

      case Set_(childValType) =>
        val setType = codeModel.ref(classOf[java.util.HashSet[_]]).narrow(getJType(childValType))

        val set = block.decl(setType,"set" + stack,JExpr._new(setType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          "reader" + stack,
          reader.invoke("readSet"))

        readSeq(childValType,seqReader,set,block,stack + 1)

        set
      case Array_(childValType,length) =>

        val set = block.decl(getJType(childValType),"array" + stack,JExpr.newArray(getJType(childValType),length))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          "reader" + stack,
          reader.invoke("readArray").arg(JExpr.lit(length)))

        readArray(length,childValType,seqReader,set,block,stack + 1)

        set
      case Map_(keyType,childValType) =>
        val mapType = codeModel
          .ref(classOf[java.util.HashMap[_,_]])
          .narrow(getJType(keyType))
          .narrow(getJType(childValType))

        val map = block.decl(mapType,"map" + stack,JExpr._new(mapType))

        val seqReader = block.decl(
          codeModel.ref(classOf[SeqReader]),
          "reader" + stack,
          reader.invoke("readMap"))

        readMap(keyType,childValType,seqReader,map,block,stack + 1)

        map
    }
  }

  private def readSeq(valType:Type_,reader:JExpression,target:JExpression,block:JBlock,stack:Int = 0){
    
    val _while = block._while(reader.invoke("readNext"))

    _while.body().add(target.invoke("add").arg(read(valType,reader,_while.body(),stack + 1)))
  }

  private def readArray(length:Int,valType:Type_,reader:JExpression,target:JExpression,block:JBlock,stack:Int = 0){

    val _for = block._for()

    _for.init(codeModel.INT,"i",JExpr.lit(0))
    _for.test(JExpr.ref("i").lt(JExpr.lit(length)).cand(reader.invoke("readNext")))
    _for.update(JExpr.ref("i").incr())

    _for.body().assign(target.component(JExpr.ref("i")),read(valType,reader,_for.body(),stack + 1))
  }

  private def readMap(keyType:Type_,valType:Type_,reader:JExpression,target:JExpression,block:JBlock,stack:Int = 0){
    val _while = block._while(reader.invoke("readNext"))

    _while.body().add(target.invoke("put")
      .arg(read(keyType,reader,_while.body(),stack + 1))
      .arg(read(valType,reader,_while.body(),stack + 10)))
  }
}
