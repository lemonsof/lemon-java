package lemon.compilers.backend

import com.sun.codemodel._
import lemon.messages.reflect._
import lemon.messages.reflect.Field_
import lemon.messages.reflect.Message_
import lemon.messages.reflect.Type_
import lemon.messages.{UnknownSymbolException, EnumValue, ConstraintException}
import lemon.compilers.CompileService


trait JavaWriterCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: Message_): JDefinedClass = {

    val model = super.omit(message)

    writerMethod(model,message)

    model
  }

  private def writerMethod(model:JDefinedClass,message: Message_) {
    val writerMethod = model.method(JMod.PUBLIC,codeModel.VOID,"write")
    writerMethod.annotate(classOf[Override])
    val writer = writerMethod.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.io.Writer]),"writer")
    val resolver = writerMethod.param(JMod.FINAL,codeModel.ref(classOf[lemon.messages.reflect.MetaDataResolver]),"resolver")
    val block = writerMethod.body()

    block.invoke(writer,"begin").arg(resolver.invoke("resolve").arg(message.name))

    message.extend.foreach{
      extend =>
        block.invoke(JExpr._super(),"write").arg(writer.invoke("writeSupper")).arg(resolver)
    }

    message.fields.foreach{
      field =>
        val attributes = resolver.invoke("resolve").arg(JExpr.lit(s"${message.name}.${field.name}"))

        writeField(field,JExpr._this().ref(field.name),block,attributes,writer,resolver)
    }

    block.invoke(writer,"end")
  }



  private def writeField(field:Field_,source:JExpression,block:JBlock,attributes:JExpression,writer:JExpression,resolver:JExpression){

    field.fieldType match {
      case ref:Ref_ =>
        if(!CompileService.symbolTable.contains(ref.name)){
          throw new UnknownSymbolException(ref.name)
        }

        writeField(
          field.copy(fieldType = CompileService.symbolTable(ref.name).asInstanceOf[Type_]),
          source,
          block,
          attributes,
          writer,
          resolver)

      case message:Message_ =>
        block._if(source.eq(JExpr._null()).not())._then()
          .add(source.invoke("write").arg(writer.invoke("writeMessage")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(attributes)).arg(resolver))
      case string:String_ =>
        block._if(source.eq(JExpr._null()).not())._then()
          .add(writer.invoke("writeString")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(source)
          .arg(attributes))

      case Var_(length,signed) =>
        block.add(writer.invoke("writeVar")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(source)
          .arg(attributes))
      case Fixed_(length,signed) =>
        block.add(writer.invoke("writeFixed")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(source)
          .arg(attributes))
      case Float_(length) =>
        if(length == 4){
          block.invoke(writer,"writeFloat")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(source)
            .arg(attributes)
        } else {
          block.invoke(writer,"writeDouble")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(source)
            .arg(attributes)
        }

      case _:Boolean_ =>
        block.add(writer.invoke("writeBoolean")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(source)
          .arg(attributes))

      case Array_(valType,length) =>
        val ifBlock = block._if(source.eq(JExpr._null()).not())._then()
        val seqWriter = ifBlock.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          field.name + "Writer",
          writer.invoke("writeArray")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(JExpr.lit(length))
            .arg(attributes))

        writeSeq(valType,seqWriter,source,ifBlock,resolver)
      case List_(valType) =>
        val ifBlock = block._if(source.eq(JExpr._null()).not())._then()
        val seqWriter = ifBlock.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          field.name + "Writer",
          writer.invoke("writeList")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(attributes))

        writeSeq(valType,seqWriter,source,ifBlock,resolver)
      case Set_(valType) =>
        val ifBlock = block._if(source.eq(JExpr._null()).not())._then()
        val seqWriter = ifBlock.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          field.name + "Writer",
          writer.invoke("writeSet")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(attributes))

        writeSeq(valType,seqWriter,source,ifBlock,resolver)

      case Map_(keyType,valType) =>
        val ifBlock = block._if(source.eq(JExpr._null()).not())._then()
        val seqWriter = ifBlock.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          field.name + "Writer",
          writer.invoke("writeMap")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(attributes))

        writeMap(keyType,valType,seqWriter,source,ifBlock,resolver)
      case enum:Enum_ =>
        val ifBlock = block._if(source.eq(JExpr._null()).not())._then()
        ifBlock.block().invoke(writer,"writeEnum")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(enum.length))
          .arg(JExpr._new(codeModel.ref(
            classOf[EnumValue]))
            .arg(source.invoke("toString"))
            .arg(source.invoke("getValue")))
          .arg(attributes)

      case _ =>
    }

  }

  def write(valType:Type_,writer:JExpression,source:JExpression,block:JBlock,resolver:JExpression,stack:Int = 0) {
    valType match {
      case ref:Ref_ =>
        if(!CompileService.symbolTable.contains(ref.name)){
          throw new UnknownSymbolException(ref.name)
        }

        write(CompileService.symbolTable(ref.name).asInstanceOf[Type_],writer,source,block,resolver)
      case message:Message_ =>
        block.add(source.invoke("write").arg(writer.invoke("writeMessage")).arg(resolver))
      case Var_(length,signed) =>
        block.invoke(writer,"writeVar")
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(source)
      case Fixed_(length,signed) =>
        block.invoke(writer,"writeFixed")
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(source)
      case Float_(length) =>
        if(length == 4){
          block.invoke(writer,"writeFloat")
            .arg(source)
        } else {
          block.invoke(writer,"writeDouble")
            .arg(source)
        }

      case _:String_ =>
        block.invoke(writer,"writeString")
          .arg(source)
      case _:Boolean_ =>
        block.invoke(writer,"writeBoolean")
          .arg(source)

      case List_(childValType) =>
        val seqWriter = block.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          "writer" + stack,
          writer.invoke("writeList"))

        writeSeq(childValType,seqWriter,source,block,resolver,stack + 1)


      case Set_(childValType) =>
        val seqWriter = block.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          "writer" + stack,
          writer.invoke("writeSet"))

        writeSeq(childValType,seqWriter,source,block,resolver,stack + 1)
      case Map_(keyType,childValType) =>
        val seqWriter = block.decl(
          codeModel.ref(classOf[lemon.messages.io.SeqWriter]),
          "writer" + stack,
          writer.invoke("writeSet"))

        writeMap(keyType,childValType,seqWriter,source,block,resolver,stack + 1)
      case enum:Enum_ =>
        block.invoke(writer,"writeEnum")
          .arg(JExpr.lit(enum.length))
          .arg(JExpr._new(codeModel.ref(
          classOf[EnumValue]))
          .arg(source.invoke("toString"))
          .arg(source.invoke("getValue")))

      case _=>
    }
  }


  def writeSeq(valType:Type_,writer:JExpression,source:JExpression,block:JBlock,resolver:JExpression,stack:Int = 0){
    val forEach = block.forEach(getJType(valType),"current" + stack,source)
    forEach.body().invoke(writer,"writeNext")
    write(valType,writer,forEach.`var`(),forEach.body(),resolver,stack + 1)
  }

  def writeMap(keyType:Type_,valType:Type_,writer:JExpression,source:JExpression,block:JBlock,resolver:JExpression,stack:Int = 0){

    val forEach = block.forEach(
      codeModel.ref(classOf[java.util.Map.Entry[_,_]])
      .narrow(getJType(keyType))
      .narrow(getJType(valType)),"entry" + stack,source.invoke("entrySet"))

    forEach.body().invoke(writer,"writeNext")

    write(keyType,writer,forEach.`var`().invoke("getKey"),forEach.body(),resolver,stack + 1)

    write(valType,writer,forEach.`var`().invoke("getValue"),forEach.body(),resolver,stack + 10)
  }
}
