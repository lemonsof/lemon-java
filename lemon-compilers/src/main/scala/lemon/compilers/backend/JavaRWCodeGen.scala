package lemon.compilers.backend

import com.sun.codemodel._
import lemon.messages.io._
import lemon.messages.reflect._
import lemon.messages.reflect.Array_
import lemon.messages.reflect.Var_
import lemon.messages.reflect.Set_
import lemon.messages.reflect.Map_
import lemon.messages.reflect.Fixed_
import lemon.messages.reflect.Message_
import lemon.messages.reflect.Float_
import lemon.messages.reflect.List_
import lemon.messages.reflect.Field_

trait JavaRWCodeGen extends JavaBackend{

  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: Message_): JDefinedClass = {

    val model = super.omit(message)

    model._implements(classOf[Serializable])

    createWriteMethod(message,model)

    createReadMethod(message,model)
  }



  private def createReadMethod(message:Message_,model:JDefinedClass):JDefinedClass = {
    val method = model.method(JMod.PUBLIC,codeModel.VOID,"read")

    val reader = method.param(JMod.FINAL,codeModel.ref(classOf[MessageReader]),"reader")

    message.extend.foreach{
      extend =>
        val body = method.body()
        body.invoke(JExpr._super(),"read").arg(JExpr
          .invoke(reader,"createMessageReader")
          .arg(JExpr._null())
          .arg(JExpr.lit(0)))
    }

    message.fields.foreach{
      field => readField(field,reader,method,model)
    }

    model
  }

  def readField(field: Field_, reader: JVar, method: JMethod, model: JDefinedClass) {

    var block = method.body()

    if(!field.required){
      val _try = block._try()

      block = _try.body()

      _try._catch(codeModel.ref(classOf[FieldNotFoundException]))
    }

    field.fieldType match {
      case Var_(length,signed) =>
        block.assign(
          JExpr.ref(field.name),
          block
            .invoke(reader,"readVar")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(JExpr.lit(length))
            .arg(JExpr.lit(signed)))

      case Fixed_(length,signed) =>
        block.assign(
          JExpr.ref(field.name),
          block
            .invoke(reader,"readFixed")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(JExpr.lit(length))
            .arg(JExpr.lit(signed)))

      case string:String_ =>
        block.assign(
          JExpr.ref(field.name),
          block
            .invoke(reader,"readString")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id)))

      case Float_(length) =>
        length match{
          case 4 =>
            block.assign(
              JExpr.ref(field.name),
              method.body()
                .invoke(reader,"readFloat")
                .arg(JExpr.lit(field.name))
                .arg(JExpr.lit(field.id)))
          case 8 =>
            block.assign(
              JExpr.ref(field.name),
              block
                .invoke(reader,"readDouble")
                .arg(JExpr.lit(field.name))
                .arg(JExpr.lit(field.id)))
        }
      case List_(valType) =>
        val collectionReader = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Reader",
          block.invoke(reader,"createListReader").arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))

        val collectionType = getJType(field.fieldType)

        val collection = block.decl(
          collectionType,
          field.name,
          JExpr._new(collectionType))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        block.invoke(collection,"add").arg(readCollectionType(collectionReader,whileLoop.body(),valType,0))

        block.assign(JExpr._this().ref(field.name),collection)

        block.invoke(reader,"closeListReader").arg(collectionReader)

      case Set_(valType) =>
        val collectionReader = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Reader",
          block.invoke(reader,"createSetReader").arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))

        val collectionType = getJType(field.fieldType)

        val collection = block.decl(
          collectionType,
          field.name,
          JExpr._new(collectionType))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        block.invoke(collection,"add").arg(readCollectionType(collectionReader,whileLoop.body(),valType,0))

        block.assign(JExpr._this().ref(field.name),collection)

        block.invoke(reader,"closeListReader").arg(collectionReader)

      case Array_(valType,length) =>
        val collectionReader = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Reader",
          block.invoke(reader,"createListReader").arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)).arg(JExpr.lit(length)))

        val collectionType = getJType(field.fieldType)

        val collection = block.decl(
          collectionType,
          field.name,
          JExpr._new(collectionType))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        block.invoke(collection,"add").arg(readCollectionType(collectionReader,whileLoop.body(),valType,0))

        block.assign(JExpr._this().ref(field.name),collection)

        block.invoke(reader,"closeListReader").arg(collectionReader)

      case Map_(keyType,valType) =>
        val collectionReader = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Reader",
          block.invoke(reader,"createListReader").arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))

        val collectionType = getJType(field.fieldType)

        val collection = block.decl(
          collectionType,
          field.name,
          JExpr._new(collectionType))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        block.invoke(collection,"put")
          .arg(readCollectionType(collectionReader,whileLoop.body(),keyType,0))
          .arg(readCollectionType(collectionReader,whileLoop.body(),valType,0))

        block.assign(JExpr._this().ref(field.name),collection)

        block.invoke(reader,"closeListReader").arg(collectionReader)
    }
  }

  private def readCollectionType(collectionReader:JVar,block:JBlock,valType:Type_,stack :Int) : JExpression = {
    valType match {
      case Var_(length,signed) =>
        JExpr.invoke(collectionReader,"readVar")
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
      case Fixed_(length,signed) =>
        JExpr.invoke(collectionReader,"readFixed")
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
      case Float_(length) =>
        length match{
          case 4 =>
            JExpr.invoke(collectionReader,"readFloat")
          case 8 =>
            JExpr.invoke(collectionReader,"readDouble")
        }
      case string:String_ =>
        JExpr.invoke(collectionReader,"readString")

      case List_(childValType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionReader]),
          "listReader" + stack,
          block.invoke(collectionReader,"createListWriter"))

        val collection = block.decl(
          getJType(valType),
          "list" + stack,
          JExpr._new(getJType(valType)))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        whileLoop.body().invoke(collection,"add").arg(readCollectionType(childWriter,whileLoop.body(),childValType,0))

        collection

      case Set_(childValType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionReader]),
          "setReader" + stack,
          block.invoke(collectionReader,"createSetWriter"))

        val collection = block.decl(
          getJType(valType),
          "set" + stack,
          JExpr._new(getJType(valType)))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        whileLoop.body().invoke(collection,"add").arg(readCollectionType(childWriter,whileLoop.body(),childValType,0))

        collection
      case Array_(childValType,length) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionReader]),
          "arrayReader" + stack,
          block.invoke(collectionReader,"createArrayWriter").arg(JExpr.lit(length)))

        val collection = block.decl(
          getJType(valType),
          "array" + stack,
          JExpr._new(getJType(valType)))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        whileLoop.body().invoke(collection,"add").arg(readCollectionType(childWriter,whileLoop.body(),childValType,0))

        collection
      case Map_(childKeyType,childValueType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionReader]),
          "arrayReader" + stack,
          block.invoke(collectionReader,"createArrayWriter"))

        val collection = block.decl(
          getJType(valType),
          "array" + stack,
          JExpr._new(getJType(valType)))

        val whileLoop = block._while(collectionReader.invoke("readNext"))

        whileLoop.body().invoke(collection,"put")
          .arg(readCollectionType(childWriter,whileLoop.body(),childKeyType,0))
          .arg(readCollectionType(childWriter,whileLoop.body(),childValueType,0))

        collection
    }
  }

  private  def createWriteMethod(message:Message_,model:JDefinedClass):JDefinedClass = {
    val method = model.method(JMod.PUBLIC,codeModel.VOID,"write")

    val writer = method.param(JMod.FINAL,codeModel.ref(classOf[MessageWriter]),"writer")

    val resolver = method.param(JMod.FINAL,codeModel.ref(classOf[MetadataResolver]),"resolver")

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
          .arg(resolver)
    }

    message.fields.foreach{
      field => writeField(field,writer,resolver,method,model)
    }


    method.body()
      .invoke(writer,"endWrite")

    model
  }

  def writeField(field:Field_,writer: JVar,resolver:JVar,method:JMethod,model:JDefinedClass) {

    field.fieldType match {
      case Var_(length,signed) =>
        method.body()
          .invoke(writer,"writeVar")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(JExpr.ref(field.name))
          .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))
      case Fixed_(length,signed) =>
        method.body()
          .invoke(writer,"writeFixed")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
          .arg(JExpr.ref(field.name))
          .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))

      case string:String_ =>
        method.body()
          .invoke(writer,"writeString")
          .arg(JExpr.lit(field.name))
          .arg(JExpr.lit(field.id))
          .arg(JExpr.ref(field.name))
          .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))

      case Float_(length) =>
        length match{
          case 4 =>
            method.body()
              .invoke(writer,"writeFloat")
              .arg(JExpr.lit(field.name))
              .arg(JExpr.lit(field.id))
              .arg(JExpr.ref(field.name))
              .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))
          case 8 =>
            method.body()
              .invoke(writer,"writeDouble")
              .arg(JExpr.lit(field.name))
              .arg(JExpr.lit(field.id))
              .arg(JExpr.ref(field.name))
              .arg(resolver.invoke("resolve").arg(JExpr.lit(model.fullName() + "#" + field.name)))
        }
      case List_(valType) =>

        if(field.required){
          method.body()._if(JExpr.ref(field.name).eq(JExpr._null()))
            ._then()
            ._throw(JExpr._new(codeModel.ref(classOf[FieldNotFoundException])).arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))
        }

        val collectionWriter = method.body().decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Writer",
          method.body().invoke(writer,"createListWriter")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(resolver.invoke("resolve")
            .arg(JExpr.lit(model.fullName() + "#" + field.name))))


        val foreach = method.body().forEach(getJType(valType),"current",JExpr.ref(field.name))

        foreach.body().invoke(collectionWriter,"writeNext")

        writeCollectionType(collectionWriter,foreach.body(),foreach.`var`(),valType,0)

        method.body().invoke(writer,"closeListWriter").arg(collectionWriter)

      case Set_(valType) =>
        if(field.required){
          method.body()._if(JExpr.ref(field.name).eq(JExpr._null()))
            ._then()
            ._throw(JExpr._new(codeModel.ref(classOf[FieldNotFoundException])).arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))
        }

        val collectionWriter = method.body().decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Writer",
          method.body().invoke(writer,"createSetWriter")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(resolver.invoke("resolve")
            .arg(JExpr.lit(model.fullName() + "#" + field.name))))


        val foreach = method.body().forEach(getJType(valType),"current",JExpr.ref(field.name))

        foreach.body().invoke(collectionWriter,"writeNext")

        writeCollectionType(collectionWriter,foreach.body(),foreach.`var`(),valType,0)

        method.body().invoke(writer,"closeSetWriter").arg(collectionWriter)

      case Array_(valType,length) =>
        if(field.required){
          method.body()._if(JExpr.ref(field.name).eq(JExpr._null()))
            ._then()
            ._throw(JExpr._new(codeModel.ref(classOf[FieldNotFoundException])).arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))
        }

        val collectionWriter = method.body().decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Writer",
          method.body().invoke(writer,"createArrayWriter")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(JExpr.lit(length))
            .arg(resolver.invoke("resolve")
            .arg(JExpr.lit(model.fullName() + "#" + field.name))))


        val foreach = method.body().forEach(getJType(valType),"current",JExpr.ref(field.name))

        foreach.body().invoke(collectionWriter,"writeNext")

        writeCollectionType(collectionWriter,foreach.body(),foreach.`var`(),valType,0)

        method.body().invoke(writer,"closeArrayWriter").arg(collectionWriter)

      case Map_(keyType,valType) =>
        if(field.required){
          method.body()._if(JExpr.ref(field.name).eq(JExpr._null()))
            ._then()
            ._throw(JExpr._new(codeModel.ref(classOf[FieldNotFoundException])).arg(JExpr.lit(field.name)).arg(JExpr.lit(field.id)))
        }

        val collectionWriter = method.body().decl(
          codeModel.ref(classOf[CollectionWriter]),
          field.name + "Writer",
          method.body().invoke(writer,"createMapWriter")
            .arg(JExpr.lit(field.name))
            .arg(JExpr.lit(field.id))
            .arg(resolver.invoke("resolve")
            .arg(JExpr.lit(model.fullName() + "#" + field.name))))


        val foreach = method.body().forEach(getJType(valType),"current",JExpr.ref(field.name).invoke("entrySet"))

        foreach.body().invoke(collectionWriter,"writeNext")

        val key = foreach.body().decl(getJType(keyType),"key",foreach.`var`().invoke("getKey"))

        val value = foreach.body().decl(getJType(valType),"value",foreach.`var`().invoke("getValue"))

        writeCollectionType(collectionWriter,foreach.body(),key,keyType,0)

        writeCollectionType(collectionWriter,foreach.body(),value,valType,0)

        method.body().invoke(writer,"closeMapWriter").arg(collectionWriter)
    }
  }

  private def writeCollectionType(collectionWriter:JVar,block:JBlock,value:JVar,valType:Type_,stack :Int){
    valType match {
      case Var_(length,signed) =>
        block.invoke(collectionWriter,"writeVar")
          .arg(value)
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
      case Fixed_(length,signed) =>
        block.invoke(collectionWriter,"writeFixed")
          .arg(value)
          .arg(JExpr.lit(length))
          .arg(JExpr.lit(signed))
      case Float_(length) =>
        length match{
          case 4 =>
            block.invoke(collectionWriter,"writeFloat").arg(value)
          case 8 =>
            block.invoke(collectionWriter,"writeDouble").arg(value)
        }
      case string:String_ =>
        block.invoke(collectionWriter,"writeString").arg(value)

      case List_(childValType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          "listWriter" + stack,
          block.invoke(collectionWriter,"createListWriter"))

        val foreach = block.forEach(getJType(valType),"current" + stack,value)

        foreach.body().invoke(childWriter,"writeNext")

        writeCollectionType(childWriter,foreach.body(),foreach.`var`(),childValType,stack +1)

        block.invoke(collectionWriter,"closeListWriter").arg(childWriter)
      case Set_(childValType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          "setWriter" + stack,
          block.invoke(collectionWriter,"createSetWriter"))

        val foreach = block.forEach(getJType(valType),"current" + stack,value)

        foreach.body().invoke(childWriter,"writeNext")

        writeCollectionType(childWriter,foreach.body(),foreach.`var`(),childValType,stack +1)

        block.invoke(collectionWriter,"closeSetWriter").arg(childWriter)
      case Array_(childValType,length) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          "arrayWriter" + stack,
          block.invoke(collectionWriter,"createArrayWriter").arg(JExpr.lit(length)))

        val foreach = block.forEach(getJType(valType),"current" + stack,value)

        foreach.body().invoke(childWriter,"writeNext")

        writeCollectionType(childWriter,foreach.body(),foreach.`var`(),childValType,stack +1)

        block.invoke(collectionWriter,"closeArrayWriter").arg(childWriter)
      case Map_(childKeyType,childValueType) =>
        val childWriter = block.decl(
          codeModel.ref(classOf[CollectionWriter]),
          "setWriter" + stack,
          block.invoke(collectionWriter,"createSetWriter"))

        val foreach = block.forEach(getJType(valType),"current",value.invoke("entrySet"))

        foreach.body().invoke(childWriter,"writeNext")

        val childKey = foreach.body().decl(getJType(childKeyType),"key" +stack,foreach.`var`().invoke("getKey"))

        val childValue = foreach.body().decl(getJType(childValueType),"value" +stack,foreach.`var`().invoke("getValue"))

        writeCollectionType(childWriter,foreach.body(),childKey,childKeyType,stack +1)

        writeCollectionType(childWriter,foreach.body(),childValue,childValueType,stack +1)

        block.invoke(collectionWriter,"closeSetWriter").arg(childWriter)
    }
  }
}
