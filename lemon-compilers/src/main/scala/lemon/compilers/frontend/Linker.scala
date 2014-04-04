package lemon.compilers.frontend

import lemon.messages.reflect._
import scala.Some
import lemon.messages.reflect.Attribute_

case class LinkContext(script:Script_,symbols :Map[String,IR])

object Linker {

  def link(scripts :Array[Script],symbols : Map[String,IR]) :Map[String,IR] ={
    val map = scripts.foldLeft(symbols){
      (map:Map[String,IR],script:Script) => {
        map ++ link0(script)
      }
    }

    map.map({
      kv:(String,IR) => {
        implicit val context = LinkContext(kv._2.script.get,map)
        (kv._1,link(kv._2))
      }
    })
  }


  def link(scripts :Array[Script]) : Map[String,IR] = {
    link(scripts,Map())
  }

  private def link(any:IR)(implicit context :LinkContext) : IR = {
    any match {
      case message : Message_ => link(message)
      case service : Service_ => link(service)
      case enum : Enum_ => link(enum)
    }

  }

  private def link(message:Message_)(implicit context :LinkContext):Message_ = {
    val name = message.script.get.namespace.fullName + "." + message.name

    var id = 0

    Message_(
      name,
      message.fields.map{
        field =>
          val result = link(field,id)
          id += 1
          result
      },
      message.extend.map(link),
      message.attributes.map(link))
  }

  private def link(field :Field_,id:Int)(implicit context :LinkContext) : Field_ = {
    Field_(
      field.name,
      link(field.fieldType),
      field.required,
      field.attributes.map(link),id)
  }

  private def link(anyType : Type_)(implicit context :LinkContext) : Type_ = {
    anyType match {
      case message : Message_ => link(message)
      case enum : Enum_ => link(enum)
      case Array_(valType,length) => Array_(link(valType),length)
      case Map_(key,value) => Map_(link(key),link(value))
      case Set_(value) => Set_(link(value))
      case List_(value) => List_(link(value))
      case ref:Ref_ => link(ref)
      case _ => anyType
    }
  }

  private def link(ref : Ref_)(implicit context :LinkContext) : Ref_ = {
    val symbols = context.symbols
    val script = context.script
    //first try to link symbol in same script
    val symbol = script.types.find {
      _.Name == ref.name
    } getOrElse {

      val nodes = ref.name.split('.')
      //check using instruction
      val importNames = script.imports.filter { importName =>
        importName.fullName.endsWith(nodes(0)) && symbols.contains(importName + nodes.tail.mkString("."))
      }

      val foundSymbols = importNames.map{ importName =>
        symbols(importName + nodes.tail.mkString("."))
      }

      if(foundSymbols.length != 1){
        throw new TypeAmbiguityError(ref,foundSymbols)
      }

      foundSymbols(0)
    }

    Ref_(symbol.script.get.namespace.fullName + "." + ref.name)
  }

  private def link(customerType : CustomerType_)(implicit context :LinkContext) : CustomerType_ = {
    customerType match {
      case message : Message_ => link(message)
      case enum : Enum_ => link(enum)
      case ref:Ref_ => link(ref)
    }
  }

  private def link(attribute : Attribute_)(implicit context :LinkContext) : Attribute_ = {
    val symbols = context.symbols
    val script = context.script
    //first try to link symbol in same script
    val symbol = script.types.find {
      _.Name == attribute.valType.name
    } getOrElse {

      val nodes = attribute.valType.name.split('.')
      //check using instruction
      val importNames = script.imports.filter { importName =>
        importName.fullName.endsWith(nodes(0)) && symbols.contains(importName + nodes.tail.mkString("."))
      }

      val foundSymbols = importNames.map{ importName =>
        symbols(importName + nodes.tail.mkString("."))
      }

      if(foundSymbols.length > 1){
        throw new TypeAmbiguityError(attribute.valType,foundSymbols)
      }

      foundSymbols(0)
    }

    Attribute_(
    MessageLiteral_(
      symbol.script.get.namespace.fullName + "." + attribute.valType.name,
      attribute.valType.values,
      attribute.valType.namedValues),
    attribute.target
    )
  }

  private def link(service:Service_)(implicit context :LinkContext): Service_ = {
    val name = service.script.get.namespace.fullName + "." + service.name
    Service_(
      name,
      service.methods.map(link),
      service.extend.map(link),
      service.attributes.map(link))
  }

  private def link(method:Method_)(implicit context :LinkContext): Method_ = {

    val returnAttributes = method.attributes.filter(_.target.exists(_ == "return"))

    val methodAttributes = method.attributes.filter(!_.target.exists(_ == "return"))

    val returnParam = Param_(
      method.returnParam.name,
      method.returnParam.vaType,
      method.returnParam.required,
      returnAttributes ++ method.returnParam.attributes
    )

    Method_(
      method.name,
      link(returnParam),
      method.params.map(link),
      method.exceptions.map(link),
      methodAttributes.map(link))
  }

  private def link(param:Param_)(implicit context :LinkContext): Param_ = {
    Param_(
      param.name,
      link(param.vaType),
      param.required,
      param.attributes.map(link))
  }

  private def link(enum:Enum_)(implicit context :LinkContext) : Enum_ = {
    val name = enum.script.get.namespace.fullName + "." + enum.name

    Enum_(
      name,
      enum.fields,
      enum.attributes.map(link))
  }

  private def link0(script : Script) : Map[String,IR] = {
    val root = script.root
    root.types.foldLeft(Map[String,IR]()){
      (map:Map[String,IR],current) => {
        val name = root.namespace.fullName + "." + current.Name

        if(map.contains(name)){
          throw new TypeRedefinitionError(current,map(name))
        }
        current.script = Some(root)
        map + ((name,current))
      }
    }
  }
}
