package lemon.compilers.frontend

case class LinkContext(script:ScriptIL,symbols :Map[String,IL])

object Linker {

  def link(scripts :Array[Script],symbols : Map[String,IL]) :Map[String,IL] ={
    val map = scripts.foldLeft(symbols){
      (map:Map[String,IL],script:Script) => {
        map ++ link0(script)
      }
    }

    map.map({
      kv:(String,IL) => {
        implicit val context = LinkContext(kv._2.script.get,map)
        (kv._1,link(kv._2))
      }
    })
  }


  def link(scripts :Array[Script]) : Map[String,IL] = {
    link(scripts,Map())
  }

  private def link(any:IL)(implicit context :LinkContext) : IL = {
    any match {
      case message : MessageIL => link(message)
      case service : ServiceIL => link(service)
      case enum : EnumIL => link(enum)
    }

  }

  private def link(message:MessageIL)(implicit context :LinkContext):MessageIL = {
    val name = message.script.get.namespace.fullName + "." + message.name

    MessageIL(
      name,
      message.fields.map(link),
      message.extend.map(link),
      message.attributes.map(link))
  }

  private def link(field :FieldIL)(implicit context :LinkContext) : FieldIL = {
    FieldIL(
      field.name,
      link(field.fieldType),
      field.required,
      field.attributes.map(link))
  }

  private def link(anyType : TypeIL)(implicit context :LinkContext) : TypeIL = {
    anyType match {
      case message : MessageIL => link(message)
      case enum : EnumIL => link(enum)
      case ArrayIL(valType,length) => ArrayIL(link(valType),length)
      case MapIL(key,value) => MapIL(link(key),link(value))
      case SetIL(value) => SetIL(link(value))
      case ListIL(value) => ListIL(link(value))
      case ref:ReferenceIL => link(ref)
      case _ => anyType
    }
  }

  private def link(ref : ReferenceIL)(implicit context :LinkContext) : ReferenceIL = {
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

      if(foundSymbols.length > 1){
        throw new TypeAmbiguityError(ref.Name,foundSymbols)
      }

      foundSymbols(0)
    }

    ReferenceIL(symbol.script.get.namespace.fullName + "." + ref.name)
  }

  private def link(customerType : CustomerTypeIL)(implicit context :LinkContext) : CustomerTypeIL = {
    customerType match {
      case message : MessageIL => link(message)
      case enum : EnumIL => link(enum)
      case ref:ReferenceIL => link(ref)
    }
  }

  private def link(attribute : AttributeIL)(implicit context :LinkContext) : AttributeIL = {
    attribute
  }

  private def link(service:ServiceIL)(implicit context :LinkContext): ServiceIL = {
    val name = service.script.get.namespace.fullName + "." + service.name
    ServiceIL(
      name,
      service.methods.map(link),
      service.extend.map(link),
      service.attributes.map(link))
  }

  private def link(method:MethodIL)(implicit context :LinkContext): MethodIL = {
    MethodIL(
      method.name,
      method.params.map(link),
      method.exceptions.map(link),
      method.attributes.map(link))
  }

  private def link(param:ParamIL)(implicit context :LinkContext): ParamIL = {
    ParamIL(
      param.name,
      link(param.vaType),
      param.required,
      param.attributes.map(link))
  }

  private def link(enum:EnumIL)(implicit context :LinkContext) : EnumIL = {
    val name = enum.script.get.namespace.fullName + "." + enum.name

    EnumIL(
      name,
      enum.fields,
      enum.attributes.map(link))
  }

  private def link0(script : Script) : Map[String,IL] = {
    val root = script.root
    root.types.foldLeft(Map[String,IL]()){
      (map:Map[String,IL],current) => {
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
