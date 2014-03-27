package lemon.compilers


object Linker {

  def link(scripts :Array[Script],symbols : Map[String,ILL]) :Map[String,ILL] ={
    val map = scripts.foldLeft(symbols){
      (map:Map[String,ILL],script:Script) => {
        map ++ link0(script)
      }
    }

    map.map({
      kv:(String,ILL) => {
        implicit val symbols = map
        (kv._1,link(kv._2))
      }
    })
  }


  def link(scripts :Array[Script]) : Map[String,ILL] = {
    link(scripts,Map())
  }

  private def link(any:ILL)(implicit symbols :Map[String,ILL]) : ILL = {
    any match {
      case message : MessageILL => link(message)
      case service : ServiceILL => link(service)
      case enum : EnumILL => link(enum)
    }

  }

  private def link(message:MessageILL)(implicit symbols:Map[String,ILL]):MessageILL = {
    MessageILL(
      message.name,
      message.fields.map(link),
      message.extend.map(link),
      message.attributes.map(link))
  }

  private def link(field :FieldILL)(implicit symbols:Map[String,ILL]) : FieldILL = {
    FieldILL(
      field.name,
      link(field.fieldType),
      field.required,
      field.attributes.map(link))
  }

  private def link(anyType : TypeILL)(implicit symbols:Map[String,ILL]) : TypeILL = {
    anyType match {
      case message : MessageILL => link(message)
      case enum : EnumILL => link(enum)
      case ArrayILL(valType,length) => ArrayILL(link(valType),length)
      case MapILL(key,value) => MapILL(link(key),link(value))
      case SetILL(value) => SetILL(link(value))
      case ListILL(value) => ListILL(link(value))
      case ref:ReferenceILL => link(ref)
      case _ => anyType
    }
  }

  private def link(ref : ReferenceILL)(implicit symbols:Map[String,ILL]) : CustomerTypeILL = {
    //first try to link symbol in same script
    require(ref.script != None)
    val script = ref.script.get
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

    if(!symbol.isInstanceOf[CustomerTypeILL]){
      throw new TypeUnsatisfiedLinkError(ref.Name)
    }

    symbol.asInstanceOf[CustomerTypeILL]
  }

  private def link(customerType : CustomerTypeILL)(implicit symbols:Map[String,ILL]) : CustomerTypeILL = {
    customerType match {
      case message : MessageILL => link(message)
      case enum : EnumILL => link(enum)
      case ref:ReferenceILL => link(ref)
    }
  }

  private def link(attribute : AttributeILL)(implicit symbols:Map[String,ILL]) : AttributeILL = {
    attribute
  }

  private def link(service:ServiceILL)(implicit symbols:Map[String,ILL]): ServiceILL = {
    ServiceILL(
      service.name,
      service.methods.map(link),
      service.extend.map(link),
      service.attributes.map(link))
  }

  private def link(method:MethodILL)(implicit symbols:Map[String,ILL]): MethodILL = {
    MethodILL(
      method.name,
      method.params.map(link),
      method.exceptions.map(link),
      method.attributes.map(link))
  }

  private def link(param:ParamILL)(implicit symbols:Map[String,ILL]): ParamILL = {
    ParamILL(
      param.name,
      link(param.vaType),
      param.required,
      param.attributes.map(link))
  }

  private def link(enum:EnumILL)(implicit symbols:Map[String,ILL]) : EnumILL = {
    EnumILL(
      enum.name,
      enum.fields,
      enum.attributes.map(link))
  }

  private def link0(script : Script) : Map[String,ILL] = {
    val root = script.root
    root.types.foldLeft(Map[String,ILL]()){
      (map:Map[String,ILL],current) => {
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
