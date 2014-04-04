package lemon.compilers

import java.io.File
import lemon.compilers.frontend.{Script, Linker}
import lemon.messages.reflect._
import java.util.concurrent.ConcurrentHashMap
import java.util
import scala.collection.JavaConverters._
import lemon.messages.reflect.Method_
import lemon.messages.reflect.Service_
import lemon.messages.reflect.Attribute_
import lemon.messages.reflect.Enum_
import lemon.messages.reflect.Message_
import lemon.messages.reflect.Field_
import lemon.messages.io.AttributeReader

private object BuiltinCompiler {
  def compile():Map[String,IR] = {

    val directory =  this.getClass.getClassLoader.getResource("__lemon_idl")

    if(directory == null){
      return Map()
    }

    val files = new File(directory.getFile).listFiles

    Linker.link(files.map {
      file=> new Script(file)
    })
  }
}

object CompileService extends MetaDataResolver{

  lazy val builtinTypes = BuiltinCompiler.compile()

  val metaData = new ConcurrentHashMap[String, util.concurrent.ConcurrentMap[String, Any]].asScala

  val symbolTable = new ConcurrentHashMap[String,IR].asScala

  symbolTable ++= builtinTypes

  def compile(files :Array[File]) : Map[String,IR] = {

    val linked = Linker.link(files.map { file=> new Script(file) })

    symbolTable ++= linked

    load(linked)
  }

  def reset(){
    metaData.clear()
  }

  override def resolve(fullName: String): util.Map[String, Any] = {
    metaData.get(fullName) getOrElse{ null }
  }

  private def load(types:Map[String,IR]):Map[String,IR] = {
    types.foreach {
      kv => load(kv._1,kv._2)
    }

    types
  }

  private def load(fullPath:String,ir:IR){
    ir match {
      case Message_(_,fields,_,attributes) =>
        fields.foreach(load(fullPath,_))
        attributes.foreach(load(fullPath,_))
      case Enum_(_,_,_,attributes) =>
        attributes.foreach(load(fullPath,_))
      case Service_(_,methods,_,attributes) =>
        methods.foreach(load(fullPath,_))
        attributes.foreach(load(fullPath,_))
    }
  }

  private def load(fullPath:String,field:Field_) {
    val fieldFullName = fullPath + "." + field.name
    field.attributes.foreach(load(fieldFullName,_))
  }

  private def load(fullPath:String,attribute:Attribute_) {
    try{
      if(metaData.contains(fullPath)){
        metaData(fullPath).put(attribute.valType.name,AttributeReader.read(attribute))
      } else {
        val attributes = new ConcurrentHashMap[String,Any]

        val old = metaData.putIfAbsent(fullPath,attributes)

        if(old != None){
          old.get.put(attribute.valType.name,AttributeReader.read(attribute))
        } else {
          attributes.put(attribute.valType.name,AttributeReader.read(attribute))
        }
      }
    } catch {
      case _: Throwable =>
    }
  }

  private def load(fullPath:String,method:Method_) {
    val methodFullName = fullPath + "@" + method.name
    method.attributes.foreach(load(methodFullName,_))
    load(methodFullName,method.returnParam)
    method.params.foreach(load(methodFullName,_))
  }

  private def load(fullPath:String,param:Param_) {
    val paramFullName = fullPath + ":" + param.name
    param.attributes.foreach(load(paramFullName,_))
  }
}
