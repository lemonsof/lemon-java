package lemon.compilers.backend

import com.sun.codemodel._
import java.util
import lemon.messages.reflect._
import lemon.messages.reflect.Map_
import lemon.messages.reflect.Array_
import lemon.messages.reflect.Fixed_
import lemon.messages.reflect.Var_
import lemon.messages.reflect.Message_
import lemon.messages.reflect.Set_
import lemon.messages.reflect.Float_
import lemon.messages.reflect.Attribute_
import lemon.messages.reflect.Ref_
import lemon.messages.reflect.List_
import lemon.messages.reflect.Field_

abstract class JavaBackend {
  protected def codeModel:JCodeModel

  protected def omit(enum:Enum_):JDefinedClass

  protected def omit(enum:Enum_,attribute:Attribute_,model :JDefinedClass):Option[JAnnotationUse]

  protected def omit(enum:Enum_,field:(Long,String),model :JDefinedClass):JEnumConstant


  protected def omit(message:Message_):JDefinedClass

  protected def omit(message:Message_,attribute:Attribute_,model :JDefinedClass):Option[JAnnotationUse]

  protected def omit(message:Message_,field:Field_,model :JDefinedClass):JFieldVar

  protected def omit(message:Message_,field:Field_,attribute:Attribute_,model :JDefinedClass,fieldModel:JFieldVar):Option[JAnnotationUse]

  protected def getJType(current:IR,boxedType:Boolean = false):JType = {
    current match {
      case Array_(valType,length) =>
        getJType(valType).array()
      case List_(valType) =>
        codeModel.ref(classOf[util.List[_]]).narrow(getJType(valType,boxedType = true))
      case Set_(valType) =>
        codeModel.ref(classOf[util.Set[_]]).narrow(getJType(valType,boxedType = true))
      case Map_(keyType,valType) =>
        codeModel.ref(classOf[util.Map[_,_]]).narrow(getJType(keyType,boxedType = true)).narrow(getJType(valType,boxedType = true))
      case _:String_ =>
        codeModel.ref(classOf[String])
      case Var_(length,_) => length match {
        case 1 =>
          if(!boxedType){
            codeModel._ref(classOf[Byte])
          } else {
            codeModel.ref(classOf[java.lang.Byte])
          }
        case 2 =>
          if(!boxedType){
            codeModel._ref(classOf[Short])
          } else {
            codeModel.ref(classOf[java.lang.Short])
          }
        case 4 =>
          if(!boxedType){
            codeModel._ref(classOf[Int])
          } else {
            codeModel.ref(classOf[java.lang.Integer])
          }

        case 8 =>
          if(!boxedType){
            codeModel._ref(classOf[Long])
          } else {
            codeModel.ref(classOf[java.lang.Long])
          }
      }
      case Fixed_(length,_) => length match {
        case 1 =>
          if(!boxedType){
            codeModel._ref(classOf[Byte])
          } else {
            codeModel.ref(classOf[java.lang.Byte])
          }
        case 2 =>
          if(!boxedType){
            codeModel._ref(classOf[Short])
          } else {
            codeModel.ref(classOf[java.lang.Short])
          }
        case 4 =>
          if(!boxedType){
            codeModel._ref(classOf[Int])
          } else {
            codeModel.ref(classOf[java.lang.Integer])
          }

        case 8 =>
          if(!boxedType){
            codeModel._ref(classOf[Long])
          } else {
            codeModel.ref(classOf[java.lang.Long])
          }
      }
      case Float_(length) => length match {
        case 4 =>
          if(!boxedType){
            codeModel._ref(classOf[Float])
          } else {
            codeModel.ref(classOf[Float])
          }
        case 8 =>
          if(!boxedType){
            codeModel._ref(classOf[Double])
          } else {
            codeModel.ref(classOf[java.lang.Double])
          }
      }
      case message:Message_ => codeModel.ref(message.name)
      case enum:Enum_ => codeModel.ref(enum.name)
      case Ref_(name) => codeModel.ref(name)
      case _:Boolean_ => codeModel.BOOLEAN
    }
  }
}
