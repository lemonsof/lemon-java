package lemon.messages.reflect

import scala.util.parsing.input.Positional

abstract class IR extends Positional{
  var script : Option[Script_] = None
  def Name:String = ""
}

abstract class Type_ extends IR

abstract class BuiltinType_ extends Type_

abstract class CustomerType_ extends Type_

abstract class Literal_ extends IR

class Void_() extends BuiltinType_

class String_() extends BuiltinType_

case class Boolean_() extends BuiltinType_

case class Using_(fullName:String) extends IR

case class Namespace_(fullName:String) extends IR

case class Attribute_(
                         valType:MessageLiteral_,
                         target:Option[String]) extends IR

case class Field_(
                     name:String,
                     fieldType : Type_,
                     required : Boolean,
                     attributes : List[Attribute_],
                     id:Int = 0) extends IR

case class Message_(
                       name:String,
                       fields : List[Field_],
                       extend : Option[CustomerType_],
                       attributes : List[Attribute_]) extends CustomerType_{
  override def Name = name
}

case class Enum_(
                    name:String,
                    fields : List[(Long,String)],
                    attributes : List[Attribute_]) extends CustomerType_{
  override def Name = name
}

case class Ref_(
                         name:String) extends CustomerType_{
  override def Name = name
}


case class Var_(
                      length : Int,
                      signed :Boolean) extends BuiltinType_
case class Fixed_(
                      length : Int,
                      signed :Boolean) extends BuiltinType_

case class Float_(
                     length : Int) extends BuiltinType_

case class Array_(
                     valType : Type_,
                     length:Int) extends BuiltinType_

case class Map_(
                   keyType:Type_,
                   valType:Type_) extends BuiltinType_

case class Set_(
                  valType:Type_) extends BuiltinType_

case class List_(
                    valType:Type_) extends BuiltinType_

case class Service_(
                       name:String,
                       methods : List[Method_],
                       extend : Option[CustomerType_],
                       attributes : List[Attribute_]) extends IR{
  override def Name = name
}

case class Param_(
                     name:String,
                     vaType:Type_,
                     required:Boolean,
                     attributes : List[Attribute_]) extends IR

case class Method_(
                      name:String,
                      returnParam:Param_,
                      params : List[Param_],
                      exceptions:List[CustomerType_],
                      attributes : List[Attribute_]) extends IR

case class Script_(
                      namespace : Namespace_,
                      imports : List[Using_],
                      types : List[IR]) extends IR

case class MessageLiteral_(
                              name:String,
                              values : Option[List[Literal_]],
                              namedValues : Option[Map[String,Literal_]]) extends Literal_

case class BooleanLiteral_(value:Boolean) extends Literal_

case class IntegerLiteral_(value:Long) extends Literal_

case class FloatLiteral_(value:Double) extends Literal_

case class StringLiteral_(value:String) extends Literal_

case class EnumLiteral_(value:String) extends Literal_

case class MapLiteral_(
                          name:String,
                          values : Map[Literal_,Literal_]) extends Literal_


case class SeqLiteral_(
                          name:String,
                          values : List[Literal_]) extends Literal_