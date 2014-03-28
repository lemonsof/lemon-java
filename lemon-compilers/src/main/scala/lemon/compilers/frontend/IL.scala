package lemon.compilers.frontend

import scala.util.parsing.input.Positional

abstract class IL extends Positional{
  var script : Option[ScriptIL] = None
  def Name:String = ""
}

abstract class TypeIL extends IL

abstract class BuiltinTypeIL extends TypeIL

abstract class CustomerTypeIL extends TypeIL

abstract class LiteralIL extends IL

class VoidIL() extends BuiltinTypeIL

class StringIL() extends BuiltinTypeIL

case class BooleanIL() extends BuiltinTypeIL

case class UsingIL(fullName:String) extends IL

case class NamespaceIL(fullName:String) extends IL

case class AttributeIL(
                         valType:MessageLiteralIL,
                         target:Option[String]) extends IL

case class FieldIL(
                     name:String,
                     fieldType : TypeIL,
                     required : Boolean,
                     attributes : List[AttributeIL],
                     id:Int = 0) extends IL

case class MessageIL(
                       name:String,
                       fields : List[FieldIL],
                       extend : Option[CustomerTypeIL],
                       attributes : List[AttributeIL]) extends CustomerTypeIL{
  override def Name = name
}

case class EnumIL(
                    name:String,
                    fields : List[(Long,String)],
                    attributes : List[AttributeIL]) extends CustomerTypeIL{
  override def Name = name
}

case class ReferenceIL(
                         name:String) extends CustomerTypeIL{
  override def Name = name
}


case class VarIL(
                      length : Int,
                      signed :Boolean) extends BuiltinTypeIL
case class FixedIL(
                      length : Int,
                      signed :Boolean) extends BuiltinTypeIL

case class FloatIL(
                     length : Int) extends BuiltinTypeIL

case class ArrayIL(
                     valType : TypeIL,
                     length:Int) extends BuiltinTypeIL

case class MapIL(
                   keyType:TypeIL,
                   valType:TypeIL) extends BuiltinTypeIL

case class SetIL(
                  valType:TypeIL) extends BuiltinTypeIL

case class ListIL(
                    valType:TypeIL) extends BuiltinTypeIL

case class ServiceIL(
                       name:String,
                       methods : List[MethodIL],
                       extend : Option[CustomerTypeIL],
                       attributes : List[AttributeIL]) extends IL{
  override def Name = name
}

case class ParamIL(
                     name:String,
                     vaType:TypeIL,
                     required:Boolean,
                     attributes : List[AttributeIL]) extends IL

case class MethodIL(
                      name:String,
                      params : List[ParamIL],
                      exceptions:List[CustomerTypeIL],
                      attributes : List[AttributeIL]) extends IL

case class ScriptIL(
                      namespace : NamespaceIL,
                      imports : List[UsingIL],
                      types : List[IL]) extends IL

case class MessageLiteralIL(
                              name:String,
                              values : Option[List[LiteralIL]],
                              namedValues : Option[Map[String,LiteralIL]]) extends LiteralIL

case class BooleanLiteralIL(value:Boolean) extends LiteralIL

case class IntegerLiteralIL(value:Long) extends LiteralIL

case class FloatLiteralIL(value:Double) extends LiteralIL

case class StringLiteralIL(value:String) extends LiteralIL

case class EnumLiteralIL(value:String) extends LiteralIL

case class MapLiteralIL(
                          name:String,
                          values : Map[LiteralIL,LiteralIL]) extends LiteralIL


case class SeqLiteralIL(
                          name:String,
                          values : List[LiteralIL]) extends LiteralIL