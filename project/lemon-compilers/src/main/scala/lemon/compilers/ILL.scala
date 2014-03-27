package lemon.compilers

import scala.util.parsing.input.Positional

abstract class ILL extends Positional{
  var script : Option[ScriptILL] = None
  def Name:String = ""
}


abstract class TypeILL extends ILL

abstract class BuiltinTypeILL extends TypeILL

abstract class CustomerTypeILL extends TypeILL

abstract class LiteralILL extends ILL

class VoidILL() extends BuiltinTypeILL

class StringILL() extends BuiltinTypeILL

case class BooleanILL() extends BuiltinTypeILL

case class UsingILL(fullName:String) extends ILL

case class NamespaceILL(fullName:String) extends ILL

case class AttributeILL(
                         valType:MessageLiteralILL,
                         target:Option[String]) extends ILL

case class FieldILL(
                     name:String,
                     fieldType : TypeILL,
                     required : Boolean,
                     attributes : List[AttributeILL]) extends ILL

case class MessageILL(
                       name:String,
                       fields : List[FieldILL],
                       extend : Option[CustomerTypeILL],
                       attributes : List[AttributeILL]) extends CustomerTypeILL{
  override def Name = name
}

case class EnumILL(
                    name:String,
                    fields : List[(Long,String)],
                    attributes : List[AttributeILL]) extends CustomerTypeILL{
  override def Name = name
}

case class ReferenceILL(
                         name:String) extends CustomerTypeILL


case class VarILL(
                      length : Int,
                      signed :Boolean) extends BuiltinTypeILL
case class FixedILL(
                      length : Int,
                      signed :Boolean) extends BuiltinTypeILL

case class FloatILL(
                     length : Int) extends BuiltinTypeILL

case class ArrayILL(
                     valType : TypeILL,
                     length:Int) extends BuiltinTypeILL

case class MapILL(
                   keyType:TypeILL,
                   valType:TypeILL) extends BuiltinTypeILL

case class SetILL(
                  valType:TypeILL) extends BuiltinTypeILL

case class ListILL(
                    valType:TypeILL) extends BuiltinTypeILL

case class ServiceILL(
                       name:String,
                       methods : List[MethodILL],
                       extend : Option[CustomerTypeILL],
                       attributes : List[AttributeILL]) extends ILL{
  override def Name = name
}

case class ParamILL(
                     name:String,
                     vaType:TypeILL,
                     required:Boolean,
                     attributes : List[AttributeILL]) extends ILL

case class MethodILL(
                      name:String,
                      params : List[ParamILL],
                      exceptions:List[CustomerTypeILL],
                      attributes : List[AttributeILL]) extends ILL

case class ScriptILL(
                      namespace : NamespaceILL,
                      imports : List[UsingILL],
                      types : List[ILL]) extends ILL

case class MessageLiteralILL(
                              name:String,
                              values : Option[List[LiteralILL]],
                              namedValues : Option[Map[String,LiteralILL]]) extends LiteralILL

case class BooleanLiteralILL(value:Boolean) extends LiteralILL

case class IntegerLiteralILL(value:Long) extends LiteralILL

case class FloatLiteralILL(value:Double) extends LiteralILL

case class StringLiteralILL(value:String) extends LiteralILL

case class EnumLiteralILL(value:String) extends LiteralILL

case class MapLiteralILL(
                          name:String,
                          values : Map[LiteralILL,LiteralILL]) extends LiteralILL


case class SeqLiteralILL(
                          name:String,
                          values : List[LiteralILL]) extends LiteralILL