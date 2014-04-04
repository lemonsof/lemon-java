package lemon.messages.io

import lemon.messages.{EnumValue, ConstraintException}


trait Reader {
  @throws[ConstraintException]
  def readSupper():Reader
  @throws[ConstraintException]
  def readMessage(name:String,id:Int):Reader
  @throws[ConstraintException]
  def readVar(name:String,id:Int,length:Int,signed:Boolean):Long
  @throws[ConstraintException]
  def readFixed(name:String,id:Int,length:Int,signed:Boolean):Long
  @throws[ConstraintException]
  def readString(name:String,id:Int):String
  @throws[ConstraintException]
  def readFloat(name:String,id:Int):Float
  @throws[ConstraintException]
  def readDouble(name:String,id:Int):Double
  @throws[ConstraintException]
  def readBoolean(name:String,id:Int):Boolean
  @throws[ConstraintException]
  def readArray(name:String,id:Int,length:Int):SeqReader
  @throws[ConstraintException]
  def readList(name:String,id:Int):SeqReader
  @throws[ConstraintException]
  def readSet(name:String,id:Int):SeqReader
  @throws[ConstraintException]
  def readMap(name:String,id:Int):SeqReader
  @throws[ConstraintException]
  def readEnum(name:String,id:Int,length:Int):EnumValue
}

trait SeqReader{
  def readNext():Boolean
  def readMessage():Reader
  def readVar(length:Int,signed:Boolean):Long
  def readFixed(length:Int,signed:Boolean):Long
  def readString():String
  def readBoolean():Boolean
  def readArray(length:Int):SeqReader
  def readList():SeqReader
  def readSet():SeqReader
  def readMap():SeqReader
  def readFloat():Float
  def readDouble():Double
  def readEnum(length:Int):EnumValue
}
