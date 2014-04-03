package lemon.messages.io


trait Reader {
  def readSupper():Reader
  def readMessage(name:String,id:Int):Reader
  def readVar(name:String,id:Int,signed:Boolean):Long
  def readFixed(name:String,id:Int,length:Int,signed:Boolean):Long
  def readString(name:String,id:Int):String
  def readBoolean(name:String,id:Int):Boolean
  def readArray(name:String,id:Int,length:Int):SeqReader
  def readList(name:String,id:Int):SeqReader
  def readSet(name:String,id:Int):SeqReader
  def readMap(name:String,id:Int):SeqReader
}

trait SeqReader{
  def readNext():Boolean
  def readMessage():Reader
  def readVar(signed:Boolean):Long
  def readFixed(length:Int,signed:Boolean):Long
  def readString():String
  def readBoolean():Boolean
  def readArray(length:Int):SeqReader
  def readList():SeqReader
  def readSet():SeqReader
  def readMap():SeqReader
}
