package lemon.messages.io

trait MessageReader {

  @throws[FieldNotFoundException]
  def createMessageReader(name:String,id:Int):MessageReader

  def closeMessageReader(reader:MessageReader)

  @throws[FieldNotFoundException]
  def readVar(name:String,id:Int,length:Int,signed :Boolean):Long
  @throws[FieldNotFoundException]
  def readFixed(name:String,id:Int,length:Int,signed :Boolean):Long
  @throws[FieldNotFoundException]
  def readFloat(name:String,id:Int):Float
  @throws[FieldNotFoundException]
  def readDouble(name:String,id:Int):Double
  @throws[FieldNotFoundException]
  def readString(name:String,id:Int):String

  @throws[FieldNotFoundException]
  def createListReader(name:String,id:Int):CollectionReader
  def closeListReader(reader:CollectionReader)

  @throws[FieldNotFoundException]
  def createArrayReader(name:String,id:Int,length:Int):CollectionReader
  def closeArrayReader(reader:CollectionReader)

  @throws[FieldNotFoundException]
  def createSetReader(name:String,id:Int):CollectionReader
  def closeSetReader(reader:CollectionReader)

  @throws[FieldNotFoundException]
  def createMapReader(name:String,id:Int):CollectionReader
  def closeMapReader(reader:CollectionReader)
}

trait CollectionReader {
  def readVar(length:Int,signed :Boolean):Long
  def readFixed(length:Int,signed :Boolean):Long
  def readFloat():Float
  def readDouble():Double
  def readString():String

  def createListReader():CollectionReader
  def closeListReader(reader:CollectionReader)


  def createArrayReader(length:Int):CollectionReader
  def closeArrayReader(reader:CollectionReader)


  def createSetReader():CollectionReader
  def closeSetReader(reader:CollectionReader)


  def createMapReader():CollectionReader
  def closeMapReader(reader:CollectionReader)

  def readNext():Boolean
}
