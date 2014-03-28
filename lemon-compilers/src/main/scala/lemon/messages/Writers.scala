package lemon.messages
import java.util

trait MessageWriter {

  def beginWrite(
                  typeName:String,
                  attributes : util.Map[String,Any])

  def endWrite()

  def createMessageWriter(
                           name:String,
                           id:Int): MessageWriter

  def closeMessageWriter(messageWriter: MessageWriter)

  //simple type write
  def writeVar(name:String,id:Int,length:Int,signed :Boolean,value: Long,attributes : util.Map[String,Any])
  def writeFixed(name:String,id:Int,length:Int,signed :Boolean,value: Long,attributes : util.Map[String,Any])
  def writeFloat(name:String,id:Int,value: Float,attributes : util.Map[String,Any])
  def writeDouble(name:String,id:Int,value: Double,attributes : util.Map[String,Any])
  def writeString(name:String,id:Int,value: String,attributes : util.Map[String,Any])

  //list
  def createListWriter(name:String,id:Int,attributes : util.Map[String,Any]):CollectionWriter
  def closeListWriter(writer:CollectionWriter)

  //array
  def createArrayWriter(name:String,id:Int,length:Int,attributes : util.Map[String,Any]):CollectionWriter
  def closeArrayWriter(writer:CollectionWriter)

  //set
  def createSetWriter(name:String,id:Int,attributes : util.Map[String,Any]):CollectionWriter
  def closeSetWriter(writer:CollectionWriter)

  //map
  def createMapWriter(name:String,id:Int,attributes : util.Map[String,Any]):CollectionWriter
  def closeMapWriter(writer:CollectionWriter)

}

trait CollectionWriter {

  def createMessageWriter():MessageWriter

  def closeMessageWriter(writer:MessageWriter)
  def writeVar(value:Long,length:Int,signed:Boolean)
  def writeFixed(value:Long,length:Int,signed:Boolean)
  def writeFloat(value: Float)
  def writeDouble(value: Double)
  def writeString(value: String)

  def writeNext()
}
