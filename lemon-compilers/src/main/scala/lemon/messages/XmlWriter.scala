package lemon.messages

import scala.xml.Elem
import java.util

final class XmlWriter(var root:Elem) extends MessageWriter with CollectionWriter{

  def this() = this(<message></message>)

  private def appendChild(xmlWriter:XmlWriter){
    root = root.copy(child = root.child ++ xmlWriter.root)
  }

  private def appendChild(node:Elem){
    root = root.copy(child = root.child ++ node)
  }

  override def closeMessageWriter(writer: MessageWriter): Unit = {
    appendChild(writer.asInstanceOf[XmlWriter])
  }

  override def createMessageWriter(
                                    name: String,
                                    id: Int): MessageWriter = {

    if(null != name){
      new XmlWriter(<message id={name}></message>)
    } else {
      new XmlWriter(<message></message>)
    }
  }

  override def closeMapWriter(writer: CollectionWriter): Unit = {
    appendChild(writer.asInstanceOf[XmlWriter])
  }

  //map
  override def createMapWriter(name: String, id: Int, attributes: util.Map[String, Any]): CollectionWriter = {
    new XmlWriter(<map id={name}></map>)
  }

  override def closeSetWriter(writer: CollectionWriter): Unit = {
    appendChild(writer.asInstanceOf[XmlWriter])
  }

  //set
  override def createSetWriter(name: String, id: Int, attributes: util.Map[String, Any]): CollectionWriter = {
    new XmlWriter(<set id={name}></set>)
  }

  override def closeArrayWriter(writer: CollectionWriter): Unit = {
    appendChild(writer.asInstanceOf[XmlWriter])
  }

  //array
  override def createArrayWriter(name: String,
                                 id: Int,
                                 length: Int,
                                 attributes: util.Map[String, Any]): CollectionWriter = {
    new XmlWriter(<array id={name}></array>)
  }

  override def closeListWriter(writer: CollectionWriter): Unit = {
    appendChild(writer.asInstanceOf[XmlWriter])
  }

  //list
  override def createListWriter(name: String, id: Int, attributes: util.Map[String, Any]): CollectionWriter = {
    new XmlWriter(<list id={name}></list>)
  }

  override def writeString(name: String, id: Int, value: String, attributes: util.Map[String, Any]): Unit = {
    appendChild(<string id={name}>{value}</string>)
  }

  override def writeDouble(name: String, id: Int, value: Double, attributes: util.Map[String, Any]): Unit = {
    appendChild(<double id={name}>{value}</double>)
  }

  override def writeFloat(name: String, id: Int, value: Float, attributes: util.Map[String, Any]): Unit = {
    appendChild(<float id={name}>{value}</float>)
  }

  override def writeFixed(
                           name: String,
                           id: Int,
                           length: Int,
                           signed :Boolean,
                           value: Long,
                           attributes: util.Map[String, Any]): Unit = {
    appendChild(<fixed id={name} length={value.toString}>{value}</fixed>)
  }

  //simple type write
  override def writeVar(
                         name: String,
                         id: Int,
                         length: Int,
                         signed :Boolean,
                         value: Long,
                         attributes: util.Map[String, Any]): Unit = {
    appendChild(<var id={name}>{value}</var>)
  }

  override def writeNext(): Unit = {

  }

  override def writeString(value: String): Unit = {
    appendChild(<string>{value}</string>)
  }

  override def writeDouble(value: Double): Unit = {
    appendChild(<double value={value.toString}/>)
  }

  override def writeFloat(value: Float): Unit = {
    appendChild(<float value={value.toString}/>)
  }

  override def writeFixed(value: Long, length: Int, signed: Boolean): Unit = {
    appendChild(<fixed value={value.toString}/>)
  }

  override def writeVar(value: Long, length: Int, signed: Boolean): Unit = {
    appendChild(<Var value={value.toString}/>)
  }

  override def createMessageWriter(): MessageWriter =  new XmlWriter(<message></message>)

  override def endWrite(): Unit = {

  }

  override def beginWrite(typeName: String, attributes: util.Map[String, Any]): Unit = {

  }
}


