package lemon.messages.io

import scala.xml.Node

class XmlReader(root:Node) extends MessageReader{

  private def node(nodeName:String,name:String) :Node = {

    val nodes = (root \ nodeName).filter(node => node.attribute("id").exists(title => title.text == name))
    if(nodes.isEmpty){
      throw new FieldNotFoundException(name,null)
    }

    nodes(0)
  }

  override def closeMapReader(reader: CollectionReader): Unit = {}

  override def createMapReader(name: String, id: Int): CollectionReader =  new XmlSeqReader(node("set",name)(0).child)

  override def closeSetReader(reader: CollectionReader): Unit = {}

  override def createSetReader(name: String, id: Int): CollectionReader = new XmlSeqReader(node("set",name)(0))

  override def closeArrayReader(reader: CollectionReader): Unit = {}

  override def createArrayReader(name: String, id: Int, length: Int): CollectionReader = new XmlSeqReader(node("array",name)(0))

  override def closeListReader(reader: CollectionReader): Unit = {}

  override def createListReader(name: String, id: Int): CollectionReader = new XmlSeqReader(node("list",name)(0))

  override def readString(name: String, id: Int): String = node("string",name).text

  override def readDouble(name: String, id: Int): Double = node("double",name).text.toDouble

  override def readFloat(name: String, id: Int): Float = node("float",name).text.toFloat

  override def readFixed(name: String, id: Int, length: Int, signed: Boolean): Long = node("fixed",name).text.toLong

  override def readVar(name: String, id: Int, length: Int, signed: Boolean): Long = node("var",name).text.toLong

  override def closeMessageReader(reader: MessageReader): Unit = {

  }

  override def createMessageReader(name: String, id: Int): MessageReader = {

    if(name == null){
      val nodes = (root \ "message").filter(node => node.attribute("id") == None)
      if(nodes.isEmpty){
        throw new FieldNotFoundException("not found supper message node",null,null)
      }

      new XmlReader(nodes(0))

    } else {

      val nodes = (root \ "message").filter(node => node.attribute("id").exists(title => title.text == name))
      if(nodes.isEmpty){
        throw new FieldNotFoundException("not found supper message node",null,null)
      }

      new XmlReader(nodes(0))
    }
  }
}

class XmlSeqReader(root:Node) extends CollectionReader {
  private val iterator = root.child.iterator
  override def readNext(): Boolean = iterator.hasNext

  override def closeMapReader(reader: CollectionReader): Unit = {}

  override def createMapReader(): CollectionReader = {
    new XmlMapReader(iterator.next())
  }

  override def closeSetReader(reader: CollectionReader): Unit = {}

  override def createSetReader(): CollectionReader = {
    new XmlSeqReader(iterator.next())
  }

  override def closeArrayReader(reader: CollectionReader): Unit = {}

  override def createArrayReader(length: Int): CollectionReader = ???

  override def closeListReader(reader: CollectionReader): Unit = ???

  override def createListReader(): CollectionReader = ???

  override def readString(): String = ???

  override def readDouble(): Double = ???

  override def readFloat(): Float = ???

  override def readFixed(length: Int, signed: Boolean): Long = ???

  override def readVar(length: Int, signed: Boolean): Long = ???
}

class XmlMapReader(root:Node) extends CollectionReader {
  override def readVar(length: Int, signed: Boolean): Long = ???

  override def readNext(): Boolean = ???

  override def closeMapReader(reader: CollectionReader): Unit = ???

  override def createMapReader(): CollectionReader = ???

  override def closeSetReader(reader: CollectionReader): Unit = ???

  override def createSetReader(): CollectionReader = ???

  override def closeArrayReader(reader: CollectionReader): Unit = ???

  override def createArrayReader(length: Int): CollectionReader = ???

  override def closeListReader(reader: CollectionReader): Unit = ???

  override def createListReader(): CollectionReader = ???

  override def readString(): String = ???

  override def readDouble(): Double = ???

  override def readFloat(): Float = ???

  override def readFixed(length: Int, signed: Boolean): Long = ???
}
