package lemon.messages.io

import lemon.messages.ConstraintException
import lemon.messages.reflect._
import lemon.messages.reflect.MessageLiteral_
import lemon.messages.reflect.SeqLiteral_
import lemon.messages.reflect.MapLiteral_

private sealed class MessageLiteralReader(literal:MessageLiteral_) extends Reader{

  private def read(name:String,id:Int):Literal_ = {
    if(literal.values != None){
      val values = literal.values.get
      if(values.length >= id){
        return values.apply(id)
      }
    }

    if(literal.namedValues != None){
      val values = literal.namedValues.get
      if(values.contains(name)){
        return values(name)
      }
    }

    throw new ConstraintException(s"message literal(${literal.name}}) not contain field($name:$id)",name,id)
  }

  @throws[ConstraintException]
  override def readMap(name: String, id: Int): SeqReader = {
    read(name,id) match {
      case seq:MapLiteral_ => new MapLiteralReader(seq)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain map field($name:$id)",name,id)
    }

  }


  @throws[ConstraintException]
  override def readSet(name: String, id: Int): SeqReader = {
    read(name,id) match {
      case seq:SeqLiteral_ => new SeqLiteralReader(seq)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain set field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readList(name: String, id: Int): SeqReader = {
    read(name,id) match {
      case seq:SeqLiteral_ => new SeqLiteralReader(seq)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain list field($name:$id)",name,id)
    }
  }


  @throws[ConstraintException]
  override def readArray(name: String, id: Int, length: Int): SeqReader = {
    read(name,id) match {
      case seq:SeqLiteral_ => new SeqLiteralReader(seq)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain array field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readBoolean(name: String, id: Int): Boolean = {
    read(name,id) match {
      case value:BooleanLiteral_ => value.value
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain boolean field($name:$id)",name,id)
    }
  }


  @throws[ConstraintException]
  override def readString(name: String, id: Int): String = {
    read(name,id) match {
      case value:StringLiteral_ => value.value
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain string field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readFixed(name: String, id: Int, length: Int, signed: Boolean): Long = {
    read(name,id) match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain fixed field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readVar(name: String, id: Int, length: Int, signed: Boolean): Long = {
    read(name,id) match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain var field($name:$id)",name,id)
    }
  }


  @throws[ConstraintException]
  override def readDouble(name: String, id: Int): Double = {
    read(name,id) match {
      case value:FloatLiteral_ => value.value
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain double field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readFloat(name: String, id: Int): Float = {
    read(name,id) match {
      case value:FloatLiteral_ => value.value.asInstanceOf[Float]
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain float field($name:$id)",name,id)
    }
  }


  @throws[ConstraintException]
  override def readMessage(name: String, id: Int): Reader = {
    read(name,id) match {
      case value:MessageLiteral_ => new MessageLiteralReader(value)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not contain message field($name:$id)",name,id)
    }
  }

  @throws[ConstraintException]
  override def readSupper(): Reader = {
    read("",0) match {
      case value:MessageLiteral_ => new MessageLiteralReader(value)
      case _ =>
        throw new ConstraintException(s"message literal(${literal.name}}) not found supper message","",0)
    }
  }

}

private sealed class SeqLiteralReader(literal:SeqLiteral_) extends SeqReader {
  val iterator = literal.values.iterator
  override def readMap(): SeqReader = {
    iterator.next() match {
      case value:MapLiteral_ => new MapLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read map from seq literal(${literal.name}})")
    }
  }

  override def readSet(): SeqReader = {
    iterator.next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read set from seq literal(${literal.name}})")
    }
  }

  override def readList(): SeqReader = {
    iterator.next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read list from seq literal(${literal.name}})")
    }
  }

  override def readArray(length: Int): SeqReader = {
    iterator.next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read array from seq literal(${literal.name}})")
    }
  }

  override def readBoolean(): Boolean = {
    iterator.next() match {
      case value:BooleanLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read boolean from seq literal(${literal.name}})")
    }
  }

  override def readString(): String = {
    iterator.next() match {
      case value:StringLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readFixed(length: Int, signed: Boolean): Long = {
    iterator.next() match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readVar(length: Int, signed: Boolean): Long = {
    iterator.next() match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readMessage(): Reader = {
    iterator.next() match {
      case value:MessageLiteral_ => new MessageLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read message from seq literal(${literal.name}})")
    }
  }

  override def readDouble(): Double = {
    iterator.next() match {
      case value:FloatLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read double from seq literal(${literal.name}})")
    }
  }

  override def readFloat(): Float = {
    iterator.next() match {
      case value:FloatLiteral_ => value.value.asInstanceOf[Float]
      case _ =>
        throw new ReadException(s"can't read float from seq literal(${literal.name}})")
    }
  }

  override def readNext(): Boolean = iterator.hasNext
}

private sealed class MapLiteralReader(literal:MapLiteral_) extends SeqReader {
  val iterator = literal.values.iterator
  var pair:Option[(Literal_,Literal_)] = None
  var readKey:Boolean = false

  private def next():Literal_ = {
    if(readKey){
      pair.get._1
    } else {
      pair.get._2
    }
  }

  override def readMap(): SeqReader = {
    next() match {
      case value:MapLiteral_ => new MapLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read map from seq literal(${literal.name}})")
    }
  }

  override def readSet(): SeqReader = {
    next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read set from seq literal(${literal.name}})")
    }
  }

  override def readList(): SeqReader = {
    next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read list from seq literal(${literal.name}})")
    }
  }

  override def readArray(length: Int): SeqReader = {
    next() match {
      case value:SeqLiteral_ => new SeqLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read array from seq literal(${literal.name}})")
    }
  }

  override def readBoolean(): Boolean = {
    next() match {
      case value:BooleanLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read boolean from seq literal(${literal.name}})")
    }
  }

  override def readString(): String = {
    next() match {
      case value:StringLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readFixed(length: Int, signed: Boolean): Long = {
    next() match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readVar(length: Int, signed: Boolean): Long = {
    next() match {
      case value:IntegerLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read string from seq literal(${literal.name}})")
    }
  }

  override def readMessage(): Reader = {
    next() match {
      case value:MessageLiteral_ => new MessageLiteralReader(value)
      case _ =>
        throw new ReadException(s"can't read message from seq literal(${literal.name}})")
    }
  }

  override def readDouble(): Double = {
    next() match {
      case value:FloatLiteral_ => value.value
      case _ =>
        throw new ReadException(s"can't read double from seq literal(${literal.name}})")
    }
  }

  override def readFloat(): Float = {
    next() match {
      case value:FloatLiteral_ => value.value.asInstanceOf[Float]
      case _ =>
        throw new ReadException(s"can't read float from seq literal(${literal.name}})")
    }
  }


  override def readNext(): Boolean = {
    readKey = true
    if(iterator.hasNext){
      pair = Some(iterator.next())
      true
    } else {
      pair = None
      false
    }
  }


}

object AttributeReader {
  def read(attribute:Attribute_):Any = {

    val message = Class.forName(attribute.valType.name).newInstance().asInstanceOf[PortableMessage]

    message.read(new MessageLiteralReader(attribute.valType))

    message
  }
}


