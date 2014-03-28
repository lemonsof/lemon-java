package lemon.messages

import lemon.compilers.frontend.MessageIL


final class XmlWriter(name:String) extends Writer{

  def this() = this(null)

  private var root =if(null != name) {
    <message id={name}> </message>
  } else {
    <message> </message>
  }

  override def endWriterSupper(writer: Writer): Unit = {
    root = root.copy(child = root.child ++ writer.asInstanceOf[XmlWriter].root)
  }

  override def beginWriterSupper(message: MessageIL): Writer = new XmlWriter()


}
