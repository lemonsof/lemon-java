package lemon.messages.io

import lemon.messages.reflect.MetaDataResolver

trait PortableMessage {

  def read(reader:Reader)

  def write(writer:Writer,resolver:MetaDataResolver)

}
