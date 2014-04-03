package lemon.messages.io

import lemon.messages.reflect.MetaDataResolver

trait PortableMessage {

  @throws[Exception]
  def read(reader:Reader)
  @throws[Exception]
  def write(writer:Writer,resolver:MetaDataResolver)

}
