package lemon.messages.reflect

import java.util

trait MetadataResolver {
  def resolve(fullName:String):util.Map[String,Any]
}
