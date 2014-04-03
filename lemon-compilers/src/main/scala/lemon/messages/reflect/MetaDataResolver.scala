package lemon.messages.reflect

import java.util

trait MetaDataResolver {
  def resolve(fullName:String):util.Map[String,Any]
}



