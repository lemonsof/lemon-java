package lemon.compilers.frontend

import lemon.messages.reflect.IR

case class TypeAmbiguityError(name:String,foundSymbols:List[IR]) extends Exception(s"Ambiguity type definition:$name"){

}
