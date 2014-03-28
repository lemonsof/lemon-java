package lemon.compilers.frontend

case class TypeAmbiguityError(name:String,foundSymbols:List[IL]) extends Exception(s"Ambiguity type definition:$name"){

}
