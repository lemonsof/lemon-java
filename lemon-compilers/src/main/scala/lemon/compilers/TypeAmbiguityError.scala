package lemon.compilers


case class TypeAmbiguityError(name:String,foundSymbols:List[ILL]) extends Exception(s"Ambiguity type definition:$name"){

}
