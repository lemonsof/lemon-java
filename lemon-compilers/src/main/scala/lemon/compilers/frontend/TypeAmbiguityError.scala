package lemon.compilers.frontend

import lemon.messages.reflect.IR

case class TypeAmbiguityError(ref:IR,foundSymbols:List[IR]) extends Exception(s"Ambiguity type definition:${ref.Name}"){

}
