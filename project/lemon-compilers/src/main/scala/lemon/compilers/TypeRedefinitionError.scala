package lemon.compilers


case class TypeRedefinitionError(lhs:ILL,rhs:ILL) extends Exception(s"redefinition error :${lhs.Name}"){

}
