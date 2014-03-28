package lemon.compilers.frontend

case class TypeRedefinitionError(lhs:IL,rhs:IL) extends Exception(s"redefinition error :${lhs.Name}"){

}
