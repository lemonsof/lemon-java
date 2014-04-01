package lemon.compilers.frontend

import lemon.messages.reflect.IR

case class TypeRedefinitionError(lhs:IR,rhs:IR) extends Exception(s"redefinition error :${lhs.Name}"){

}
