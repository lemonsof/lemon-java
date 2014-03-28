package lemon.compilers.backend

import com.sun.codemodel._
import lemon.compilers.frontend._
import java.util
import lemon.compilers.frontend.SetIL
import lemon.compilers.frontend.MessageIL
import lemon.compilers.frontend.MapIL
import lemon.compilers.frontend.AttributeIL
import lemon.compilers.frontend.FieldIL
import lemon.compilers.frontend.ListIL
import lemon.compilers.frontend.VarIL
import lemon.compilers.frontend.FloatIL
import lemon.compilers.frontend.ArrayIL
import lemon.compilers.frontend.FixedIL

abstract class JavaBackend {
  protected def codeModel:JCodeModel

  protected def omit(message:MessageIL):JDefinedClass

  protected def omit(message:MessageIL,attribute:AttributeIL,model :JDefinedClass):Option[JAnnotationUse]

  protected def omit(message:MessageIL,field:FieldIL,model :JDefinedClass):JFieldVar

  protected def omit(message:MessageIL,field:FieldIL,attribute:AttributeIL,model :JDefinedClass,fieldModel:JFieldVar):Option[JAnnotationUse]

  protected def getJType(current:IL):JType = {
    current match {
      case ArrayIL(valType,length) =>
        getJType(valType).array()
      case ListIL(valType) =>
        codeModel.ref(classOf[util.List[_]]).narrow(getJType(valType))
      case SetIL(valType) =>
        codeModel.ref(classOf[util.Set[_]]).narrow(getJType(valType))
      case MapIL(keyType,valType) =>
        codeModel.ref(classOf[util.Map[_,_]]).narrow(getJType(keyType)).narrow(getJType(valType))
      case _:StringIL =>
        codeModel.ref(classOf[String])
      case VarIL(length,signed) => length match {
        case 1 => codeModel.ref(classOf[Byte])
        case 2 => codeModel.ref(classOf[Short])
        case 4 => codeModel.ref(classOf[Int])
        case 8 => codeModel.ref(classOf[Long])
      }
      case FixedIL(length,_) => length match {
        case 1 => codeModel.ref(classOf[Byte])
        case 2 => codeModel.ref(classOf[Short])
        case 4 => codeModel.ref(classOf[Int])
        case 8 => codeModel.ref(classOf[Long])
      }
      case FloatIL(length) => length match {
        case 4 => codeModel.ref(classOf[Float])
        case 8 => codeModel.ref(classOf[Double])
      }
      case ReferenceIL(name) => codeModel.ref(name)
    }
  }
}
