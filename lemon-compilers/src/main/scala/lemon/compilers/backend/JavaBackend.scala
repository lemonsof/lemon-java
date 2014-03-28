package lemon.compilers.backend

import com.sun.codemodel.{JFieldVar, JAnnotationUse, JDefinedClass, JCodeModel}
import lemon.compilers.frontend.{FieldIL, AttributeIL, MessageIL}

abstract class JavaBackend {
  protected def codeModel:JCodeModel

  protected def omit(message:MessageIL):JDefinedClass

  protected def omit(message:MessageIL,attribute:AttributeIL,model :JDefinedClass):Option[JAnnotationUse]

  protected def omit(message:MessageIL,field:FieldIL,model :JDefinedClass):JFieldVar

  protected def omit(message:MessageIL,field:FieldIL,attribute:AttributeIL,model :JDefinedClass,fieldModel:JFieldVar):Option[JAnnotationUse]
}
