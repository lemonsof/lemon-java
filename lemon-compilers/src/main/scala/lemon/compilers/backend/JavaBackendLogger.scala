package lemon.compilers.backend

import lemon.compilers.frontend.{FieldIL, AttributeIL, MessageIL}
import com.sun.codemodel.{JCodeModel, JFieldVar, JAnnotationUse, JDefinedClass}
import org.slf4j.LoggerFactory


trait JavaBackendLogger extends JavaBackend{

  private def logger = LoggerFactory.getLogger(classOf[JavaBackendLogger])

//  abstract override protected def codeModel: JCodeModel = super.codeModel

  abstract override protected def omit(message: MessageIL): JDefinedClass = {
    logger.debug(s"generate message ${message.name}")
    val model = super.omit(message)
    logger.debug(s"generate message ${message.name} -- success")
    model
  }

  abstract override protected def omit(
                               message: MessageIL,
                               field: FieldIL,
                               attribute: AttributeIL,
                               model: JDefinedClass,
                               fieldModel: JFieldVar): Option[JAnnotationUse] = {

    logger.debug(s"generate field attribute (${attribute.target} : ${attribute.valType})")
    val result =super.omit(message,field,attribute,model,fieldModel)
    logger.debug(s"generate field attribute (${attribute.target} : ${attribute.valType}) -- success")
    result
  }

  abstract override protected def omit(
                               message: MessageIL,
                               field: FieldIL,
                               model: JDefinedClass): JFieldVar = {

    logger.debug(s"generate field ${field.name}")
    val result =super.omit(message,field,model)
    logger.debug(s"generate field ${field.name} -- success")
    result
  }

  abstract override protected def omit(
                               message: MessageIL,
                               attribute: AttributeIL,
                               model: JDefinedClass): Option[JAnnotationUse] = {


    logger.debug(s"generate message attribute (${attribute.target} : ${attribute.valType})")
    val result =super.omit(message,attribute,model)
    logger.debug(s"generate message attribute (${attribute.target} : ${attribute.valType}) -- success")
    result
  }
}
