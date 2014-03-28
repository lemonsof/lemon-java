package lemon.messages;


import lemon.compilers.frontend.FieldIL;
import lemon.compilers.frontend.MessageIL;

public interface Writer {

    Writer createSupperWriter(MessageIL message) throws Exception;

    void closeSupperWriter(Writer writer) throws Exception;

    void writeVar(FieldIL field,long value) throws Exception;

    void writeFixed(FieldIL fieldIR,long value) throws Exception;

    void writeFloat(FieldIL fieldIR,float value) throws Exception;

    void writeDouble(FieldIL fieldIR,float value) throws Exception;


}

