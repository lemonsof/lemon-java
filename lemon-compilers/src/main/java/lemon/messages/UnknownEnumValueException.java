package lemon.messages;


public class UnknownEnumValueException extends Exception {
    public UnknownEnumValueException(String enumType,long value) {
        super(enumType + ":unknown enum constant value :"+value);
    }
}
