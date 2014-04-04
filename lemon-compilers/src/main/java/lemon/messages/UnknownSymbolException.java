package lemon.messages;


public class UnknownSymbolException extends Exception{
    public UnknownSymbolException(String name) {
        super("unknown symbol:" + name);
    }
}
