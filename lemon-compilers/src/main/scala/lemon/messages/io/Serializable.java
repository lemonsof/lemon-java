package lemon.messages.io;

public interface Serializable {
    void read(MessageReader reader);
    void write(MessageWriter messageWriter);
}
