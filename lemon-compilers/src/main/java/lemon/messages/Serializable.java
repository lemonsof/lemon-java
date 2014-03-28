package lemon.messages;

public interface Serializable {
    void read(Reader reader);
    void write(Writer writer);
}
