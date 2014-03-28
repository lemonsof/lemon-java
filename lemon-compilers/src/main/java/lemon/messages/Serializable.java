package lemon.messages;

import java.util.Map;

public interface Serializable {
    void read(Reader reader);
    void write(MessageWriter messageWriter);

}
