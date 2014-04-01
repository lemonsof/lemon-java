package lemon.messages.io;


public class FieldNotFoundException extends Exception{
    private final String name;
    private final Integer id;

    public FieldNotFoundException(String message, String name, Integer id) {
        super(message);
        this.name = name;
        this.id = id;
    }

    public FieldNotFoundException(String name,Integer id) {
        this.name = name;
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public Integer getId() {
        return id;
    }
}
