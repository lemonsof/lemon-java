package lemon.messages;


public class ConstraintException extends Exception{

    private final String field;
    private final int id;

    public ConstraintException(String message,String field,int id) {
        super(message);
        this.field = field;
        this.id = id;
    }

    public String getField() {
        return field;
    }

    public int getId() {
        return id;
    }
}
