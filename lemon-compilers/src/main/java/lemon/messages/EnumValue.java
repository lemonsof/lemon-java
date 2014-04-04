package lemon.messages;


public final class EnumValue {
    private String  name;
    private Long    value;

    public EnumValue(String name) {
        this.name = name;
    }
    public EnumValue(String name, long value) {
        this.name = name;
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Long getValue() {
        return value;
    }

    public void setValue(long value) {
        this.value = value;
    }
}
