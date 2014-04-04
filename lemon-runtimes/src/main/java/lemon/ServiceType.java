
package lemon;


public enum ServiceType {

    Local(0L),
    Remote(1L);
    private long value;

    private ServiceType(final long value) {
        this.value = value;
    }

    public long getValue() {
        return this.value;
    }

}
