
package lemon.java;

import lemon.messages.ConstraintException;
import lemon.messages.io.PortableMessage;
import lemon.messages.io.Reader;
import lemon.messages.io.Writer;
import lemon.messages.reflect.MetaDataResolver;

public class Java
    implements PortableMessage
{

    private String mapping;

    public String getMapping() {
        return this.mapping;
    }

    public void setMapping(final String mapping) {
        this.mapping = mapping;
    }

    public Java clone(final Java target) {
        target.mapping = this.mapping;
        return target;
    }

    @Override
    public void write(final Writer writer, final MetaDataResolver resolver) {
        writer.begin(resolver.resolve("lemon.java.Java"));
        if (!(this.mapping == null)) {
            writer.writeString("mapping", 0, this.mapping, resolver.resolve("lemon.java.Java.mapping"));
        }
        writer.end();
    }

    @Override
    public void read(final Reader read)
        throws Exception
    {
        try {
            read.readString("mapping", 0);
            this.mapping = read.readString("mapping", 0);
        } catch (ConstraintException ignored) {
        }
    }

}
