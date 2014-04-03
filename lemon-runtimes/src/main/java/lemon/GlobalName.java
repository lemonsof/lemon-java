
package lemon;

import lemon.messages.ConstraintException;
import lemon.messages.io.PortableMessage;
import lemon.messages.io.Reader;
import lemon.messages.io.Writer;
import lemon.messages.reflect.MetaDataResolver;

public class GlobalName
    implements PortableMessage
{

    private int group;
    private int id;
    private boolean status;

    public int getGroup() {
        return this.group;
    }

    public void setGroup(final int group) {
        this.group = group;
    }

    public int getId() {
        return this.id;
    }

    public void setId(final int id) {
        this.id = id;
    }

    public boolean getStatus() {
        return this.status;
    }

    public void setStatus(final boolean status) {
        this.status = status;
    }

    public GlobalName clone(final GlobalName target) {
        target.group = this.group;
        target.id = this.id;
        target.status = this.status;
        return target;
    }

    @Override
    public void write(final Writer writer, final MetaDataResolver resolver) {
        writer.begin(resolver.resolve("lemon.GlobalName"));
        writer.writeVar("group", 0, 4, true, this.group, resolver.resolve("lemon.GlobalName.group"));
        writer.writeVar("id", 0, 4, true, this.id, resolver.resolve("lemon.GlobalName.id"));
        writer.writeBoolean("status", 0, this.status, resolver.resolve("lemon.GlobalName.status"));
        writer.end();
    }

    @Override
    public void read(final Reader read)
        throws Exception
    {
        try {
            read.readVar("group", 0, 4, true);
            this.group = ((int) read.readVar("group", 0, 4, true));
        } catch (ConstraintException ignored) {
        }
        try {
            read.readVar("id", 0, 4, true);
            this.id = ((int) read.readVar("id", 0, 4, true));
        } catch (ConstraintException ignored) {
        }
        try {
            read.readBoolean("status", 0);
            this.status = read.readBoolean("status", 0);
        } catch (ConstraintException ignored) {
        }
    }

}
