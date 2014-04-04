
package lemon;

import java.util.ArrayList;
import java.util.List;
import lemon.messages.ConstraintException;
import lemon.messages.EnumValue;
import lemon.messages.UnknownEnumValueException;
import lemon.messages.io.PortableMessage;
import lemon.messages.io.Reader;
import lemon.messages.io.SeqReader;
import lemon.messages.io.SeqWriter;
import lemon.messages.io.Writer;
import lemon.messages.reflect.MetaDataResolver;

public class Manifest
    implements PortableMessage
{

    private lemon.GlobalName appName;
    private String version;
    private double description;
    private List<String> actors;
    private lemon.ServiceType[] clients = new lemon.ServiceType[ 10 ] ;
    private int[] others = new int[ 10 ] ;

    public lemon.GlobalName getAppName() {
        return this.appName;
    }

    public void setAppName(final lemon.GlobalName appName) {
        this.appName = appName;
    }

    public String getVersion() {
        return this.version;
    }

    public void setVersion(final String version) {
        this.version = version;
    }

    public double getDescription() {
        return this.description;
    }

    public void setDescription(final double description) {
        this.description = description;
    }

    public List<String> getActors() {
        return this.actors;
    }

    public void setActors(final List<String> actors) {
        this.actors = actors;
    }

    public lemon.ServiceType[] getClients() {
        return this.clients;
    }

    public int[] getOthers() {
        return this.others;
    }

    public Manifest clone(final Manifest target) {
        target.appName = this.appName.clone(new lemon.GlobalName());
        target.version = this.version;
        target.description = this.description;
        List<String> actors0 = new ArrayList<String>();
        for (String actors0_: this.actors) {
            actors0 .add(actors0_);
        }
        target.actors = actors0;
        lemon.ServiceType[] clients0 = new lemon.ServiceType[ 10 ] ;
        System.arraycopy(this.clients, 0, clients0, 0, 10);
        target.clients = clients0;
        int[] others0 = new int[ 10 ] ;
        System.arraycopy(this.others, 0, others0, 0, 10);
        target.others = others0;
        return target;
    }

    @Override
    public void write(final Writer writer, final MetaDataResolver resolver) {
        writer.begin(resolver.resolve("lemon.Manifest"));
        if (!(this.appName == null)) {
            this.appName.write(writer.writeMessage("appName", 0, resolver.resolve("lemon.Manifest.appName")), resolver);
        }
        if (!(this.version == null)) {
            writer.writeString("version", 1, this.version, resolver.resolve("lemon.Manifest.version"));
        }
        writer.writeDouble("description", 2, this.description, resolver.resolve("lemon.Manifest.description"));
        if (!(this.actors == null)) {
            SeqWriter actorsWriter = writer.writeList("actors", 3, resolver.resolve("lemon.Manifest.actors"));
            for (String current0 : this.actors) {
                actorsWriter.writeNext();
                actorsWriter.writeString(current0);
            }
        }
        if (!(this.clients == null)) {
            SeqWriter clientsWriter = writer.writeArray("clients", 4, 10, resolver.resolve("lemon.Manifest.clients"));
            for (lemon.ServiceType current0 : this.clients) {
                clientsWriter.writeNext();
                clientsWriter.writeEnum(1, new EnumValue(current0 .toString(), current0 .getValue()));
            }
        }
        if (!(this.others == null)) {
            SeqWriter othersWriter = writer.writeArray("others", 5, 10, resolver.resolve("lemon.Manifest.others"));
            for (int current0 : this.others) {
                othersWriter.writeNext();
                othersWriter.writeFixed(4, true, current0);
            }
        }
        writer.end();
    }

    @Override
    public void read(final Reader read)
        throws Exception
    {
        try {
            this.appName = new lemon.GlobalName();
            this.appName.read(read.readMessage("appName", 0));
            read.readMessage("appName", 0);
        } catch (ConstraintException ignored) {
        }
        try {
            read.readString("version", 1);
            this.version = read.readString("version", 1);
        } catch (ConstraintException ignored) {
        }
        try {
            read.readDouble("description", 2);
            this.description = read.readDouble("description", 2);
        } catch (ConstraintException ignored) {
        }
        try {
            this.actors = new ArrayList<String>();
            SeqReader actorsReader = read.readList("actors", 3);
            while (actorsReader.readNext()) {
                this.actors.add(actorsReader.readString());
            }
        } catch (ConstraintException ignored) {
        }
        try {
            SeqReader clientsReader = read.readArray("clients", 4, 10);
            for (int i = 0; ((i< 10)&&clientsReader.readNext()); i ++) {
                lemon.ServiceType enum1 = null;
                EnumValue enumValue1 = clientsReader.readEnum(1);
                if (enumValue1 .getName() == null) {
                    for (lemon.ServiceType current: lemon.ServiceType.values()) {
                        if (current.getValue() == enumValue1 .getValue()) {
                            enum1 = current;
                            break;
                        }
                    }
                    if (enum1 == null) {
                        throw new UnknownEnumValueException("lemon.ServiceType", enumValue1 .getValue());
                    }
                } else {
                    lemon.ServiceType.valueOf(enumValue1 .getName());
                    enum1 = lemon.ServiceType.valueOf(enumValue1 .getName());
                }
                this.clients[i] = enum1;
            }
        } catch (ConstraintException ignored) {
        }
        try {
            SeqReader othersReader = read.readArray("others", 5, 10);
            for (int i = 0; ((i< 10)&&othersReader.readNext()); i ++) {
                this.others[i] = ((int) othersReader.readFixed(4, true));
            }
        } catch (ConstraintException ignored) {
        }
    }

}
