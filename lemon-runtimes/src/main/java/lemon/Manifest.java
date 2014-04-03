
package lemon;

import java.util.ArrayList;
import java.util.List;
import lemon.messages.ConstraintException;
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
    private String description;
    private List<String> actors;
    private lemon.GlobalName[] clients = new lemon.GlobalName[ 10 ] ;
    private int[] others = new int[ 10 ] ;
    private int hello;

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

    public String getDescription() {
        return this.description;
    }

    public void setDescription(final String description) {
        this.description = description;
    }

    public List<String> getActors() {
        return this.actors;
    }

    public void setActors(final List<String> actors) {
        this.actors = actors;
    }

    public lemon.GlobalName[] getClients() {
        return this.clients;
    }

    public int[] getOthers() {
        return this.others;
    }

    public int getHello() {
        return this.hello;
    }

    public void setHello(final int hello) {
        this.hello = hello;
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
        lemon.GlobalName[] clients0 = new lemon.GlobalName[ 10 ] ;
        System.arraycopy(this.clients, 0, clients0, 0, 10);
        target.clients = clients0;
        int[] others0 = new int[ 10 ] ;
        System.arraycopy(this.others, 0, others0, 0, 10);
        target.others = others0;
        target.hello = this.hello;
        return target;
    }

    @Override
    public void write(final Writer writer, final MetaDataResolver resolver) {
        writer.begin(resolver.resolve("lemon.Manifest"));
        if (!(this.appName == null)) {
            this.appName.write(writer.writeMessage("appName", 0, resolver.resolve("lemon.Manifest.appName")), resolver);
        }
        if (!(this.version == null)) {
            writer.writeString("version", 0, this.version, resolver.resolve("lemon.Manifest.version"));
        }
        if (!(this.description == null)) {
            writer.writeString("description", 0, this.description, resolver.resolve("lemon.Manifest.description"));
        }
        if (!(this.actors == null)) {
            SeqWriter actorsWriter = writer.writeList("actors", 0);
            for (String current0 : this.actors) {
                actorsWriter.writeNext();
                actorsWriter.writeString(current0);
            }
        }
        if (!(this.clients == null)) {
            SeqWriter clientsWriter = writer.writeArray("clients", 0, 10);
            for (lemon.GlobalName current0 : this.clients) {
                clientsWriter.writeNext();
                current0 .write(clientsWriter.writeMessage(), resolver);
            }
        }
        if (!(this.others == null)) {
            SeqWriter othersWriter = writer.writeArray("others", 0, 10);
            for (int current0 : this.others) {
                othersWriter.writeNext();
                othersWriter.writeFixed(4, true, current0);
            }
        }
        writer.writeVar("hello", 0, 4, true, this.hello, resolver.resolve("lemon.Manifest.hello"));
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
            read.readString("version", 0);
            this.version = read.readString("version", 0);
        } catch (ConstraintException ignored) {
        }
        try {
            read.readString("description", 0);
            this.description = read.readString("description", 0);
        } catch (ConstraintException ignored) {
        }
        try {
            this.actors = new ArrayList<String>();
            SeqReader actorsReader = read.readList("actors", 0);
            while (actorsReader.readNext()) {
                this.actors.add(actorsReader.readString());
            }
        } catch (ConstraintException ignored) {
        }
        try {
            SeqReader clientsReader = read.readArray("clients", 0, 10);
            for (int i = 0; ((i< 10)&&clientsReader.readNext()); i ++) {
                lemon.GlobalName message1 = new lemon.GlobalName();
                Reader messageReader1 = clientsReader.readMessage();
                message1 .read(messageReader1);
                this.clients[i] = message1;
            }
        } catch (ConstraintException ignored) {
        }
        try {
            SeqReader othersReader = read.readArray("others", 0, 10);
            for (int i = 0; ((i< 10)&&othersReader.readNext()); i ++) {
                this.others[i] = ((int) othersReader.readFixed(4, true));
            }
        } catch (ConstraintException ignored) {
        }
        try {
            read.readVar("hello", 0, 4, true);
            this.hello = ((int) read.readVar("hello", 0, 4, true));
        } catch (ConstraintException ignored) {
        }
    }

}
