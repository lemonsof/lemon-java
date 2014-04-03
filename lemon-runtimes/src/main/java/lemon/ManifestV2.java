
package lemon;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import lemon.messages.ConstraintException;
import lemon.messages.io.PortableMessage;
import lemon.messages.io.Reader;
import lemon.messages.io.SeqReader;
import lemon.messages.io.SeqWriter;
import lemon.messages.io.Writer;
import lemon.messages.reflect.MetaDataResolver;

public class ManifestV2
    extends Manifest
    implements PortableMessage
{

    private Map<String, String> properties;
    private List<List<String>> actors2;
    private Map<String, Map<String, Integer>> properties2;

    public Map<String, String> getProperties() {
        return this.properties;
    }

    public void setProperties(final Map<String, String> properties) {
        this.properties = properties;
    }

    public List<List<String>> getActors2() {
        return this.actors2;
    }

    public void setActors2(final List<List<String>> actors2) {
        this.actors2 = actors2;
    }

    public Map<String, Map<String, Integer>> getProperties2() {
        return this.properties2;
    }

    public void setProperties2(final Map<String, Map<String, Integer>> properties2) {
        this.properties2 = properties2;
    }

    public ManifestV2 clone(final ManifestV2 target) {
        super.clone(target);
        Map<String, String> properties0 = new HashMap<String, String>();
        for (Map.Entry<String, String> properties0_: this.properties.entrySet()) {
            properties0 .put(properties0_.getKey(), properties0_.getValue());
        }
        target.properties = properties0;
        List<List<String>> actors20 = new ArrayList<List<String>>();
        for (List<String> actors20_: this.actors2) {
            List<String> actors21 = new ArrayList<String>();
            for (String actors21_: actors20_) {
                actors21 .add(actors21_);
            }
            actors20 .add(actors21);
        }
        target.actors2 = actors20;
        Map<String, Map<String, Integer>> properties20 = new HashMap<String, Map<String, Integer>>();
        for (Map.Entry<String, Map<String, Integer>> properties20_: this.properties2 .entrySet()) {
            Map<String, Integer> properties21 = new HashMap<String, Integer>();
            for (Map.Entry<String, Integer> properties21_: properties20_.getValue().entrySet()) {
                properties21 .put(properties21_.getKey(), properties21_.getValue());
            }
            properties20 .put(properties20_.getKey(), properties21);
        }
        target.properties2 = properties20;
        return target;
    }

    @Override
    public void write(final Writer writer, final MetaDataResolver resolver) {
        writer.begin(resolver.resolve("lemon.ManifestV2"));
        super.write(writer.writeSupper(), resolver);
        if (!(this.properties == null)) {
            SeqWriter propertiesWriter = writer.writeMap("properties", 0);
            for (Map.Entry<String, String> entry0 : this.properties.entrySet()) {
                propertiesWriter.writeNext();
                propertiesWriter.writeString(entry0 .getKey());
                propertiesWriter.writeString(entry0 .getValue());
            }
        }
        if (!(this.actors2 == null)) {
            SeqWriter actors2Writer = writer.writeList("actors2", 0);
            for (List<String> current0 : this.actors2) {
                actors2Writer.writeNext();
                SeqWriter writer1 = actors2Writer.writeList();
                for (String current2 : current0) {
                    writer1 .writeNext();
                    writer1 .writeString(current2);
                }
            }
        }
        if (!(this.properties2 == null)) {
            SeqWriter properties2Writer = writer.writeMap("properties2", 0);
            for (Map.Entry<String, Map<String, Integer>> entry0 : this.properties2 .entrySet()) {
                properties2Writer.writeNext();
                properties2Writer.writeString(entry0 .getKey());
                SeqWriter writer10 = properties2Writer.writeSet();
                for (Map.Entry<String, Integer> entry11 : entry0 .getValue().entrySet()) {
                    writer10 .writeNext();
                    writer10 .writeString(entry11 .getKey());
                    writer10 .writeVar(4, true, entry11 .getValue());
                }
            }
        }
        writer.end();
    }

    @Override
    public void read(final Reader read)
        throws Exception
    {
        super.read(read.readSupper());
        this.properties = new HashMap<String, String>();
        SeqReader propertiesReader = read.readMap("properties", 0);
        while (propertiesReader.readNext()) {
            this.properties.put(propertiesReader.readString(), propertiesReader.readString());
        }
        try {
            this.actors2 = new ArrayList<List<String>>();
            SeqReader actors2Reader = read.readList("actors2", 0);
            while (actors2Reader.readNext()) {
                ArrayList<String> list1 = new ArrayList<String>();
                SeqReader reader1 = actors2Reader.readList();
                while (reader1 .readNext()) {
                    list1 .add(reader1 .readString());
                }
                this.actors2 .add(list1);
            }
        } catch (ConstraintException ignored) {
        }
        try {
            this.properties2 = new HashMap<String, Map<String, Integer>>();
            SeqReader properties2Reader = read.readMap("properties2", 0);
            while (properties2Reader.readNext()) {
                HashMap<String, Integer> map10 = new HashMap<String, Integer>();
                SeqReader reader10 = properties2Reader.readMap();
                while (reader10 .readNext()) {
                    map10 .put(reader10 .readString(), ((int) reader10 .readVar(4, true)));
                }
                this.properties2 .put(properties2Reader.readString(), map10);
            }
        } catch (ConstraintException ignored) {
        }
    }

}
