package io.github.matzoliv.schemejvm.runtime;

public class TaggedObject {
    private String tag;
    private Object object;

    public TaggedObject(String tag, Object object) {
        this.tag = tag;
        this.object = object;
    }

    public String getTag() {
        return tag;
    }

    public Object getObject() {
        return object;
    }

    public void setObject(Object object) {
        this.object = object;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof TaggedObject) {
            TaggedObject that = (TaggedObject)other;
            return (this.tag == null || that.tag == null ?
                    this.tag == that.tag :
                    this.tag.equals(that.tag))
                    &&
                    (this.object == null || that.object == null ?
                    this.object == that.object :
                    this.object.equals(that.object));
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = 37 * hash + (this.tag == null ? 0 : this.tag.hashCode());
        hash = 37 * hash + (this.object == null ? 0 : this.object.hashCode());
        return hash;
    }

    @Override
    public String toString() {
        return String.format("#%s[%s]", tag, object);
    }
}
