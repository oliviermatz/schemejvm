package io.github.matzoliv.schemejvm.runtime;

public class Symbol {
    private String name;

    public Symbol(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Symbol) {
            Symbol that = (Symbol)other;
            return (this.name == null || that.name == null ?
                    this.name == that.name :
                    this.name.equals(that.name));
        }
        return false;
    }

    @Override
    public int hashCode() {
        return this.name.hashCode();
    }

    @Override
    public String toString() {
        return this.name;
    }
}
