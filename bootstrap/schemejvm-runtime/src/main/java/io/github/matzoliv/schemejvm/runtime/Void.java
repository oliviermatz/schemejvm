package io.github.matzoliv.schemejvm.runtime;

public class Void {
    public static Void value = new Void();

    @Override
    public String toString() {
        return "#!void";
    }
}
