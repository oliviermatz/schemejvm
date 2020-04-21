package io.github.matzoliv.schemejvm.runtime;

public class Eof {
    public static Eof value = new Eof();

    @Override
    public String toString() {
        return "#!eof";
    }
}
