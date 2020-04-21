package io.github.matzoliv.schemejvm.runtime;

public class ReflectionUtils {
    public static Class<?> getClassForName(final String className) throws ClassNotFoundException {
        switch (className) {
            case "boolean":
                return boolean.class;
            case "byte":
                return byte.class;
            case "short":
                return short.class;
            case "int":
                return int.class;
            case "long":
                return long.class;
            case "float":
                return float.class;
            case "double":
                return double.class;
            case "char":
                return char.class;
            case "void":
                return void.class;
            case "boolean[]":
                return boolean[].class;
            case "byte[]":
                return byte[].class;
            case "short[]":
                return short[].class;
            case "int[]":
                return int[].class;
            case "long[]":
                return long[].class;
            case "float[]":
                return float[].class;
            case "double[]":
                return double[].class;
            case "char[]":
                return char[].class;
            default:
                return Class.forName(className);
        }
    }

    public static char intToChar(int n) {
        return (char)n;
    }

    public static int charToInt(char c) {
        return (int)c;
    }
}
