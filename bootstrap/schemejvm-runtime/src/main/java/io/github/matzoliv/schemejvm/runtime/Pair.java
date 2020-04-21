package io.github.matzoliv.schemejvm.runtime;

public class Pair {

    private Object car;
    private Object cdr;

    public Pair(Object car, Object cdr) {
        this.car = car;
        this.cdr = cdr;
    }

    public Object getCar() {
        return car;
    }

    public void setCar(Object car) {
        this.car = car;
    }

    public Object getCdr() {
        return cdr;
    }

    public void setCdr(Object cdr) {
        this.cdr = cdr;
    }

    @Override
    public boolean equals(Object other) {
        if (other instanceof Pair) {
            Pair that = (Pair)other;
            return (this.car == null || that.car == null ?
                    this.car == that.car :
                    this.car.equals(that.car))
                    &&
                    (this.cdr == null || that.cdr == null ?
                    this.cdr == that.cdr :
                    this.cdr.equals(that.cdr));
        }
        return false;
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = 37 * hash + (car == null ? 0 : car.hashCode());
        hash = 37 * hash + (cdr == null ? 0 : cdr.hashCode());
        return hash;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        Object cursor = this;
        sb.append("(");

        while (cursor != null && cursor instanceof Pair) {
            Pair cursorPair = (Pair)cursor;
            sb.append(cursorPair.car == null ? "()" : cursorPair.car.toString());
            cursor = cursorPair.cdr;
            if (cursor != null) {
                sb.append(" ");
            }
        }

        if (cursor != null) {
            sb.append(". ");
            sb.append(cursor.toString());
        }
        sb.append(")");
        return sb.toString();
    }
}
