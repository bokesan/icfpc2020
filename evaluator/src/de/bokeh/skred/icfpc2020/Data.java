package de.bokeh.skred.icfpc2020;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Data {

    private static final Data NIL_VALUE = new Data(Type.NIL, 0, 0, null);

    public static final Data ZERO = number(0);

    public enum Type {
        NUM,
        VEC,
        LIST,
        NIL
    }

    private static class IntRef {
        int value;

        void advance() {
            value++;
        }
    }

    private final Type type;
    private final long numValue;
    private final long second;
    private final List<Data> elems;

    private Data(Type type, long numValue, long second, List<Data> elems) {
        this.type = type;
        this.numValue = numValue;
        this.second = second;
        this.elems = elems;
    }

    public static Data number(long value) {
        return new Data(Type.NUM, value, 0, null);
    }

    public static Data vec(long x, long y) {
        return new Data(Type.VEC, x, y, null);
    }

    public static Data list(Data... elems) {
        if (elems.length == 0) {
            return NIL_VALUE;
        }
        return new Data(Type.LIST, 0, 0, Arrays.asList(elems));
    }

    public static Data parse(String s) {
        IntRef pos = new IntRef();
        Data value = parse(s, pos);
        if (pos.value < s.length()) {
            throw new IllegalArgumentException("invalid data item: " + s);
        }
        return value;
    }

    private static Data parse(String s, IntRef pos) {
        skipSpace(s, pos);
        switch (s.charAt(pos.value)) {
            case '[':
                return parseVec(s, pos);
            case '(':
                return parseList(s, pos);
            case 'n':
                pos.value += 3;
                return NIL_VALUE;
            default:
                return number(parseNumber(s, pos));
        }
    }

    private static long parseNumber(String s, IntRef pos) {
        int start = pos.value;
        do {
            pos.advance();
        } while (Character.isDigit(s.charAt(pos.value)));
        return Long.parseLong(s.substring(start, pos.value));
    }

    private static Data parseVec(String s, IntRef pos) {
        pos.advance();
        skipSpace(s, pos);
        long x = parseNumber(s, pos);
        expect(',', s, pos);
        skipSpace(s, pos);
        long y = parseNumber(s, pos);
        expect(']', s, pos);
        return vec(x, y);
    }

    private static Data parseList(String s, IntRef pos) {
        List<Data> xs = new ArrayList<>();
        skipSpace(s, pos);
        while (s.charAt(pos.value) != ')') {
            pos.advance();
            Data x = parse(s, pos);
            xs.add(x);
            skipSpace(s, pos);
        }
        pos.advance();
        return new Data(Type.LIST, 0, 0, xs);
    }

    private static void expect(char expected, String s, IntRef pos) {
        skipSpace(s, pos);
        if (s.charAt(pos.value) != expected) {
            throw new IllegalArgumentException("invalid data item ['" + expected + "' expected at " + pos.value + "]: " + s);
        }
        pos.advance();
    }

    private static void skipSpace(String s, IntRef pos) {
        while (pos.value < s.length() && s.charAt(pos.value) == ' ') {
            pos.advance();
        }
        if (pos.value >= s.length()) {
            throw new IllegalArgumentException("invalid data item: " + s);
        }
    }

    public Point toPoint() {
        if (type != Type.VEC) {
            throw new IllegalArgumentException("not a vec: " + this);
        }
        return Point.of((int) numValue, (int) second);
    }

    public Data nth(int i) {
        if (type != Type.LIST) {
            throw new IllegalArgumentException("not a list: " + this);
        }
        if (i < 0 || i >= elems.size()) {
            throw new IndexOutOfBoundsException("list of length " + elems.size() + " at index " + i);
        }
        return elems.get(i);
    }

    public int length() {
        switch (type) {
            case NIL: return 0;
            case LIST: return elems.size();
            default:
                throw new IllegalArgumentException("not a list: " + this);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Data data = (Data) o;
        return  type == data.type &&
                numValue == data.numValue &&
                second == data.second &&
                Objects.equals(elems, data.elems);
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, numValue, second, elems);
    }

    @Override
    public String toString() {
        switch (type) {
            case NUM:
                return Long.toString(numValue);
            case VEC:
                return "[" + numValue + "," + second + "]";
            case LIST:
                StringBuilder b = new StringBuilder();
                char sep = '(';
                for (Data x : elems) {
                    b.append(sep);
                    sep = ',';
                    b.append(x);
                }
                b.append(')');
                return b.toString();
            case NIL:
                return "nil";
            default:
                throw new AssertionError();
        }
    }

}
