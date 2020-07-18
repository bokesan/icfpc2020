package de.bokeh.skred.icfpc2020;

import java.math.BigInteger;

public class Modulate {

    public static String modulate(BigInteger n) {
        StringBuilder signal = new StringBuilder();
        if (n.signum() >= 0) {
            signal.append("01");
        } else {
            signal.append("10");
            n = n.negate();
        }
        BigInteger ceil = BigInteger.ONE;
        while (n.compareTo(ceil) >= 0) {
            signal.append('1');
            ceil = ceil.multiply(BigInteger.valueOf(16));
        }
        signal.append('0');
        // TODO: complete
        return signal.toString();
    }

}
