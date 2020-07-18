package de.bokeh.skred.red;

import java.math.BigInteger;

public class PrimIs0 extends Function {

    public PrimIs0() {
        super("if0", 3);
    }

    @Override
    Node exec(RedContext c) {
        Node n = c.getArg1();
        Node x0 = c.getArg2();
        Node x1 = c.getArg3();
        Node redex = c.get3();
        redex.overwriteHole();
        c.setTos(n);
        c.eval();
        boolean isZero = c.getTos().intValue().equals(BigInteger.ZERO);
        c.pop3();
        Node result = isZero ? x0 : x1;
        redex.overwriteInd(result);
        c.setTos(result);
        return null;
    }

    private static boolean isK(Node node) {
        return (node instanceof Function) && (((Function) node).getName().equals("K"));
    }

}
