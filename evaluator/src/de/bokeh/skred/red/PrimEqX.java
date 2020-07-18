package de.bokeh.skred.red;

public class PrimEqX extends Function {

    public PrimEqX() {
        super("eqX", 2);
    }

    @Override
    Node exec(RedContext c) {
        c.rearrange2();
        c.eval();
        c.swap();
        c.eval();
        Node a2 = c.getTos();
        Node a1 = c.get1();
        boolean r = a1.intValue().equals(a2.intValue());
        Node result;
        c.pop2();
        if (r) {
            result = Function.getK();
            c.getTos().overwriteInd(result);
            c.setTos(result);
        } else {
            c.getTos().overwriteApp(Function.valueOf("S"), Function.getK());
            result = c.getTos();
        }
        return null;
    }

}
