package de.bokeh.skred.red;

public class PrimLtX extends Function {

    public PrimLtX() {
        super("ltX", 2);
    }

    @Override
    Node exec(RedContext c) {
        c.rearrange2();
        c.eval();
        c.swap();
        c.eval();
        Node a2 = c.getTos();
        Node a1 = c.get1();
        boolean r = a1.intValue().compareTo(a2.intValue()) < 0;
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
