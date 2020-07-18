package de.bokeh.skred.red;

public class PrimNegate extends Function {

    public PrimNegate() {
        super("negate", 1);
    }

    @Override
    Node exec(RedContext c) {
        c.setTos(c.getArg(1));
        c.eval();
        Node x = c.getTos();
        Int r = Int.valueOf(x.intValue().negate());
        c.pop1();
        c.getTos().overwriteInd(r);
        c.setTos(r);
        return r;
    }

}
