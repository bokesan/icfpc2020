package de.bokeh.skred.red;

public class PrimTag extends Function {

    public PrimTag() {
        super("tag", 1);
    }

    @Override
    Node exec(RedContext c) {
        Node con = c.getArg1();
        c.setTos(con);
        c.eval();
        con = c.getTos();
        int tag = con.getTag();
        Node redex = c.get1();
        redex.overwriteInd(Int.valueOf(tag));
        c.pop1();
        return null;
    }

}
