package de.bokeh.skred.red;

public class PrimSend extends Function {

    public PrimSend() {
        super("send", 1);
    }

    @Override
    Node exec(RedContext c) {
        Node a_x = c.getArg1();
        System.out.println("SEND: " + a_x.toString(false, 100));
        throw new AssertionError();
    }


}
