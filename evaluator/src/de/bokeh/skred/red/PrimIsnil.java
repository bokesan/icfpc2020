package de.bokeh.skred.red;

public class PrimIsnil extends Function {

    public PrimIsnil() {
        super("isnil", 1);
    }

    @Override
    Node exec(RedContext c) {
        c.setTos(c.getArg(1));
        c.eval();
        Node tos = c.getTos();
        c.pop1();
        Node r;
        if (tos.isApp() && isK(tos.getFun()) && isK(tos.getArg())) {
            c.getTos().overwriteInd(tos.getFun()); // TRUE
            c.setTos(tos.getFun());
        } else {
            c.getTos().overwriteApp(Function.valueOf("S"), tos.getArg());
        }
        return null;
    }

    private static boolean isK(Node node) {
        return (node instanceof Function) && (((Function) node).getName().equals("K"));
    }

}
