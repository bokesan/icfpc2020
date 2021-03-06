package de.bokeh.skred.red;

import java.util.*;


abstract public class Function extends Node {

    private final int numArgs;
    private final String name;
    private long evalCount = 0;
    private long unwindCount = 0;
    private long argCheckCount = 0;
    
    protected Function(String name, int numArgs) {
        this.numArgs = numArgs;
        this.name = name;
    }
    
    @Override
    public Node eval(RedContext c) {
        evalCount++;
        return this;
    }

    @Override
    public Node unwind(RedContext c) {
        unwindCount++;
        // System.err.println("unwinding: " + this + ", arity " + this.numArgs + ", " + c.numArgs() + " args on spine");
        if (c.numArgs() >= this.numArgs) {
            return exec(c);
        }
        argCheckCount++;
        return c.argCheckFailed();
    }
    
    @Override
    public Node getArg() {
        throw new RedException("getArg of function");
    }

    @Override
    public void overwriteApp(Node f, Node a) {
        throw new RedException("tried to overwrite function");
    }

    @Override
    public void overwriteInd(Node target) {
        throw new RedException("tried to overwrite function");
    }

    @Override
    public void overwriteHole() {
        throw new RedException("tried to overwrite function");
    }

    abstract Node exec(RedContext c);

    public String toString(boolean parens, int maxDepth) {
        return getName();
    }
    
    @Override
    public boolean hasVars() {
        return false;
    }
    
    // Factory

    private static final Map<String, Function> functionsByName = new HashMap<String, Function>();
    
    protected static final Function ERROR = new PrimError();
    private static Function I;
    /** I without eval for indirection. */
    public static final Function I_FOR_IND = new CombI();
    private static Function K;
    private static Function K1;
    protected static final Function S = new CombS();
    protected static final Function B = new CombB();
    protected static final Function C = new CombC();
    
    public static void init(boolean evalProjections) {
        if (evalProjections) {
            I = new CombI_Eval();
            K = new CombK_Eval();
            K1 = new CombK1_Eval();
        } else {
            I = new CombI();
            K = new CombK();
            K1 = new CombK1();
        }
        
        register(S);
        register(B);
        register(C);
        register(new CombS1());
        register(new CombB1());
        register(new CombC1());
        register(new CombBs());
        register(I);
        register(K);
        register(new CombY());
        register(new CombW());
        register(K1);
        // register(new CombCons());
        register(new PrimAddInt());
        register(new PrimMulInt());
        register(new PrimSubInt());
        register(new PrimQuotInt());
        register(new PrimRemInt());
        register(new PrimSucc());
        register(new PrimPred());
        register(new PrimNegate());
        // register(new PrimIsnil());
        register(new PrimRsubInt());
        register(new PrimRquotInt());
        register(new PrimRremInt());
        register(new PrimCompare());
        register(new PrimLess());
        register(new PrimLessEq());
        register(new PrimGreater());
        register(new PrimGreaterEq());
        register(new PrimEq());
        register(new PrimEqX());
        register(new PrimLtX());
        register(new PrimIs0());
        register(new PrimNeq());
        register(new PrimZero());
        register(new PrimTag());
        register(new PrimSend());
        
        register(ERROR);
        register(new PrimRead());
        register(new PrimStdPort());
        register(new PrimPutChar());
        register(new PrimGetChar());
    }
    
    public static Function valueOf(String name) {
        return functionsByName.get(name);
    }

    private static void register(Function f) {
        Function old = functionsByName.put(f.getName(), f);
        assert old == null;
    }
    
    public static class Stats {
        public final String name;
        public final long evalCount;
        public final long unwindCount;
        public final long argCheckCount;
        
        public Stats(String name, long eval, long unwind, long argCheck) {
            this.name = name;
            evalCount = eval;
            unwindCount = unwind;
            argCheckCount = argCheck;
        }
    }
    
    private static class CmpStatsUnwindDesc implements Comparator<Stats> {

        /* (non-Javadoc)
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        public int compare(Stats o1, Stats o2) {
            if (o1.unwindCount > o2.unwindCount)
                return -1;
            if (o1.unwindCount < o2.unwindCount)
                return 1;
            if (o1.evalCount > o2.evalCount)
                return -1;
            if (o1.evalCount < o2.evalCount)
                return 1;
            if (o1.argCheckCount > o2.argCheckCount)
                return -1;
            if (o1.argCheckCount < o2.argCheckCount)
                return 1;
            return o1.name.compareTo(o2.name);
        }
        
    }
    
    public static List<Stats> getStats() {
        ArrayList<Stats> st = new ArrayList<>();
        for (Function f : functionsByName.values()) {
            if (f.evalCount + f.unwindCount > 0) {
                st.add(new Stats(f.getName(), f.evalCount, f.unwindCount, f.argCheckCount));
            }
        }
        st.sort(new CmpStatsUnwindDesc());
        return st;
    }

    /**
     * @return the i
     */
    protected static Function getI() {
        return I;
    }

    /**
     * @return the k
     */
    protected static Function getK() {
        return K;
    }

    /**
     * @return the k1
     */
    protected static Function getK1() {
        return K1;
    }
    
    public int getArity() {
        return numArgs;
    }

    public static Node primPack(int tag, int arity) {
        if (arity == 0) {
            return Data.valueOf(tag);
        }
        String name = "Pack{" + tag + "," + arity + "}";
        Function f = functionsByName.get(name);
        if (f == null) {
            f = new PrimPack(tag, arity);
            functionsByName.put(name, f);
        }
        return f;
    }

    private static Function cache(Function f) {
        Function f1 = functionsByName.get(f.getName());
        if (f1 == null) {
            f1 = f;
            functionsByName.put(f.getName(), f);
        }
        return f1;
    }

    public static Function primCase(int[] arities, boolean def) {
        return cache(PrimCase.of(arities, def));
    }
    
    public static Function primUnpack(int arity) {
        return cache(new PrimUnpack(arity));
    }

    public static Function primGet(int index) {
	return cache(new PrimGet(index));
    }

    public String getName() {
        return name;
    }
}
