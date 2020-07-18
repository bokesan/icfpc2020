package de.bokeh.skred.red;

public class Data0 extends Data {

    private final int tag;
    
    Data0(int tag) {
        this.tag = tag;
    }

    @Override
    public int getNumFields() {
        return 0;
    }

    @Override
    public Node getField(int i) {
        throw new FieldIndexOutOfBoundsException(i, this);
    }

    @Override
    public int getTag() {
        return tag;
    }

}
