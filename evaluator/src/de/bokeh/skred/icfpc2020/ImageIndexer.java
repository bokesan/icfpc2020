package de.bokeh.skred.icfpc2020;

import java.util.Iterator;

public class ImageIndexer implements Iterable<Integer> {

    private final int  minValue;
    private final int maxValue;
    private final int maxAbs;

    private class SignFlippingIterator implements Iterator<Integer> {

        private int next = 0;

        @Override
        public boolean hasNext() {
            return Math.abs(next) <= maxAbs;
        }

        @Override
        public Integer next() {
            int curr = next;
            if (next >= 0) {
                if (next == 0) {
                    next = 1;
                } else {
                    next = -next;
                    if (next < minValue)
                        next = 1 - next;
                }
            } else {
                next = 1 - next;
                if (next > maxValue)
                    next = -next;
            }
            return curr;
        }
    }

    private class IntIterator implements Iterator<Integer> {

        private int value;

        IntIterator() {
            if (minValue >= 0)
                value = minValue;
            else
                value = maxValue;
        }

        @Override
        public boolean hasNext() {
            if (minValue >= 0) {
                return value <= maxValue;
            } else {
                return value >= minValue;
            }
        }

        @Override
        public Integer next() {
            int curr = value;
            if (minValue >= 0) {
                value++;
            } else {
                value--;
            }
            return curr;
        }
    }

    public ImageIndexer(int minValue, int maxValue) {
        if (minValue > maxValue)
            throw new IllegalArgumentException("minValue < maxValue: " + minValue + " - " + maxValue);
        this.minValue = minValue;
        this.maxValue = maxValue;
        maxAbs = Math.max(Math.abs(minValue), Math.abs(maxValue));
    }

    @Override
    public Iterator<Integer> iterator() {
        if (minValue < 0 && maxValue > 0)
            return new SignFlippingIterator();
        return new IntIterator();
    }

}
