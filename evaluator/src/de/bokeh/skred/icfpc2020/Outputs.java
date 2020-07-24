package de.bokeh.skred.icfpc2020;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Outputs {

    public static List<Point> getPoints(String output) {
        Pattern p = Pattern.compile("\\[(-?[0-9]+),(-?[0-9]+)]");
        Matcher m = p.matcher(output);
        if (m.groupCount() != 2)
            throw new AssertionError("bad pattern");
        List<Point> result = new ArrayList<>();
        while (m.find()) {
            int x = Integer.parseInt(m.group(1));
            int y = Integer.parseInt(m.group(2));
            result.add(Point.of(x, y));
        }
        return result;
    }

    public static int[] getBounds(String output) {
        List<Point> points = getPoints(output);
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (Point p : points) {
            minX = Math.min(minX, p.getX());
            minY = Math.min(minY, p.getY());
            maxX = Math.max(maxX, p.getX());
            maxY = Math.max(maxY, p.getY());
        }
        return new int[]{minX, minY, maxX, maxY};
    }

    public static int[] getBounds(Data images) {
        int minX = Integer.MAX_VALUE;
        int minY = Integer.MAX_VALUE;
        int maxX = Integer.MIN_VALUE;
        int maxY = Integer.MIN_VALUE;
        for (int i = 0; i < images.length(); i++) {
            Data image = images.nth(i);
            for (int k = 0; k < image.length(); k++) {
                Point p = image.nth(k).toPoint();
                minX = Math.min(minX, p.getX());
                minY = Math.min(minY, p.getY());
                maxX = Math.max(maxX, p.getX());
                maxY = Math.max(maxY, p.getY());
            }
        }
        return new int[]{minX, minY, maxX, maxY};
    }

}
