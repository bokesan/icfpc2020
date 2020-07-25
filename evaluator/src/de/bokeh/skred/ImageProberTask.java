package de.bokeh.skred;

import de.bokeh.skred.icfpc2020.Data;
import de.bokeh.skred.icfpc2020.ImageIndexer;
import de.bokeh.skred.icfpc2020.Outputs;
import de.bokeh.skred.icfpc2020.Point;
import de.bokeh.skred.red.RedException;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;

public class ImageProberTask implements Runnable {

    private final Map<de.bokeh.skred.icfpc2020.Data, List<Point>> results;
    private final SkRed sk;
    private final ExecutorService executorService;
    private final Data state;
    private final List<Point> vectors;

    public ImageProberTask(Map<de.bokeh.skred.icfpc2020.Data, List<Point>> results, SkRed sk, ExecutorService executorService, Data state, List<Point> vectors) {
        this.results = results;
        this.sk = sk;
        this.executorService = executorService;
        this.state = state;
        this.vectors = new ArrayList<>(vectors);
    }

    @Override
    public void run() {
        showJobsInfo(executorService);
        int[] bounds = Outputs.getBounds(state.nth(2));
        String key = resultKey(state.nth(1));
        System.out.format("Testing %s (inputs %d, bounds %s)\n",
                key,
                vectors.size(),
                Arrays.toString(bounds));
        ImageIndexer xindices = new ImageIndexer(bounds[0], bounds[2]);
        ImageIndexer yindices = new ImageIndexer(bounds[1], bounds[3]);
        for (int x : xindices) {
            for (int y : yindices) {
                Point point = Point.of(x, y);
                vectors.add(point);
                try {
                    String outputString = sk.run(sk.loadIcfp2020(state.nth(1), point));
                    Data output = Data.parse(outputString);
                    if (output.length() != 3) {
                        throw new AssertionError("invalid output: " + outputString);
                    }
                    Data istate = output.nth(1);
                    if (results.putIfAbsent(istate, vectors) == null) {
                        String prefix;
                        if (output.nth(0).equals(Data.ZERO)) {
                            prefix = "I";
                            System.out.format("  %s - adding to queue: %s with %s\n",
                                    key,
                                    resultKey(istate),
                                    point);
                            executorService.submit(new ImageProberTask(results, sk, executorService, output, vectors));
                        } else {
                            prefix = "S";
                            System.out.format("  Found SEND: %s with %s after %.3fs\n",
                                    resultKey(istate),
                                    point,
                                    sk.getElapsedSeconds());
                        }
                        String fileName = String.format("%s-%03d-%s.txt", prefix, vectors.size(), resultKey(output));
                        try (BufferedWriter w = UniqueFile.newBufferedWriter(fileName, 100)) {
                            w.write(vectors.toString());
                            w.newLine();
                            w.write(output.toString());
                            w.newLine();
                        }
                    }
                } catch (RedException | IOException ex) {
                    System.err.println("Error at " + point);
                }
                vectors.remove(vectors.size() - 1);
            }
        }
    }

    private void showJobsInfo(ExecutorService executorService) {
        if (executorService instanceof ThreadPoolExecutor) {
            ThreadPoolExecutor e = (ThreadPoolExecutor) executorService;
            long completed = e.getCompletedTaskCount();
            long total = e.getTaskCount();
            System.out.format("  Tasks: %d completed, %d active+pending, %.3fs elapsed\n",
                    completed, total - completed, sk.getElapsedSeconds());
        }
    }

    private static String resultKey(Data result) {
        int hash = result.hashCode();
        return Integer.toUnsignedString(hash, Character.MAX_RADIX);
    }
}
