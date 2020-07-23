package de.bokeh.skred;

import de.bokeh.skred.SkRed;
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

    private final Map<String, List<Point>> results;
    private final SkRed sk;
    private final ExecutorService executorService;
    private final String state;
    private final List<Point> vectors;

    public ImageProberTask(Map<String, List<Point>> results, SkRed sk, ExecutorService executorService, String state, List<Point> vectors) {
        this.results = results;
        this.sk = sk;
        this.executorService = executorService;
        this.state = state;
        this.vectors = new ArrayList<>(vectors);
    }

    @Override
    public void run() {
        int[] bounds = Outputs.getBounds(state);
        String key = resultKey(state);
        System.out.format("Testing %s (size %d, inputs %d, bounds %s)\n",
                key,
                state.length(),
                vectors.size(),
                Arrays.toString(bounds));
        ImageIndexer xindices = new ImageIndexer(bounds[0], bounds[2]);
        ImageIndexer yindices = new ImageIndexer(bounds[1], bounds[3]);
        for (int x : xindices) {
            for (int y : yindices) {
                Point point = Point.of(x, y);
                vectors.add(point);
                try {
                    String output = sk.run(sk.loadIcfp2020(vectors));
                    if (results.putIfAbsent(output, vectors) == null) {
                        String prefix;
                        if (output.startsWith("( 0")) {
                            prefix = "I";
                            System.out.format("    %s - adding to queue: %s, size %d with %s\n",
                                    key,
                                    resultKey(output),
                                    output.length(), point);
                            executorService.submit(
                                    new ImageProberTask(results, sk, executorService, output, vectors)
                            );
                            showJobsInfo(executorService);
                        } else {
                            prefix = "S";
                            System.out.format("    Found SEND: %s, size %d with %s\n",
                                    resultKey(output),
                                    output.length(), point);
                        }
                        String fileName = String.format("%s-%03d-%s.txt", prefix, vectors.size(), resultKey(output));
                        try (BufferedWriter w = UniqueFile.newBufferedWriter(fileName, 100)) {
                            w.write(vectors.toString());
                            w.newLine();
                            w.write(output);
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
            System.out.format("Tasks: %d completed, %d active+pending\n", completed, total - completed);
        }
    }

    private static String resultKey(String result) {
        int hash = result.hashCode();
        return Integer.toUnsignedString(hash, Character.MAX_RADIX);
    }
}
