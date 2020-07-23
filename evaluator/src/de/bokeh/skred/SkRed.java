package de.bokeh.skred;

import java.io.*;
import java.lang.management.*;
import java.util.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.google.common.collect.ImmutableList;
import de.bokeh.skred.core.Parser;
import de.bokeh.skred.icfpc2020.ImageIndexer;
import de.bokeh.skred.icfpc2020.Outputs;
import de.bokeh.skred.icfpc2020.Point;
import de.bokeh.skred.input.*;
import de.bokeh.skred.red.*;


/**
 * Main class.
 */
public class SkRed {

    private PrintWriter stats = null;
    private final List<String> programFiles = new ArrayList<>();
    private final List<String> cmdArgs = new ArrayList<>();
    private boolean evalProjections = false;
    private boolean useBStar = true;
    private boolean optimize = true;
    private boolean prelude = true;
    private boolean justCompile = false;
    private boolean detailedStats = false;
    private boolean icfpc2020 = false;
    private boolean testAll = false;
    private String mainDef = "main";
    private AppFactory appFactory;

    public static void main(String[] args) throws IOException {
        SkRed sk = new SkRed();
        if (!sk.getArgs(args)) {
            sk.usage();
            System.exit(1);
        }
        sk.logConfig();
        Node prog;
        if (sk.icfpc2020) {
            if (sk.testAll) {
                int numOfCores = Runtime.getRuntime().availableProcessors();
                ExecutorService executorService = Executors.newFixedThreadPool(numOfCores);
                Map<String, List<Point>> results = Collections.synchronizedMap(new HashMap<>());
                List<Point> vectors = ImmutableList.of(Point.ORIGIN);
                String initial = sk.run(sk.loadIcfp2020(vectors));
                results.put(initial, vectors);
                executorService.submit(new ImageProberTask(results, sk, executorService, initial, vectors));
                // wait how?
                // printResults(results);
                return;
            }
            prog = sk.loadIcfp2020();
        } else {
            prog = sk.loadProgram();
        }
        if (prog != null) {
            System.out.println(sk.run(prog));
        }
    }

    private static void printResults(Map<String, List<Point>> results) {
        for (Map.Entry<String, List<Point>> e : results.entrySet()) {
            System.out.println(e.getValue());
            System.out.println(e.getKey());
            System.out.println();
        }
    }

    private void logConfig() {
        if (stats != null) {
            stats.println("skred version " + Version.getLongVersionString());
            if (detailedStats) {
                OperatingSystemMXBean os = ManagementFactory.getOperatingSystemMXBean();
                stats.println("OS: " + os.getName());
                stats.println("OS Version: " + os.getVersion());
                stats.println("Arch: " + os.getArch());
                stats.println("evaluate projection combinators: " + evalProjections);
                stats.println("overwriting: " + appFactory);
            }
        }
    }

    private Node loadIcfp2020() throws IOException {
        Function.init(evalProjections);
        SkReader r = new de.bokeh.skred.icfpc2020.Parser(appFactory);
        long startTime = System.nanoTime();
        SkReader preludeReader = new Parser(appFactory, useBStar);
        if (prelude) {
            try (BufferedReader in = new BufferedReader(new FileReader("icfpc-prelude.core"))) {
                preludeReader.readDefns(in, "icfpc-prelude.core");
            }
        }
        for (String s : programFiles) {
            BufferedReader in = new BufferedReader(new FileReader(s));
            r.readDefns(in, s);
            in.close();
        }
        r.addDefns(preludeReader);
        if (justCompile) {
            r.dumpDefns(mainDef, System.out);
            return null;
        }
        long t0 = System.nanoTime();
        // Node prog = appFactory.mkApp(r.getGraph(ICFPC_MAIN), Int.valueOf(0));
        Node prog = r.getGraph(mainDef);
        if (detailedStats) {
            double link = (System.nanoTime() - t0) * 1.0e-6;
            stats.format("linking: %.3fms\n", link);
        }
        double elapsed = (System.nanoTime() - startTime) * 1.0e-6;
        if (stats != null)
            stats.format("load+compile time: %.3fms\n", elapsed);
        return prog;
    }

    Node loadIcfp2020(List<Point> vectors) throws IOException {
        Function.init(evalProjections);
        SkReader r = new de.bokeh.skred.icfpc2020.Parser(appFactory);
        long startTime = System.nanoTime();
        SkReader preludeReader = new Parser(appFactory, useBStar);
        try (BufferedReader in = new BufferedReader(new FileReader("icfpc-prelude2.core"))) {
            preludeReader.readDefns(in, "icfpc-prelude2.core");
        }
        StringReader rd = new StringReader(
                "galaxyinp = [" +
                vectors.stream().map(SkRed::asCons).collect(Collectors.joining(",")) +
                "]");
        preludeReader.readDefns(rd, "memory");
        for (String s : programFiles) {
            BufferedReader in = new BufferedReader(new FileReader(s));
            r.readDefns(in, s);
            in.close();
        }
        r.addDefns(preludeReader);
        if (justCompile) {
            r.dumpDefns(mainDef, System.out);
            return null;
        }
        long t0 = System.nanoTime();
        // Node prog = appFactory.mkApp(r.getGraph(ICFPC_MAIN), Int.valueOf(0));
        Node prog = r.getGraph(mainDef);
        if (detailedStats) {
            double link = (System.nanoTime() - t0) * 1.0e-6;
            stats.format("linking: %.3fms\n", link);
        }
        double elapsed = (System.nanoTime() - startTime) * 1.0e-6;
        if (stats != null)
            stats.format("load+compile time: %.3fms\n", elapsed);
        return prog;
    }

    private static String lit(int n) {
        if (n >= 0)
            return Integer.toString(n);
        return "(neg " + (-n) + ")";
    }

    private static String asCons(Point p) {
        return "cons " + lit(p.getX()) + " " + lit(p.getY());
    }

    private Node loadProgram() throws IOException {
        Function.init(evalProjections);
        SkReader r = new Parser(appFactory, useBStar);
        r.addDefn("cmdLine", makeStringList(cmdArgs, appFactory));
        long startTime = System.nanoTime();
        if (prelude) {
            loadPrelude(r);
        }
        for (String s : programFiles) {
            long t0 = System.nanoTime();
            BufferedReader in = new BufferedReader(new FileReader(s));
            r.readDefns(in, s);
            in.close();
            if (detailedStats) {
                long elapsed = System.nanoTime() - t0;
                stats.format("loading %s: %.3fms\n", s, elapsed * 1.0e-6);
            }
        }
        if (justCompile) {
            r.dumpDefns(mainDef, System.out);
            return null;
        }
        long t0 = System.nanoTime();
        Node prog = appFactory.mkApp(r.getGraph(mainDef), Int.valueOf(0));
        if (detailedStats) {
            double link = (System.nanoTime() - t0) * 1.0e-6;
            stats.format("linking: %.3fms\n", link);
        }
        double elapsed = (System.nanoTime() - startTime) * 1.0e-6;
        if (stats != null)
            stats.format("load+compile time: %.3fms\n", elapsed);
        return prog;
    }

    private static boolean isCons(Node a) {
        return (a instanceof Data) && a.getTag() == 1 && a.getNumFields() == 2;
    }

    private static boolean isNil(Node a) {
        return (a instanceof Data) && a.getTag() == 0 && a.getNumFields() == 0;
    }

    private static boolean isProperList(RedContext c, Node a) {
        if (isNil(a))
            return true;
        if (!isCons(a)) {
            return false;
        }
        c.push(a.getField(1));
        c.eval();
        Node b = c.getTos();
        c.pop1();
        return isProperList(c, b);
    }

    private String evalList(RedContext c, Node graph) {
        StringBuilder out = new StringBuilder();
        printList(c, graph, out);
        return out.toString();
    }

    private void printList(RedContext c, Node value, StringBuilder out) {
        c.push(value);
        c.eval();
        Node a = c.getTos();
        if (isNil(a)) {
            out.append("nil");
        } else if (isProperList(c, a)) {
            String sep = "( ";
            while (isCons(a)) {
                out.append(sep);
                sep = " , ";
                printList(c, a.getField(0), out);
                a = a.getField(1);
                c.pop1();
                c.push(a);
                c.eval();
                a = c.getTos();
            }
            out.append(" )");
        } else if (!isCons(a)) {
            out.append(a);
        } else {
            out.append('[');
            printList(c, a.getField(0), out);
            out.append(',');
            printList(c, a.getField(1), out);
            out.append(']');
        }
    }


    String run(Node program) {
        long startTime = System.nanoTime();
        RedContext c = new RedContext(appFactory);
        String result = evalList(c, program);
        if (c.getTos().getTag() != 0) {
            System.err.println("IO error: " + c.getTos());
        }
        if (stats != null) {
            double elapsed = (System.nanoTime() - startTime) * 1.0e-9;
            if (detailedStats) {
                stats.println(c.toString());
            }
            long totalGCTime = 0;
            for (GarbageCollectorMXBean gb : ManagementFactory.getGarbageCollectorMXBeans()) {
                long t = gb.getCollectionTime();
                if (t > 0) {
                    totalGCTime += t;
                }
                if (detailedStats) {
                    stats.print("GC " + gb.getName() + ": " + gb.getCollectionCount() + " GCs, " + t + " ms.");
                    stats.println(" " + Arrays.deepToString(gb.getMemoryPoolNames()));
                }
            }
            if (detailedStats) {
                stats.println("Combinator         unwind   argcheck       eval");
            }
            long totalUnwind = 0;
            long totalArgCheck = 0;
            long totalEval = 0;
            for (Function.Stats s : Function.getStats()) {
                totalUnwind += s.unwindCount;
                totalArgCheck += s.argCheckCount;
                totalEval += s.evalCount;
                if (detailedStats) {
                    stats.format("%-12s %12d %10d %10d\n",
                                 s.name, s.unwindCount, s.argCheckCount, s.evalCount);
                }
            }
            if (detailedStats) {
                stats.format("%-12s %12d %10d %10d\n", "=total=", totalUnwind, totalArgCheck, totalEval);
            }
            long numReductions = totalUnwind - totalArgCheck;
            stats.format("%.3f sec. elapsed, %d reductions, %.3f Mrps\n", elapsed, numReductions, 1.0e-6 * numReductions / elapsed);
            stats.format("%.3f sec. in GC (%.1f%%)\n", totalGCTime / 1000.0, (totalGCTime / 10.0) / elapsed);
            stats.flush();
            // stats.close();
        }
        return result;
    }

    private void usage() {
        System.err.println(
          "usage: skred [option...] file1.core ... [-- args]\n" +
          "\n" +
          "Options:\n" +
          "  --compile       compile only; write compiled code to stdout\n" +
          "  -s[=file]       write short runtime statistics to stderr or file\n" +
          "  -S[=file]       write detailed runtime statistics to stderr or file\n" +
          "  --no-prelude    don't load the prelude\n" +
          "  --main=<name>   definition to evalueate (dfault main)\n" +
          "  --useB1         use B' instead of B* combinator\n" +
          "  --app=Cond      use conditionals to implement indirections (default)\n" +
          "  --app=ST        use state pattern to implement indirections\n" +
          "  --app=IndI      use I combinator to implement indirections\n" +
          "  --evalprojections evaluate result of projection functions\n" +
          "  --noopt         disable optimizations"
        );
    }
    
    private boolean getArgs(String[] args) throws FileNotFoundException {
        String appFactoryId = "Cond";
        for (int i = 0; i < args.length; i++) {
            String s = args[i];
            if (s.equals("--app=ST")) {
                appFactoryId = "ST";
            }
            else if (s.equals("--all")) {
                testAll = true;
            }
            else if (s.equals("--app=Cond")) {
                appFactoryId = "Cond";
            }
            else if (s.equals("--app=IndI")) {
                appFactoryId = "IndI";
            }
            else if (s.equals("--no-prelude")) {
                prelude = false;
            }
            else if (s.equals("--evalprojections")) {
                evalProjections = true;
            }
            else if (s.equals("--useB1")) {
                useBStar = false;
            }
            else if (s.equals("--noopt")) {
                optimize = false;
            }
            else if (s.equals("--compile")) {
                justCompile = true;
            }
            else if (s.equals("--icfpc2020")) {
                icfpc2020 = true;
            }
            else if (s.equals("-s")) {
                stats = new PrintWriter(System.err);
            }
            else if (s.equals("-S")) {
                detailedStats = true;
                stats = new PrintWriter(System.err);
            }
            else if (s.startsWith("-s=")) {
                File statsFile = new File(s.substring(3));
                stats = new PrintWriter(new FileOutputStream(statsFile));
            }
            else if (s.startsWith("-S=")) {
                detailedStats = true;
                File statsFile = new File(s.substring(3));
                stats = new PrintWriter(new FileOutputStream(statsFile));
            } else if (s.startsWith("--main=")) {
                mainDef = s.substring(7);
            } else if (s.equals("--")) {
                i++;
                for ( ; i < args.length; i++) {
                    cmdArgs.add(args[i]);
                }
            } else if (s.startsWith("-")) {
                return false;
            } else {
                programFiles.add(s);
            }
        }
        switch (appFactoryId) {
        case "IndI": appFactory = new AppIndIFactory(optimize); break;
        case "ST":   appFactory = new AppSTFactory(optimize); break;
        default:     appFactory = new AppCondFactory(optimize); break;
        }
        return true;
    }

    private void loadPrelude(SkReader r) throws IOException {
        long t0 = System.nanoTime();
        InputStream s = SkRed.class.getResourceAsStream("/lib/prelude.core");
        if (s == null) {
            s = ClassLoader.getSystemResourceAsStream("prelude.core");
        }
        if (s == null) {
            System.err.println("error: 'prelude.core' not found in classpath.");
            System.exit(1);
        }
        BufferedReader in = new BufferedReader(new InputStreamReader(s));
        r.readDefns(in, "prelude");
        in.close();
        if (detailedStats) {
            long elapsed = System.nanoTime() - t0;
            stats.format("loaded *prelude*: %.3fms\n", elapsed / 1.0e6);
        }
    }

    private static Node makeStringList(List<String> cmdArgs, AppFactory appFactory) {
        List<Node> args = new ArrayList<>();
        for (String s : cmdArgs) {
            args.add(Data.makeString(s));
        }
        return appFactory.mkList(args);
    }
}
