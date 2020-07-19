package de.bokeh.skred.icfpc2020;

import de.bokeh.skred.core.SyntaxError;
import de.bokeh.skred.input.AbstractSkReader;
import de.bokeh.skred.red.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.math.BigInteger;

public class Parser extends AbstractSkReader {

    private final Node NIL;
    private final Node FALSE;
    private final Node TRUE;
    private final Node CAR;
    private final Node CDR;
    private final Node CONS;

    private String[] tok;
    private int pos;

    public Parser(AppFactory appFactory) {
        super(appFactory);
        Node k = Function.valueOf("K");
        TRUE = k;
        FALSE = appFactory.mkApp(Function.valueOf("S"), TRUE);
        NIL = appFactory.mkApp(k, k);
        CAR = appFactory.mkApp(Function.valueOf("C"), Function.valueOf("I"), TRUE);
        CDR = appFactory.mkApp(Function.valueOf("C"), Function.valueOf("I"), FALSE);
        CONS = appFactory.mkApp(Function.valueOf("S'"),
                appFactory.mkApp(Function.valueOf("S'"), Function.valueOf("C")),
                appFactory.mkApp(Function.valueOf("C'"), Function.valueOf("C"), k),
                Function.valueOf("K'"));
    }

    private String nextToken() {
        return tok[pos++];
    }

    @Override
    public void readDefns(Reader in, String fileName) throws IOException {
        try (BufferedReader reader = new BufferedReader(in)) {
            for (int ln = 1;; ln++) {
                String line = reader.readLine();
                if (line == null)
                    break;
                if (!line.startsWith("-- "))
                    readDefn(line, ln);
            }
        }
    }

    private void readDefn(String s, int line) {
        tok = s.split(" ");
        if (!tok[1].equals("=")) {
            throw new AssertionError("Definition expected on line " + line);
        }
        String name = tok[0];
        pos = 2;
        Node expr = parseExpr();
        if (pos != tok.length) {
            throw new SyntaxError(line, pos, "not all tokens consumed");
        }
        addDefn(name, expr);
    }

    private Node parseExpr() {
        String token = nextToken();
        switch (token.charAt(0)) {
            case '-':
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                BigInteger n = BigInteger.valueOf(Long.parseLong(token));
                return Int.valueOf(n);
            case 'a':
                if (token.equals("ap")) {
                    Node fun = parseExpr();
                    Node arg = parseExpr();
                    return appFactory.mkApp(fun, arg);
                }
                // FALL THRU
            default:
                return Symbol.valueOf(token);
        }
    }

}
