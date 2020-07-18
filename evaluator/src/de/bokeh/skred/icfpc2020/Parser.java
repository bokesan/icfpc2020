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
    private int line;

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

    private String currentToken() {
        return tok[pos];
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
        this.line = line;
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
        String token = currentToken();
        pos++;
        switch (token) {
            case "ap":
                Node fun = parseExpr();
                Node arg = parseExpr();
                return appFactory.mkApp(fun, arg);
            // case "t": return TRUE;
            // case "f": return FALSE;
            /*
            case "mod":
            case "dem":
            case "i": return Function.valueOf("I");
            case "k": return Function.valueOf("K");
            case "b": return Function.valueOf("B");
            case "c": return Function.valueOf("C");
            case "s": return Function.valueOf("S");
            case "inc": return Function.valueOf("succ");
            case "dec": return Function.valueOf("pred");
            case "neg": return Function.valueOf("negate");
            case "add": return Function.valueOf("add");
            case "mul": return Function.valueOf("mul");
            case "div": return Function.valueOf("quot");
            case "eq": return Function.valueOf("eqX");
            case "lt": return Function.valueOf("ltX");

             */
            // case "cons": return CONS;
            // case "car": return CAR;
            // case "cdr": return CDR;
            // case "nil": return NIL;
            // case "isnil": return Function.valueOf("isnil");
            default:
                /*
                Function f = Function.valueOf(token);
                if (f != null) {
                    System.out.println("DETECTED FUNCTION: " + token);
                    return f;
                }
                 */
                try {
                    BigInteger n = BigInteger.valueOf(Long.parseLong(token));
                    return Int.valueOf(n);
                } catch (NumberFormatException e) {
                    // FALL THROUGH
                }
                /*
                if (!token.startsWith(":")) {
                    throw new SyntaxError(line, pos - 1, "':name' expected, but got '" + token + "'");
                }

                 */
                return Symbol.valueOf(token);
        }
    }

}
