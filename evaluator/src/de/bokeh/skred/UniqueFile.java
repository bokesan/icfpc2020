package de.bokeh.skred;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;

public class UniqueFile {

    public static BufferedWriter newBufferedWriter(String fileName, int maxIndex) throws IOException {
        for (int index = 0; index <= maxIndex; index++) {
            Path path = Paths.get(addIndex(fileName, index));
            try {
                return Files.newBufferedWriter(path, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW);
            } catch (IOException e) {
                // ignore
            }
        }
        throw new IOException("could not create file for writing: " + fileName);
    }

    private static String addIndex(String fileName, int index) {
        if (index == 0) {
            return fileName;
        }
        int extPos = fileName.lastIndexOf('.');
        if (extPos >= 0) {
            return fileName.substring(0, extPos + 1) + index + fileName.substring(extPos);
        } else {
            return fileName + "." + index;
        }
    }

}
