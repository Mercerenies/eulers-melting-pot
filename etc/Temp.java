
// We're going to do this in Java then hand-translate it to Jasmin. It's going to be hell. Good luck! :)

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;

public class Temp {

    public static boolean isTriangular(int t) {
        for (int i = 1; true; i++) {
            int nth = i * (i + 1) / 2;
            if (nth == t)
                return true;
            else if (nth > t)
                return false;
        }
    }

    public static void main(String[] args) {

        try {

            BufferedReader file = new BufferedReader(new FileReader(new File("./files/p042_words.txt")));

            String str = file.readLine();
            str = str.replaceAll("\"", "");
            String[] arr = str.split(",");

            file.close();

            int counter = 0;
            for (int i = 0; i < arr.length; i++) {
                String curr = arr[i];
                int currx = 0;
                for (int j = 0; j < curr.length(); j++) {
                    int base = curr.charAt(j) - 'A' + 1;
                    currx += base;
                }
                if (isTriangular(currx))
                    counter++;
            }

            System.out.println(counter);

        } catch (Exception e) {}

    }

}
