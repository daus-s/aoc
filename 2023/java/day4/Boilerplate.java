import java.io.File;  // Import the File class
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.util.Scanner; // Import the Scanner class to read text files

public class Boilerplate {
    public static void main(String[] args) {
        int m = Integer.parseInt(args[2]);
        part1(m);
        part2(m);
    }

    public static void part1(int mode) {
        long startTime = System.currentTimeMillis();
        String filename = "input.txt";
        if (mode == 1) {
            //debug
            filename = "test.txt";
        } else if (mode == 0) {
            filename = "input.txt";
        }
        int ans = 0;
        //get file input
        try {
            File file = new File("input.txt");
            Scanner reader = new Scanner(file);
            while (reader.hasNextLine()) {
                String data = reader.nextLine();
                //logic
            } 
            reader.close();

        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        long endTime = System.currentTimeMillis();
        System.out.println(ans);
        System.out.printf("runtime: %d\n",(endTime-startTime));
    }

    public static void part2(int mode) {
        long startTime = System.currentTimeMillis();
        String filename = "input.txt";
        if (mode == 1) {
            //debug
            filename = "test.txt";
        } else if (mode == 0) {
            filename = "input.txt";
        }

        int ans = 0;
        //get file input
        try {
            File file = new File(filename);
            Scanner reader = new Scanner(file);
            while (reader.hasNextLine()) {
                String data = reader.nextLine();
                //logic
            } 
            reader.close();

        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        long endTime = System.currentTimeMillis();
        System.out.println(ans);
        System.out.printf("runtime: %d\n",(endTime-startTime));
    }
}