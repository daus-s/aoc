import java.io.File;  // Import the File class
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.util.Arrays;
import java.util.Scanner; // Import the Scanner class to read text files

public class CubeConundrum {
    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();

        int ans1 = 0; //use identity if sum-> 0 if product -> 1
        int ans2 = 0;
        int i = 1; //1-based in file
        //get file input
        try {
            File myObj = new File("input.txt");
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                if (part1(data))
                    ans1+=i; //max value is 5050 (100+1)*100/2 = 101*50 = 100*50 + 50*1= 5000 + 50 = 5050
                ans2 += part2(data);
                i++;
                //logic
            } 
            myReader.close();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        long endTime = System.currentTimeMillis();
        System.out.println(ans1);
        System.out.println(ans2);
        System.out.printf("runtime: %d",(endTime-startTime));
    }

    public static boolean part1(String data) {
        //get just the strings
        int max_r = 12, max_g = 13, max_b = 14;
        String game = data.substring(data.indexOf(":")+1).trim();
        String pulls[] = game.split(";");
        for (String s1 : pulls) {
            String hands[] = s1.trim().split(",");
            for (String s2 : hands) {
                String info[] = s2.trim().split(" ");
                String color = info[1];
                int count = Integer.parseInt(info[0]);
                System.out.printf("count:%d, color:%s\n",count, color);
                if (color.equals("red")) {
                    if (count > max_r) {
                        return false;
                    }
                }
                if (color.equals("blue"))  {
                    if (count > max_b) {
                        return false;
                    }
                }
                if (color.equals("green"))  {
                    if (count > max_g) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    public static int part2(String data) {
        //get just the strings
        int r=0, g=0, b=0;
        String game = data.substring(data.indexOf(":")+1).trim();
        String pulls[] = game.split(";");
        for (String s1 : pulls) {
            String hands[] = s1.trim().split(",");
            for (String s2 : hands) {
                String info[] = s2.trim().split(" ");
                String color = info[1];
                int count = Integer.parseInt(info[0]);
                System.out.printf("count:%d, color:%s\n",count, color);
                if (color.equals("red")) {
                    if (count >= r) {
                        r = count;
                    }
                }
                if (color.equals("blue"))  {
                    if (count >= b) {
                        b = count;
                    }
                }
                if (color.equals("green"))  {
                    if (count >= g) {
                        g = count;
                    }
                }
            }
        }
        return r*g*b;    
    }
}