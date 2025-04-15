import java.io.File;  // Import the File class
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.util.Scanner; // Import the Scanner class to read text files

public class Trebuchet {
    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        int sum = 0;
        //get file input
        try {
            File myObj = new File("input.txt");
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                String digits = "";
                //process here

                //replace all the number words
                String names[] = {"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
                for (int i = 0; i < data.length(); i++) {
                    try {
                        int x = Integer.parseInt(data.charAt(i)+"");
                        digits += x+"";
                    } catch (NumberFormatException nfe) {
                        for (int j = 0; j < names.length; j++) {
                            if (data.substring(i).startsWith(names[j])) { 
                                digits += "" + (j + 1);
                            }
                        }
                    }
                }
                int f = Integer.parseInt(digits.substring(0,1));
                int l = Integer.parseInt(digits.substring(digits.length()-1));                
                System.out.println(data + "=>" + digits +"\ndigits: " + f + " " + l);
                sum += 10*f + l;
            }
            myReader.close();
          } catch (FileNotFoundException e) {
                e.printStackTrace();
          }
        long endTime = System.currentTimeMillis();
        System.out.println(sum);
        System.out.printf("runtime: %d\n",(endTime-startTime));    
    }
}