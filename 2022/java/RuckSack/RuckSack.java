import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class RuckSack 
{
    public static void main(String[] args)
    {
        String priority = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int MODE = Integer.parseInt(args[1]);

        int sum = 0;
        //args [0] is the filename
        try {
            File input = new File(args[0]);
            Scanner reader = new Scanner(input);
            if (MODE==0) 
            {
            
                
                while (reader.hasNextLine()) 
                {
                    String line = reader.nextLine();
                    int l = line.length();
                    int h = l/2; //read int half
                    String first = line.substring(0, h);
                    String second = line.substring(h, l);
                    boolean flag = false;

                    for (int i = 0; i < h; i++)
                    {
                        for (int j = 0; j < h; j++)
                        {
                            if (first.charAt(i) == second.charAt(j))
                            {
                                flag = true;
                                sum += priority.indexOf(first.charAt(i));
                                break;
                            }
                        }
                        //dont continue when we find it
                        if (flag)
                            break;
                    }

                }
                System.out.printf("sum: %d\n", sum);
            }
            if (MODE==1)
            {
                String prev = "";
                String line = "";
                String next = ""; //1 -> 2 -> 3
                while (reader.hasNextLine()) 
                {
                    prev = reader.nextLine();
                    line = reader.nextLine();
                    next = reader.nextLine();
                    for (int i = 1; i < priority.length(); ++i)
                    {
                        char at = priority.charAt(i);
                        if (prev.contains(""+at) && 
                            line.contains(""+at) &&
                            next.contains(""+at))
                        {
                            sum += priority.indexOf(at);
                        }
                    }
                }
                System.out.printf("sum: %d\n", sum);

            }
            reader.close();
 
        }
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        }
        

        
    }
}