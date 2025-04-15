import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class MaxCalories 
{
    public static void main(String[] args)
    {
        int MODE = Integer.parseInt(args[1]);
        //ArrayList
        ArrayList<Integer> elves = new ArrayList<Integer>();
        int counter = 0;
        int value = 0;
        //args [0] is the filename
        try {
            File input = new File(args[0]);
            Scanner reader = new Scanner(input);
            while (reader.hasNextLine()) 
            {
                String line = reader.nextLine();
                if (line.trim().equals(""))
                {
                    elves.add(value); 
                    ++counter;
                    value = 0;
                }
                else 
                {
                    try 
                    {
                        value += Integer.parseInt(line);
                    }
                    catch (NumberFormatException nfe)                    
                    {
                        nfe.printStackTrace();
                    }
                }
            }
            reader.close();
        } 
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        }
        if (MODE==0) 
        {
            int max = 0;
            for (int i: elves)
            {
                if (i>max)
                {
                    max = i;
                }
            }
            System.out.println("maximum calories is:" + max);
        }
        if (MODE==1)
        {
            int a = 0; //1st
            int b = 0; //2nd
            int c = 0; //3rd
            for (int i: elves)
            {
                if (i>=a)
                {
                    c = b;
                    b = a;
                    a = i;
                }
                else if (i>=b)
                {
                    c = b;
                    b = i;
                }
                else if (i >= c)
                {
                    c = i;
                }
            }
            System.out.println("calories carried by the top 3 elves are:" + (a+b+c));
        }
    }
}
