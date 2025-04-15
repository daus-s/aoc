import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class IDs 
{
    public static void main(String[] args)
    {
        int MODE = Integer.parseInt(args[1]);

        int value = 0;
        //args [0] is the filename
        try {
            File input = new File(args[0]);
            Scanner reader = new Scanner(input);
            while (reader.hasNextLine()) 
            {
                String line = reader.nextLine();
                String[] tokens = line.split(",");
                String e1 = tokens[0];
                String e2 = tokens[1];
                String[] indicies1 = e1.split("-");
                String[] indicies2 = e2.split("-");
                int elf1LB = Integer.parseInt(indicies1[0]); // elf 1 lb
                int elf1UB = Integer.parseInt(indicies1[1]); // elf 1 ub
                int elf2LB = Integer.parseInt(indicies2[0]); //elf 2 lb
                int elf2UB = Integer.parseInt(indicies2[1]); //elf 2 ub
                if (MODE==0) 
                {
                    if ((elf1LB <= elf2LB && elf1UB >= elf2UB ) || (elf1LB >= elf2LB && elf1UB <= elf2UB ))//first condition range 1 is within range 2
                    {
                        value++;
                    }
                }
                if (MODE==1)
                {
                    if ((elf1LB <= elf2LB && elf1UB >= elf2LB)|| // lower edge hits
                        (elf2LB <= elf1LB && elf2UB >= elf1LB)|| //
                        (elf2UB >= elf1LB && elf2UB <= elf1UB)||
                        (elf1UB <= elf2UB && elf1UB >= elf2LB))
                    //first condition range 1 is within range 2
                    {
                        value++;
                    }
                }
                
            }

            reader.close();
        } 
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        }
        System.out.println(value);
    }
}
