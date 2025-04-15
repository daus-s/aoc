import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Stacks 
{
    public static void main(String[] args)
    {
        int MODE = Integer.parseInt(args[1]);

        ArrayList<String> graph = new ArrayList<>(9);
        for (int i = 0; i < 9; ++i)
        {
            graph.set(i,"");
        }
        String digits = "1234567890";
        int value = 0;
        boolean instructions = false;
        //args [0] is the filename
        try {
            File input = new File(args[0]);
            Scanner reader = new Scanner(input);
            while (reader.hasNextLine()) 
            {
                String line = reader.nextLine();

                if (line.equals(""))
                {
                    instructions = true;
                }
                //ok now operate on input
                if (!instructions)
                {
                    //still parsing the stacks
                    for (int i = 0; i < 9; ++i)
                    {   
                        String stack = graph.get(i);
                        char add = line.charAt((4*i) + 1);
                        if (!digits.contains(""+add) && add!=' ')
                        {
                            stack = stack + ""  + add;
                            graph.set(i, stack);
                        }
                        else break;
                        
                    }
                }
                if (instructions)
                {
                    
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
            System.out.println(graph);
        }
        if (MODE==1)
        {
            
        }
    }
}
