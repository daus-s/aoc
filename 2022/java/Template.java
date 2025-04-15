import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Template 
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
            }
            reader.close();
        } 
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        }
        if (MODE==0) 
        {
            
        }
        if (MODE==1)
        {
            
        }
    }
}
