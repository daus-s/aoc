import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class RPS
{
    public static void main(String[] args)
    {
        int MODE = Integer.parseInt(args[1]);
        int score = 0;
        try {
            File input = new File(args[0]);
            Scanner reader = new Scanner(input);
            while (reader.hasNextLine()) 
            {
                String line = reader.nextLine();
                char elf = line.charAt(0);
                char u = line.charAt(2);
                if (MODE == 0)
                {
                    score += round(elf, u);
                }
                if (MODE == 1)
                {
                    score += shape_res(elf, u);
                }
                
            }
            reader.close();
        } 
        catch (FileNotFoundException e) 
        {
            e.printStackTrace();
        }
        System.out.println("score:" + score);
    }

    public static int result(char c)
    {
        if (c=='X')
            return 0;
        if (c=='Y')
            return 3;
        if (c=='Z')
            return 6;
        return 0;
    }

    public static int shape_res(char s, char r)
    {
        if (r == 'X') //lose
        {
            if (s=='A') //elf plays rock
            {
                return 3+result(r);//scissor
            }
            if (s=='B') //elf plays paper
            {
                return 1+result(r);//rock
            }
            if (s=='C') //elf plays scissor
            {
                return 2+result(r);//apper
            }
        }
        if (r == 'Y') //draw
        {
            if (s=='A') //elf plays rock
            {
                return 1+result(r);//rock
            }
            if (s=='B') //elf plays paper
            {
                return 2+result(r);//paper
            }
            if (s=='C') //elf plays scissor
            {
                return 3+result(r);//scisor
            }
        }
        if (r == 'Z') //win
        {
            if (s=='A') //elf plays rock
            {
                return 2+result(r);//paper
            }
            if (s=='B') //elf plays paper
            {
                return 3+result(r);//scissor
            }
            if (s=='C') //elf plays scissor
            {
                return 1+result(r);//rock
            }
        }
        return 0;
    }

    //a is elf, b is human
    public static int round(char a, char b)
    {
        if (a=='A') //elf played rock
        {
            if (b=='X')
            {
                return 4; //3 for draw + 1 for rock
            }
            if (b=='Y')
            {
                return 8; //6 for win + 2 for paper
            }
            if (b=='Z')
            {
                return 3; //0 for loss + 3 for scissors
            }
        }
        if (a=='B') //elf played paper
        {
            if (b=='X') //human played rock
            {
                return 1; //0 for loss + 1 for rock
            }
            if (b=='Y') //human played paper
            {
                return 5; //3 for draw + 2 for paper
            }
            if (b=='Z') //human played sciissorsz
            {
                return 9; //6 for win + 3 for scissors
            }
        }
        if (a=='C') //elf played scissors
        {
            if (b=='X') // human played rock
            {
                return 7; //6 for win + 1 for rock
            }
            if (b=='Y') //human played paper
            {
                return 2; //0 for loss + 2 for paper
            }
            if (b=='Z') //human played scissors
            {
                return 6; //3 for draw + 3 for scissors
            }
        }
        return 0;
    }
}