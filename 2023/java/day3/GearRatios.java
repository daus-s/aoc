import java.io.File;  // Import the File class
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.util.ArrayList;
import java.util.Scanner; // Import the Scanner class to read text files

public class GearRatios {
    public static void main(String[] args) {
        int m = Integer.parseInt(args[0]);
        part1(m);
        part2(m);
    }

    public static void part1(int mode) {
        long startTime = System.currentTimeMillis();
        int l = mode==1 ? 10 : 140; // he;; yeah 
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
            int y = 0;
            char grid[][] = new char[l][l];
            while (reader.hasNextLine()) {
                String data = reader.nextLine();
                for (int x = 0; x < l; ++x) {
                    grid[y][x] = data.charAt(x);
                }
                y++;
                //logic    
            } 

            for (y = 0; y < l; y++) {
                for (int x = 0; x < l; x++) {
                    if (!(grid[y][x] == '.' || 
                        grid[y][x] == '1' || 
                        grid[y][x] == '2' || 
                        grid[y][x] == '3' || 
                        grid[y][x] == '4' || 
                        grid[y][x] == '5' || 
                        grid[y][x] == '6' || 
                        grid[y][x] == '7' || 
                        grid[y][x] == '8' || 
                        grid[y][x] == '9' ||
                        grid[y][x] == '0')) {
                        //grid[y][x] is a symbol
                        /* 
                         *    (x-1, y-1) (x, y-1) (x+1, y-1)
                         *    (x-1, y)   (x, y)   (x+1, y)
                         *    (x-1, y+1) (x, y+1) (x+1, y+1)
                        */
                        //do bounds checking 
                        //top row
                        System.out.printf("array: [%d][%d]=%c\n", y, x, grid[y][x]);
                        if (y-1 >= 0) {
                            if (x-1 >= 0) {
                                if (Character.isDigit(grid[y-1][x-1])) {
                                    ans += findNumber(y-1,x-1, grid);
                                }
                            }
                            if (x+1 < l) {
                                if (Character.isDigit(grid[y-1][x+1])) {
                                    ans += findNumber(y-1,x+1, grid);
                                }
                            }
                            if (Character.isDigit(grid[y-1][x])) {
                                ans += findNumber(y-1,x, grid);
                            }
                        }
                        if (y+1 < l) {
                            if (x-1 >= 0) {
                                if (Character.isDigit(grid[y+1][x-1])) {
                                    ans += findNumber(y+1,x-1, grid);
                                }
                            }
                            if (x+1 < l) {
                                if (Character.isDigit(grid[y+1][x+1])) {
                                    ans += findNumber(y+1,x+1, grid);
                                }
                            }
                            if (Character.isDigit(grid[y+1][x])) {
                                ans += findNumber(y+1,x, grid);
                            }
                        }
                        if (x-1 >= 0) {
                            if (Character.isDigit(grid[y][x-1])) {
                                ans += findNumber(y,x-1, grid);
                            }
                        }
                        if (x+1 < l) {
                            if (Character.isDigit(grid[y][x+1])) {
                                ans += findNumber(y,x+1, grid);
                            }
                        }
                    }
                }
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
        int l = mode==1 ? 10 : 140; // he;; yeah 
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
            int y = 0;
            char grid[][] = new char[l][l];
            while (reader.hasNextLine()) {
                String data = reader.nextLine();
                for (int x = 0; x < l; ++x) {
                    grid[y][x] = data.charAt(x);
                }
                y++;
            } 

            for (y = 0; y < l; y++) {
                for (int x = 0; x < l; x++) {
                    if (!(grid[y][x] == '.' || 
                        grid[y][x] == '1' || 
                        grid[y][x] == '2' || 
                        grid[y][x] == '3' || 
                        grid[y][x] == '4' || 
                        grid[y][x] == '5' || 
                        grid[y][x] == '6' || 
                        grid[y][x] == '7' || 
                        grid[y][x] == '8' || 
                        grid[y][x] == '9' ||
                        grid[y][x] == '0')) {
                        //grid[y][x] is a symbol
                        /* 
                         *    (x-1, y-1) (x, y-1) (x+1, y-1)
                         *    (x-1, y)   (x, y)   (x+1, y)
                         *    (x-1, y+1) (x, y+1) (x+1, y+1)
                        */
                        //do bounds checking 
                        //top row
                        ArrayList<Integer> gears = new ArrayList<>();
                        System.out.printf("array: [%d][%d]=%c\n", y, x, grid[y][x]);
                        if (y-1 >= 0) {
                            if (x-1 >= 0) {
                                if (Character.isDigit(grid[y-1][x-1])) {
                                    gears.add(findNumber(y-1,x-1, grid));
                                }
                            }
                            if (x+1 < l) {
                                if (Character.isDigit(grid[y-1][x+1])) {
                                    gears.add(findNumber(y-1,x+1, grid));
                                }
                            }
                            if (Character.isDigit(grid[y-1][x])) {
                                gears.add(findNumber(y-1,x, grid));
                            }
                        }
                        if (y+1 < l) {
                            if (x-1 >= 0) {
                                if (Character.isDigit(grid[y+1][x-1])) {
                                    gears.add(findNumber(y+1,x-1, grid));
                                }
                            }
                            if (x+1 < l) {
                                if (Character.isDigit(grid[y+1][x+1])) {
                                    gears.add(findNumber(y+1,x+1, grid));
                                }
                            }
                            if (Character.isDigit(grid[y+1][x])) {
                                gears.add(findNumber(y+1,x, grid));
                            }
                        }
                        if (x-1 >= 0) {
                            if (Character.isDigit(grid[y][x-1])) {
                                gears.add(findNumber(y,x-1, grid));
                            }
                        }
                        if (x+1 < l) {
                            if (Character.isDigit(grid[y][x+1])) {
                                gears.add(findNumber(y,x+1, grid));
                            }
                        }
                        if (gears.size() == 2) {
                            ans += gears.get(0) * gears.get(1);
                        }
                    }
                }
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

    public static int findNumber(int y, int x, char[][] grid) {
        //bounds checking is done
        StringBuilder numString = new StringBuilder("");
        int left = 0;
        int right = 1;
        // System.out.printf("cartesian: (%d, %d)\n", x, y);
        // System.out.printf("array: [%d][%d]\n", y, x);
        while (x-left>=0) {
            if (Character.isDigit(grid[y][x-left])) {
                numString.insert(0, (grid[y][x-left]));
                grid[y][x-left] = '.'; 
                left++;
            }
            else {
                break;
            }
        }
        while (x+right<grid.length) {
            if (Character.isDigit(grid[y][x+right])) {
                numString.append(grid[y][x+right]);
                grid[y][x+right] = '.';
                right++;
            }
            else {
                break;
            }
        }
        System.out.println(numString);
        return Integer.parseInt(numString.toString());
    }
}