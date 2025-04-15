from Directory import Directory
from File import File

input = open("input.txt", "r")
input = input.readlines()

depth = 0
system = Directory("/", '', depth)
current = system

for line in input:
    tokens = line.split(' ')
    if not line[0] == '$':
        if line[0:3]=='dir':
            d = Directory(tokens[1][:-1], current.parent + "-" + current.name, depth)
            current.update(d)
        else:
            f = File(line.split(' ')[1][:-1], line.split(' ')[0],  depth)
            current.update(f)
    elif line.split(' ')[1] == 'cd':
        print("CDs nuts")
        if tokens[2][:-1] == '..':
            print('go up')
            path = current.parent
            print(f'path: {path}')
            arr = path.split("-")
            print(f'arr:{arr}')
            print(f'last ele in path (parent): {current.parent.split("-")[-1]}')
            tmp = system
            print(f'home: {tmp}')
            for lvl in arr:
                tmp = tmp.get(lvl)
            depth -= 1
        elif tokens[2] == '/\n':
            #do nothing, were already in that directory
            pass
        else:
            current = current.get(tokens[2][:-1])
            depth += 1
        

    elif line.split(' ')[1] == 'ls': 
        #this is correct all we need to do is read
        pass

print(system.ls())