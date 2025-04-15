MODE = 2

def f(i):
    return 4*i + 1

input = open("input.txt", "r")
input = input.readlines()

#print(input)

stacksTXT = input[0:9]
for txt in stacksTXT:
    print(txt, end='')
stacks=[[],[],[],[],[],[],[],[],[]]
i = 8
while i >= 0:
    j = 0
    while j <= 7:
        c = stacksTXT[j][f(i)]
        if not c == ' ':
            stacks[i].append(c) 
        j+=1
    i-=1

instructions = input[10:]
for i in range(len(instructions)):
    spl = instructions[i].split()
    instructions[i] = [int(spl[1]), int(spl[3]), int(spl[5])]



# for column in stacks:
#     for char in column:
#         print(char, end='')
#     print('')

# read instructions
print('----------------------------------------')
while instructions:
    move = instructions[0][0]
    src = instructions[0][1] - 1
    dst = instructions[0][2] - 1
    print(f'move:{move}, src:{src + 1}, dst:{dst + 1}')
    print('----------------------------------------')

    first = True
    sn = 1
    for column in stacks:
        if not first:
            print('')
        else:
            first = False
        print(f'{sn}: ', end='')
        for char in column:
            print(char, end='')
    #     sn += 1
    x = 0
    if MODE == 2:
        toMove = stacks[src][0:move]
        print(toMove)
        stacks[dst] = toMove + stacks[dst]
        stacks[src] = stacks[src][move:]
    if MODE == 1:
        while x < move:
            stacks[dst].insert(0, stacks[src].pop(0))
            x+=1
    del instructions[0]
    # print('\n----------------------------------------')


for stack in stacks:
    print(stack[0], end='')