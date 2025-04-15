def condition(stream):
    for i in range(len(stream)):
        j = i
        while j < len(stream):
            if stream[i] == stream[j] and not j == i:
                return False
            j += 1
    return True

input = open("input.txt", "r")
input = input.readlines()[0][0:-1]
print(input)


ans = len(input)
for i in range(len(input)-3):
    window = input[i:i+14]
    if condition(window) :
        ans = i+14
        break

print(ans)