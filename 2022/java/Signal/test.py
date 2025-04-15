
def barfoo(stream):
    for i in range(len(stream)):
        j = i
        while j < len(stream):
            if stream[i] == stream[j] and not j == i:
                return False
            j += 1
    return True


window = 'abcdefgaijklmn'
foobar = (not window[0] == window[1]) and (not window[0] == window[2]) and (not window[0] == window[3]) and  (not window[1] == window[2]) and (not window[1] == window[3]) and (not window[2] == window[3])
# assert(foobar==False)
assert(barfoo(window)==False)

