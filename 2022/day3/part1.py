import sys

def find_repeat(s):
    first = set(s[:len(s)//2])
    second = set(s[len(s)//2:])
    return (first & second).pop()

def decode(c):
    if 'a' <= c <= 'z':
        return (ord(c) - ord('a'))+1
    else:
        return (ord(c) - ord('A'))+27

found = []
for l in sys.stdin:
    found.append(find_repeat(l))

print("solution:", sum(map(decode, found)))
