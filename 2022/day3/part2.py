import sys

def find_badge(a, b, c):
    return (set(a) & set(b) & set(c)).pop()

def decode(c):
    if 'a' <= c <= 'z':
        return (ord(c) - ord('a'))+1
    else:
        return (ord(c) - ord('A'))+27

elves = []
for l in sys.stdin:
    elves.append(l.strip())

groups = zip(elves[::3], elves[1::3], elves[2::3])
print("solution:", sum([decode(find_badge(*g)) for g in groups]))
