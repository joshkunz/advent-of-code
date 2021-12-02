import sys

last = None
s = 0
for l in sys.stdin:
    v = int(l)
    if last is None:
        last = v
        continue
    if v > last:
        print(f"{v}\t1")
        s += 1
    else:
        print(f"{v}\t0")
    last = v
print(s)
