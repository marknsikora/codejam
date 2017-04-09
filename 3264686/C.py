#!/usr/bin/env python3

from collections import defaultdict

def main():
    tests = int(input())

    for case in range(tests):
        line = input()
        n, k = line.split()

        n = int(n)
        k = int(k)

        d = defaultdict(int)
        d[n] = 1
        for _ in range(k):
            c = max(d.keys())

            d[c] -= 1
            if d[c] == 0:
                del d[c]

            l = c // 2
            r = (c - 1) // 2

            d[l] += 1
            d[r] += 1

        print("Case #{}: {} {}".format(case + 1, c // 2, (c - 1) // 2))

if __name__ == '__main__':
    main()
