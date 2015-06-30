from math import sqrt

def n_to_t(r, n):
    m = n-1
    return (m+1) * (2*m + 2*r + 1)

with open('A-large.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (r, t) = map(int, file.readline().split())
        a = r - 0.5
        n = int((sqrt(a*a + 2*t) - a)/2)
        while n_to_t(r, n) < t:
            n += 1
        while n_to_t(r, n) > t:
            n -= 1
        print 'Case #{}: {}'.format(case, n)
