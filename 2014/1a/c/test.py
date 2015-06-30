from random import randint
import math

def swap(nums, k, p):
    tmp = nums[k]
    nums[k] = nums[p]
    nums[p] = tmp

def good(N):
    nums = list(range(N))
    for k in range(N):
        p = randint(k, N-1)
        swap(nums, k, p)
    return nums

def bad(N):
    nums = list(range(N))
    for k in range(N):
        p = randint(0, N-1)
        swap(nums, k, p)
    return nums

def mean(lst):
    return float(sum(lst)) / len(lst)
def var(lst):
    m = mean(lst)
    return mean([(x-m)*(x-m) for x in lst])
def stdev(lst):
    return math.sqrt(var(lst))

def repeat(permutation, n):
    lst = list(permutation)
    orbits = [0 for _ in permutation]
    done = [1 for _ in permutation]
    for step in range(n):
        new = list(lst)
        for i in range(len(permutation)):
            new[i] = lst[permutation[i]]
            orbits[i] += done[i]
        lst = new
        for i in range(len(lst)):
            if i == lst[i]:
                done[i] = 0
        if lst == permutation:
            return orbits
    return orbits

def match(perm):
    return sum([1 if (i==x) else 0 for (i, x) in enumerate(perm)])

def evaluate(runs):
    N = len(runs[0])
    matrix = [[0] * N for _ in range(N)]
    included = 0
    for run in runs:
        # if run[0] == 0:
        for i in range(N):
            matrix[run[i]][i] += 1
        included += 1
    ps = [[(1.0 + x) / (N + included) for x in row] for row in matrix]
    return ps
    # with open(output, 'w') as file:
    #     for j in range(2, N):
    #         file.write('{} {} {}\n'.format(K, j, 0.085))
    #     for i in range(2, N):
    #         for j in range(2, N):
    #             file.write('{} {} {}\n'.format(K+i, j, ps[i][j]))
    #         file.write('\n')
    #     for j in range(2, N):
    #         file.write('{} {} {}\n'.format(K+N, j, 0.119))

N = 1000
pgood = evaluate([good(N) for _ in range(50000)])
pbad = evaluate([bad(N) for _ in range(50000)])
for i in range(N):
    for j in range(N):
        print i, j, pgood[i][j] / pbad[i][j]
    print

# bads = [bad(1000) for _ in range(500)]
# print mean([max(repeat(run, 1000)) for run in bads])
