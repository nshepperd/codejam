def distance(N, paths):
    to = [set() for _ in range(N)]
    for i in range(N-1):
        to[paths[i][0]].add(i)
        to[paths[i][1]].add(i)

    distances = [-1 for _ in range(N)]
    visited = [False for _ in range(N)]
    frontier = [(0, N-1)]
    while frontier:
        (d, i) = frontier[0]
        del frontier[0]

        if visited[i]:
            continue
        visited[i] = True
        distances[i] = d

        for j in to[i]:
            frontier.append((d + 1, j))
        frontier.sort()
    
    dforest = [[] for _ in range(max(distances)+1)]
    unreachable = []
    for i in range(len(distances)):
        if distances[i] >= 0:
            dforest[distances[i]].append(i)
        else:
            unreachable.append(i)
    return dforest, unreachable

def solve(N, paths):
    print distance(N, paths)
        # state = [0 for _ in range(N-1)]
        # location = 0
        # steps = 0
        # for _ in range(2**(N)+1):
        #     new = paths[location][state[location]]
        #     state[location] = state[location] ^ 1
        #     location = new
        #     steps += 1
        #     if location == N-1:
        #         return steps
        # return 'Infinity'


with open('E-small-practice.in') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        N = int(file.readline())
        paths = []
        for _ in range(N-1):
            paths.append(tuple([int(s) - 1 for s in file.readline().split()]))
        print 'Case #{}: {}'.format(case, solve(N, paths))
