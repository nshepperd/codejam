def getcost(N, dist):
    return N*N - dist*(dist-1)//2 #sum(N - i for i in range(dist))

def divide(travel):
    start = 0
    end = 0
    for i in range(len(travel)):
        if travel[i] > 0:
            end = i + 1
        else:
            if end > start:
                yield (end - start, start, end, min(travel[start:end]))
            start = i + 1
            end = i + 1
    if end > start:
        yield (end - start, start, end, min(travel[start:end]))

def solve(N, pairs):
    pairs = list(pairs)
    pairs.sort()

    cost = 0

    travel = [0] * (N-1)
    for (a, b, n) in pairs:
        for i in range(a-1, b-1):
            travel[i] += n

    while max(travel):
        # print travel
        divide(travel)
        (_, a, b, n) = sorted(divide(travel), reverse=True)[0]
        cost += n * getcost(N, b-a)
        for i in range(a, b):
            travel[i] -= n

    return cost

def basic_cost(N, pairs):
    return sum(n * getcost(N, b-a) for (a, b, n) in pairs)        

with open('A-large-practice.in') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (N, M) = map(int, file.readline().split())
        pairs = []
        for _ in range(M):
            pairs.append(map(int, file.readline().split()))
        print 'Case #{}: {}'.format(case, basic_cost(N, pairs) - solve(N, pairs))
