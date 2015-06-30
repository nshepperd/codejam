def possible(lawn, N, M):
    rowmax = [max(lawn[y]) for y in range(N)]
    colmax = [max(lawn[y][x] for y in range(N)) for x in range(M)]
    for y in range(N):
        for x in range(M):
            if lawn[y][x] not in (rowmax[y], colmax[x]):
                return 'NO'
    return 'YES'

with open('B-large.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (N, M) = map(int, file.readline().split())
        lawn = []
        for _ in range(N):
            lawn.append(map(int, file.readline().split()))
        for row in lawn:
            print row
        print 'Case #{}: {}'.format(case, possible(lawn, N, M))
