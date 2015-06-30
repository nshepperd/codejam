import sys

def reachable(vines, i, h):
    result = []
    pos = vines[i][0]
    for j in range(i+1, len(vines)):
        if vines[j][0] <= pos + h:
            result.append((j, min(vines[j][1], vines[j][0] - vines[i][0])))
    return result

def goal(vines, end, i, h):
    return end <= vines[i][0] + h

def is_visited(visited, i, h):
    for (j, k) in visited:
        if i <= j and h <= k:
            return True
    return False

def clean(visited, frontier):
    if not frontier:
        return visited
    m = min(i for (i, _) in frontier)
    j = 0
    while j < len(visited):
        if visited[j][0] < m:
            del visited[j]
        else:
            j += 1

    visited.sort(key=(lambda tup: abs(tup[0]) + abs(tup[1])), reverse=True)
    new = []
    for (i, h) in visited:
        if not is_visited(new, i, h):
            new.append((i, h))
    return new

def solve(vines, end):
    visited = []
    frontier = [(0, vines[0][0])]
    c = 0
    while frontier:
        c += 1
        if c % 30 == 0:
            sys.stderr.write('{}   {}       \r'.format(len(visited), len(frontier)))
            sys.stderr.flush()
        (index, h) = frontier[0]
        del frontier[0]

        visited = clean(visited, frontier)

        if goal(vines, end, index, h):
            return 'YES'

        for next in reachable(vines, index, h):
            if is_visited(visited, *next):
                continue
            visited.append(next)
            frontier.append(next)
    return 'NO'

with open('A-large-practice.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        number = int(file.readline())
        vines = []
        for _ in range(number):
            vines.append(tuple(map(int, file.readline().split())))
        end = int(file.readline())
        print 'Case #{}: {}'.format(case, solve(vines, end))
