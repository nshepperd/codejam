from collections import namedtuple

Event = namedtuple('Event', ('x', 'y', 't'))

START = Event(0, 0, 0)

def travel_time(one, two):
    return 100 * max(abs(two.x - one.x), abs(two.y - one.y))

def memoize(f):
    memory = {}
    def _f(zombies, i, t):
        key = (id(zombies), i, t)
        if key not in memory:
            memory[key] = f(zombies, i, t)
        return memory[key]
    return _f

def get_reachable(zombies, i, t):
    """Zombies reachable from zombie i at time t, together with arrival times."""
    times = {}
    for j in range(len(zombies)):
        if i == j:
            continue
        arrival = max(t + travel_time(zombies[i], zombies[j]),
                      t + 750,
                      zombies[j].t)
        if arrival <= zombies[j].t + 1000:
            times[j] = arrival
    return times

def get_first(zombies):
    """Zombies reachable from the start, together with arrival times."""
    times = {}
    for j in range(len(zombies)):
        arrival = max(travel_time(START, zombies[j]), zombies[j].t)
        if arrival <= zombies[j].t + 1000:
            times[j] = arrival
    return times

@memoize
def get_next_score(zombies, i, now):
    """Highest possible additional score having just smashed zombie 'i' at time 'now'."""
    best = 0
    for (j, arrival) in get_reachable(zombies, i, now).items():
        score = 1 + get_next_score(zombies, j, arrival)
        if score > best:
            best = score
    return best

def solve(zombies):
    return max([0] + [1 + get_next_score(zombies, i, now) for (i, now) in get_first(zombies).items()])

with open('A-large-practice.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        Z = int(file.readline())
        zombies = []
        for i in range(Z):
            (x, y, t) = map(int, file.readline().split())
            zombies.append(Event(x, y, t))
        zombies.sort(key = lambda event: event.t)
        print 'Case #{}: {}'.format(case, solve(zombies))
