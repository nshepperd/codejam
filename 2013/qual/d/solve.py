from collections import Counter

def solve_fast(keys, left, chests, locks):
    # print keys, left
    frontier = [(keys.copy(), left.copy(), [])]
    while frontier:
        (keys, left, path) = frontier.pop()

        if not left:
            return True

        skip = False
        for key in list(keys.keys()):
            index = locks[key].intersection(left)
            for c in index:
                if key in chests[c][1]:
                    # the chest returns a key of same kind it requires
                    # in which case you might as well open it immediately
                    for k in chests[c][1]:
                        if k in locks:
                            keys[k] = keys.get(k, 0) + 1
                    keys[key] -= 1
                    left.remove(c)
                    path.append(c)
                    skip = True
        if skip:
            frontier.append((keys, left, path))

        for key in keys.keys():
            index = locks[key].intersection(left)
            if not index:
                continue
            any_key = [c for c in index if set(chests[c][1]).intersection(set(locks.keys()))]
            for c in index:
                if any_key and not set(chests[c][1]).intersection(set(locks.keys())):
                    # skip opening chests without any keys
                    # as long as we can open one that will give us a useful key
                    continue
                newkeys = keys.copy()
                newleft = left.copy()
                for k in chests[c][1]:
                    if k in locks:
                        newkeys[k] = newkeys.get(k, 0) + 1
                newkeys[key] -= 1
                if newkeys[key] == 0:
                    del newkeys[key]
                newleft.remove(c)
                frontier.append((newkeys, newleft, path + [c]))
            break
    return False

def solve(keys, left, chests, locks, path=[]):
    # print 'a-', left
    if not left:
        yield path
        return
    # print 'b-', left
    if not solve_fast(keys.copy(), left.copy(), chests, locks):
        return
    # print 'c-', left
    for i in left:
        # print 'd-', i, left
        k = chests[i][0]
        if k in keys:
            newkeys = keys.copy()
            newkeys[k] -= 1
            for m in chests[i][1]:
                newkeys[m] = newkeys.get(m, 0) + 1
            if newkeys[k] == 0:
               del newkeys[k]
            newleft = left.copy()
            newleft.remove(i)
            for solution in solve(newkeys, newleft, chests, locks, path + [i]):
                yield solution
                return

def head(generator):
    for item in generator:
        return item
    return None

with open('D-large-practice.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (_, N) = map(int, file.readline().split())
        initial_keys = Counter(map(int, file.readline().split()))
        chests = []
        for _ in range(N):
            line = map(int, file.readline().split())
            lock = line[0]
            keys = line[2:]
            chests.append((lock, keys))

        # print 'keys', initial_keys
        # print 'chests', chests

        key_chests = {k : set() for k in initial_keys.keys()}
        for i in range(N):
            lock = chests[i][0]
            if lock not in key_chests:
                key_chests[lock] = set()
            key_chests[lock].add(i)

        # print solve_fast(initial_keys, set(range(N)), chests, key_chests)

        # print N, set(range(N))

        solution = head(solve(initial_keys, set(range(N)), chests, key_chests))
        if solution:
            print 'Case #{}: {}'.format(case, ' '.join(str(i+1) for i in solution))
        else:
            print 'Case #{}: IMPOSSIBLE'.format(case)
