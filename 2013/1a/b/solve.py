def find_higher(first, window):
    for i in range(len(window)):
        if window[i] > first:
            return (i, window[i])
    return None

with open('B-large-practice.in') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (emax, regain, n) = map(int, file.readline().split())
        window = (emax // regain) + (1 if emax % regain else 0) - 1
        values = map(int, file.readline().split())

        gain = 0
        energy = emax
        for i in range(n):
            win = values[i+1 : i+window+1]
            if find_higher(values[i], win):
                (j, v) = find_higher(values[i], win)
                # use up enough that we will be recharged by then
                ideal_energy_now = min(max(0, emax - (j+1)*regain), energy)
                gain += values[i] * (energy - ideal_energy_now)
                energy = ideal_energy_now
            else:
                # use up all energy
                gain += energy * values[i]
                energy = 0
            energy = min(energy + regain, emax)

        print 'Case #{}: {}'.format(case, gain)
