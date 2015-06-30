import fractions

def propel(y, dy, t):
    newy = (y + dy * t) % (2*H)
    if newy > H:
        newy = 2*H - newy
    flips = ((y + dy * t) // H) % 2
    if flips:
        newdy = -dy
    else:
        newdy = dy

    return (newy, newdy)    

def go(y, dy, period, v):
    start = (y, dy)

    c = 0
    while True:
        (ny, ndy) = propel(y, dy, period)
        if abs(ny - y) > v * period:
            return c

        c += 1
        (y, dy) = (ny, ndy)
        if (y, dy) == start:
            return None

with open('sample.in') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (H, W) = map(int, file.readline().split())
        (N1, N2) = map(int, file.readline().split())
        (V1, V2) = map(int, file.readline().split())
        (y, x, dy, dx) = map(fractions.Fraction, map(int, file.readline().split()))

        period = 2*W / abs(dx)

        if dx > 0:
            t_to_left = (2*W - x) / abs(dx)
        elif dx < 0:
            t_to_left = x / abs(dx)

        (y1, dy1) = propel(y, dy, t_to_left)

        b = None
        for i in range(N1):
            (yy, dyy) = propel(y1, dy1, period*i)
            tmp = go(yy, dyy, N1*period, V1)
            if tmp is not None:
                tmp = N1 * tmp + i
                if b is None or tmp < b:
                    b = tmp

        left = b

        if dx > 0:
            t_to_right = (W - x) / abs(dx)
        elif dx < 0:
            t_to_right = (W + x) / abs(dx)

        (y2, dy2) = propel(y, dy, t_to_right)

        b = None
        for i in range(N2):
            (yy, dyy) = propel(y2, dy2, period*i)
            tmp = go(yy, dyy, N2*period, V2)
            if tmp is not None:
                tmp = N2 * tmp + i
                if b is None or tmp < b:
                    b = tmp

        right = b
        print left, right
