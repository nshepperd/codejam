def rows(board):
    return board
def cols(board):
    for i in range(4):
        yield [board[j][i] for j in range(4)]
def diags(board):
    yield [board[i][i] for i in range(4)]
    yield [board[3-i][i] for i in range(4)]

def lines(board):    
    for l in rows(board):
        yield l
    for l in cols(board):
        yield l
    for l in diags(board):
        yield l

def isx(line):
    return ('.' not in line) and ('X' in line) and ('O' not in line)
def iso(line):
    return ('.' not in line) and ('O' in line) and ('X' not in line) 
def isdraw(board):
    for row in board:
        if '.' in row:
            return False
    return True

def state(board):
    x = False
    o = False
    draw = isdraw(board)
    for line in lines(board):
        x = x or isx(line)
        o = o or iso(line)
    assert not (x and o)

    if x:
        return 'X won'
    elif o:
        return 'O won'
    elif draw:
        return 'Draw'
    else:
        return 'Game has not completed'

with open('A-large.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        board = []
        for _ in range(4):
            board.append(file.readline().strip())
        file.readline()
        print 'Case #{}: {}'.format(case, state(board))
