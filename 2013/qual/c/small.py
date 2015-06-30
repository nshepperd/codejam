from math import sqrt

def is_palindrome(n):
    return tuple(str(n).rstrip('L')) == tuple(reversed(str(n).rstrip('L')))

# def digits(left, total):
#     inner = total - len(left) * 2
#     right = ''.join(reversed(left))
#     small = str(long(left + ('0'*inner) + right)**2)
#     large = str(long(left + ('9'*inner) + right)**2)

#     assert len(small) == len(large)

#     right_valid = ''.join(reversed(small[-len(left):]))
#     left_valid = ''
#     left_range = None
#     for i in range(len(small)):
#         if small[i] == large[i]:
#             left_valid += small[i]
#         else:
#             left_range = (small[i], large[i])
#             break

#     # check that this is okay
#     # print '||', left, '|', long(left + ('0'*inner) + right), long(left + ('9'*inner) + right)
#     # print '~~', left_valid, left_range, right_valid
#     for i in range(len(right_valid)):
#         if i < len(left_valid):
#             if right_valid[i] != left_valid[i]:
#                 return False
#         else:
#             if not (left_range[0] <= right_valid[i] <= left_range[1]):
#                 # print 'rejecting {} <= {} <= {}'.format(left_range[0], right_valid[i], left_range[1])
#                 return False
#             break
#     return True

# def search(left, total):
#     if len(left) < (total // 2) - 1:
#         for d in '012':
#             if digits(left + d, total):
#                 search(left + d, total)
#     else:
#         right = ''.join(reversed(left))
#         for d in '012':
#             if is_palindrome(int(left + d + d + right)**2):
#                 print (left + d + d + right)

# def search_odd(left, total):
#     if len(left) < (total // 2):
#         for d in '012':
#             if digits(left + d, total):
#                 search_odd(left + d, total)
#     else:
#         right = ''.join(reversed(left))
#         for d in '012':
#             if is_palindrome(int(left + d + right)**2):
#                 print (left + d + right)

numbers = [1, 2, 3, 11, 22]

def pattern_even(left, total, ones = 0):
    if len(left) < (total // 2):
        if ones < 3:
            pattern_even(left + '1', total, ones + 1)
        pattern_even(left + '0', total, ones)
    else:
        right = ''.join(reversed(left))
        numbers.append(int(left + right))

def pattern_odd(left, total, ones = 0):
    if len(left) < (total // 2):
        if ones < 3:
            pattern_odd(left + '1', total, ones + 1)
        pattern_odd(left + '0', total, ones)
    else:
        right = ''.join(reversed(left))
        for d in '012':
            if is_palindrome(int(left + d + right)**2):
                numbers.append(int(left + d + right))

for i in range(4, 50+1, 2):
    pattern_even('1', i)
    numbers.append(int('2' + ('0' * (i-2)) + '2'))
for i in range(3, 49+1, 2):
    pattern_odd('1', i)
    inner = '0' * ((i-1) // 2)
    numbers.append(int('2' + inner + '0' + inner + '2'))
    numbers.append(int('2' + inner + '1' + inner + '2'))

with open('C-large-2.in', 'r') as file:
    T = int(file.readline())
    for case in range(1, T+1):
        (A, B) = map(int, file.readline().split())
        # print A, '->', B
        count = 0
        for n in numbers:
            p = n*n
            if p < A:
                continue
            if p > B:
                break
            count += 1
        print 'Case #{}: {}'.format(case, count)


# n = 101
# while n <= 10000000:
#     if is_palindrome(n):
#         p = n*n
#         if is_palindrome(p):
#             print n
#     n += 1
