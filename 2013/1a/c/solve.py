# subsets = [{} for _ in range(4096)]

maxdigit = 8

# for sss in range(4096):

def numbersets(k=12):
    if k == 0:
        yield ()
    else:
        for d in range(2, maxdigit+1):
            for ds in numbersets(k-1):
                yield (d,) + ds

# print list(numbersets())

def subsetproducts(nums):
    ret = {}
    for subset in range(4096):
        total = 1
        for i in range(12):
            if (1 << i) & subset:
                total *= nums[i]
        ret[total] = ret.get(total, 0) + 1
    return ret

digits = {nums : subsetproducts(nums) for nums in numbersets()}
# print digits                    # 

def prob(s, nums):
    p = 1
    for n in nums:
        p *= digits[s].get(n, 0)
    return p


with open('C-small-1-attempt0.in') as file:
    file.readline()
    file.readline()
    R = 8000
    print 'Case #1:'
    for case in range(R):
        nums = map(int, file.readline().split())
        print ''.join(map(str, max(digits.keys(), key = lambda s: prob(s, nums))))
