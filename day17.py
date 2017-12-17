def spin(step):
    mem = [0]
    pos = 0
    for i in range(1, 2018):
        pos = (pos + step) % i + 1
        mem.insert(pos, i)
    return mem

my_spin = spin(349)
print("Part 1: {}".format(my_spin[my_spin.index(2017) + 1]))

def spin_2(step):
    pos = 0
    res = 0
    for i in range(1, 50000000):
        pos = (pos + step) % i + 1
        if pos == 1:
            res = i
    return res

print("Part 2: {}".format(spin_2(349)))
