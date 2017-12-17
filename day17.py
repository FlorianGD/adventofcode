def spin(step):
    mem = [0]
    pos = 0
    for i in range(1, 2018):
        pos = (pos + step) % i + 1
        mem.insert(pos, i)
    return mem

my_spin = spin(349)
print(my_spin[my_spin.index(2017) + 1])