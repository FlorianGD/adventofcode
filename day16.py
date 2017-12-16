def spin(prog, n):
    head = prog[:-n]
    tail = prog[-n:]
    return tail + head

def exchange(prog, i, j):
    prog[i], prog[j] = prog[j], prog[i]
    return prog

def partner(prog, a, b):
    indices = [i for i, j in enumerate(prog) if j in [a, b]]
    return exchange(prog, int(indices[0]), int(indices[1]))

PROGRAMS = list('abcdefghijklmnop')

def dance(prog, instructions):
    for instr in instructions:
        if instr[0] == "s":
            prog = spin(prog, int(instr[1:]))
        if instr[0] == "x":
            a = instr[1:].split('/')
            prog = exchange(prog, int(a[0]), int(a[1]))
        if instr[0] == "p":
            a = instr[1:].split('/')
            prog = partner(prog, a[0], a[1])
    return "".join(prog)

assert dance(PROGRAMS[:5], "s1,x3/4,pe/b".split(",")) == "baedc"

with open("day16-input.txt") as f:
    my_input = f.read()
    my_input = my_input.strip().split(',')

print("Part1:{}".format(dance(PROGRAMS[:], my_input)))

def gen_dance(prog, instructions, n):
    seen = []
    for i in range(n):
        s = ''.join(prog)
        if s in seen: 
            return seen[n % i]
        seen.append(s)
        for instr in instructions:
            if instr[0] == "s":
                prog = spin(prog, int(instr[1:]))
            if instr[0] == "x":
                a = instr[1:].split('/')
                prog = exchange(prog, int(a[0]), int(a[1]))
            if instr[0] == "p":
                a = instr[1:].split('/')
                prog = partner(prog, a[0], a[1])
    return "".join(prog)

assert gen_dance(list("abcde"), "s1,x3/4,pe/b".split(","), 2) == "ceadb"

print("Part 2: {}".format(gen_dance(PROGRAMS[:], my_input, 1000000000)))