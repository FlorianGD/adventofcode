## Part 1

with open("day18-input.txt") as f:
    my_input = f.readlines()
my_input = [x.strip("\n").split(" ") for x in my_input]

registers = dict()
sounds = list()

def eval_reg(val):
    try:
        res = int(val)
    except ValueError:
    # Get the value from the registers, with 0 as default
        res = registers.get(val, 0)
    return res

def process_instr(instr, reg, val_or_reg = None):
    res = 1
    if instr == "set":
        registers[reg] = eval_reg(val_or_reg)
    elif instr == "add":
        registers[reg] = registers.get(reg, 0) + eval_reg(val_or_reg)
    elif instr == "mul":
        registers[reg] = registers.get(reg, 0) * eval_reg(val_or_reg)
    elif instr == "mod":
        registers[reg] = registers.get(reg, 0) % eval_reg(val_or_reg)
    elif instr == "jgz":
        if registers.get(reg, 0) > 0:
            res = eval_reg(val_or_reg)
    elif instr == "snd":
        sounds.append(registers.get(reg, 0))
    elif instr == "rcv":
        if registers.get(reg, 0) != 0:
            res = 0
    return res

def process_all(instructions):
    i = 0
    while True:
        a = instructions[i]
        instr = a[0]
        reg = a[1] 
        val_or_reg = None if len(a) == 2 else a[2]
        ret = process_instr(instr, reg, val_or_reg)
        if ret != 0:
            i += ret
        else:
            return sounds.pop()

print("Part1: {}".format(process_all(my_input)))

## Part 2
registers0 = {'p':0}
registers1 = {'p':1}
sounds = {0:[], 1: []}
send1 = 0

def eval_reg2(registers, val):
    try:
        res = int(val)
    except ValueError:
    # Get the value from the registers, with 0 as default
        res = registers.get(val, 0)
    return res

def process_instr2(registers, prog, instr, reg, val_or_reg):
    global send1
    res = 1
    if instr == "set":
        registers[reg] = eval_reg2(registers, val_or_reg)
    elif instr == "add":
        registers[reg] = registers.get(reg, 0) + eval_reg2(registers, val_or_reg)
    elif instr == "mul":
        registers[reg] = registers.get(reg, 0) * eval_reg2(registers, val_or_reg)
    elif instr == "mod":
        registers[reg] = registers.get(reg, 0) % eval_reg2(registers, val_or_reg)
    elif instr == "jgz":
        if eval_reg2(registers, reg) > 0:
            res = eval_reg2(registers, val_or_reg)
    elif instr == "snd":
        sounds[1 - prog].append(eval_reg2(registers, reg))
        if prog == 1:
            send1 += 1
    elif instr == "rcv":
        if len(sounds[prog]) > 0:
            registers[reg] = sounds[prog].pop(0)
        else:
            res = 0
    return res

def parse_instr(instruction):
    instr = instruction[0]
    reg = instruction[1] 
    val_or_reg = None if len(instruction) == 2 else instruction[2]
    return instr, reg, val_or_reg

def two_reg(instructions):
    max_len = len(instructions)
    i0 = 0
    i1 = 0
    while True:
        if i0 < 0 or i0 >  max_len:
            i0 = -1
            ret0 = 0
        else:
            instr0, reg0, val_or_reg0 = parse_instr(instructions[i0])
            ret0 = process_instr2(registers0, 0, instr0, reg0, val_or_reg0)
            i0 += ret0
        
        if i1 < 0 or i1 >  max_len:
            i1 = -1
            ret1 = 0
        else:
            instr1, reg1, val_or_reg1 = parse_instr(instructions[i1])
            ret1 = process_instr2(registers1, 1, instr1, reg1, val_or_reg1)
            i1 += ret1        
        
        if ret0 == 0 and ret1 == 0:
            return send1

print("Part2: {}".format(two_reg(my_input)))