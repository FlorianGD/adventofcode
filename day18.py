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

print(process_all(my_input))