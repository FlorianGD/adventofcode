## Part 1

with open("day23-input.txt") as f:
    my_input = f.readlines()
my_input = [x.strip("\n").split(" ") for x in my_input]

registers = dict.fromkeys(list("abcdefgh"), 0)

def eval_reg(val):
    try:
        res = int(val)
    except ValueError:
    # Get the value from the registers, with 0 as default
        res = registers.get(val, 0)
    return res

def process_instr(instr, reg, val_or_reg, count_mul):
    res = 1
    if instr == "set":
        registers[reg] = eval_reg(val_or_reg)
    elif instr == "sub":
        registers[reg] -= eval_reg(val_or_reg)
    elif instr == "mul":
        registers[reg] *= eval_reg(val_or_reg)
        count_mul = count_mul + 1
    elif instr == "jnz":
        if eval_reg(reg) != 0:
            res = eval_reg(val_or_reg)   
    return res, count_mul

def process_all(instructions):
    i = 0
    count_mul = 0
    while True:
        try:
            a = instructions[i]
            instr = a[0]
            reg = a[1] 
            val_or_reg = None if len(a) == 2 else a[2]
            ret, count_mul = process_instr(instr, reg, val_or_reg, count_mul)
            i += ret
        except IndexError:
            return count_mul

print("Part1: {}".format(process_all(my_input)))