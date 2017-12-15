# def judge(start_a, start_b):
#     while True:        
#         next_a = (start_a * 16807) % 2147483647
#         next_b = (start_b * 48271) % 2147483647
#         yield next_a % (2 ** 16) == next_b % (2 ** 16)
#         start_a = next_a
#         start_b = next_b

# my_judge = judge(783, 325)
# total = sum(next(my_judge) for _ in range(40000000))

# print("Part 1: {}".format(total))

def gen(mult, start, cond):
    while True:
        start = (start * mult) % 2147483647
        if start % cond == 0:
        	yield start

gen_a = gen(16807, 783, 4)
gen_b = gen(48271, 325, 8)

total_2 = 0
for _ in range(5000000):
	total_2 += next(gen_a) % (2 ** 16) == next(gen_b) % (2 ** 16)

print("Part 2: {}".format(total_2))