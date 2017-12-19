## Part 1
with open("day19-input.txt") as f:
    my_input = f.readlines()
maze = [x.strip('\n') for x in my_input]

def up_or_down(maze, i, j, sign, letters):
    '''
    Move up or down inside the maze given initial i and j
    sign of 1 moves down, -1 moves up
    Stop whenever the maze returns "+" or " "
    If a letter is found, it is appended to letters
    Returns new j
    '''
    while maze[j + sign][i] not in "+ ":
        j += sign
        if maze[j][i] in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
            letters.append(maze[j][i])
    return j + sign

def left_or_right(maze, i, j, sign, letters):
    '''
    Move left or right inside the maze given initial i and j
    sign of 1 moves right, -1 moves left
    Stop whenever the maze is different from '+' or " "
    If a letter is found, it is appended to letters
    Returns new i
    '''
    while maze[j][i + sign] not in "+ ":
        i += sign
        if maze[j][i] in "ABCDEFGHIJKLMNOPQRSTUVWXYZ":
            letters.append(maze[j][i])
    return i + sign

def next_left_right(maze, i, j):
    '''
    We were moving up or down and have to find the next move : left (-1) or right (1)
    There are spaces all around the maze so we can't get out by checking i + 1
    '''
    return -1 if maze[j][i + 1] == " " else 1

def next_up_down(maze, i, j):
    '''
    We were moving left or right and have to find the next move : up (-1) or down (1)
    There are spaces all around the maze so we can't get out by checking j + 1
    '''
    return -1 if maze[j + 1][i] == " " else 1

def walk_maze(maze):
    '''
    Walk all the maze and return the letters as found
    '''
    letters = []
    # Find beginning of maze
    i = maze[0].index('|')
    # First move is down
    sign = 1
    j = up_or_down(maze, i, 0, sign, letters)
    
    while True:
        sign = next_left_right(maze, i, j)
        # if next move is a space, we reached the end
        if maze[j][i + sign] == " ":
            break
        i = left_or_right(maze, i, j, sign, letters)

        sign = next_up_down(maze, i, j)
        # if next move is a space, we reached the end
        if maze[j + sign][i] == " ":
            break
        j = up_or_down(maze, i, j, sign, letters)
    return "".join(letters)

print("Part 1: {}".format(walk_maze(maze)))

## Part 2

def walk_maze2(maze):
    steps = 1
    letters = []
    # Find beginning of maze
    i = maze[0].index('|')
    # First move is down
    sign = 1
    j = up_or_down(maze, i, 0, sign, letters)
    steps += j
    while True:
        sign = next_left_right(maze, i, j)
        # if next move is a space, we reached the end
        if maze[j][i + sign] == " ":
            break
        i0 = i
        i = left_or_right(maze, i, j, sign, letters)
        steps += abs(i - i0)

        sign = next_up_down(maze, i, j)
        # if next move is a space, we reached the end
        if maze[j + sign][i] == " ":
            break
        j0 = j
        j = up_or_down(maze, i, j, sign, letters)
        steps += abs(j - j0)
    return steps - 1

print("Part 2: {}".format(walk_maze2(maze)))
