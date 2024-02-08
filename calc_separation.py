import re
import sys

def get_score(file_name, t=2):

    # Define a pattern for extracting interactions and rows
    pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

    # Read the input file
    with open(file_name, 'r') as file:
        content = file.read()

    # Use regular expressions to find all interactions and their details
    matches = re.finditer(pattern, content)

    # Create a list to store the extracted data
    count = 0
    # num = 0
    rows = []
    not_sep = set()

    # Iterate through the matches
    for match in matches:
        count += 1
        rows.append([int(x) for x in match.group(3).split()])

    # Add rows indexes to a set that are not separated to get a count
    # This works by comparing each row i to row i+1 up to number of rows
    for i in range(count-1):
        for j in range(i+1, count):

            # calculate the separation of each interaction
            # if they have less than t difference, add both
            # here we compare every interaction to every other

            c = 0
            for k in range(len(rows[i])):
                for l in range(len(rows[j])):
                    if rows[i][k] == rows[j][l]:
                        c += 1
                        break
            if (abs(len(rows[i]) - c) + abs(len(rows[j]) - c)) < t:
                not_sep.add(i)
                not_sep.add(j)


    if count > 0:
        print((count-len(not_sep))/count)
    else:
        print("Error in the input file.")

# num_parameters = len(sys.argv) - 1
# if num_parameters == 2:
#     locating_array = sys.argv[1]
#     t_way = int(sys.argv[2])
#     get_score(locating_array, t_way)
# else:
#     locating_array = sys.argv[1]
#     get_score(locating_array)

# get_score("check.txt", 2)
get_score("factorial_check.txt")
