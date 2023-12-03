import re
import random
import sys
import copy

def add_row(LA, check, num_of_added=1, t=2, append_LA=""):

    # Define a pattern for extracting interactions and rows
    pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

    # Read the input file
    with open(check, 'r') as file:
        content = file.read()

    # Use regular expressions to find all interactions and their details
    matches = re.finditer(pattern, content)

    # Create a list to store the separation data, and the design
    interactions = []
    design = []
    count = 0

    # Read in the design and parameters, assuming the input is in the format expected by checker
    with open(LA, 'r') as file:
        line = file.readline().strip()
        line = file.readline().strip()
        row_col = [int(value) for value in line.split()]
        num_factors = row_col[1]
        param_values = [int(x) for x in file.readline().strip().split('\t')]
        line = file.readline().strip()

        while line == "0":
            line = file.readline().strip()
        row = [int(value) for value in line.split()]
        design.append(row)
        for line in file:
            # Split the line by whitespace and convert the values to integers
            row = [int(value) for value in line.split()]
            design.append(row)


    for match in matches:
        interaction_number = int(match.group(1))
        int_values = match.group(2).replace('(','').replace(')', '').replace(',','').split(' ')
        rows = [int(row)-1 for row in match.group(3).split()]
        

        # Check if the 'rows' list contains < t elements
        if len(rows) < t:
            count += 1
            interaction_data = {
                'interaction_number': interaction_number,
                'int_values': int_values,
                'rows': rows,
            }

            interactions.append(interaction_data)

    # Print the extracted data
    # for interaction in interactions:
    #     print(f"Interaction {interaction['interaction_number']}:")
    #     print(f"Int: {interaction['int_values']}")
    #     print(f"Rows: {interaction['rows']}")

    for z in range(num_of_added):
        with open(LA, 'r') as file:
            lines = file.readlines()
        row_to_add = [[False, -1, -1] for i in range(num_factors)]
        interactions_to_remove = copy.deepcopy(interactions)

        # print(interactions_to_remove)
        # for r in design:
        #     print(r)
        num_removed = 0
        index = 0
        used_rows = set({})
        trials = count
        first = True
        first_row = 0
        # Randomly select interactions to add
        # Check that they aren't from the same row
        while trials >= 0 and count > 0:
            index = random.randint(0,count-1)
            interaction = interactions[index]
            contains = int(interaction['int_values'][0][1:]) in used_rows
            # vals = True
            # for i in range(len(param_values)):
            #     if design[int(interaction['int_values'][0][1:])][i] > param_values[i] - 1:
            #         vals = False
            #         break
            # print(int(interaction['int_values'][0][1:]))
            if row_to_add[int(interaction['int_values'][0][1:])][0] == False and \
                    row_to_add[int(interaction['int_values'][2][1:])][0] == False \
                        and not contains:
                row_to_add[int(interaction['int_values'][0][1:])][0] = True
                row_to_add[int(interaction['int_values'][2][1:])][0] = True
                row_to_add[int(interaction['int_values'][0][1:])][1] = interaction['rows'][0]
                row_to_add[int(interaction['int_values'][2][1:])][1] = interaction['rows'][0]
                row_to_add[int(interaction['int_values'][0][1:])][2] = int(interaction['int_values'][1])
                row_to_add[int(interaction['int_values'][2][1:])][2] = int(interaction['int_values'][3])
                if first:
                    first_row = interaction['rows'][0]
                    first = False
                interactions_to_remove.remove(interaction)
                num_removed += 1
            used_rows.add(interaction['rows'][0])
            trials -= 1
        count -= num_removed
        # print(row_to_add)
        # print(count)
        if count <= 0:
            break
        interactions = copy.deepcopy(interactions_to_remove)
        if not first:
            add = ""
            for i in range(len(row_to_add)):
                if row_to_add[i][2] == -1:
                    temp = random.randint(0,param_values[i]-1)
                    while temp == design[first_row][i]:
                        temp = random.randint(0,param_values[i]-1)
                    add += str(temp) + "\t"
                else:
                    # print("else " + str(row_to_add[i][2]), end = " ")
                    add += str(row_to_add[i][2]) + "\t"
            # print("\n")
            
            add = add[:-1]
            # print(add)
        header_data = lines[1].split('\t')
        lines[1] = "{}\t{}".format(int(header_data[0]) + 1, header_data[1])
        if append_LA != "":
            with open(append_LA, 'a') as f:
                f.writelines(add + "\t")
                f.write("\n")
        with open(LA, 'w') as file:
            file.writelines(lines)
            file.write(add)
            file.write("\n")
# add_row("formatted_LA.txt", "output.txt", 1, 2, "/home/michael/Desktop/Designs/2_2_3_3/2_2_3_3_separation.tsv")

num_parameters = len(sys.argv) - 1
if num_parameters == 5:
    locating_array = sys.argv[1]
    output = sys.argv[2]
    num_of_rows_to_add = int(sys.argv[3])
    t_way = int(sys.argv[4])
    append = sys.argv[5]
    add_row(locating_array, output, num_of_rows_to_add, t_way, append)
elif num_parameters == 4:
    locating_array = sys.argv[1]
    output = sys.argv[2]
    num_of_rows_to_add = int(sys.argv[3])
    t_way = int(sys.argv[4])
    add_row(locating_array, output, num_of_rows_to_add, t_way)
elif num_parameters == 3:
    locating_array = sys.argv[1]
    output = sys.argv[2]
    num_of_rows_to_add = int(sys.argv[3])
    add_row(locating_array, output, num_of_rows_to_add)
elif num_parameters == 2:
    locating_array = sys.argv[1]
    output = sys.argv[2]
    add_row(locating_array, output)