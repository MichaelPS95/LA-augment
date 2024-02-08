import re
import random
import sys
import copy


# This works by counting similarities of each row where the interaction
# appears previously, if the number of same elements == len(row), it's 
# a duplicate, if it checks all rows in the design that have the interaction
# returns false if there are no dupes found after looking through design
def duplicate_row(des, row, rows_appeared):
    count = 0
    for j in range(len(rows_appeared)-1):
        for i in range(len(row)):
            print(f"des[{rows_appeared[j]}][{i}] = {des[rows_appeared[j]][i]}, row[{i}][0] = {row[i][0]}")
            if des[rows_appeared[j]][i] == row[i][0]:
                count += 1
        print(f"count = {count}")
        if count == len(row):
            return True
        count = 0
    
    return False
        

# This function checks interaction compatability for when
# adding interactions to a row to be added during augment
def compatable(row, int1, t_val):
    indices = []
    int_vals = []

    # Get the indices of the interaction columns, and the values
    for i in range(t_val):
        indices.append(int(int1['int_values'][i*2][1:]))
        int_vals.append(int(int1['int_values'][i*2+1]))

    # Check that they don't conflict with existing entries
    for i in range(len(indices)):
        if row[indices[i]][0] != -1:
            return False

    for k in range(len(row)):
        for i in range(len(row[k][1])-1):
            for j in range(len(int1['rows'])):
                print(f"row[{k}][1][{i}] = {row[k][1][i]}, int1['rows'][{j}] = {int1['rows'][j]}, {row[k][1][i] == int1['rows'][j]}")
                if row[k][1][i] == int1['rows'][j]:
                    return False
    return True


# This function check separation between interaction to calculate which
# interactions need to be considered for adding rows
def check_separation(int1, int2, t_val):
    # print(int1)
    # print(int2)
    if len(int1) == 0:
        return 0
    c = 0
    for i in range(len(int1)):
        for j in range(len(int2)):
            if int1[i] == int2[j]:
                c += 1
                break
    if max(abs(len(int1)-c), abs(len(int2)-c)) < t_val:
        # need to add both interactions to the pool that aren't separated
        return 3
    elif abs(len(int1)-c) < t_val:
        # need to just add the first interaction
        return 2
    elif abs(len(int2)-c) < t_val:
        # just second interaction
        return 1
    # interactions are t-separated
    return 0


def add_row(LA, check, num_to_add=1, t=2, append_LA=""):

    # Define a pattern for extracting interactions and rows
    pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

    # Read the input file
    with open(check, 'r') as file:
        content = file.read()

    # Use regular expressions to find all interactions and their details
    matches = re.finditer(pattern, content)

    # Create a list to store the separation data, and the design
    interactions = []
    not_sep = set()
    missing_rows = []
    design = []
    count = 0
    n_s = []

    # Read in the design and parameters, assuming the input is in the format expected by checker
    with open(LA, 'r') as file:
        line = file.readline().strip()
        line = file.readline().strip()
        row_col = [int(value) for value in line.split()]
        num_factors = row_col[1]
        param_values = [int(x) for x in file.readline().strip().split()]
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
        

        # Add all the interactions
        count += 1
        interaction_data = {
        'interaction_number': interaction_number,
        'int_values': int_values,
        'rows': rows,
        }

        interactions.append(interaction_data)

    # for interaction in interactions:
    #     print(interaction)
    
    for i in range(len(interactions)-1):
        for j in range(i+1, len(interactions)):

            # calculate the separation of each interaction to every other interaction
            # if there exists a pair of interactions that have less than t difference
            # add both to the set of interactions that need to be added to the design

            c = 0
            for k in range(len(interactions[i]['rows'])):
                for l in range(len(interactions[j]['rows'])):
                    if interactions[i]['rows'][k] == interactions[j]['rows'][l]:
                        c += 1
                        # break
            if abs(len(interactions[i]['rows'])-c) + abs(len(interactions[j]['rows']) - c) < t:
                not_sep.add(i+1)
                not_sep.add(j+1)


    for num in not_sep:
        n_s.append(interactions[num-1])

    # If design is t-separated there is no work to be done
    if len(n_s) == 0 or len(not_sep) == 0:  
        print(f"The design is already {t}-separated.")
        return


    for i in range(num_to_add):
        full = False
        row_to_add = [[-1, []] for j in range(num_factors)]
        for j in range(len(interactions)*2):
            rand = random.randint(0,len(n_s)-1)
            interaction = n_s[rand]
            if(compatable(row_to_add, interaction, t)):
                interaction['rows'].append(len(design)+1)
                for k in range(t):
                    row_to_add[int(interaction['int_values'][k*2][1:])][0] = int(interaction['int_values'][k*2+1])
                    row_to_add[int(interaction['int_values'][k*2][1:])][1] = interaction['rows']
            # print(row_to_add)
            count = 0
            for i in range(num_factors):
                if row_to_add[i][0] != -1:
                    count += 1

            if count == num_factors:
                full = True
                break
        if not full:
            r = []
            for j in range(len(row_to_add)):
                if row_to_add[j][1] != []:
                    r =  row_to_add[j][1]
                    break
            for j in range(num_factors):
                if row_to_add[j][1] == []:
                    row_to_add[j][0] = random.randrange(0,param_values[j])
            while(duplicate_row(design, row_to_add, r)):
                for j in range(num_factors):
                    if row_to_add[j][1] == []:
                        row_to_add[j][0] = random.randrange(0,param_values[j])



        add = ""
        for n in range(len(row_to_add )):
            add += str(row_to_add[n][0]) + "\t"
        # print("\n")
        with open(LA, 'r') as file:
            lines = file.readlines()
            header_data = lines[1].strip().split()
            lines[1] = "{}\t{}".format(int(header_data[0]) + 1, header_data[1]) + "\n"
        with open(LA, 'w') as file:
                file.writelines(lines)
                file.write(add)
                file.write("\t\n")


add_row("array.tsv", "check.txt", 1, 2, "array.tsv")


# add_row(LA, check, num_of_added=1, t=2, append_LA="")

# num_parameters = len(sys.argv) - 1
# if num_parameters == 5:
#     locating_array = sys.argv[1]
#     output = sys.argv[2]
#     num_of_rows_to_add = int(sys.argv[3])
#     t_way = int(sys.argv[4])
#     append = sys.argv[5]

#     for arg in sys.argv:
#         print(arg)
#     add_row(locating_array, output, num_of_rows_to_add, t_way, append)
# elif num_parameters == 4:
#     locating_array = sys.argv[1]
#     output = sys.argv[2]
#     num_of_rows_to_add = int(sys.argv[3])
#     t_way = int(sys.argv[4])
#     add_row(locating_array, output, num_of_rows_to_add, t_way)
# elif num_parameters == 3:
#     locating_array = sys.argv[1]
#     output = sys.argv[2]
#     num_of_rows_to_add = int(sys.argv[3])
#     add_row(locating_array, output, num_of_rows_to_add)
# elif num_parameters == 2:
#     locating_array = sys.argv[1]
#     output = sys.argv[2]
#     add_row(locating_array, output)
