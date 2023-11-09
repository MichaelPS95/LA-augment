import re

def add_row(LA, check, t=2):

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

    # Read in the design and parameters, assuming the input is in the format expected by checker
    with open(LA, 'r') as file:
        line = file.readline().strip()
        line = file.readline().strip()
        row_col = [int(value) for value in line.split()]
        num_factors = row_col[1]
        param_values = [int(x) for x in file.readline().split('\t')]
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
        rows = [int(row) for row in match.group(3).split()]

        # Check if the 'rows' list contains < t elements
        if len(rows) < t:
            interaction_data = {
                'interaction_number': interaction_number,
                'int_values': int_values,
                'rows': rows,
            }

            interactions.append(interaction_data)

    # Print the extracted data
    for interaction in interactions:
        print(f"Interaction {interaction['interaction_number']}:")
        print(f"Int: {interaction['int_values']}")
        print(f"Rows: {interaction['rows']}")
    row_to_add = [[False, -1, -1] for i in range(num_factors)]

    for r in design:
        print(r)
    for interaction in interactions:
        if row_to_add[int(interaction['int_values'][0][1:])][0] == False and \
                row_to_add[int(interaction['int_values'][2][1:])][0] == False and \
                    row_to_add[int(interaction['int_values'][0][1:])][1] != interaction['rows'][0] and \
                        row_to_add[int(interaction['int_values'][2][1:])][1] != interaction['rows'][0] :
            row_to_add[int(interaction['int_values'][0][1:])][0] = True
            row_to_add[int(interaction['int_values'][2][1:])][0] = True
            row_to_add[int(interaction['int_values'][0][1:])][1] = interaction['rows'][0]
            row_to_add[int(interaction['int_values'][2][1:])][1] = interaction['rows'][0]
            row_to_add[int(interaction['int_values'][0][1:])][2] = int(interaction['int_values'][1])
            row_to_add[int(interaction['int_values'][2][1:])][2] = int(interaction['int_values'][3])
            
    
    print(row_to_add)

add_row("/home/michael/Desktop/Array-Checker/Sample-Input/Colbourn1.tsv", "t_way.txt")
