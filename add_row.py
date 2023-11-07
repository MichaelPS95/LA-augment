import re

def add_row(file_name):

    # Define a pattern for extracting interactions and rows
    pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

    # Read the input file
    with open(file_name, 'r') as file:
        content = file.read()

    # Use regular expressions to find all interactions and their details
    matches = re.finditer(pattern, content)

    # Create a list to store the extracted data
    interactions = []
    # Iterate through the matches
    for match in matches:
        t = len(match.group(1))
        interaction_number = int(match.group(1))
        int_values = match.group(2).split()
        rows = [int(row) for row in match.group(3).split()]

        # Check if the 'rows' list contains only one element
        if len(rows) < t:
            interaction_data = {
                'interaction_number': interaction_number,
                'int_values': int_values,
                'rows': rows
            }

            interactions.append(interaction_data)

    # Print the extracted data
    for interaction in interactions:
        print(f"Interaction {interaction['interaction_number']}:")
        print(f"Int: {interaction['int_values']}")
        print(f"Rows: {interaction['rows']}\n")

add_row("t_way.txt")