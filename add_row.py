import re

def add_row(file_name, t=2):

    # Define a pattern for extracting interactions and rows
    pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

    # Define a pattern for extracting the "Factor" rows
    factor_pattern = r'Factor (\d+):'

    # Read the input file
    with open(file_name, 'r') as file:
        content = file.read()

        # Use regular expressions to find all interactions and their details
        matches = re.finditer(pattern, content)

        # Create a list to store the extracted data
        interactions = []

        factor_count = 0  # Initialize the factor count

        # Iterate through the content to count the factors before interactions
        for factor_match in re.finditer(factor_pattern, content):
            factor_count += 1

        # Reset the file pointer and iterate through the matches to extract interactions
        file.seek(0)

    for match in matches:
        interaction_number = int(match.group(1))
        int_values = match.group(2).split()
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

add_row("t_way.txt")
