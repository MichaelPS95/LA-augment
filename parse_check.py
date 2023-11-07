import re

# Define a pattern for extracting interactions and rows
pattern = r'Interaction (\d+):\n\s+Int: { ([^}]+) }\n\s+Rows: { ([\d\s]+) }'

# Read the input file
# Going to output t_way from the check call, this name should be fixed
with open('t_way.txt', 'r') as file:
    content = file.read()

# Use regular expressions to find all interactions and their details
matches = re.finditer(pattern, content)

# Create a list to store the extracted data
interactions = []

# Iterate through the matches
for match in matches:
    interaction_number = int(match.group(1))
    int_values = match.group(2).split()
    rows = [int(row) for row in match.group(3).split()]

    # Check if the 'rows' list contains only one element
    # Need to only look at rows without 2-way coverage 
    # might come back to this for t-way upon further instruction
    if len(rows) == 1:
        interaction_data = {
            'interaction_number': interaction_number,
            'int_values': int_values,
            'rows': rows
        }

        interactions.append(interaction_data)

# Print the extracted data
# for interaction in interactions:
#     print(f"Interaction {interaction['interaction_number']}:")
#     print(f"Int: {interaction['int_values']}")
#     print(f"Rows: {interaction['rows']}\n")
