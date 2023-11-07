import re

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
    num = 0

    # Iterate through the matches
    for match in matches:
        count += 1
        rows = [int(row) for row in match.group(3).split()]

        # Check if the 'rows' list contains only one element
        if len(rows) < t:
            num += 1


    # Print the extracted data
    # for interaction in interactions:
    #     print(f"Interaction {interaction['interaction_number']}:")
    #     print(f"Int: {interaction['int_values']}")
    #     print(f"Rows: {interaction['rows']}\n")
    return (count-num)/count

print(get_score("t_way.txt"))
