import sys
import re
import os


def create_responses(responses, base_len, num_added):
    with open(responses, "r") as f:
        resp = []
        num_responses = int(f.readline().strip())
        header = f.readline().strip()
        for i in range(num_responses):
            l = f.readline().strip()
            resp.append(l)
        directory, file = os.path.split(responses)
        for i in range(num_added+1):
            path = directory + '/' + str(i) + '_' + file
            with open(path, 'w') as f:
                f.write(str(base_len+i) + '\n')
                f.write(header + '\n')
                for j in range(base_len+i):
                    f.write(str(resp[j]) + '\n')
                    
create_responses("/home/michael/Desktop/correct_designs/2233/piecewise/separated/responses.tsv", 9, 13)