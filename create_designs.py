import sys
import re
import os


def create_designs(LA="",base_len=0, num_added=0):
    design = []
    with open(LA,"r") as f:
        for line in f:
            l = line.strip().split()
            design.append(l)
    # len = len(design)

    # print(design)

    directory, file = os.path.split(LA)

    # print(directory)
    # print(file)

    for i in range(num_added+1):
        path = directory + '/' + str(i) + '_' + file
        with open(path, 'w') as f:
            for j in range(base_len+i):
                line = ""
                for k in range(len(design[j])):
                    line += str(design[j][k]) + "\t"
                f.write(line + '\n')



create_designs("/home/michael/Desktop/correct_designs/2233/piecewise/separated/2233.tsv", 9, 13)