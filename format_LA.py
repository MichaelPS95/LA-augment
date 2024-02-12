import sys
import re
import os


def format(LA, params, base_len=0, num_added=0):
    design = []
    with open(LA,"r") as f:
        for line in f:
            l = line.strip().split()
            design.append(l)
    # print(design)
    num_params = 0
    param_list = []
    with open(params, "r") as f:
        num_params = int(f.readline().strip())
        line = f.readline().strip().split()
        for num in line:
            param_list.append(int(num))
        # print(num_params)
        # print(param_list)
    
    for i in range(num_added+1):
        directory, file = os.path.split(LA)
        path = directory + '/' + str(i) + '_' + file
        # print(path)
        with open(path, "w") as f:
            f.write("v2.0\n")
            f.write(str(base_len+i) + "\t" + str(num_params) + "\n")
            for num in param_list:
                f.write(str(num) + "\t")
            f.write("\n")
            for j in range(num_params+1):
                f.write("0\n")
            for j in range(base_len+i):
                for num in design[j]:
                    f.write(num + "\t")
                f.write("\n")

format("/home/michael/Desktop/correct_designs/2233/piecewise/separated/2233.tsv", "/home/michael/Desktop/correct_designs/2233/params.tsv",9,13)