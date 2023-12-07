import sys
import re


def format(LA, params, output):
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

    with open(output, "w") as f:
        f.write("v2.0\n")
        f.write(str(len(design)) + "\t" + str(num_params) + "\n")
        for num in param_list:
            f.write(str(num) + "\t")
        f.write("\n")
        for i in range(num_params+1):
            f.write("0\n")
        for row in design:
            for num in row:
                f.write(num + "\t")
            f.write("\n")


for i in range(4):
    s1 = "/home/michael/Desktop/Designs/2_2_2_2_factor/separation/" + str(i) + "_separation.tsv"
    s2 = "/home/michael/Desktop/Designs/2_2_2_2_factor/separation/" + str(i) + "_formatted.tsv"
    # print(s1)
    # print(s2)
    format(s1 , f"/home/michael/Desktop/Designs/2_2_2_2_factor/2_2_2_2_params.tsv", s2)