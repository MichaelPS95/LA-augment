import sys
import re

def make_data_file(LA, params, output):
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
    
    alphabet_size = 26

    with open(output, "w") as f:
        f.write(str(num_params) + "\n")
        for i in range(num_params):
            letter = chr(ord('A') + (i) % alphabet_size)
            number = i // alphabet_size + 1
            result = f"{letter}{number}"
            line = ""
            line = line + result + "\t" + str(param_list[i]) + "\t0\t"
            for j in range(param_list[i]):
                line += str(j) + "\t"
            if(i == num_params-1):
                f.write(line)
            else:
                f.write(line+"\n")


for i in range(1):
    s1 = "/home/michael/Desktop/Designs/2_2_3_3_two_interactions/separation/unformatted_designs/0_separation.tsv"
    s2 = "/home/michael/Desktop/Designs/2_2_3_3_two_interactions/separation/FD.tsv"
    make_data_file(s1 , f"/home/michael/Desktop/Designs/2_2_3_3_two_interactions/2_2_3_3_params.tsv", s2)
