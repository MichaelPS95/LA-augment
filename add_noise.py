import random
import sys
import re

def add_noise(responses):

    with open(responses, "r") as f:
        resp = []
        num_responses = int(f.readline().strip())
        header = f.readline().strip()
        for i in range(num_responses):
            l = round(float(f.readline().strip()) + (random.choice([-1, 1])) * round(random.uniform(0, 1), 4),4)
            resp.append(l)
    with open(responses, "w") as f:
        f.write(str(num_responses) + "\n")
        f.write(header + "\n")
        for i in range(num_responses):
            if i == num_responses-1:
                f.write(str(resp[i]))
            else:
                f.write(str(resp[i]) + "\n")


for i in range(3):
    st = "/home/michael/Desktop/Designs/2_2_2_2_interaction/optimal/" +str(i) + "_response.tsv"
    add_noise(st)