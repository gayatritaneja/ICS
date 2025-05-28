from typing import List

def dot_product(l1, l2):
    dot_prod = 0

    if len(l1) == len(l2):
        for i in range(len(l1)):
            dot_prod += (l1[i] * l2[i])
    
    else:
        raise ValueError("Vectors must be of euqal length.")
    
    return dot_prod

A = [1, 7]
B = [3, 3]

print(dot_product(A,B))
