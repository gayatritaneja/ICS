from typing import List

def flip_vertical(l: List[List[int]]) -> List[List[int]]:
    n = len(l)
    l_new = []

    for i in range(n-1, -1, -1):
        row = []
        for j in range(len(l[0])):
            row.append(l[i][j])
        
        l_new.append(row)

    
    return l_new

A = [
    [1, 2, 3],
    [0, 4, 5],
    [1, 0, 6],
    [2, 3, 5]
]
print(flip_vertical(A))