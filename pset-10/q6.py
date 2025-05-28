from typing import List

def rotate_ccwise(l: List[List[int]]) -> List[List[int]]:
    n = len(l)
    l_new = []

    for i in range(n):
        row = []
        for j in range(len(l[0])-1, -1, -1):
            row.append(l[i][j])
        
        l_new.append(row)
    
    return transpose(l_new)

def transpose(l: List[List[int]]) -> List[List[int]]:
    n = len(l)
    m = len(l[0])
    l_new = []

    for i in range(m):
        row = []
        for j in range(n):
            row.append(l[j][i])
        l_new.append(row)
    
    return l_new


A = [
    [1, 2, 3],
    [0, 4, 5],
    [1, 0, 6],
    [2, 3, 5]
]
print(rotate_ccwise(A))