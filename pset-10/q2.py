from typing import List

def matrix_product(l1, l2):
    ans_matrix = []

    if len(l2) != len(l1[0]):
        raise ValueError("Multiplication is not defined for the given matrix.")
    
    for i in range(len(l1)):
        row = []
        for j in range(len(l2[0])):
            ans = 0
            for k in range(len(l2)):
                ans += (l1[i][k] * l2[k][j])
            row.append(ans)
        ans_matrix.append(row)

    return ans_matrix

A = [[1, 2],
     [3, 4]]

B = [[5, 6],
     [7, 8]]

print(matrix_product(A, B))