from typing import List

def inverse_matrix(l: List[List[float]]) -> List[List[float]]:
    d = det(l)
    if d == 0:
        raise ValueError("Inverse is not defined for singular matrices")
    else:
        cofactor_matrix = cof(l)
        transpose = tp(cofactor_matrix)
        ans = []

        for i in range(len(l)):
            ans_row = []
            for j in range(len(l)):
                ans_row.append(transpose[i][j]/d)
            
            ans.append(ans_row)
    
    return ans



def det(l: List[List[float]]) -> float:
    n = len(l)
    if n == 1:
        return l[0][0]
    elif n == 2:
        return (l[0][0] * l[1][1]) - (l[0][1] * l[1][0])
    
    else:
        ans = 0

        for i in range(n):
            temp_mat = []

            for j in range(1, n):
                row = []
                for k in range(n):
                    if k!= i:
                        row.append(l[j][k])

                temp_mat.append(row)
            
            ans += ((-1) ** i) * (l[0][i]) * det(temp_mat) 

    return ans


def cof(l: List[List[float]]) -> List[List[float]]:
    n = len(l)
    ans = []

    for i in range(n):
        row = []
        for j in range(n):
            temp_mat = []

            for r in range(n):
                if r != i:
                    sub_row = []

                    for c in range(n):
                        if c != j:
                            sub_row.append(l[r][c])

                    temp_mat.append(sub_row)
            
            row.append(((-1) ** (i + j)) * det(temp_mat))

        ans.append(row)

    return ans


def tp(l: List[List[float]]) -> List[List[float]]:
    n = len(l)

    for i in range(n):
        for j in range(i+1, n):
            l[i][j], l[j][i] = l[j][i], l[i][j]
    
    return l



A = [
    [1, 2, 3],
    [0, 4, 5],
    [1, 0, 6]
]

print(inverse_matrix(A))