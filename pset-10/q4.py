from typing import List

def gaussian_elimination(l: List[List[float]]) -> List[List[float]]:
    n = len(l[0]) - 1

    reduced_r = reduce_ref(l, n)
    if reduced_r == None:
        raise ValueError("Invalid matrix.")

    return(answer(reduced_r, n))


def reduce_ref(l: List[List[float]], n: int) -> List[List[float]] | None:
    for pivot_col in range(n):
        max_row = pivot_col
        max_val = abs(l[max_row][pivot_col])

        for i in range(pivot_col + 1, n):
            if (abs(l[i][pivot_col]) > max_val):
                max_row = i
                max_val = l[i][pivot_col]

        if l[pivot_col][max_row] == 0:
            return None 

        if (max_row != pivot_col):
            for a in range(n + 1):
                l[pivot_col][a], l[max_row][a] = l[max_row][a], l[pivot_col][a]

        for i in range(pivot_col + 1, n):

            f = l[i][pivot_col]/l[pivot_col][pivot_col]

            for j in range(pivot_col + 1, n + 1):
                l[i][j] -= l[pivot_col][j] * f

            l[i][pivot_col] = 0
    return l

def answer(l: List[List[float]], n: int) -> List[float]:
    x = [None for _ in range(n)] 

    for i in range(n - 1, -1, -1):

        x[i] = l[i][n]

        for j in range(i + 1, n):
            x[i] -= l[i][j] * x[j]

        x[i] = (x[i]/l[i][i])

    return x


l = [[5.0, -3.0, 1.0, 14.0],
[2.0,  3.0, 3.0, 15.0],
[3.0,  2.0, -4.0, 3.0]]

print(gaussian_elimination(l))
