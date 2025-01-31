from turtle import *

t = Turtle()
t.speed(10) 

def koch(depth, length):
    if depth == 0: 
        t.forward(length)
    else:
        length /= 3
        for i in (60, -120, 60, 0):
            koch(depth - 1, length)
            t.left(i)


for i in range(3): 
    koch(5, 300) #taking depth = 5, length = 300
    t.right(120)

t.getscreen()._root.mainloop()