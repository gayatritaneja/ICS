from turtle import *

t = Turtle()
t.shape("turtle")

t.penup()
t.goto(0, -100)
t.pendown()

t.pencolor('red')
t.pensize('12')
t.speed(10)


t.circle(200)

t.right(90)
t.penup()
t.pendown()
t.left(90)


for i in range(10):
    t.circle(130, 360)
    t.circle(200, 36)



t.getscreen()._root.mainloop()