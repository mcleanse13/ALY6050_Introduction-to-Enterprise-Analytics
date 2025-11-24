#ALY 6050 Module 5 Project

library(lpSolve)

Names <- c("x1", "x2", "x3", "x4")

# Objective function coefficients
C <- c(499.99, 729.99, 700.99, 269.99) - c(330, 370, 410, 127)
Objective <- C

#Constraints
Const1 <- c(169.99, 359.99, 290.99, 142.99)
Const2 <- c(5, 8, 0, 0)
Const3 <- c(0, 0, 0, 142.99)
Const4 <- c(25, 40, 25, (25/4))

# Direction of constraints (1 for <=, -1 for >=)
Directions <- c("<=", ">=", ">=", "<=")

# Right-hand side of constraints
b <- c(170000, 289.19, 0, 12300)

Constraints <- matrix(c(Const1, Const2, Const3, Const4, ncol=4, byrow = TRUE))

# Solve the linear programming problem
LP <- lp("max", Objective, Constraints, Directions, b, compute.sens = TRUE)

DF1 <- data.frame("Product"= Names, "Optimal Decisions"=round(LP$solution,0))
DF1

LP$solution
LP$objval
LP$duals.from
LP$duals.to
LP$sens.coef.from
LP$sens.coef.to
LP$constraints

summary(LP)
