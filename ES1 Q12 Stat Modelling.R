#ES1 Question 12

#Verifying the equivalence of the F and T tests for stated conditions.

#We first need to generate some data that we can perform the F and T tests on.

x = c(1:50)
norm = rnorm(50)
intercept = 0
slope = 1.0

Y = intercept + slope*x + norm

#This gives us a normal linear model with 2 "unknowns", which we can then perform a regression on.

X = matrix(1, nrow = 50, ncol = 1)
X <- cbind(X,x)

#This creates our matrix X.

beta.mle = solve(t(X) %*% X) %*% t(X) %*% Y

var.mle = sum((Y - X %*% beta.mle)^2) / (50 - 2) #50 is number of rows of X, 2 is number of unknowns.

var.beta.0 = solve(t(X) %*% X)
var.beta.0 <- var.beta.0[1,1]

# Gives us the required entry of the inverse matrix.

#We now calculate our projection matrices.

H = X %*% solve(t(X) %*% X) %*% t(X)
X.0 = matrix(1:50, nrow = 50, ncol = 1)
H.0 = X.0 %*% solve(t(X.0) %*% X.0) %*% t(X.0)

I = diag(50)

# We now use this to calculate values for our F-test:

numerator = sum(((H - H.0) %*% Y)^2)
denominator = sum(((I - H) %*% Y)^2)/(50-2)

f.test = numerator / denominator

t.test.squared = (beta.mle[1])^2 / ((var.beta.0) * var.mle)

# we now have completed the f and t-test (squared) on our model. We return the difference:

difference = f.test - t.test.squared
print(difference)

# Upon running the code once, we obtain 2.355 * 10^(-15), which we can attribute to the small accumulative error 
#of number storage. We verify the tests are the same for this example.

