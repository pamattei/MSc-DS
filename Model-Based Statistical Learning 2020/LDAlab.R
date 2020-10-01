# M2 DSAI - Model-based statistical learning course
# Linear discriminant analysis lab

library(pgmm)
library(mvtnorm)


data(wine)
head(wine)

y = wine$Type
y_binary = (y==1)
X = as.matrix(wine[,c(2,4)])


n = dim(X)[1]

# Proportions

pi1 = sum(y==1)/n
pi2 = (sum(y==2)+sum(y==3))/n
p = c(pi1,pi2)
print(p)


# Means

mu1 = colMeans(X[y==1,])
mu2 = colMeans(X[y!=1,])

# Covariance

Sigma = cov(X[y==1,])*pi1 + cov(X[y!=1,])*pi2

# Prediction


pxgiven1 =  dmvnorm(X, mean = mu1, sigma = Sigma) # computes p(x|mu1,Sigma)
pxgiven2 =  dmvnorm(X, mean = mu2, sigma = Sigma) # computes p(x|mu2,Sigma)

pclass1givenx = pxgiven1 * pi1 /(pxgiven1 * pi1 + pxgiven2*pi2)

y_pred = pclass1givenx>0.5

cat("The training classif. error is "); cat(mean(y_binary!=y_pred))

