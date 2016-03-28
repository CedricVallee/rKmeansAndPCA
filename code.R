#A. Generate simulated data
set.seed(10)
x1 <- matrix(rnorm(20 * 50, mean = 0, sd = 5), ncol = 50)
x2 <- matrix(rnorm(20 * 50, mean = 5, sd = 5), ncol = 50)
x3 <- matrix(rnorm(20 * 50, mean = -5, sd = 5), ncol = 50)
m=rbind(x1,x2,x3)

#B. Perform PCA on the 60 observations
pc.out = prcomp(m,scale=TRUE)
biplot(pc.out,scale=0,main="Principal component scores")

#C. Perform K-means clustering with k=3
k=3
km.out = kmeans(m, k, nstart = 20)
km.out$cluster
lab <- c(rep('A', 20), rep('B', 20), rep('C', 20))
table(lab,km.out$cluster)

#D. Perform K-means clustering with k=2
k=2
km.out = kmeans(m, k, nstart = 20)
km.out$cluster
lab <- c(rep('A', 20), rep('B', 20), rep('C', 20))
table(lab,km.out$cluster)

#E. Perform K-means clustering with k=4
k=4
km.out = kmeans(m, k, nstart = 20)
km.out$cluster
lab <- c(rep('A', 20), rep('B', 20), rep('C', 20))
table(lab,km.out$cluster)

#F. Perform K-means clustering with k=3 on the first two PC scores
k=3
km.out = kmeans(pc.out$x[,1:2], k, nstart = 20)
km.out$cluster
lab <- c(rep('A', 20), rep('B', 20), rep('C', 20))
table(lab,km.out$cluster)

#G. Perform K-means clustering with k=3 after scaling each variables
mnew = scale(m)
k=3
km.out = kmeans(m, k, nstart = 20)
km.out$cluster
lab <- c(rep('A', 20), rep('B', 20), rep('C', 20))
table(lab,km.out$cluster)