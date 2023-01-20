######Factor Analysis with MLE
R<-matrix(c(1,0.6386,0.4752,0.3227,0.5520,0.3262,0.3509,0.4008,0.1821,-0.0352,0.6386,1,0.4953,0.5668,0.4706,0.3520,0.3998,0.5167,0.3102,
            0.1012,0.4752,0.4953,1,0.4357,0.2539,0.2812,0.7926,0.4728,0.4682,-0.0120,0.3227,0.5668,0.4357,1,0.3449,0.3503,0.3657,0.6040,0.2344,0.2380,
            0.5520,0.4706,0.2539,0.3449,1,0.1546,0.2100,0.4213,0.2116,0.4125,0.3262,0.3520,0.2812,0.3503,0.1546,1,0.2553,0.4163,0.1712,0.0002,0.3509,
            0.3998,0.7926,0.3657,0.2100,0.2553,1,0.4036,0.4179,0.0109,0.4008,0.5167,0.4728,0.6040,0.4213,0.4163,0.4036,1,0.3151,0.2395,0.1821,0.3102,
            0.4682,0.2344,0.2116,0.1712,0.4179,0.3151,1,0.0983,-0.0352,0.1012,-0.0120,0.2380,0.4125,0.0002,0.0109,0.2395,0.0983,1)
          ,10,10);R
dimnames(R)<-list(c("100m run","Long jump","Shot put","High jump","400m run",
                    "110m hardless","Discus","Pole wault","Javelin","1500m run"),
                  c("100m run","Long jump","Shot put","High jump","400m run",
                    "110m hardless","Discus","Pole wault","Javelin","1500m run"))

sapply(1:4, function(f)
  factanal(covmat=R, factors= f, method="mle",n.obs=280)$PVAL)

factanal(covmat = R, factors = 4,n.obs =280,rotation="none")

out=factanal(covmat = R, factors = 4,n.obs =280)
summary(out)
out$loadings
out$uniquenesses
######Goodness of fit

apply(out$loadings^2,1,sum)# communality
1 - apply(out$loadings^2,1,sum) # uniqueness
Lambda <- out$loadings
Psi <- diag(out$uniquenesses)
Rhat <- Lambda %*% t(Lambda) + Psi
round(R - Rhat, 6)

####Factor analysis with principal component method######
R.eigen <- eigen(R)
D1=diag(c(R.eigen$values[1:4]))
G1<- as.matrix(R.eigen$vectors[,1:4])
Lambda.hat=G1 %*% sqrt(D1)
h2.hat <- rowSums(Lambda.hat^2)
psi.hat=1-h2.hat
##############################################################

install.packages("psych")
library(psych)
library("psych")
PCMfit <- principal(R, nfactors=4, covar=FALSE, rotate="none")
PCMfit <- principal(R, nfactors=4, covar=FALSE, rotate="varimax")

PCMfit ###Drug use by American college students###############################


