###Expectations of life###########

life=matrix(c(63,34,38,59,56,62,50,65,56,69,65,64,56,60,61,49,59,63,59,
              65,65,64,64,67,61,68,67,65,59,58,57,51,29,30,42,38,44,39,44,46,47,48,50,
              44,44,45,40,42,44,44,48,48,63,43,45,40,46,45,46,43,44,46,30,13,17,20,18,
              24,20,22,24,24,26,28,25,22,22,22,22,23,24,28,26,21,21,23,21,23,23,24,23,
              24,28,13,5,7,6,7,7,7,7,11,8,9,11,10,6,8,9,6,8,8,14,9,7,6,8,10,8,8,9,10,9,9,
              67,38,38,64,62,69,55,72,63,75,68,66,61,65,65,51,61,67,63,68,67,68,68,74,67,
              75,74,71,66,62,60,54,32,34,46,46,50,43,50,54,53,50,51,48,45,49,41,43,48,46,
              51,49,47,47,51,46,52,51,51,49,47,49,34,17,20,25,25,28,23,27,33,29,27,29,27,25,
              27,23,22,26,25,29,27,25,24,28,25,29,28,28,27,25,28,15,6,7,8,10,14,8,9,19,10,
              10,11,12,9,10,8,7,9,8,13,10,9,8,10,11,10,10,10,12,10,11),31)
gender = c("m0","m25","m50","m75","w0","w25","w50","w75")
count = c("Algeria","cameroon","madagascar","mauritius","reunion","seychelles",
          "south africa(C)","south africa(W)","tunisia","canada","costa rica",
          "dominican rep","el salvador","greenland","grenada","guatemala","honduras",
          "jamaica","mexico","nicarague","panama","trinidad(62)","trinidad(67)",
          "united states(66)","united states(NW66)","united states(W66)",
          "united states(67)","arjentina","chile","colombia","ecuador")
colnames(life)= gender
rownames(life)= count

sapply(1:3, function(f)
  factanal(life, factors= f, method="mle")$PVAL)

fa.life=factanal(life, factors= 3, method="mle", rotation="none")
summary(fa.life)
fa.life=factanal(life, factors= 3, method="mle")
fa.life
(scores <- factanal(life, factors= 3, mrthod= "mle",
                    scores= "regression")$scores)
x<-scores[,1]
y<-scores[,2]
plot(x, y, xlab = "factor1", ylab = "factor2", xlim=c(-2,3), ylim=c(-2,3),
     type = "n")
text(x, y, labels = count, cex = 0.5)

factanal(life, factors = 3, rotation = "none")
(scores <- factanal(life, factors= 3,rotation = "promax",
                    mrthod= "mle",scores= "regression")$scores)
x2<-scores[,1]
y2<-scores[,2]
plot(x2, y2, xlab = "factor1", ylab = "factor2", xlim=c(-2,3), ylim=c(-2,3),
     type = "n")
text(x, y, labels = count, cex = 0.5)

######Goodness of fit

apply(fa.life$loadings^2,1,sum)# communality
1 - apply(fa.life$loadings^2,1,sum) # uniqueness
Lambda <- fa.life$loadings
Psi <- diag(fa.life$uniquenesses)
S <- fa.life$correlation;S;cor(life)
Shat <- Lambda %*% t(Lambda) + Psi
round(S - Shat, 6)

###Rotation
life.fa.none <- factanal(life, factors = 3, rotation = "none")
life.fa.varimax <- factanal(life, factors = 3, rotation = "varimax")
life.fa.promax <- factanal(life, factors = 3, rotation = "promax")

par(mfrow = c(1,3))
plot(life.fa.none$loadings[,1], 
     life.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")
abline(h = 0, v = 0)
text(life.fa.varimax$loadings[,1]-0.08, 
     life.fa.varimax$loadings[,2]+0.08,
     colnames(life),
     col="blue")

plot(life.fa.varimax$loadings[,1], 
     life.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

text(life.fa.varimax$loadings[,1]-0.08, 
     life.fa.varimax$loadings[,2]+0.08,
     colnames(life),
     col="blue")
abline(h = 0, v = 0)

plot(life.fa.promax$loadings[,1], 
     life.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")
abline(h = 0, v = 0)
text(life.fa.varimax$loadings[,1]-0.08, 
     life.fa.varimax$loadings[,2]+0.08,
     colnames(life),
     col="blue")

####Factor analysis with principal component method######
S=cor(life)
S.eigen <- eigen(S)
D1=diag(c(S.eigen$values[1:3]))
G1<- as.matrix(S.eigen$vectors[,1:3])
Lambda.hat=G1 %*% sqrt(D1)
h2.hat <- rowSums(Lambda.hat^2)
psi.hat=1-h2.hat
##############################################################
