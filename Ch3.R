##########blood 66###########
R=matrix(c(1.000, 0.290, 0.202, -0.055, -0.105, -0.252, -0.229, 0.058, 0.290, 1.000, 0.415, 
0.285, -0.376, -0.349, -0.164, -0.129, 0.202, 0.415, 1.000, 0.419, -0.521, -0.441, -0.145, 
-0.076, -0.055, 0.285, 0.419, 1.000, -0.877, -0.076, 0.023, -0.131, -0.105, -0.376, -0.521, -0.877, 
1.000, 0.206, 0.034, 0.151, -0.252, -0.349, -0.441, -0.076, 0.206, 1.000, 0.192, 0.077, -0.229,
-0.164, -0.145, 0.023, 0.034, 0.192, 1.000, 0.423, 0.058, -0.129, -0.076, -0.131, 0.151, 0.077, 0.423, 1.000),8)
R
S=diag(c(0.371, 41.253, 1.935, 0.077, 0.071, 4.037, 2.732, 0.297))
S
blood_cov=S%*%R%*%S
blood_cov
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loadings = TRUE)
blood_pcacor <- princomp(covmat = R)
summary(blood_pcacor, loadings = TRUE)


############73
plot(blood_pcacor$sdev^2, xlab = "Component number",
ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(blood_pcacor$sdev^2), xlab = "Component number",
ylab = "log(Component variance)", type="l",
main = "Log(eigenvalue) diagram")


#################headsize74##########################################################################
headsize=matrix(c(191,195,181,183,176,208,189,197,188,192,179,183,174,190,188,163,195,186,181,175,192,174,176,197,190,155,149,148,153,144,157,150,159,152,150,158,147,150,159,151,137,155,153,145,140,154,143,139,167,163,179,201,185,188,171,192,190,189,197,187,186,174,185,195,187,161,183,173,182,165,185,178,176,200,187,145,152,149,149,142,152,149,152,159,151,148,147,152,157,158,130,158,148,146,137,152,147,143,158,150),25,4)
colnames(headsize)=c("head1","breadth1","head2","breadth2")
headsize
head_dat<-headsize[, c("head1", "head2")]
colMeans(head_dat)
cov(head_dat)
head_pca <- princomp(x = head_dat)
head_pca
print(summary(head_pca), loadings= TRUE)
###############77
a1<-183.84-0.721*185.72/0.693
b1<-0.721/0.693
a2<-183.84-(-0.693*185.72/0.721)
b2<--0.693/0.721
#y1=0.693*head_dat[,1]+0.721*head_dat[,2]
#y2=-0.721*head_dat[,1]+0.693*head_dat[,2]
#layout(matrix(1:2, nc = 2))
#plot(y1,y2)
plot(head_dat, xlab="First son's head length(mm)",ylab = "Second son's head length")
abline(a1, b1)
abline(a2, b2, lty=2)
#################78
xlim<-range(head_pca$scores[,1])
plot(head_pca$scores, xlim = xlim, ylim = xlim)
####olympic hepthathlon results 79##################
heptathlon =  data.frame(hurdles=c(12.69,12.85,13.20,13.61,13.51,13.75,13.38,13.55,13.63,13.25,13.75,13.24,13.85,13.71,13.79,13.93,13.47,14.07,14.39,14.04,14.31,14.23,14.85,14.53,16.42),
highjump=c(1.86,1.80,1.83,1.80,1.74,1.83,1.80,1.80,1.83,1.77,1.86,1.80,1.86,1.83,1.80,1.86,1.80,1.83,1.71,1.77,1.77,1.71,1.68,1.71,1.50),
shot=c(15.80,16.23,14.20,15.23,14.76,13.50,12.88,14.13,14.28,12.62,13.01,12.88,11.58,13.16,12.32,14.21,12.75,12.69,12.68,11.81,11.66,12.95,10.00,10.83,11.78),
run200m=c(22.56,23.65,23.10,23.92,23.93,24.65,23.59,24.48,24.86,23.59,25.03,23.59,24.87,24.78,24.61,25.00,25.47,24.83,24.92,25.61,25.69,25.50,25.23,26.61,26.16),
longjump=c(7.27,6.71,6.68,6.25,6.32,6.33,6.37,6.47,6.11,6.28,6.34,6.37,6.05,6.12,6.08,6.40,6.34,6.13,6.10,5.99,5.75,5.50,5.47,5.50,4.88),
javelin=c(45.66,42.56,44.54,42.78,47.46,42.82,40.28,38.00,42.20,39.06,37.86,40.28,47.50,44.58,45.44,38.60,35.76,44.34,37.76,35.68,39.48,39.64,39.14,39.26,46.38),
run800m=c(128.51,126.12,124.20,132.24,127.90,125.79,132.54,133.65,136.05,134.74,131.49,132.54,134.93,142.82,137.06,146.67,138.48,146.43,138.02,133.90,133.35,144.02,137.30,139.17,163.43),
score=c(7291,6897,6858,6540,6540,6411,6351,6297,6252,6252,6205,6171,6137,6109,6101,6087,5975,5972,5746,5734,5686,5508,5290,5289,4566))
heptathlon
competitors = c("Joyner-Kersee(USA)","John(GDR)","Behmer(GDR)","Sablovskaite(URS)","Choubenkova(URS)","Schulz(GDR)","Fleming(AUS)","Greiner(USA)",
"Lajbnerova(CZE)","Bouraga(URS)","Wijnsma(HOL)","Dimitrova(BUL)","Scheider(SWI)","Braun(FRG)","Ruotsalainen(FIN)","Yuping(CHN)","Hagger(GB)",
"Brown(USA)","Mulliner(GB)","Hautenauve(BEL)","Kytola(FIN)","Geremias(BRA)","Hui-Ing(TAI)","Jeong-Mi(KOR)","Launa(PNG)")
variables = c("hurdles","highjump","shot","run200m","longjump","javelin","run800m","score")
colnames(heptathlon) = variables 
rownames(heptathlon) = competitors
heptathlon
heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)
cor(heptathlon)
score <-which(colnames(heptathlon) == "score")
pairs(heptathlon,pch=".", cex=1.5)
heptathlon <-heptathlon[-grep("PNG",rownames(heptathlon)),]
heptathlon1<-heptathlon[-grep("PNG",rownames(heptathlon)),]
heptathlon1
score <-which(colnames(heptathlon)=="score")
round(cor(heptathlon[,-score]),2)
heptathlon_pca <-prcomp(heptathlon[,-score],scale=TRUE)
print(heptathlon_pca)
plot(heptathlon[,-score],pch =".",cex = 1.5)
summary(heptathlon_pca)
a1<-heptathlon_pca$rotation[,1]
a1
center<-heptathlon_pca$center;colMeans(heptathlon)
scale<-heptathlon_pca$scale
hm<-as.matrix(heptathlon[,-score])
drop(scale(hm,center=center,scale=scale)%*%
heptathlon_pca$rotation[,1])
predict(heptathlon_pca)[,1]
plot(heptathlon_pca)
cor(heptathlon$score, heptathlon_pca$x[,1])
plot(heptathlon$score, heptathlon_pca$x[,1])
##########ХЭНе 94##############
biplot(heptathlon_pca, col = c("gray","black"))



##########air pollution 86#################################################################################################################################
USairpollution=matrix(c(46,11,24,47,11,31,110,23,65,26,9,17,17,35,
56,10,28,14,14,13,30,10,10,16,29,18,9,31,14,69,10,61,94,26,28,12,
29,56,29,8,36,47.6,56.8,61.5,55.0,47.1,55.2,50.6,54.0,49.7,51.5,
66.2,51.9,49.0,49.9,49.1,68.9,52.3,68.4,54.5,61.0,55.6,61.6,75.5,
45.7,43.5,59.4,68.3,59.3,51.5,54.6,70.3,50.4,50.0,57.8,51.0,56.7,
51.1,55.9,57.3,56.6,54.0,44,46,368,652,391,35,3344,462,1007,266,
641,454,104,1064,412,721,361,136,381,91,291,337,207,569,699,275,
204,96,181,1692,213,347,343,197,137,453,379,775,434,125,80,116,
244,497,905,463,71,3369,453,751,540,844,515,201,1513,158,1233
,746,529,507,132,593,624,335,717,744,448,361,308,347,1950,582,
520,179,299,176,716,531,622,757,277,80,8.8,8.9,9.1,9.6,12.4,6.5,
10.4,7.1,10.9,8.6,10.9,9.0,11.2,10.1,9.0,10.8,9.7,8.8,10.0,8.2,
8.3,9.2,9.0,11.8,10.6,7.9,8.4,10.6,10.9,9.6,6.0,9.4,10.6,7.6,8.7,
8.7,9.4,9.5,9.3,12.7,9.0,33.36,7.77,48.34,41.31,36.11,40.75,34.44,
39.04,34.99,37.01,35.94,12.95,30.85,30.96,43.37,48.19,38.74,54.47,
37.00,48.52,43.11,49.10,59.80,29.07,25.94,46.00,56.77,44.68,30.18,
39.93,7.05,36.22,42.75,42.59,15.17,20.66,38.79,35.89,38.89,30.58,
40.25,135,58,115,111,166,148,122,132,155,134,78,86,103,129,127,103,
121,116,99,100,123,105,128,123,137,119,113,116,98,115,36,147,125,115,
89,67,164,105,111,82,114),41)
cities = c("Albany", "Albuquerque","Atlanta","Baltimore","Buffalo",
"Charleston","Chicago", "Cincinnati","Cleveland","Columbus","Dallas",
"Denver","DesMoines","Detroit","Hartford","Houston","Indianapolis",
"Jacksonville","Kansas City","Little Rock","Louisville","Memphis",
"Miami", "Milwaukee","Minneapolis","Nashville","New Orleans","Norfolk",
"Omaha","Philadelphia", "Phoenix","Pittsburgh","Providence","Richmond",
"Salt Lake City","San Francisco","Seattle","St. Louis","Washington","Wichita","Wilmington")
variables = c("SO2","temp","manu","popul","wind", "precip","predays")
colnames(USairpollution) = variables
rownames(USairpollution)= cities
USairpollution
cor(USairpollution[,-1])
usair_pca <- princomp(USairpollution[,-1], cor = TRUE)
data("USairpollution", package = "HSAUR2")
panel.hist <- function(x, ...) {
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="grey", ...)
}
USairpollution$negtemp <- USairpollution$temp * (-1)
USairpollution$temp <- NULL
pairs(USairpollution[,-1], diag.panel = panel.hist,
pch = ".", cex = 1.5)
summary(usair_pca, loadings = TRUE)
library(MVA)
pairs(usair_pca$scores[,1:3], ylim = c(-6, 4), xlim = c(-6, 4),
 panel = function(x,y,...) {
 text(x, y, abbreviate(row.names(USairpollution)),
 cex = 0.6)
bvbox(cbind(x,y),add=TRUE)
})
par(mfrow=c(3,2))
out <- sapply(1:6, function(i) {
 plot(USairpollution$SO2,usair_pca$scores[,i],
 xlab = paste("PC",i, sep = ""),
 ylab = "Sulphur dioxide concentration")
})

x=usair_pca$score
SO2=USairpollution[,1]
usair_reg <- lm(SO2 ~x[,c(1,4,5,6)])
summary(usair_reg)
###################### head measurements 96############################
headsize.std <- sweep(headsize,  2,
 apply(headsize,  2,  sd),  FUN  =  "/")
headsize.std
R <-  cor(headsize.std)
  r11  <-  R[1:2,  1:2]
  r22  <-  R[-(1:2),  -(1:2)]
  r12  <-  R[1:2,  -(1:2)]
  r21  <-  R[-(1:2),  1:2]
  (E1  <-  solve(r11)  %*%  r12  %*%  solve(r22)  %*%r21)
(E2  <-  solve(r22)  %*%  r21  %*%  solve(r11)  %*%r12)
(e1  <-  eigen(E1))
(e2  <-  eigen(E2))
girth1 <- headsize.std[,1:2] %*% e1$vectors[,1]
girth2 <- headsize.std[,3:4] %*% e2$vectors[,1]
shape1 <- headsize.std[,1:2] %*% e1$vectors[,2]
shape2 <- headsize.std[,3:4] %*% e2$vectors[,2]
(g <- cor(girth1, girth2))
(s <- cor(shape1, shape2))
plot(girth1, girth2)
plot(shape1, shape2)
%%%%%%%%%%%%health and personality 99%%%%%%%%%
LAdepr=matrix(c(1,0.212,0.124,-0.164,-0.101,-0.158,0.212,1,0.098,0.308,-0.207,-0.183,0.124,0.098,1,0.044,-0.106,-0.180,-0.164,0.308,0.044,1,-0.208,-0.192,-0.101,-0.207,-0.106,-0.208,1,0.492,-0.158,-0.183,-0.180,-0.192,0.492,1),6,6)
LAdepr
colnames(LAdepr)=c("CESD","Health","Gender","Age","Edu","Income")
LAdepr
r11  <-  LAdepr[1:2,  1:2]
  r22  <-  LAdepr[-(1:2),  -(1:2)]
  r12  <-  LAdepr[1:2,  -(1:2)] 
  r21  <- LAdepr[-(1:2),  1:2]
 (E1  <-  solve(r11)  %*%  r12  %*%  solve(r22)  %*%r21)
(E2  <-  solve(r22)  %*%  r21  %*%  solve(r11)  %*%r12)
(e1  <-  eigen(E1))
(e2  <-  eigen(-E2))