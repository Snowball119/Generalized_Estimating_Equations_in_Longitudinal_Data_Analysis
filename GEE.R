### Z1: 1 for DFMO and 0 for Placebo
### Z2: 1 for base>4 and 0 for base<=4

#baseline data
basedata <- read.csv("kbase.csv",header=TRUE)

#cancer data
cancdata <- read.csv("kcanc.csv",header=TRUE)
attach(basedata)
attach(cancdata)
str(basedata)
dim(basedata)
str(cancdata)
dim(cancdata)

base <- basedata$scbase
trtgrp <- basedata$ trtgrp
age<- basedata$age
sex<- basedata$sex

table(trtgrp)

###	m[i]: total number of obs times for experimental subject i
id <- cancdata$randno
length(id)
n <- nrow(basedata) # n different experimental subject
n
m <- numeric(n)
for (i in 1:n)
  m[i] <- sum(id==i) ## count the number of id==i, that is to count how many time ith subject is observed

max_observation_time = max(m)
max_observation_time
###	Z: here is defined by categories(treatment or placebo)
# Z is defined as 2 rows 291 columns with elements 0 
Z <- matrix(0, 2, n)
#Z[1,] <- as.numeric(trtgrp=="DFMO");Z[2,] <- base (trtgrp=="PLACEBO)
#Z[,trtgrp=="DFMO" & base<=4] denote for subject have treatment
#and Total Skin Cancers Reported up to Randomization(scbase) is less or equal to 4

#trtgrp2 <- ifelse((trtgrp == "PLACEBO"),1,0)
#trtgrp2

Z[,trtgrp=="DFMO" & base<2] <- c(1, 0)
Z[,trtgrp=="DFMO" & base>=2] <- c(1, 1)
Z[,trtgrp=="PLACEBO" & base<2] <- c(0, 0)
Z[,trtgrp=="PLACEBO" & base>=2] <- c(0, 1)
Z[1,]
Z[2,]
dim(Z)

#library(DescTools)
#Desc(Z[1,])
#table(Z[1,],Z[2,])
#Desc(table(Z[1,],Z[2,]))

sex <- ifelse(basedata$sex=="Male",1,0)
length(sex)

### T=T2 for obs times on 2-dim count data.
### Note that subject 267 has no count data, therefore removed
### T is a n by nt matrix; 
### T[i,j] gives on the jth oberservation time, how many days from randomization does ith subject has
### note that here the length of study is set to 1

T <- matrix(0, n, max(m))
T
for (i in 1:n)	{
  if (i!=267)
    T[i, 1:m[i]] <- sort(dysfrnd[id==i])
}
dim(T)
T # T[i,j] mean the days from randamization for jth observation time of ith subject

cdata <- cancdata[-267,]
n <- n-1
T2 <- T <- T[-267,]; m <- m[-267]; Z <- Z[,-267];base<- base[-267]; trtgrp <- trtgrp[-267];
sex<-sex[-267]; age<-age[-267]

id <- id[id!=267]
table(id)
treatment = Z[1,]
base2 = Z[2,]  # base2 = 1 stands for base>=4

table(treatment,base)
table(treatment,base2)
table(treatment)

dim(T2)
max(T2[,])/365 
max(dysfrnd)
length(dysfrnd)
max(T2)


#The longest day from randamization is 1879, the trial has already taken more than 5 years on 
T <- T/max(T) 

#Treat the entire timeline as 1
# let the longest days from randamization =1, then to see how long each obearvation takes comparing to 1
T
dim(T)

min(T) #means for the jth obearvation of ith subject, the days of randomizaiton is 0; 
# larger T[i,j] means longer the trial takes
#--------------------------------------------------------------------------------------------
#####	S: collection of all obs times(different level of days from randomization)
### Exclude Randomization days that is 0
S1 = sort(unique(T2[T2!=0]))
length(S1)
table(S1)
obsTime = S1
summary(obsTime)


S=sort(unique(T[T!=0]));
length(S)
min(S)
max(S)

test = sort(unique(cancdata$dysfrnd))
length(test)

###	nt,
# How many unique days from randamization at observation time sorting from trial with shorter days from randomization 
#to relatively longer trials
#nt ??ʾ??ʵ?鿪ʼʱ?ж??ٴι۲죬??ʵ?鿪ʼ????1879????,obs_time ?ĳ?????1159??level??ÿ??level?ǲ?ͬ???????????ߵ?
#????ǰ?????ߵ?level??1879?죬????11??,??Щsubject?й?ͬ??obs time???????Ե?22,80,96,133,148,178??subject???ֱ??????μ?ʵ???ĵ?1483???????˹۲?;
#?ֱ??Ե?1,14,91,131,192,229,269,274?Ŷ??????????μ?ʵ???ĵ?350???????˹۲?
# ???ˣ?obs_timeӦ???ǲ?ͬ????????????????????ͬ??level
nt = length(S);	
nt 




# nt =1159 means that there are 1159 differnt levels(!!!) of days (not days itself) from randamization
#nt measures differnt observation time, rather than the days from randamization
n
# n=290, nt=1159 that is 290 subject were observed 1159 times, among which the longest days is 1879 days??shortest is 11 days
#----------------------------------------------------------------------------------------------------------------------------
###	TT
#Extract the longest trial days for each subject
TT <- apply(T, 1, max) # take the max for each row in matrix T
length(TT)


###	dN[i,j]: whether subject i is observed on time j (j=1,...nt)
#??????ʵ??��?????????ʱ?䣨days from randomization???ﵽ1879??,????11?죬????1159????ͬ??level-->??nt??ʾ
#????ĳ??ʵ??????��˵???䱻?۲?????????????17??max(m)?????۲????????????ˣ??????ǲμ?ʵ??ʱ????ģ?
dN <- matrix(0, n, nt);	
for (j in 1:nt) {
  
  for (i in 1:n) {
    dN[i, j] = (sum(T[i,]== S[j])!=0); # mention that S is just created from T
  }
}
class(dN)
dNtd <- dN
dim(dNtd)
table(dNtd)
#For dNtd[i,j]=1 means ith subject is observed in jth observation time (the jth observation time is meansured by days from randamization)
# dNtd[i,j]=0 means the ith subject was not observed in the j time (a specific days from randamization)



days = sort(unique(cdata$dysfrnd)) # differnt level of days from randamization
length(days)
max(days)
Days <-matrix(0,n,nt)
for (i in 1:n){
      Days[i,] <- days
}
dim(Days)
table(Days)

Year = Days/365
table(Year)
dim(Year)
max(Year)
diffYear <- matrix(0, n, nt);	
for (j in 2:nt) {
  for (i in 1:n) {
    diffYear[i,j] = Year[i,j]-Year[i,j-1]
  }
}
class(diffYear)
table(diffYear)
dim(diffYear)





days1 = sort(unique(S))
length(days1)
Days1 <-matrix(0,n,nt)
for (i in 1:n){
  Days1[i,] <- days1
}
dim(Days1)
table(Days1)






###	inn[i,j]: the additional # of basal cell carcenoma since last observed on time j (j=1,...,nt)
###	inn2[i,j]: the additional # of squamous cell carcenoma since last observed on time j (j=1,...,nt)
n
nt

inn2 <- inn <- matrix(0, n, nt)
idnm <- unique(id)
for (i in 1:n)	{
  for (j in 1:nt)	{
    if (sum(randno==idnm[i] & dysfrnd==(S[j]*max(dysfrnd)))>0)	{
      temp <- which(randno==idnm[i] & dysfrnd==(S[j]*max(dysfrnd)))
      inn[i, j] <- countba[temp]
      inn2[i, j] <- countsq[temp]
    }
  }
}

dim(inn)
dim(inn2)
inn[which(! inn == 0)]



###	N[i,j]: the cumulative # of basal cell carcenoma on time j (j=1,...,nt)
###	N2[i,j]: the cumulative # of squamous cell carcenoma on time j (j=1,...,nt)

N2 <- N <- matrix(0, n,nt);

for (j in 1:nt) {
  for (i in 1:n) {
    N[i,j]=sum(inn[i,1:j]);
    N2[i,j]=sum(inn2[i,1:j]);
  }
}
dim(N)
dim(N2)
BCC=N
SCC=N2
table(BCC)
table(SCC)

### Y[i,j]: the cumulative # of both tumors on time j (j=1,...,nt)
Ytd <- Y <- N+N2
dim(Y)
#S=sort(unique(T[T!=0]));
#nt = length(S);


#write.csv(as.data.frame(inn),file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreased.csv",row.names = F)

#write.csv(as.data.frame(inn2),file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/scIncreased.csv",row.names = F)

#write.csv(as.data.frame(N),file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcCumulative.csv",row.names = F)

#write.csv(as.data.frame(N2),file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/scCumulative.csv",row.names = F)



bcIncreased =read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreased.csv",header = TRUE)
dim(bcIncreased)
bcIncreased1 = cbind.data.frame(treatment,bcIncreased)
dim(bcIncreased1)
dim(bcIncreased1[treatment==1,])
dim(bcIncreased1[treatment==0,])


bcaddedTreatment = bcIncreased1[treatment == 1,2:1160]
dim(bcaddedTreatment)
bcaddedPlacebo = bcIncreased1[treatment == 0,2:1160]
dim(bcaddedPlacebo)
#write.csv(bcaddedTreatment,file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasedwithTreatment.csv",row.names = F)
#write.csv(bcaddedPlacebo,file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasdPlacebo.csv",row.names = F)
bcIncrdTreatment = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasedwithTreatment.csv",header = TRUE)
bcIncrdPlacebo = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasdPlacebo.csv",header = TRUE)




bcCumulative = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcCumulative.csv",header = TRUE)
dim(bcCumulative)
bcCumulative1 = cbind.data.frame(treatment,bcCumulative)
dim(bcCumulative1)


#write.csv(bcCumulative1,file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcCulmulativewithTreatment.csv",row.names = F)
bcCmltTrtgrp = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcCulmulativewithTreatment.csv",header = TRUE)
dim(bcCmltTrtgrp)
bcCmltTreatment = bcCmltTrtgrp[treatment == 1,2:1160]
dim(bcCmltTreatment)
bcCmltPlacebo = bcCmltTrtgrp[treatment == 0,2:1160]
dim(bcCmltPlacebo)

scCumulative = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/scCumulative.csv",header = TRUE)
dim(scCumulative)
scCumulative1 = cbind.data.frame(treatment,scCumulative)
#write.csv(scCumulative1,file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/scCulmulativewithTreatment.csv",row.names = F)
scCmltTrtgrp = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/scCulmulativewithTreatment.csv",header = TRUE)
scCmltTreatment = scCmltTrtgrp[treatment == 1,2:1160]
dim(scCmltTreatment)
scCmltPlacebo = scCmltTrtgrp[treatment == 0,2:1160]
dim(scCmltPlacebo)



####################################
# Plot trajectories of bcc
par(mfrow = c(1,2))
plot(1,type="l",xlim = c(0,max(obsTime)),ylim = c(0,max(bcCmltTrtgrp[,2:1160])),xlab="Observation Days",main="Individual Trajectories_Treatment(n=143)")
for( i in 1:143){
  lines(obsTime, bcCmltTreatment[i,],type = "l",col=4)
}
plot(0,type="l",xlim = c(0,max(obsTime)),ylim = c(0,max(bcCmltTrtgrp[,2:1160])),xlab="Observation Days",main="Placebo Group (n=147)")
for( i in 1:147){
  lines(obsTime, bcCmltPlacebo[i,],col=2)
}


# Plot trajectories of scc
par(mfrow = c(1,2))
plot(1,type="l",xlim = c(0,max(obsTime)),ylim = c(0,max(scCmltTrtgrp[,2:1160])),xlab="Observation Days",main="Individual Trajectories_Treatment(n=143)")
for( i in 1:143){
  lines(obsTime, scCmltTreatment[i,],type = "l",col=4)
}
plot(0,type="l",xlim = c(0,max(obsTime)),ylim = c(0,max(scCmltTrtgrp[,2:1160])),xlab="Observation Days",main="Placebo Group (n=147)")
for( i in 1:147){
  lines(obsTime, scCmltPlacebo[i,],col=2)
}

dev.off()

###################################################################
# Manova for Cumulative number
dim(bcCumulative)

BCC1 = bcCumulative[,1:1159]
length(treatment)

dim(BCC1)
str(BCC1)
bcc5<-bcc4<-bcc3<-bcc2<-bcc1 <-vector(mode = "numeric")
for (i in 1:290)
{
  for (j in 1:232)
  bcc1[i] = sum(BCC1[i,j])
}
bcc1
for (i in 1:290)
{
  for (j in 233:233+232)
    bcc2[i] = sum(BCC1[i,j])
}
bcc2
for(i in 1:290)
{
  for (j in 456:456+232)
    bcc3[i] = sum(BCC1[i,j])
}
bcc3
for (i in 1:290)
{
  for (j in 690:690+232)
    bcc4[i] = sum(BCC1[i,j])
}
bcc4
for(i in 1:290 )
{
  for (j in 923:1159)
    bcc5[i] = sum(BCC1[i,j])
}
bcc5

#BCC_Manova = cbind(treatment,bcc1,bcc2,bcc3,bcc4,bcc5)
BCC_Manova = as.matrix(cbind(bcc1,bcc2,bcc3,bcc4,bcc5))
summary(manova(BCC_Manova ~ treatment))



################################################################################
#Q-Q Plot
dim(BCC_Manova)
BCC_Manova
n = nrow(BCC_Manova)
p = ncol(BCC_Manova)


BCC_Manovabar = colMeans(BCC_Manova)
BCC_Manovabar
S = cov(BCC_Manova)
S

########################################
# Test marginal normality.

par(mfrow = c(2,2))  # A 2 by 2 panel of plots
for(i in 1:3) {
  y = BCC_Manova[,i]
  v=qqnorm(y, ylab = colnames(BCC_Manova)[i])
  text(0, .9*max(v$y), paste("p = ", round(shapiro.test(y)[[2]],3)))
  qqline(y)
}

# Trivariate normality:
dsqd = vector(length = n)
qsqd = vector(length = n)
for(i in 1:n) {
  dsqd[i] = t(BCC_Manova[i,] - BCC_Manovabar) %*% solve(S) %*% (BCC_Manova[i,] - BCC_Manovabar)
  qsqd[i] = qchisq((i-.5)/n, p, lower.tail = TRUE)
}
dsqd = sort(dsqd)
qqplot(qsqd, dsqd, main = "Chisquare Q-Q Plot", xlab = "Chisquare quantiles", ylab = "sample quantiles")
abline(0,1)
text(6, max(qsqd-2), paste("corr = ", round(cor(qsqd,dsqd),3)))


#############3
#SCC  Manova
dim(scCumulative)
SCC1 = scCumulative[,1:1159]
dim(SCC1)
scc5<-scc4<-scc3<-scc2<-scc1 <-vector(mode = "numeric")
for (i in 1:290)
{
  for (j in 1:232)
    scc1[i] = sum(SCC1[i,j])
}
scc1
for (i in 1:290)
{
  for (j in 233:233+232)
    scc2[i] = sum(SCC1[i,j])
}
scc2
for(i in 1:290)
{
  for (j in 456:456+232)
    scc3[i] = sum(SCC1[i,j])
}
scc3
for (i in 1:290)
{
  for (j in 690:690+232)
    scc4[i] = sum(SCC1[i,j])
}
scc4
for(i in 1:290 )
{
  for (j in 923:1159)
    scc5[i] = sum(SCC1[i,j])
}
scc5

SCC_Manova = as.matrix(cbind(scc1,scc2,scc3,scc4,scc5))
summary(manova(SCC_Manova~ treatment))

skinManova = as.matrix(cbind.data.frame(BCC_Manova,SCC_Manova))
summary(manova(skinManova~ treatment))


#Q-Q Plot for SCC
dim(SCC_Manova)
SCC_Manova
n = nrow(SCC_Manova)
p = ncol(SCC_Manova)


SCC_Manovabar = colMeans(SCC_Manova)
SCC_Manovabar
S = cov(SCC_Manova)
S

########################################
# Test marginal normality.

par(mfrow = c(2,2))  # A 2 by 2 panel of plots
for(i in 1:3) {
  y = SCC_Manova[,i]
  v=qqnorm(y, ylab = colnames(SCC_Manova)[i])
  text(0, .9*max(v$y), paste("p = ", round(shapiro.test(y)[[2]],3)))
  qqline(y)
}

# Trivariate normality:
dsqd = vector(length = n)
qsqd = vector(length = n)
for(i in 1:n) {
  dsqd[i] = t(SCC_Manova[i,] - SCC_Manovabar) %*% solve(S) %*% (SCC_Manova[i,] - SCC_Manovabar)
  qsqd[i] = qchisq((i-.5)/n, p, lower.tail = TRUE)
}
dsqd = sort(dsqd)
qqplot(qsqd, dsqd, main = "Chisquare Q-Q Plot", xlab = "Chisquare quantiles", ylab = "sample quantiles")
abline(0,1)
text(6, max(qsqd-2), paste("corr = ", round(cor(qsqd,dsqd),3)))


###################################################################
# Manova for increased number
dim()
bcIncrdTreatment = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasedwithTreatment.csv",header = TRUE)
bcIncrdPlacebo = read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/bcIncreasdPlacebo.csv",header = TRUE)

bccI = bcIncreased1[,2:1160]
str(bcIncreased1)
dim(bcIncreased1)
bccI5<-bccI4<-bccI3<-bccI2<-bccI1 <-vector(mode = "numeric")
for (i in 1:290)
{
  for (j in 1:232)
    bccI1[i] = sum(bccI[i,j])
}
bccI1
for (i in 1:290)
{
  for (j in 233:233+232)
    bccI2[i] = sum(bccI[i,j])
}
bccI2
for(i in 1:290)
{
  for (j in 456:456+232)
    bccI3[i] = sum(bccI[i,j])
}
bccI3
for (i in 1:290)
{
  for (j in 690:690+232)
    bccI4[i] = sum(bccI[i,j])
}
bccI4
for(i in 1:290 )
{
  for (j in 923:1159)
    bccI5[i] = sum(bccI[i,j])
}
bccI5


#bccI_Manova = cbind(treatment,bccI1,bccI2,bccI3,bccI4,bccI5)
bccI_Manova = as.matrix(cbind(bccI1,bccI2,bccI3,bccI4,bccI5))
summary(manova(bccI_Manova ~ treatment))

bccI_bar = colMeans(bccI_Manova)
bccI_bar
S_bccI = cov(bccI_Manova)
S_bccI

# Test Marginal Normality
par(mfrow = c(2,2)) 
for(i in 1:5) {
  y = bccI_Manova[,i]
  v=qqnorm(y, ylab = colnames(bccI_Manova)[i])
  text(0, .9*max(v$y), paste("p = ", round(shapiro.test(y)[[2]],3)))
  qqline(y)
}




bartlett.test(bccI1~treatment)

SCC_Manova = as.matrix(cbind(scc1,scc2,scc3,scc4,scc5))
summary(manova(SCC_Manova~ treatment))

skinManova = as.matrix(cbind.data.frame(bccI_Manova,SCC_Manova))
summary(manova(skinManova~ treatment))


#SCC 
dim(scCmltTrtgrp)
SCC1 = scCmltTrtgrp[,2:1160]
dim(SCC1)
scc5<-scc4<-scc3<-scc2<-scc1 <-vector(mode = "numeric")
for (i in 1:290)
{
  for (j in 1:232)
    scc1[i] = sum(SCC1[i,j])
}
scc1
for (i in 1:290)
{
  for (j in 233:233+232)
    scc2[i] = sum(SCC1[i,j])
}
scc2
for(i in 1:290)
{
  for (j in 456:456+232)
    scc3[i] = sum(SCC1[i,j])
}
scc3
for (i in 1:290)
{
  for (j in 690:690+232)
    scc4[i] = sum(SCC1[i,j])
}
scc4
for(i in 1:290 )
{
  for (j in 923:1159)
    scc5[i] = sum(SCC1[i,j])
}
scc5






######################################################################
dim(bcCmltTreatment)
dim(bcCmltPlacebo)
1159/5
length(treatment[treatment==1])
for (i in 1:232)
{
  bccTreatment_sum1[i] = sum(bcCmltTreatment[,i])
}
bccTreatment_sum1
for (i in 233:233+232)
{
  bccTreatment_sum2 = sum(bcCmltTreatment[,i])
}
bccTreatment_sum2
for(i in 456:456+232)
{
  bccTreatment_sum3 = sum(bcCmltTreatment[,i])
}
bccTreatment_sum3
for(i in 779:779+232)
{
  bccTreatment_sum4 = sum(bcCmltTreatment[,i])
}
bccTreatment_sum4
for(i in 1002:1159)
{
  bccTreatment_sum5 = sum(bcCmltTreatment[,i])
}
bccTreatment_sum5
bccTreatment_sum = c(bccTreatment_sum1,bccTreatment_sum2,bccTreatment_sum3,bccTreatment_sum4,bccTreatment_sum5)

# For Placebo Group
for (i in 1:232)
{
  bccPlacebo_sum1 = sum(bcCmltPlacebo[,i])
}
bccPlacebo_sum1
for (i in 233:233+232)
{
  bccPlacebo_sum2 = sum(bcCmltPlacebo[,i])
}
bccPlacebo_sum2
for(i in 456:456+232)
{
  bccPlacebo_sum3 = sum(bcCmltPlacebo[,i])
}
bccPlacebo_sum3
for(i in 779:779+232)
{
  bccPlacebo_sum4 = sum(bcCmltPlacebo[,i])
}
bccPlacebo_sum4
for(i in 1002:1159)
{
  bccPlacebo_sum5 = sum(bcCmltPlacebo[,i])
}
bccPlacebo_sum5

bccPlacebo_sum = c(bccPlacebo_sum1,bccPlacebo_sum2,bccPlacebo_sum3,bccPlacebo_sum4,bccPlacebo_sum5)




#############################################################################################################
############################################################################################################3
#--------------------------------------------------------------------------------------------------------------------
# Stack outcome variable in one column by 1159 observation time, for each time, Age,sex,treatment,base keep same with ID
length(idnm)
length(id)
length(age)
length(sex)
length(treatment)
length(base)
length(base2)
dim(dNtd)

#install.packages("reshape")
library(reshape)
df_dNtd = cbind.data.frame(id,age,sex,treatment,base,dNtd)
melt_dNtd = melt(df_dNtd,id=(c("idnm","age","sex","treatment","base")))
head(melt_dNtd)
dim(melt_dNtd)
observed = melt_dNtd$value
melt_dNtd$variable


obsdays_level = melt_dNtd$variable
length(obsdays_level)
table(obsdays_level)

df_Days = cbind.data.frame(idnm,age,sex,treatment,base,Days)
melt_Days = melt(df_Days,id=(c("idnm","age","sex","treatment","base")))
head(melt_Days)
dim(melt_Days)
obsDays = melt_Days$value
length(obsDays)

df_Days1 = cbind.data.frame(idnm,age,sex,treatment,base,Days1)
melt_Days1 = melt(df_Days1,id=(c("idnm","age","sex","treatment","base")))
head(melt_Days1)
dim(melt_Days1)
obsDays1 = melt_Days1$value
length(obsDays1)



df_Year = cbind.data.frame(idnm,age,sex,treatment,base,Year)
melt_Days1 = melt(df_Days1,id=(c("idnm","age","sex","treatment","base")))
head(melt_Year)
dim(melt_Year)
obsYear = melt_Year$value
length(obsDays1)







df_Ytd = cbind.data.frame(idnm,age,sex,treatment,base,Ytd)
melt_Ytd <- melt(df_Ytd,id=(c("idnm","age","sex","treatment","base")))
head(melt_Ytd)
dim(melt_Ytd)
melt_Ytd$value
bothcc = melt_Ytd$value

df_N = cbind.data.frame(idnm,age,sex,treatment,base,N)
melt_N <- melt(df_N,id=(c("idnm","age","sex","treatment","base")))
head(melt_N)
dim(melt_N) #1159*290=336,110
bcc = melt_N$value

df_N2 = cbind.data.frame(idnm,age,sex,treatment,base,N2)
melt_N2 <- melt(df_N2,id=(c("idnm","age","sex","treatment","base")))
head(melt_N2)
dim(melt_N2)
scc = melt_N2$value


df_inn = cbind.data.frame(idnm,age,sex,treatment,base,inn)
melt_inn <- melt(df_inn,id=(c("idnm","age","sex","treatment","base")))
head(melt_inn)
dim(melt_inn)
bcadded = melt_inn$value

df_inn2 = cbind.data.frame(idnm,age,sex,treatment,base,inn2)
melt_inn2 <- melt(df_inn2,id=(c("idnm","age","sex","treatment","base")))
head(melt_inn2)
dim(melt_inn2)
scadded = melt_inn2$value


data = cbind.data.frame(obsdays_level,obsDays,obsDays1,melt_Days,observed,bcc,scc, bcadded,scadded,bothcc)
head(data)
data = subset(data,select = -c(variable,value))
head(data)
dim(data)
str(data)

# only keep the datapoint when observed
data1 = data[observed==1,]
dim(data1)

write.csv(data,
          file="D:/001UNC Charlotte/2017Fall/Research Seminar/skinCanc.csv",row.names = F) # with obervatoin that not observed

write.csv(data1,
          file="D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/skinCanc_observed.csv",row.names = F) # only keep observed values


#######################################################################################################
#######################################################################################################
#----------------------------------------------------------------------------------------
skincanc <- read.csv("D:/001UNC Charlotte/2017Fall/Research Seminar/R code and dataset/skinCanc_observed.csv",header=TRUE)
dim(skincanc)
head(skincanc)
attach(skincanc)
str(skincanc)





library(lattice)
x= sort(unique(obsDays))
xyplot(bcc~x,data = skincanc)
xyplot(bcc~x,groups = idnm,data=skincanc)
xyplot(bcc~x,groups = sex,data=skincanc)


library(ggplot2)


#ggplot(skincanc,aes(x =obsDays))+geom_line(aes(y=bcc),colour="blue")+
#  geom_line(aes(y=scc),colour="red")


ggplot(skincanc,aes(x = obsDays,y = bcc,colour=sex))+geom_line()+ylab(label="Cumulative Basal Lesions")+xlab(label="Observation Days")

ggplot(skincanc,aes(x = obsDays,y = scc,colour=treatment))+geom_line()+ylab(label="Cumulative Squamous Lesions")+xlab(label="Observation Days")
dev.off()

#-----------------------------------------------------------------------------------------
# calculate average by gender
totalObsTime = length(unique(obsDays))
totalObsTime
bcc_mean<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcc_mean[i]= mean(bcc[obsdays_level == i])
}
bcc_mean
length(bcc_mean)

plot(sort(unique(obsDays)),bcc_mean, xlab = "Observation Days", ylab = "bcc_mean",
     main = "Mean of Basal Lesions",col =4,type ="l")


bcc_mean_male<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcc_mean_male[i]= mean(bcc[obsdays_level == i] & bcc[sex ==1])
}
table(bcc_mean_male)
length(bcc_mean_male)
plot(sort(unique(obsDays)),bcc_mean_male, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Male",
     main = "Mean Basal Lesions of Male",col =4,type ="l")


bcc_mean_female<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcc_mean_female[i]= mean(bcc[obsdays_level == i] & bcc[sex ==0])
}
table(bcc_mean_female)
length(bcc_mean_female)
plot(sort(unique(obsDays)),bcc_mean_female, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Female",
     main = "Mean Basal Lesions of Female",col ="red",type ="l")


#ggplot(aes(x = sort(unique(obsDays)))) + geom_line(aes(y = bcc_mean_male),
#                                                  colour="blue")+ 
#         geom_line(aes(y = bcc_mean_female,colour="red"))

x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(bcc_mean_male,bcc_mean_female),pch = 20,xlab="Observation time",ylab="Mean of Basal Lesions",
        main="Mean value of Basal Lesions at different observation time")

matplot(x,cbind(bcc_mean,bcc_mean_male,bcc_mean_female),pch = 20,xlab="Observation time",ylab="Mean of Basal Lesions",
        main="Mean value of Basal Lesions at different observation time")



# SCC between gender
scc_mean_male<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scc_mean_male[i]= mean(scc[obsdays_level == i] & scc[sex ==1])
}
table(scc_mean_male)
length(scc_mean_male)
plot(sort(unique(obsDays)),scc_mean_male, xlab = "Observation Days", ylab = "Mean of Squamouse Lesions for Male",
     main = "Mean Squamous Lesions of Male",col =4,type ="l")


scc_mean_female<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scc_mean_female[i]= mean(scc[obsdays_level == i] & scc[sex ==0])
}
table(scc_mean_female)
length(scc_mean_female)
plot(sort(unique(obsDays)),scc_mean_female, xlab = "Observation Days", ylab = "Mean of Squmous Lesions for Female",
     main = "Mean Squamouse Lesions of Female",col ="red",type ="l")


#ggplot(aes(x = sort(unique(obsDays)))) + geom_line(aes(y = bcc_mean_male),
#                                                  colour="blue")+ 
#         geom_line(aes(y = bcc_mean_female,colour="red"))

x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(scc_mean_male,scc_mean_female),pch = ".",xlab="Observation time",ylab="Mean of Basal Lesions",
        main="Mean value of Squamous Lesions at different observation time")

dev.off()
#_________________________________________________________________________________________________________________
#-----------------------------------------------------------------------------------------
#Calculate average by treatment group
totalObsTime = length(unique(obsDays))
totalObsTime
bcc_mean_treatment<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcc_mean_treatment[i]= mean(bcc[obsdays_level == i] & bcc[treatment ==1])
}
table(bcc_mean_treatment)
length(bcc_mean_treatment)
plot(sort(unique(obsDays)),bcc_mean_treatment, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Treatment Group",
     main = "Mean Basal Lesions of treatment",col =4,type ="l")


bcc_mean_placebo<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcc_mean_placebo[i]= mean(bcc[obsdays_level == i] & bcc[treatment ==0])
}
table(bcc_mean_placebo)
length(bcc_mean_placebo)
plot(sort(unique(obsDays)),bcc_mean_placebo, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Placebo Group ",
     main = "Mean Basal Lesions of Placebo",col ="red",type ="l")


x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(bcc_mean_treatment,bcc_mean_placebo),pch =c("+","-"),cex = c(0.5,1),col=c(4,2),xlab="Observation time",ylab="Mean of Basal Lesions",
        main="Mean value of Basal Lesions between Treatment at different observation time")


# scc by treatment group
totalObsTime = length(unique(obsDays))
totalObsTime
scc_mean_treatment<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scc_mean_treatment[i]= mean(scc[obsdays_level == i] & scc[treatment ==1])
}
table(scc_mean_treatment)
length(scc_mean_treatment)
plot(sort(unique(obsDays)),scc_mean_treatment, xlab = "Observation Days", ylab = "Mean of Squamouse Lesions for Treatment Group",
     main = "Mean Squamouse Lesions of treat",col =4,type ="l")


scc_mean_placebo<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scc_mean_placebo[i]= mean(scc[obsdays_level == i] & scc[treatment ==0])
}
table(scc_mean_placebo)
length(scc_mean_placebo)
plot(sort(unique(obsDays)),scc_mean_placebo, xlab = "Observation Days", ylab = "Mean of Squamouse Lesions for Placebo Group ",
     main = "Mean Squamouse Lesions of Placebo",col ="red",type ="l")


x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(scc_mean_treatment,scc_mean_placebo),pch =c("+","-"),cex = c(0.5,1),col=c(4,2),xlab="Observation time",ylab="Mean of Squamouse Lesions",
        main="Mean value of Squamouse Lesions between Treatment at different observation time")



#######################################################
#___              Increased Number --------
#Calculate average by treatment group for increased number
str(skincanc)
totalObsTime = length(unique(obsDays))
totalObsTime
bcadded_mean_treatment<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcadded_mean_treatment[i]= mean(bcadded[obsdays_level == i] & bcadded[treatment ==1])
}
table(bcadded_mean_treatment)
length(bcadded_mean_treatment)
plot(sort(unique(obsDays)),bcadded_mean_treatment, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Treatment Group",
     main = "Mean Basal Lesions of treatment",col =4,type ="l")


bcadded_mean_placebo<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  bcadded_mean_placebo[i]= mean(bcadded[obsdays_level == i] & bcadded[treatment ==0])
}
table(bcadded_mean_placebo)
length(bcadded_mean_placebo)
plot(sort(unique(obsDays)),bcadded_mean_placebo, xlab = "Observation Days", ylab = "Mean of Basal Lesions for Placebo Group ",
     main = "Mean Basal Lesions of Placebo",col ="red",type ="l")


x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(bcadded_mean_treatment,bcadded_mean_placebo),pch =c("+","-"),col=c(4,2),xlab="Observation time",ylab="Mean of Basal Lesions",
        main="Mean value of Basal Lesions between Treatment at different observation time")


# scadded by treatment group
totalObsTime = length(unique(obsDays))
totalObsTime
scadded_mean_treatment<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scadded_mean_treatment[i]= mean(scadded[obsdays_level == i] & scadded[treatment ==1])
}
table(scadded_mean_treatment)
length(scadded_mean_treatment)
plot(sort(unique(obsDays)),scadded_mean_treatment, xlab = "Observation Days", ylab = "Mean of Squamouse Lesions for Treatment Group",
     main = "Mean Squamouse Lesions of treat",col =4,type ="l")


scadded_mean_placebo<- vector(mode="numeric")
for (i in 1:totalObsTime){ 
  scadded_mean_placebo[i]= mean(scadded[obsdays_level == i] & scadded[treatment ==0])
}
table(scadded_mean_placebo)
length(scadded_mean_placebo)
plot(sort(unique(obsDays)),scadded_mean_placebo, xlab = "Observation Days", ylab = "Mean of Squamouse Lesions for Placebo Group ",
     main = "Mean Squamouse Lesions of Placebo",col ="red",type ="l")


x = sort((unique(obsDays)))
length(x)
matplot(x,cbind(scadded_mean_treatment,scadded_mean_placebo),pch =c("+","-"),cex = c(0.5,1),col=c(4,2),xlab="Observation time",ylab="Mean of Squamouse Lesions",
        main="Mean value of Squamouse Lesions between Treatment at different observation time")


############################################################################################################






