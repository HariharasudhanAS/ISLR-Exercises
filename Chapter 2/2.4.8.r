x=read.csv(file="/home/orange/Desktop/College.csv")
x
fix(x)
rownames(x)=x[,1]
#fix(x)
x=x[,-1]
#fix(x)
summary(x)
pdf("/home/orange/Desktop/Figur.pdf")
pairs(x[,1:10])
attach(x)
plot(Outstate,Private)
#?rep
Elite=rep("No",nrow(x))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
#?as.factor
college=data.frame(x,Elite)
summary(Elite)
plot(Outstate,Elite)
par(mfrow=c(2,2))
#fix(x)
#?hist()
hist(Apps)
hist(Accept)
#fix(x)
hist(Enroll)
hist(Outstate)
dev.off()