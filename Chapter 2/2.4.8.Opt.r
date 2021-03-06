college = read.csv("/home/orange/Desktop/College.csv")
rownames(college) = college[,1]
college = college[,-1]
attach(college)
pdf("/home/orange/Desktop/Col.pdf")
par(mfrow=c(2,2))
plot(Outstate,Room.Board)
plot(Grad.Rate,Room.Board)
plot(Top10perc,(Accept/Apps))
plot(Top10perc,Top25perc)
dev.off()
cor(Outstate,Room.Board)
cor(Grad.Rate,Room.Board)
cor(Top10perc,(Accept/Apps))
cor(Top10perc,Top25perc)
cor(Grad.Rate,(Outstate+Personal+Room.Board+Books))
cor(Outstate,Expend)
cor(Expend, Grad.Rate)
cor(PhD, Grad.Rate)
