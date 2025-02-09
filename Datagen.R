id <- 1:1600
x <- rep(0:39,each=40)
y <- rep(0:39,times=40)
stand <- (1+x%/%20) +2*(y%/%20)
p95 <- rnorm(1600,50,10+5*stand)
volume <- rnorm(1600,p95+0.05*p95^2+100*stand+50,60)
example1 <- data.frame(id=id,x=x,y=y,stand=stand,volume=volume, p95=p95)
write.csv(example1,"example1.csv",row.names=FALSE)
write.csv(example1[,-5],"example2.csv",row.names=FALSE)


# Example 3
set.seed(1234)
id <- 1:1600
x <- rep(0:39,each=40)
y <- rep(0:39,times=40)
stand <- (1+x%/%10) +4*(y%/%10)
unique(stand)
stand_effect <- rnorm(16,0,150)
p95 <- rnorm(1600,180,20)
summary(p95)
volume <- rnorm(1600,5+15*p95+stand_effect[stand],50)
summary(volume)
example3 <- data.frame(id=id,x=x,y=y,stand=stand,volume=volume, p95=p95)
example3$stand <- factor(example3$stand)
ggplot(example3,aes(x=x,y=y,fill=stand)) + geom_tile()
ggplot(example3,aes(x=x,y=y,fill=volume)) + geom_tile()
ggplot(example3,aes(x=x,y=y,fill=p95)) + geom_tile()
plot(example3$p95,example3$volume,col=rgb(0.05+stand/20,0.05+stand/20,0.05+stand/20,maxColorValue = 1),pch=20)


example3$volume <- ifelse(example3$id%in%sample(1600,50),example3$volume,NA)
plot(example3$p95,example3$volume,col=example3$stand,pch=20)

write.csv(example3,"example3.csv",row.names=FALSE)


