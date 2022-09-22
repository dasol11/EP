# -*- coding: utf-8 -*-


#normalize <- function(x) {
 # return((x-min(x))/(max(x)-min(x)))
#}

df=read.csv("node_cluster.csv")[,-1]
dim(df)
df
#add=runif(300)
#df$X.latitude.=df$X.latitude.+add
#df$Y.longitude.=df$Y.longitude.+add


df1=df[df$cluster==3,]

se=df1[df1$Who=='Seller',1]
bu=df1[df1$Who=='Buyer',1]
b=c()
for(i in 1:length(bu)){
  b=rbind(b,c(bu[i]-se))
}
b
# 플러스면 전기 더사야됨
# 마이너스면 전기 더안사고 0
#install.packages('RcppHungarian')
library(RcppHungarian)

aa=HungarianSolver(abs(b))
aa

dh=c()
for(i in 1:nrow(aa$pairs)){
  dh=rbind(dh,b[aa$pairs[i,1],aa$pairs[i,2]])
}

dh
dh2=dh
dh2
for(i in 1:nrow(dh)){
  if(dh[i,1]<0){
    dh2[i,1]=0 
  }
}
dh2
sum(dh2)


for (i in 1:nrow(aa$pairs)){
  if(b[aa$pairs[i,1],aa$pairs[i,2]]>0){ #이프
    
    se[aa$pairs[i,2]]=0}
  else{
    se[aa$pairs[i,2]]=abs(dh[i,1])
  }
}

df=abs(b)
dim(df)
head(df)
aa$pairs
df=as.data.frame(df)
df
sell=c()
for(i in 1:length(se)){
  sell=cbind(sell,paste("",i))
}

sell=c(sell)
colnames(df)=c(sell)



buy=c()
for(i in 1:length(bu)){
  buy=cbind(buy,paste("",i))
}

buy=c(buy)
rownames(df)=c(buy)
df


comb=as.data.frame(aa$pairs)
colnames(comb)=c('Buyer','Seller')
comb

# #

library(ggplot2)


assignments = aa$pairs[,2]


dim(df2[df2$id=='Seller',])
dim(df2[df2$id=='Buyer',])

df2 = data.frame('x.axis' = df1$X.latitude.,
                 'y.axis' = df1$Y.longitude.,
                 'id' = df1$Who)

passengers.points=t(df1[df1$Who=='Seller',c(5,6)])
cars.points=t(df1[df1$Who=='Buyer',c(5,6)])


df.assign1 = data.frame('x' = cars.points[1,],
                        'y' = cars.points[2,],
                        'xend' = passengers.points[1,assignments], #패신저 할당된 x좌표
                        'yend' = cars.points[2,])
df.assign2 = data.frame('x' = passengers.points[1,assignments],
                        'y' = cars.points[2,],
                        'xend' = passengers.points[1,assignments],
                        'yend' = passengers.points[2,assignments])


colnames(df2)=c('Latitude',"Longitude",'id')
head(df2)
ggplot(df2, aes(Latitude,Longitude)) + geom_point(aes(color = id, group = id), size = 3) + # car and passengers
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df.assign1) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), data = df.assign2)+
  scale_x_continuous(minor_breaks = seq(1, 50, 1)) +
  scale_y_continuous(minor_breaks = seq(1, 50, 1)) +
  ggtitle('Optimal Maching(Surplus Electricity : 8.061kWh)') + theme(text = element_text(size = 20))   



sum(se[-aa$pairs[,2]])+sum(abs(resi[ind2]))#전체 셀러들의 남은 전기

paste(sum(se[-aa$pairs[,2]])+sum(abs(resi[ind2])))
se=df1[df1$Who=='Seller',1]
bu=df1[df1$Who=='Buyer',1]

resi=se[aa$pairs[,2]]-bu[aa$pairs[,1]]

sum(abs(se[aa$pairs[,2]]-bu[aa$pairs[,1]]))


aa$pairs[,1]


ind1=se[aa$pairs[,2]]-bu[aa$pairs[,1]]<0
ind2=se[aa$pairs[,2]]-bu[aa$pairs[,1]]>0

sum(bu)#원래 바이어 사고싶은전기 총량
sum(abs(resi[ind1]))#실제로 사고 더 사고싶은전기; 전체 바이어들의 사고싶은 전기
sum(bu)-sum(abs(resi[ind1]))#이만큼 판거잖아
sum(abs(resi[ind2]))#실제로 팔고 남은전기

sum(se[-aa$pairs[,2]])+sum(abs(resi[ind2]))#전체 셀러들의 남은 전기

sum(se[-aa$pairs[,2]])+sum(abs(resi[ind2]))+sum(bu)-sum(abs(resi[ind1]))#원래 전기
sum(se)

par(mfrow=c(2,1))
par(mfrow=c(1,2))

plot(se,type="h",xlab='Seller',cex.lab=1.5,ylab='kWh')
plot(bu,type="h",xlab='Buyer',cex.lab=1.5,ylab='kWh')

length(se)
length(bu)


df[,aa$pairs[,2]]=NA
df[aa$pairs[,1],]=NA
df
for(i in 1:nrow(df)){
df[aa$pairs[i,1],aa$pairs[i,2]]=1
}
df
sum(df)
head(df)
#install.packages('corrplot')
df

df[1,1]=NA
head(df)
par(mfrow=c(1,1))
# 
plot_table <- function(d, colors, marginColor,main="", text.cex=1.0)
{
  plot(c(-1,ncol(d)),c(0,nrow(d)+1), type="n", xaxt="n", yaxt="n", xlab="Seller",ylab="Buyer",main=main, bty="n")
  
  for (c in 1:ncol(d)) {
    rect(c-1, nrow(d), c, nrow(d) + 1, col=marginColor)
    text(c-.5,nrow(d) +.5,colnames(d)[c], cex=text.cex)
  }
  
  for (r in 1:nrow(d)) {
    rect(-1, r-1, 0, r, col=marginColor)
    text(-.5, r-.5,rownames(d)[nrow(d) - r + 1], cex=text.cex)
  }
  
  for (r in 1:nrow(d))
    for (c in 1:ncol(d)) {
      rect(c-1, r-1, c, r, col=colors[nrow(d) - r + 1,c])
      text(c-.5,r-.5,d[nrow(d) - r + 1,c], cex=text.cex)
    }
}
colors <- matrix(sapply(df, function(x) ifelse(x == 0, "white","steel blue")),ncol=ncol(df))
par(mfrow=c(1,1))
plot_table(df, colors, "gray",main="returns", text.cex=0.65)
