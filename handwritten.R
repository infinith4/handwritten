#how to calculate y_i? (13.02.09)

#setwd("C:\\Users\\tH4\\Dropbox\\R\\handwritten")
setwd("./Dropbox/R/handwritten")

getwd()

## Read in data
train <- read.csv("./data/train.csv", header=TRUE)
train<-as.matrix(train)
dim(train)
#42000   785

##Color ramp def.
colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors) #16?i???ŕԂ?

cus_col(256)

## Plot the average image of each digit
par(mfrow=c(1,5),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(10,28*28)) #NA??10*784?̍s???쐬
#all_img

di0 = train[train[,1]==0,-1]
di0mat=di0[-1,]
dim(di0mat) #4131  784

di0.cor=cor(di0mat)

di0.cor


X=t(di0mat)
dim(X) #p x n

Xvar=function(X){
    return((1/n)*rowSums(X))#各行の和
}

xvar=Xvar(X) 
length(xvar)

xvar#標本平均ベクトル

n=ncol(X)
n
p=nrow(X)
p

#標本分散共分散行列の初期化
S=matrix(0,nrow=p,ncol=p)
#S
#(X[,i]-xvar)%*%t(X[,i]-xvar)
for(i in 1:n){
    S=S+(X[,i]-xvar)%*%t(X[,i]-xvar)
}

S=(1/n)*S

S
dim(S)

eigen(S)$values
eigen(S)$vectors[,1]
length(eigen(S)$vectors[,1]) #p


di=0
m=128
yvec=matrix(0,nrow=m,ncol=1)
for(i in 1:m){
    yvec[i]=t(eigen(S)$vectors[,i])%*%X[,1] #1番目の標本の第i主成分
}


z<-array(X[,1],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main="original data",col=cus_col(256))
n
restorevec=matrix(0,nrow=p,ncol=1)
for(i in 1:m){
    restorevec=restorevec+yvec[i]*eigen(S)$vectors[,i] #1番目の標本の第i主成分
    if(i==16||i==32||i==64||i==128){
        z<-array(restorevec,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
        z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
        image(1:28,1:28,z,main=i,col=cus_col(256))
    }
}

#2番目の標本の復元###################
m=128
z<-array(X[,2],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main="original data",col=cus_col(256))


yvec=matrix(0,nrow=m,ncol=1)
for(i in 1:m){
    yvec[i]=t(eigen(S)$vectors[,i])%*%X[,2] #2番目の標本の第i主成分
}

for(i in 1:m){
    restorevec=restorevec+yvec[i]*eigen(S)$vectors[,i] #1番目の標本の第i主成分
    if(i==16||i==32||i==64||i==128){
        z<-array(restorevec,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
        z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
        image(1:28,1:28,z,main=i,col=cus_col(256))
    }
}

#3番目の標本の復元###################
m=128
z<-array(X[,3],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main="original data",col=cus_col(256))


yvec=matrix(0,nrow=m,ncol=1)
for(i in 1:m){
    yvec[i]=t(eigen(S)$vectors[,i])%*%X[,3] #3番目の標本の第i主成分
}

for(i in 1:m){
    restorevec=restorevec+yvec[i]*eigen(S)$vectors[,i] #1番目の標本の第i主成分
    if(i==16||i==32||i==64||i==128){
        z<-array(restorevec,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
        z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
        image(1:28,1:28,z,main=i,col=cus_col(256))
    }
}



#4番目の標本の復元###################
m=128
z<-array(X[,4],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main="original data",col=cus_col(256))


yvec=matrix(0,nrow=m,ncol=1)
for(i in 1:m){
    yvec[i]=t(eigen(S)$vectors[,i])%*%X[,4] #4番目の標本の第i主成分
}

for(i in 1:m){
    restorevec=restorevec+yvec[i]*eigen(S)$vectors[,i] #1番目の標本の第i主成分
    if(i==16||i==32||i==64||i==128){
        z<-array(restorevec,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
        z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
        image(1:28,1:28,z,main=i,col=cus_col(256))
    }
}

