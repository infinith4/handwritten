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
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
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

n

di=0
z<-array(eigen(S)$vectors[,1],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3]+eigen(S)$vectors[,4],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

#di=1 ################################################

di = train[train[,1]==1,-1]
dimat=di[-1,]
dim(dimat) #4131  784

X=t(dimat)
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

di=1
z<-array(eigen(S)$vectors[,1],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3]+eigen(S)$vectors[,4],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

#di=2 ################################################

di = train[train[,1]==2,-1]
dimat=di[-1,]
dim(dimat) #4131  784

X=t(dimat)
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

di=1
z<-array(eigen(S)$vectors[,1],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3]+eigen(S)$vectors[,4],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))


#di=3 ################################################

di = train[train[,1]==3,-1]
dimat=di[-1,]
dim(dimat) #4131  784

X=t(dimat)
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

di=1
z<-array(eigen(S)$vectors[,1],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(eigen(S)$vectors[,1]+eigen(S)$vectors[,2]+eigen(S)$vectors[,3]+eigen(S)$vectors[,4],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))


#######################################################
y1=matrix(0,nrow=n,ncol=1)
y2=matrix(0,nrow=n,ncol=1)
for(i in 1:n){
    y1[i,]=t(eigen(S)$vectors[,1])%*%X[,i]    
}

y1

X[,100]
#相関行列を求める
#標本分散共分散行列の初期化
R=matrix(0,nrow=p,ncol=p)

Z=matrix(0,nrow=p,ncol=n)
dim(Z)

#標本分散共分散行列の初期化
#基準化



for(i in 1:n){
    for(j in 1:p){
        Z[j,i]=(X[j,i]-xvar[j])/sqrt(S[j,j])
    }
}

Z
dim(Z)


for(j in 1:p){
    for(k in 1:p){
        R[j,k]=S[j,k]/(sqrt(S[j,j])*sqrt(S[k,k]))
    }
}
dim(R) #p x p

#Rの固有値
Revalues=eigen(R)$value
Revalues

#Rの固有ベクトル
Revectors=eigen(R)$vectors
Revectors[,1]=-Revectors[,1]
Revectors



#寄与率
#i番目の寄与率
for(i in 1:10){
    cat(Revalues[i]/sum(Revalues),",")
}


#累積寄与率(cumulative contribution ratio)
cum=0

for(j in 1:p){
    for(i in 1:j){
        cum=cum+Revalues[i]/sum(Revalues)
    }
    cat(cum,",")
    cum=0
}



X

y1=matrix(0,nrow=n,ncol=1)
for(i in 1:n){
    y1[i,]=t(Revectors[,1])%*%Z[,i]
    y2[i,]=t(Revectors[,2])%*%Z[,i]
    cat(y1[i,],y2[i,],"\n")
    
}
Z
y1

Revectors[,2]
y2=matrix(0,nrow=n,ncol=1)
for(i in 1:n){
    y2[i,]=t(Revectors[,2])%*%Z[,i]
}

y2

y3=matrix(0,nrow=n,ncol=1)
for(i in 1:n){
    y3[i,]=t(Revectors[,3])%*%Z[,i]
}

y3

z<-array(y1,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(y2,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

z<-array(y3,dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))




for(di in 0:9){
print(di)

#train[train[,1]==di,-1]?́Adi??label?????v?????s?܂??A(di)?̕????摜?̏W?܂??B?i(di)?̕????摜?̏W?܂??̐??j*784

#apply(train[train[,1]==di,-1],2,sum)?ōs??#train[train[,1]==di,-1]?̗??a???????A1*784?̍s???ɂ????B

#(di??label?????v?????s,-1?́A1????(label)?����????s???B)
all_img[di+1,]<-apply(train[train[,1]==di,-1],2,sum) #train[train[,1]==di,-1] 


#?Q?l?F
##x <- array(1:16, dim=c(2, 4, 2))
##apply(x, c(1,2), mean) 

cat(max(all_img[di+1,]))
#max(all_img[di+1,])?ŁAall_img[di+1,]?̂??ׂĂ̗v?f?����??A
#all_img[di+1,]<-all_img[di+1,]/max(all_img[di+1,])*255 #?
 
z<-array(all_img[di+1,],dim=c(28,28)) #28*28?̉摜?ɂ??邽?߁B
z<-z[,28:1] ##right side up #??28??????1???ɁA??27??????2???ɁA....
image(1:28,1:28,z,main=di,col=cus_col(256))

}


train[train[,1]==1,-1]

for(di in 0:9){
       print(di)

       #train[train[,1]==di,-1]?́Adi??label?????v?????s?܂??A(di)?̕????摜?̏W?܂??B?i(di)?̕????摜?̏W?܂??̐??j*784

       #apply(train[train[,1]==di,-1],2,sum)?ōs??#train[train[,1]==di,-1]?̗??a???????A1*784?̍s???ɂ????B

       #(di??label?????v?????s,-1?́A1????(label)?����????s???B)

       all_img[di+1,]<-apply(train[train[,1]==di,-1],2,mean)

#?Q?l?F
##x <- array(1:16, dim=c(2, 4, 2))
##apply(x, c(1,2), mean) 

#cat(max(all_img[di+1,]))
#max(all_img[di+1,])?ŁAall_img[di+1,]?̂??ׂĂ̗v?f?����??A

z<-array(all_img[di+1,],dim=c(28,28))
z<-z[,28:1] ##right side up
image(1:28,1:28,z,main=di,col=cus_col(256))
}
