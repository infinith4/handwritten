setwd("C:\\Users\\tH4\\Dropbox\\R\\handwritten")

## Read in data
train <- read.csv("./data/train.csv", header=TRUE)
train<-as.matrix(train)
dim(train)
#42000   785

##Color ramp def.
colors<-c('white','black')
cus_col<-colorRampPalette(colors=colors) #16進数で返す

cus_col(256)

## Plot the average image of each digit
par(mfrow=c(4,3),pty='s',mar=c(1,1,1,1),xaxt='n',yaxt='n')
all_img<-array(dim=c(10,28*28)) #NAの10*784の行列作成
#all_img


for(di in 0:9)
{
print(di)

#train[train[,1]==di,-1]は、diとlabelが一致する行つまり、(di)の文字画像の集まり。（(di)の文字画像の集まりの数）*784

#apply(train[train[,1]==di,-1],2,sum)で行列#train[train[,1]==di,-1]の列和を取り、1*784の行列にする。

#(diとlabelが一致する行,-1は、1列目(label)を除いた行列。)
all_img[di+1,]<-apply(train[train[,1]==di,-1],2,sum) #train[train[,1]==di,-1] 


#参考：
##x <- array(1:16, dim=c(2, 4, 2))
##apply(x, c(1,2), mean) 

cat(max(all_img[di+1,]))
#max(all_img[di+1,])で、all_img[di+1,]のすべての要素を割る、
#all_img[di+1,]<-all_img[di+1,]/max(all_img[di+1,])*255 #?
 
z<-array(all_img[di+1,],dim=c(28,28)) #28*28の画像にするため。
z<-z[,28:1] ##right side up #第28列を第1列に、第27列を第2列に、....
image(1:28,1:28,z,main=di,col=cus_col(256))
}




for(di in 0:9)
{
print(di)

#train[train[,1]==di,-1]は、diとlabelが一致する行つまり、(di)の文字画像の集まり。（(di)の文字画像の集まりの数）*784

#apply(train[train[,1]==di,-1],2,sum)で行列#train[train[,1]==di,-1]の列和を取り、1*784の行列にする。

#(diとlabelが一致する行,-1は、1列目(label)を除いた行列。)

all_img[di+1,]<-apply(train[train[,1]==di,-1],2,mean)

#参考：
##x <- array(1:16, dim=c(2, 4, 2))
##apply(x, c(1,2), mean) 

#cat(max(all_img[di+1,]))
#max(all_img[di+1,])で、all_img[di+1,]のすべての要素を割る、

z<-array(all_img[di+1,],dim=c(28,28))
z<-z[,28:1] ##right side up
image(1:28,1:28,z,main=di,col=cus_col(256))
}

