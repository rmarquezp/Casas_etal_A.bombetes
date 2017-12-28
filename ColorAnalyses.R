##Analysis of RGB scores obtained from photographs of Andinobates bombetes frogs and clay models. 

library(scatterplot3d)
library(rgl)
library(MASS)

data=read.csv("RGBScores.csv")

#Compute R', G', and B' as relative to the overall brightness. 

data$Rprime=numeric(length(data[,1]))
data$Gprime=numeric(length(data[,1]))
data$Bprime=numeric(length(data[,1]))

for(i in 1:length(data[,1])){
	TotalBr=sum(data[i,1:3])
	data$Rprime[i]=data$red[i]/TotalBr
	data$Gprime[i]=data$green[i]/TotalBr
	data$Bprime[i]=data$blue[i]/TotalBr
}

## Do PCA on R', G', and B' values

pca=prcomp(data[,5:7],scale=T,retx=T)

scores=predict(pca)

PropVar=100*summary(pca)$importance[2,]
names=c()
for (i in 1:length(PropVar)){names[i]=paste("PC",i," (",round(PropVar[i],digits=1),"%)",sep="")}
colnames(scores)=names


##Calculate LM based Amézquita el al (2009 Biol. J. Linn. Soc. 98, 826-838)

data$LM=data$Rprime-data$Gprime

###Export table with all the data

write.csv(cbind(data,scores),file="ColorationOutputs.csv", quote=F,row.names=F)

##Plot

#colPick=function(x){
#	
#	if(x=="FrogRed"){return("red")}
#	if(x=="FrogYellow"){return("yellow")}	
#	if(x=="ModelControl"){return("brown")}
#	if(x=="ModelRed"){return("red")}
#	if(x=="ModelYellow"){return("yellow")}
#	}

#symPick=function(x){
	
#	if(x=="FrogControl"){return(22)}
#	if(x=="FrogRed"){return(22)}
#	if(x=="FrogYellow"){return(22)}
	
#	if(x=="ModelControl"){return(24)}
#	if(x=="ModelRed"){return(24)}
#	if(x=="ModelYellow"){return(24)}
#	}

#cols=as.character(sapply(data$Morph,colPick))
#syms=unlist(sapply(data$Morph,symPick))


#par(mfrow=c(1,2))

#plot(scores[,1:2],pch=syms,bg=cols,xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))

#plot(data$B,data$LM,pch=syms,bg=cols,xlab="B",ylab="LM",xlim=c(0.05,0.65),ylim=c(0.05,0.65))


#par(mfrow=c(1,2))

#barplot(pca$rotation[,1],border=NA,col=c("red","green","blue"),ylab="Loading",ylim=c(-0.6,0.7), main=names[1], cex.lab=1.5)
#barplot(pca$rotation[,2],border=NA,col=c("red","green","blue"),ylim=c(-0.6,0.7), main=names[2],cex.lab=1.5)
