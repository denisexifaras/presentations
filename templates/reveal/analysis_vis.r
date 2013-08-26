png(file="~/Sites/decoded_presentation/templates/reveal/small_data_tweets.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
y1 = c(5,6,10,11,18,20,120,150,80,10,15,20,25,30,50,100,200,75,10,5,10,3,7,5)
x1 = c(1:7,8,9,10,11,12,13,14,15,16,17,18,19,20,21:24);
plot(x1,y1 + runif(24,0,10) + 5,xlab="",ylab="",ylim=c(0,350),xlim=c(1,24),axes=F,
	pch = 16,col="darkblue",cex = 2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),line=2,cex.axis=2)
axis(2, at= c(0,100,200,300), labels = c(0,100,200,300),line=2,cex.axis=2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
dev.off();


x1 = sort(c(rnorm(500,9,5), rnorm(500,18,5)))
y1 = c(runif(125,5,15) + (1:125),sort(rnorm(125,180,60)),sort(rnorm(125,180,20),decreasing=T),runif(250,30,100),
	sort(rnorm(125,180,20)),sort(rnorm(125,180,60),decreasing=T),runif(125,5,15)-(1:125))
x1b = x1 + runif(500,0,1); #x1b[which(x1b > 24)] = 24;
y1b = y1 +runif(500,1,50); #y1b[which(y1b > 300)] = 300;
ll = which(y1b > 300)
y1b[ll] = 300 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y1b < 0); y1b[zz] = runif(length(zz),20,60)

shift0 = x1b + 0;
shift0[which(shift0 > 24)] = shift0[(which(shift0>24))] - 24;
shift0[which(shift0 < 0)] = shift0[(which(shift0<0))] + 24;
kol = as.vector(col2rgb("blue"))/1000; 


x2 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y2 = c(runif(125,5,15) + (1:125),sort(rnorm(125,150,90)),sort(rnorm(125,150,20),decreasing=T),runif(250,30,100),
	sort(rnorm(125,150,20)),sort(rnorm(125,150,50),decreasing=T),runif(125,5,15)-(1:125))
x2b = x1 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y2b = y1 +runif(500,1,80); 
ll = which(y2b > 250)
y2b[ll] = 250 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y2b < 0); y2b[zz] = runif(length(zz),20,60)
kol2 = as.vector(col2rgb("red"))/1000; 
shift3 = x2 + 3;
shift3[which(shift3 > 24)] = shift3[(which(shift3>24))] - 24;
shift3[which(shift3 < 0)] = shift3[(which(shift3<0))] + 24;
#plot(x2b[shift3],y2b,ylab="#Tweets (M)", xlab = "Time (hrs)",xlim=c(1,24),ylim=c(0,350),pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2))

x3 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y3 = c(runif(125,5,15) + (1:125),sort(rnorm(125,100,90)),sort(rnorm(125,100,20),decreasing=T),runif(250,30,100),
	sort(rnorm(125,100,20)),sort(rnorm(125,100,50),decreasing=T),runif(125,5,15)-(1:125))
x3b = x1 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y3b = y3b +runif(500,1,50); 
ll = which(y3b > 250)
y3b[ll] = 250 + runif(ll,0,30)*((-1)^(1:ll)) 
zz = which(y3b < 0); y3b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift6 = x2 + 6;
shift6[which(shift6 > 24)] = shift6[(which(shift6>24))] - 24;
shift6[which(shift6 < 0)] = shift6[(which(shift6<0))] + 24;
#plot(x2b[shift3

png(file="~/Sites/decoded_presentation/templates/reveal/big_data_tweets_colour.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
plot(shift0,y1b,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,400),pch=16,col = rgb(0.7,kol[1],kol[3],alpha = 0.2),axes=F,cex=2,cex.lab=2)
points(shift3,y2b,pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2),cex=2)
points(shift6,y3b,pch=16,col = rgb(0.5,0.1,0.7,alpha = 0.2),cex=2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),cex.axis=2,line=2)
axis(2, at= c(0,100,200,300), labels = c(0,100,200,300),cex.axis=2, line = 2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
legend("topleft",bty="n",col=c(rgb(0.7,kol[1],kol[3],alpha = 1),rgb(kol2[1],kol2[1],kol2[3],alpha = 1),rgb(0.5,0.1,0.7,alpha = 1)),legend=c("UK","Moscow","Delhi"),ncol=3,pch=c(16,16,16),cex=2)
dev.off();

png(file="~/Sites/decoded_presentation/templates/reveal/big_data_tweets_black.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
plot(shift0,y1b,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,400),pch=16,col = rgb(0,0,0,alpha = 0.2),axes=F,cex=2,cex.lab=2)
points(shift3,y2b,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
points(shift6,y3b,pch=16,col = rgb(0,0,0,alpha = 0.2),cex=2)
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),cex.axis=2,line=2)
axis(2, at= c(0,100,200,300), labels = c(0,100,200,300),cex.axis=2, line = 2)
title(xlab="Time", cex.lab = 2, line = 6)
title(ylab="#Tweets (million)",cex.lab = 2,line=6)
dev.off();


individual <- paste("Individual", 1:10, sep=" ")

n=100;
age = floor(rnorm(10,50,10))
gender = sample(c("Male","Female"),n,replace=T)
Mult.Sclerosis = c();
for (i in 1:10){
	Mult.Sclerosis[i] = "No";
	u = runif(1); 
	if (gender[i] == "Male"){ if (u < 0.1){ Mult.Sclerosis[i] = "Yes"; } }
	if (gender[i] == "Female"){ if (u<0.4){ Mult.Sclerosis[i] = "Yes" } }
}
Mult.Sclerosis[10] = "Yes";
#Mult.Sclerosis[8] = "No";
#Mult.Sclerosis[7] = "No";

MS_data = cbind(individual,gender,age,Mult.Sclerosis);
MS_data = data.frame(MS_data)


