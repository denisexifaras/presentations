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

#San Fran, NYC, London, Moscow, Delhi, Manilla
#10, 13, 18, 21, 23,1
#-8, -5, 3, 5, 7

#london
x1 = sort(c(rnorm(500,9,5), rnorm(500,18,5)))
y1 = c(runif(125,5,15) + (1:125),sort(rnorm(125,180,60)),sort(rnorm(125,180,20),decreasing=T),runif(250,30,140),
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
x_london = shift0; y_london = y1b;

#moscow
x2 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y2 = c(runif(125,5,15) + (1:125),sort(rnorm(125,150,90)),sort(rnorm(125,150,20),decreasing=T),runif(250,30,120),
	sort(rnorm(125,150,20)),sort(rnorm(125,150,50),decreasing=T),runif(125,5,15)-(1:125))
x2b = x2 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y2b = y2 +runif(500,1,80); 
ll = which(y2b > 250)
y2b[ll] = 250 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y2b < 0); y2b[zz] = runif(length(zz),20,60)
kol2 = as.vector(col2rgb("red"))/1000; 
shift3 = x2b + 3;
shift3[which(shift3 > 24)] = shift3[(which(shift3>24))] - 24;
shift3[which(shift3 < 0)] = shift3[(which(shift3<0))] + 24;
x_moscow = shift3; y_moscow = y2b;
#plot(x2b[shift3],y2b,ylab="#Tweets (M)", xlab = "Time (hrs)",xlim=c(1,24),ylim=c(0,350),pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2))

#delhi
x3 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y3 = c(runif(125,15,35) + (1:125),sort(rnorm(125,100,90)),sort(rnorm(125,100,90),decreasing=T),runif(250,30,100),
	sort(rnorm(125,100,20)),sort(rnorm(125,100,50),decreasing=T),runif(125,15,35)-(1:125))
x3b = x3 + runif(500,0,1); #x1b[which(x1b > 24)] = 24;
y3b = y3 +runif(500,1,50); 
ll = which(y3b > 150)
y3b[ll] = 150 + runif(ll,0,30)*((-1)^(1:ll)) 
zz = which(y3b < 0); y3b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift5 = x3b + 5;
shift5[which(shift5 > 24)] = shift5[(which(shift5>24))] - 24;
shift5[which(shift5 < 0)] = shift5[(which(shift5<0))] + 24;
x_delhi = shift5; y_delhi = y3b;
#plot(x2b[shift3

#manilla
x4 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y4 = c(runif(125,5,15) + (1:125),sort(rnorm(125,150,50)),sort(rnorm(125,150,50),decreasing=T),runif(250,30,100),
	sort(rnorm(125,100,20)),sort(rnorm(125,100,20),decreasing=T),runif(125,5,15)-(1:125))
x4b = x4 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y4b = y4 +runif(500,1,50); 
ll = which(y4b > 550)
y4b[ll] = 550 + runif(ll,0,30)*((-1)^(1:ll)) 
zz = which(y4b < 0); y4b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift7 = x4b + 7;
shift7[which(shift7 > 24)] = shift7[(which(shift7>24))] - 24;
shift7[which(shift7 < 0)] = shift7[(which(shift7<0))] + 24;
x_manilla = shift7; y_manilla = y4b;
#plot(x2b[shift3

#SF
x5 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y5 = c(runif(125,5,15) + (1:125),sort(rnorm(125,200,30)),sort(rnorm(125,200,30),decreasing=T),runif(250,30,100),
	sort(rnorm(125,300,50)),sort(rnorm(125,300,50),decreasing=T),runif(125,15,35)-(1:125))
x5b = x5 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y5b = y5 +runif(500,1,50); 
ll = which(y5b > 450)
y5b[ll] = 450 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y5b < 0); y5b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shift8 = x5b - 8;
shift8[which(shift8 > 24)] = shift8[(which(shift8>24))] - 24;
shift8[which(shift8 < 0)] = shift8[(which(shift8<0))] + 24;
x_sf = shift8; y_sf = y5b;
#plot(x2b[shift3

#NYC
x6 = sort(c(rnorm(500,9,5), rnorm(500,18,5)));
y6 = c(runif(125,5,105) + (1:125),sort(rnorm(125,200,20)),sort(rnorm(125,200,20),decreasing=T),runif(250,30,200),
	sort(rnorm(125,400,100)),sort(rnorm(125,400,100),decreasing=T),runif(125,5,205)-(1:125))
x6b = x6 + runif(500,0,0.5); #x1b[which(x1b > 24)] = 24;
y6b = y6 +runif(500,1,50); 
ll = which(y6b > 450)
y6b[ll] = 450 + runif(ll,0,50)*((-1)^(1:ll)) 
zz = which(y6b < 0); y6b[zz] = runif(length(zz),20,60)
kol3 = as.vector(col2rgb("green"))/1000; 
shiftnyc = x6b - 5;
shiftnyc[which(shiftnyc > 24)] = shiftnyc[(which(shiftnyc>24))] - 24;
shiftnyc[which(shiftnyc < 0)] = shiftnyc[(which(shiftnyc<0))] + 24;
x_nyc = shiftnyc; y_nyc = y6b;
#plot(x2b[shift3

data = cbind(x_sf,y_sf,x_nyc,y_nyc,x_london,y_london,x_moscow,y_moscow, x_delhi,y_delhi,x_manilla,y_manilla);
write.csv(data,"~/Sites/decoded_presentation/templates/reveal/tweet_simulated_data.csv",quote=F,row.names=F)

plot(x_london,y_london,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,600),pch=16,col = rgb(0.7,kol[1],kol[3],alpha = 0.2),axes=F)
points(x_moscow,y_moscow,pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2))
points(x_delhi,y_delhi,pch=16,col = rgb(0.5,0.1,0.7,alpha = 0.2))
points(x_nyc,y_nyc,pch=16,col = rgb(0.1,0.1,0.7,alpha = 0.2))
points(x_sf,y_sf,pch=16,col = rgb(0.7,0.7,0.1,alpha = 0.2))
points(x_manilla,y_manilla,pch=16,col = rgb(0.4,0.4,0.1,alpha = 0.2))
axis(1,at=c(1,5,8.5,12.5,16,20,24),labels = c("midnight","6am","9am","noon","3pm","6pm","midnight"),line=2)
axis(2, at= c(0,100,200,300,400,500), labels = c(0,100,200,300,400,500),line = 2)
title(xlab="Time", line = 6)
title(ylab="#Tweets (million)",line=6)
legend("topleft",bty="n",col=c(rgb(0.7,kol[1],kol[3],alpha = 1),rgb(kol2[1],kol2[1],kol2[3],alpha = 1),rgb(0.5,0.1,0.7,alpha = 1)),legend=c("UK","Moscow","Delhi"),ncol=3,pch=c(16,16,16))


png(file="~/Sites/decoded_presentation/templates/reveal/big_data_tweets_colour.png",width = 1033,height = 484)
par(bg="white",mar=c(10,10,2,2))
plot(x_london,y_london,ylab="", xlab = "",xlim=c(0,24),ylim=c(0,400),pch=16,col = rgb(0.7,kol[1],kol[3],alpha = 0.2),axes=F,cex=2,cex.lab=2)
points(x_moscow,y_moscow,pch=16,col = rgb(kol2[1],kol2[1],kol2[3],alpha = 0.2),cex=2)
points(x_delhi,y_delhi,pch=16,col = rgb(0.5,0.1,0.7,alpha = 0.2),cex=2)
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


