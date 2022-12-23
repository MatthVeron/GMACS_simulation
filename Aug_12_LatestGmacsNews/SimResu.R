setwd("C:\\Research\\gmacs\\Unified\\Build\\RunsT\\")

Nsim <- 100
Nyear <- 46

Est <- matrix(0,nrow=Nsim,ncol=Nyear)

True <- scan("GmacsAllT.out",skip=10273,n=Nyear)
print(True)

for (Isim in 1:Nsim)
 {
  FileName <- paste0("GmacsAll",Isim,".out")
  Est[Isim,] <- scan(FileName,skip=10273,n=Nyear,quiet=T)
  Est[Isim,] <-  (Est[Isim,]-True)/True*100
  
}
Years <- 1975:2020
quants <- matrix(0,nrow=5,ncol=Nyear)

par(oma=c(2,2,2,2),mar=c(5,5,20,5))
for (II in 1:Nyear)
  quants[,II] <- quantile(Est[,II],prob=c(0.05,0.25,0.5,0.75,0.95))
ymax <- max(abs(quants))*1.1
plot(Years,quants[3,],ylim=c(-ymax,ymax),xlab="Year",ylab="Relative error (%)",type="l")
xx <- c(Years,rev(Years))
yy <- c(quants[1,],rev(quants[5,]))
polygon(xx,yy,col="gray10")
xx <- c(Years,rev(Years))
yy <- c(quants[2,],rev(quants[4,]))
polygon(xx,yy,col="gray90")
lines(Years,quants[3,],col="blue",lwd=3)
abline(h=0,lwd=2,col="red")



        

