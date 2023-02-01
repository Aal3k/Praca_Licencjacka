library("quantmod")
library("ggplot2")


#Wgranie danych

filename <- "https://stooq.pl/q/d/l/?s=pkn&i=d"
x        <- read.csv(filename)
y <- x[,c(1,5)]
y<-y[4200:5793,]
colnames(y) <- c("Daty","Orlen")
Orlen     <- as.numeric(y$Orlen)
daty     <- as.Date(y$Daty, format="%Y-%m-%d")
Orlen.Close.Price        <- zoo(Orlen, order.by = daty)
plot(Orlen.Close.Price,xlab="Lata",ylab="Ceny zamknięcia",main="PKN Orlen SA (01.09.2016r - 17.01.2023r)")  # wykres cen zamknięcia
hist(Orlen.Close.Price,xlab = "Ceny zamknięcia",main="Histogram ceny akcji Orlen",ylab="Gęstość")


#porównanie cen
y1 <- x[,c(1,2)]
colnames(y1) <- c("Daty","Orlen")
Orlen      <- as.numeric(y1$Orlen)
daty     <- as.Date(y1$Daty, format="%Y-%m-%d")
Orlen.Open.Price        <- zoo(Orlen, order.by = daty)

y2 <- x[,c(1,3)]
colnames(y2) <- c("Daty","Orlen")
Orlen    <- as.numeric(y2$Orlen)
daty     <- as.Date(y2$Daty, format="%Y-%m-%d")
Orlen.Max.Price        <- zoo(Orlen, order.by = daty)

y3 <- x[,c(1,4)]
colnames(y3) <- c("Daty","Orlen")
Orlen   <- as.numeric(y3$Orlen)
daty     <- as.Date(y3$Daty, format="%Y-%m-%d")
Orlen.Min.Price        <- zoo(Orlen, order.by = daty)


par(mfrow=c(2,2))
plot(Orlen.Close.Price,ylab="Ceny",xlab="Daty",main="Ceny zamknięcia") 
plot(Orlen.Open.Price,ylab="Ceny",xlab="Daty",main="Ceny otwarcia") 
plot(Orlen.Max.Price,ylab="Ceny",xlab="Daty",main="Ceny najwyższe") 
plot(Orlen.Min.Price,ylab="Ceny",xlab="Daty",main="Ceny najniższe") 
par(mfrow=c(1,1))

##### 

x<-x[4200:5793,]
y.HLC<-x[,3:5]
y.HL<-x[,3:4]

#wolumen 
Vol<-x[,6]
colnames(y) <- c("Daty","Wolumen")
Wolumen   <- as.numeric(Vol$Wolumen)
barplot(Wolumen, main="Wolumen akcji PKN Orlen", ylab="Wolumen")





#Wskaźniki impetu


#RSI (ustawienia pdosatwowe,n=14,poziomy 30,70)

Amzn.RSI.14<-RSI(Orlen.Close.Price,n=14)
plot(daty,Amzn.RSI.14,main="RSI(14)",xlab="Lata",ylab="RSI",type="l")
abline(h=70, col="red")
abline(h=30,col="red")


#RSI (n=21,poziomy 30,70)

Amzn.RSI.28<-RSI(Orlen.Close.Price,n=28)
plot(Amzn.RSI.28,main="RSI(28) PKN Orlen",xlab="Lata",ylab="RSI")
abline(h=70, col="red")
abline(h=30,col="red")

#RSI (n=7,poziomy 20,80)

Amzn.RSI.7<-RSI(Orlen.Close.Price,n=7)
plot(Amzn.RSI.7,main="RSI(7)",xlab="Lata",ylab="RSI")
abline(h=80, col="red")
abline(h=20,col="red")



#CCI (ustawienia pdosatwowe,n=20,poziomy -100,100)

Amzn.CCI.20<-CCI(y.HLC,n=20)
plot(Amzn.CCI.20,type="l")
abline(h=100, col="red")
abline(h=-100,col="red")

#CCI (n=7,poziomy -100,100)

Amzn.CCI.7<-CCI(y.HLC,n=7)
plot(Amzn.CCI.7,type="l")
abline(h=100, col="red")
abline(h=-100,col="red")


#CCI (n=68,poziomy -100,100)

Amzn.CCI.78<-CCI(y.HLC,n=78)
plot(daty,Amzn.CCI.78,type="l",main="CCI(78) PKN Orlen",xlab="Lata",ylab="CCI")
abline(h=220, col="red")
abline(h=-200,col="red")






#Wskaźniki zmienności



#Bolinger Bands(ustawienia pdosatwowe,n=20,sd=2)

Amzn.BB.20<-BBands(y.HLC,n=20,sd=2)
dn.20<-Amzn.BB.20[,1]
ma.20<-Amzn.BB.20[,2]
up.20<-Amzn.BB.20[,3]

plot(as.numeric(Orlen.Close.Price),type="l")
lines(dn.20,type="l",col="blue")
lines(ma.20,type="l",col="red")
lines(up.20,type="l",col="blue")



#Bolinger Bands(ustawienia krótkoterminowe,n=10,sd=1.5)

Amzn.BB.10<-BBands(y.HLC,n=10,sd=1.5)
dn.10<-Amzn.BB.10[,1]
ma.10<-Amzn.BB.10[,2]
up.10<-Amzn.BB.10[,3]

plot(as.numeric(Orlen.Close.Price),type="l")
lines(dn.10,type="l",col="blue")
lines(ma.10,type="l",col="red")
lines(up.10,type="l",col="blue")


#Bolinger Bands(ustawienia długoterminowe,n=50,sd=2.5)

Amzn.BB.50<-BBands(y.HLC,n=50,sd=2.5)
dn.50<-Amzn.BB.50[,1]
ma.50<-Amzn.BB.50[,2]
up.50<-Amzn.BB.50[,3]

plot(as.numeric(Orlen.Close.Price),type="l",main="BB(50,2.5) PKN Orlen",xlab="Lata",ylab="Ceny zamknięcia")
lines(dn.50,type="l",col="blue")
lines(ma.50,type="l",col="red")
lines(up.50,type="l",col="blue")

#DonchianChannel(ustawienia podstawowe,n=10)

Amzn.DC.10<-DonchianChannel(y.HL,n=10)
h.10<-Amzn.DC.10[,1]
m.10<-Amzn.DC.10[,2]
l.10<-Amzn.DC.10[,3]

plot(as.numeric(Orlen.Close.Price),type="l")
lines(h.10,type="l",col="blue")
lines(m.10,type="l",col="red")
lines(l.10,type="l",col="blue")


#DonchianChannel(n=20)

Amzn.DC.20<-DonchianChannel(y.HL,n=20)
h.20<-Amzn.DC.20[,1]
m.20<-Amzn.DC.20[,2]
l.20<-Amzn.DC.20[,3]

plot(as.numeric(Orlen.Close.Price),type="l")
lines(h.20,type="l",col="blue")
lines(m.20,type="l",col="red")
lines(l.20,type="l",col="blue")



#DonchianChannel(n=50)

Amzn.DC.50<-DonchianChannel(y.HL,n=60)
h.50<-Amzn.DC.50[,1]
m.50<-Amzn.DC.50[,2]
l.50<-Amzn.DC.50[,3]

plot(as.numeric(Orlen.Close.Price),type="l",main="DC(60) PKN Orlen",xlab="Lata",ylab="Ceny zamknięcia")
lines(h.50,type="l",col="blue")
lines(m.50,type="l",col="red")
lines(l.50,type="l",col="blue")


#Wskaźnik trendu


#SAR(ustawienia podstawowe, accel=(0.02,0.2))

Amzn.SAR.02<-SAR(y.HL)
plot(as.numeric(Orlen.Close.Price),type="l",main="SAR dla cen McDonalds",xlab = "Lata",ylab="Ceny zamknięcia" )
points(Amzn.SAR.02,lwd=3,cex=0.1)



#SAR(accel=(0.0081,0.2))
Amzn.SAR.0081<-SAR(y.HL,accel=c(0.0081,0.2))
plot(as.numeric(Orlen.Close.Price),type="l",main="SAR dla cen McDonalds",xlab = "Lata",ylab="Ceny zamknięcia" )
points(Amzn.SAR.0081,lwd=3,cex=0.1)


#SAR(accel=(0.002,0.2))
Amzn.SAR.02<-SAR(y.HL,accel=c(0.0005,0.2))
plot(as.numeric(Orlen.Close.Price),type="l",main="SAR(0.0005,0.2) dla cen PKN Orlen",xlab = "Lata",ylab="Ceny zamknięcia" )
points(Amzn.SAR.02,lwd=2,cex=0.05,col="blue")



#SMA

Amzn.SMA.200<-SMA(Orlen.Close.Price,n=200)
Amzn.SMA.50<-SMA(Orlen.Close.Price,n=50)
Amzn.SMA.100<-SMA(Orlen.Close.Price,n=100)


plot(as.numeric(Orlen.Close.Price),type="l",main="Porównanie średnich dla cen PKN Orlen",xlab = "Lata",ylab="Ceny zamknięcia" )
lines(as.numeric(Amzn.SMA.200),col="blue")
lines(as.numeric(Amzn.SMA.50),col="red")
lines(as.numeric(Amzn.SMA.100),col="green")
legend(0,45, legend=c("SMA(200)", "SMA(50)"),
       col=c("blue", "red"), lty=1, cex=0.8,box.lty=0)

#Wkaźniki siły rynku

#MFI(ustawienia podstawowowe,n=14,poziomy 20,80)

Amzn.MFI.14<-MFI(y.HLC,Vol,n=14)
plot(Amzn.MFI.14,type="l",ylab="MFI",xlab="Daty",main="Money Flow Index(14)")
abline(h=20, col="red")
abline(h=80,col="red")


#MFI(n=9,poziomy 15,85)

Amzn.MFI.9<-MFI(y.HLC,Vol,n=9)
plot(Amzn.MFI.9,type="l")
abline(h=15, col="red")
abline(h=85,col="red")


#MFI(n=44,poziomy 30,60)

Amzn.MFI.44<-MFI(y.HLC,Vol,n=68)
plot(daty,Amzn.MFI.44,type="l",ylab="MFI",xlab="Daty",main="MFI(68) PKN Orlen")
abline(h=35, col="red")
abline(h=70,col="red")


#logarytmiczne stopy zwrotu

y <- x[,c(1,5)]
colnames(y) <- c("Daty","Orlen")
Orlen    <- as.numeric(y$Orlen)
daty     <- as.Date(y$Daty, format="%Y-%m-%d")
Orlen.Close.Price        <- zoo(Orlen, order.by = daty)

n_pkn <- length(Orlen)
r.pkn <- log(Orlen[-1]/Orlen[-n_pkn])


hist(r.pkn,xlab="Ceny zamknięcia",main="Histogram logarytmicznych stóp zwrotu PKN Orlen")


install.packages("libstableR")
library(libstableR)
p <- r.pkn
wyn_i<-stable_fit_init(p)# estymator McCullocha
wyn_k<-stable_fit_koutrouvelis(p,wyn_i)# esymator Koutrouvelisa
wyn_m<-stable_fit_mle(p,wyn_k) # estymator największej wiarogodności
wyn_m2<-stable_fit_mle2d(p,wyn_k) # zmodyfikowany estymator największej wiarogodności
