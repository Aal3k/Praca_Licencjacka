install.packages("quantmod")
install.packages("ggplot2")
library("quantmod")
library("ggplot2")


#Wgranie danych

filename <- "https://stooq.pl/q/d/l/?s=amzn.us&i=d"
x        <- read.csv(filename)
y <- x[,c(1,5)]
y<-y[6000:6417 ,]
colnames(y) <- c("Daty","Amazon")
Amazon     <- as.numeric(y$Amazon)
daty     <- as.Date(y$Daty, format="%Y-%m-%d")
Amazon.Close.Price        <- zoo(Amazon, order.by = daty)
plot(Amazon.Close.Price)  # wykres cen zamknięcia
legend("bottomleft",inset=0.05, legend=c("Ceny zamknięcia", "Linia środkowa", "Kanał Donchiana"),
       col=c("black","red", "blue"), lty=1, cex=1)




x[6000:6417,]

y.HLC<-x[,3:5]
y.HLC<-y.HLC[6000:6417 ,]

y.HL<-x[,3:4]
y.HL<-y.HL[6000:6417 ,]


Vol<-x[,6]
Vol<-Vol[6000:6417]






#Wskaźniki impetu


#RSI (ustawienia pdosatwowe,n=14,poziomy 30,70)

Amzn.RSI.14<-RSI(Amazon.Close.Price,n=14)
plot(Amzn.RSI.14)
abline(h=70, col="red")
abline(h=30,col="red")


#RSI (n=4,poziomy 20,80)

Amzn.RSI.4<-RSI(Amazon.Close.Price,n=4)
plot(Amzn.RSI.4)
abline(h=80, col="red")
abline(h=20,col="red")

#RSI (n=7,poziomy 20,80)

Amzn.RSI.7<-RSI(Amazon.Close.Price,n=7)
plot(Amzn.RSI.7)
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

Amzn.CCI.68<-CCI(y.HLC,n=68)
plot(Amzn.CCI.68,type="l")
abline(h=100, col="red")
abline(h=-100,col="red")






#Wskaźniki zmienności



#Bolinger Bands(ustawienia pdosatwowe,n=20,sd=2)

Amzn.BB.20<-BBands(y.HLC,n=20,sd=2)
dn.20<-Amzn.BB.20[,1]
ma.20<-Amzn.BB.20[,2]
up.20<-Amzn.BB.20[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(dn.20,type="l",col="blue")
lines(ma.20,type="l",col="red")
lines(up.20,type="l",col="blue")



#Bolinger Bands(ustawienia krótkoterminowe,n=10,sd=1.5)

Amzn.BB.10<-BBands(y.HLC,n=10,sd=1.5)
dn.10<-Amzn.BB.10[,1]
ma.10<-Amzn.BB.10[,2]
up.10<-Amzn.BB.10[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(dn.10,type="l",col="blue")
lines(ma.10,type="l",col="red")
lines(up.10,type="l",col="blue")


#Bolinger Bands(ustawienia długoterminowe,n=50,sd=2.5)

Amzn.BB.50<-BBands(y.HLC,n=50,sd=2.5)
dn.50<-Amzn.BB.50[,1]
ma.50<-Amzn.BB.50[,2]
up.50<-Amzn.BB.50[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(dn.50,type="l",col="blue")
lines(ma.50,type="l",col="red")
lines(up.50,type="l",col="blue")

#DonchianChannel(ustawienia podstawowe,n=10)

Amzn.DC.10<-DonchianChannel(y.HL,n=10)
h.10<-Amzn.DC.10[,1]
m.10<-Amzn.DC.10[,2]
l.10<-Amzn.DC.10[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(h.10,type="l",col="blue")
lines(m.10,type="l",col="red")
lines(l.10,type="l",col="blue")


#DonchianChannel(n=20)

Amzn.DC.20<-DonchianChannel(y.HL,n=20)
h.20<-Amzn.DC.20[,1]
m.20<-Amzn.DC.20[,2]
l.20<-Amzn.DC.20[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(h.20,type="l",col="blue")
lines(m.20,type="l",col="red")
lines(l.20,type="l",col="blue")



#DonchianChannel(n=50)

Amzn.DC.50<-DonchianChannel(y.HL,n=50)
h.50<-Amzn.DC.50[,1]
m.50<-Amzn.DC.50[,2]
l.50<-Amzn.DC.50[,3]

plot(as.numeric(Amazon.Close.Price),type="l")
lines(h.50,type="l",col="blue")
lines(m.50,type="l",col="red")
lines(l.50,type="l",col="blue")


#Wskaźnik trendu


#SAR(ustawienia podstawowe, accel=(0.02,0.2))

Amzn.SAR.02<-SAR(y.HL)
plot(as.numeric(Amazon.Close.Price),type="l")
points(Amzn.SAR.02,lwd=3,cex=0.1)



#SAR(accel=(0.0081,0.2))
Amzn.SAR.0081<-SAR(y.HL,accel=c(0.0081,0.2))
plot(as.numeric(Amazon.Close.Price),type="l")
points(Amzn.SAR.0081,lwd=3,cex=0.1)


#SAR(accel=(0.002,0.2))
Amzn.SAR.02<-SAR(y.HL,accel=c(0.002,0.2))
plot(as.numeric(Amazon.Close.Price),type="l")
points(Amzn.SAR.02,lwd=3,cex=0.1)



#SMA

Amzn.SMA.20<-SMA(Amazon.Close.Price,n=20)
Amzn.SMA.50<-SMA(Amazon.Close.Price,n=50)
Amzn.SMA.100<-SMA(Amazon.Close.Price,n=100)


plot(as.numeric(Amazon.Close.Price),type="l")
lines(as.numeric(Amzn.SMA.20),col="blue")
lines(as.numeric(Amzn.SMA.50),col="red")
lines(as.numeric(Amzn.SMA.100),col="green")


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

Amzn.MFI.44<-MFI(y.HLC,Vol,n=44)
plot(Amzn.MFI.44,type="l")
abline(h=30, col="red")
abline(h=60,col="red")




