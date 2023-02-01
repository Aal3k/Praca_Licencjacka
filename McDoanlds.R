library("quantmod")
library("ggplot2")


#Wgranie danych

filename <- "https://stooq.pl/q/d/l/?s=mcd.us&i=d"
x        <- read.csv(filename)
y <- x[,c(1,5)]
y<-y[8000:13357 ,]
colnames(y) <- c("Daty","McDonalds")
McDonalds    <- as.numeric(y$McDonalds)
daty     <- as.Date(y$Daty, format="%Y-%m-%d")
McDonalds.Close.Price  <- zoo(McDonalds, order.by = daty)
plot(McDonalds.Close.Price,ylab="Ceny zamknięcia",xlab="Daty",main="McDonalds")  # wykres cen otwarcia



x[6000:6417,]

y.HLC<-x[,3:5]
y.HLC<-y.HLC[6000:6417 ,]

y.HL<-x[,3:4]
y.HL<-y.HL[6000:6417 ,]


Vol<-x[,6]
Vol<-Vol[6000:6417]