
n<-100
mu<-3
std<-1

vari=std^2

x<-rnorm(n, mean = mu, sd = std)
hist(x)

like<- -n/2*log(2*pi*vari) -1/(2*vari)*sum((x-mu)^2)
like

