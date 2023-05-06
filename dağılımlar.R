x <- seq(0,50,1)
print(x)
#p=0.5
dbinom(x,51,0.5)
#P(x<=18)
pbinom(18,50,0.5)
#P(14<=x<=18)
pbinom(18,50,0.5)-pbinom(14,50,0.5)
# Bir fabrikada üretim yapan makinelerden birinin ürettiği ürünlerin
# %10 olasılıkla kusurlu olduğu tespit edilmiştir.Bu ürünlerden rasgele 5
# adet alındığında, 1 adedinin kusurlu olmalı olasılığı nedir?
#p(x=1)
dbinom(1,5,0.10)
# En fazla bir tanesinin kusurlu olma olasılığı nedir? P(x=0)+P(x=1)=P(x<=1) 
dbinom(0,5,0.10)+dbinom(1,5,0.10)
#pbinom ile dağılım fonk(kümülatif değerleri) yukarıdaki soruyu böyle de yapabiliriz
pbinom(1,5,0.10)
#En az iki tanesinin kusurlu olma?P(x>=2)=1-P(x<=1)
1-pbinom(1,5,0.10)
#binom rastgele sayı üretimi
rbinom(10,5,0.5) #0-5 arasında 10 tane 0.5 olasılıkla değer çeker

#Poisson Dağılımı (Binomda np<5 ise poissona yakınsar)
x <- seq(0,49.1)
dpois(x,0.5)

#p(x<=3)
dpois(3,0.5)
#Bir fabrikada üretilen ürünler 0.004 olasılıkla kusurludur.
#üretilen ürünlerden rassal örneklem ile 1000 adet alınmıştır lambda=np =4
#3 tane ürünün kusurlu olma olasılığı? P(x=3)
dpois(x=3,lambda=1000*0.004)
#P(x<=3)=p(0)+p(1)+p(2) #ppois yaparız kümülatif 
ppois(2,1000*0.004)

#p(x>=2)
1-ppois(1,1000*0.004)

#DÜZGÜN DAĞILIM(UNİFORM)
x <- c(0.3,0,0.4)
dunif(x,min = 0.2,max=0.4)

#p(x<=0.6)
punif(0.6,min=0.2,max=0.4)

xvalues <- seq(-3,3,length=300)
oyf <- dunif(xvalues,min = -3,max = 3)
plot(xvalues,oyf.type="T",xlim=c(-3,3))

runif(10,min = 0.2,max = 0.4)
r <- runif(1000,min = 0.2,max = 0.4)
hist(r)

# p(112<=X<=115)
punif(115,min=100,max=120)-punif(112,min=100,max=120)

##ÜSTEL DAĞILIM

x <- seq(0,10,length=200)
fx <- dexp(x,rate = 1.65)
plot(x,fx,xlim=c(0,8),ylim=c(0,1.65),type = "l")

#P(X<=2.5)
pexp(2.5,rate=0.9)

#rastgele sayı üretme
rexp(10,rate = 0.9)
pexp(40,rate = 1/160)*100

#NORMAL DAĞILIM
x <- seq(-4,4,length=10)
#olasılık yoğunluk fonksiyonu
dnorm(x,mean = 3.42,sd=0.2)

#dağılım fonksiyonu
#P(X<=3)
pnorm(3,mean = 3.42,sd=0.2)

xvals <- seq(-5,2,length=300)
oyf <- dnorm(xvals,mean = 3.42,sd=0.2)
plot(xvals,oyf,type="l",xlim = c(-4.4,-2.5))

rnorm(10,mean = 3,sd=0.2)

pnorm(70,mean = 75,sd=8)
pnorm(80,mean = 75,sd=8)-pnorm(70,mean = 75,sd=8)

#Dağılım Fonksiyonu ile çözüm(derste yapılan çözüm)
x <- pbinom(18,50,0.5)-pbinom(14,50,0.5)
#Yoğunluk Fonksiyonları Toplam ile çözüm
y <- dbinom(15:18,50,0.5)
z <- sum(y)

#Döngü ile Çözüm
toplam <- 0
for (i in 15:18) {
  u <- dbinom(i,50,0.5)
  toplam=toplam+u
}
print(toplam)

#12.ders
x <- rexp(100,rate = 3)
ks.test(x,"pnorm")
ks.test(x,"pexp",3) #bizim ürettiğimiz x değerleri kolmogorov 
# ile test ettiğimizde bu dağılımın üstel dağılımından geldiğini test ederiz

y <- rnorm(100)
ks.test(y,"pnorm") #standart normal dağılımlı mean=0,sd=1 (bizim testimiz normal dağılımdan geliyor)
ks.test(y,"pnorm",mean=3,sd=0.2)

#lilliefors testi için nortest paketinin yüklü olması gerekir
lillie.test(y)
z <- rnorm(100,mean = 5,sd=0.5)
lillie.test(z) #p>0.5 normal dağılımlıdır

ks.test(z,"pnorm",mean=5,sd=0.5) 
# örneklem<50 =shapiro.test örneklem>50 lillie.test

shapiro.test(x) #üretilen x değerleri normal dağılımdan gelmemektedr(p<0.5)
shapiro.test(y)

#varyansların homojenliği Levene Testi
iris <- read.csv("iris.csv",header=T)
leveneTest(SepalLengthCm~Species,data=iris)
#p<0.5 varyanslar homojen değildir
leveneTest(SepalWidthCm~Species,data=iris)
leveneTest(SepalWidthCm~Species,data=iris,center=mean)#Daha doğru default olarak medyana bakıyor

erkek <- c(56,58,59,77,81,90)
kız <- c(62,63,64,67,77,93)
veri <- c(erkek,kız)
grup <- c(rep("e",length(erkek)),rep("k",length(kız)))
bveri<- data.frame(veri,grup)
leveneTest(veri~grup,data=bveri,center=mean)

#En az bir tanesi normallik sağlamıyorsa parametrik olmayan
