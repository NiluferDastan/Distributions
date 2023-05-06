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
