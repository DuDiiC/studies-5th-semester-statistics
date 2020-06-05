##KOLOKWIUM##
#zad1
a = file.choose()
b = read.table(a, header = TRUE)

reszta = b[,4] - (52*b[,5])
reszta
table(reszta)
mean(reszta) #42836.29
median(reszta) #43014
sd(reszta)  #20709.55
min(reszta) #64
max(reszta) #91683
IQR(reszta) #30890
#widzimy tutaj ze srednia nie rozni sie zbytnio od mediany, 
#zatem nie mamy wielu obserwacji skrajnych, lecz s¹ one doœæ mocno
#rozrzucone (wszystkie obserwacje), o czym œwiadczy nam odchylenie 
#standardowe. Wartoœci¹ najmniejsza jest 64, zaœ najwiêksz¹ 91683.
#Rozstep miedzykwartylowy równie¿ mówi nam o tym, ¿e zró¿nicowanie
#naszej cechy reszta jest du¿e.
install.packages("modes") #dominanty nie ma
library(modes)
modes(reszta)


#zad2
quantile(reszta,0.6)
#50305.2

#zad3
library(moments)
skewness(reszta) #-0.007957
                #niby jest <0, ale nie wiem czy tutaj bardziej symetryczny 
                #czy DELIKATNIE lewostronnie skoœny. Jest tu rz¹d tysiêcznych

#zad4
kurtosis(reszta) 
# mezokurtyczne - wartoœæ kurtozy wynosi 0, sp³aszczenie rozk³adu 
  #jest podobne do sp³aszczenia rozk³adu normalnego (dla którego 
  #kurtoza wynosi dok³adnie 0)
#leptokurtyczne - kurtoza jest dodatnia, wartoœci cechy bardziej
  #skoncentrowane ni¿ przy rozk³adzie normalnym
#platokurtyczne - kurtoza jest ujemna, wartoœci cechy mniej 
  #skoncentrowane ni¿ przy rozk³adzie normalnym

#zad5 
boxplot(reszta~b[,2])


#zad7
  #Hipoteza zerowa: Badana zmienna ma rozk³ad normalny.
  #Hipoteza alternatywna: Badana zmienna ma rozk³ad inny ni¿ normalny.
alfa = 0.05
dochod = b[,4]

mean(dochod)
var(dochod)

quantile(dochod, 1/3)
qnorm(1/3)
ks.test(dochod,pnorm,exact=TRUE)
# p-value = 1.665e-15 
#p<alfa - odrzucanie h0 i przyjecie h1
#zatem badana zmienna ma rozklad inny ni¿ normalny

#zad8
alfa = 0.05
h0 = 68400 #srednia wartosc zmiennej dochod wynosi 68400
#h1 srednia wartosc zmiennej dochod jest mniejsza niz 68400
t.test(dochod, mu=68400, alternative = 'l')
#p-value = 0.0426
#zatem 0.0425 < alfa odrzucamy h0 i przyjmujemy h1, 
#Srednia wartosc zmiennej dochod jest mniejsza niz 68400
