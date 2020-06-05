# Zadanie 1.

# a)  
# i)
n = length(LakeHuron)
y1 = cut(LakeHuron, sqrt(n), include.lowest = TRUE)
rowne_dlugosci = table(y1)
rowne_dlugosci
# ii)
kwantyle = quantile(LakeHuron, c(0, 0.2, 0.4, 0.6, 0.8, 1))
y2 = cut(LakeHuron, kwantyle, include.lowest = TRUE)
rowne_licznosci = table(y2)

# b)
szereg = table(cut(LakeHuron, sqrt(length(LakeHuron)), include.lowest = TRUE))
szereg_s = names(sort(szereg, decreasing = TRUE))
dominanta = szereg_s[1]
dominanta
# Zadanie 2.

# a)
n = length(Indometh$conc)

# b)
przedzial = range(Indometh$conc)

# c)
srednia = mean(Indometh$conc)
mediana = median(Indometh$conc)
summary(Indometh$conc)

# komentarz: srednia wynikow jest wyzsza od mediany, co oznacza, ze wartosc cechy wiekszosci 
# jednostek statystycznych jest po lewej stronie od wartosci przecietnej

# d)  
# mamy do czynienia z rozkładem symetrii prawostronnej, dla pewnosci mozemy 
# jeszcze wyznaczyc przedzial dominanty:
  
tab = sort(table(cut(Indometh$conc, sqrt(length(Indometh$conc)), include.lowest = TRUE)), decreasing = TRUE)
dominanta = tab[1]

# widzimy zaleznosc srednia > mediana > dominanta
# do tego wspolczynnik skosnosci:
   
library(moments)
wsp_skos = skewness(Indometh$conc)
    
# jest dodatni.

# e)  
srednia_50proc = mean(Indometh$conc, trim = 0.25)
    
# f)  
kurtoza = kurtosis(Indometh$conc)
# Kurtoza jest wieksza od zera, co oznacza, ze dane sa bardzo skoncentrowane wokol sredniej, 
# a rozklad jest bardziej wysmukly od standardowego normalnego

# g)  
rozstep_miedzykwantylowy = IQR(Indometh$conc)
rozstep_miedzykwantylowy

# h)  
ponizej_30proc = quantile(Indometh$conc, c(0.3))

# i)  
powyzej_80proc = quantile(Indometh$conc, c(0.8))

# Zadanie 3.

setosa_length = iris$Sepal.Length[iris$Species=="setosa"]
versicolor_length = iris$Sepal.Length[iris$Species=="versicolor"]
virginicar_length = iris$Sepal.Length[iris$Species=="virginica"]

# analiza:

n_setosa = length(setosa_length)
n_versicolor = length(versicolor_length)
n_virginicar = length(virginicar_length)

var(setosa_length)
var(versicolor_length)
var(virginicar_length)

sd(setosa_length)
sd(versicolor_length)
sd(virginicar_length)

range_setosa = range(setosa_length)
range_versicolor = range(versicolor_length)
range_verginicar = range(virginicar_length)

library(moments)

kurtoza_setosa = kurtosis(setosa_length)
kurtoza_versicolor = kurtosis(versicolor_length)
kurtoza_virginicar = kurtosis(virginicar_length)
wsp_skos_setosa = skewness(setosa_length)
wsp_skos_versicolor = skewness(versicolor_length)
wsp_skos_virginicar = skewness(virginicar_length)

summary(setosa_length)
summary(versicolor_length)
summary(virginicar_length)

# Zdecydowanie srednia dlugosc jest najwieksza w przypadku gatunku Virginicar, natomiast
# najmniejsza dla gatunku setosa
# We wszystkich przypadkach mamy dodatnie wratoci wspolczynnika skosnosci, zatem
# mamy doczynienia z prawostronna asymetria, natomiast nieznaczna. Swiadczy o tym rowniez
# mala roznica miedzy srednia a mediana.
# Kurioza natomiast jest mniejsza niz 3 we wszystkich przypadkach, wiec dane sa mniej
# skoncentrowane niz w przypadku rozkladu normalnego.
