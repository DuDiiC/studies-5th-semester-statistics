# Zadanie 1.
tabela = read.csv("Roczniaki.csv", header=TRUE, sep = ";", dec=',');

# a) 	
tabela[1:6,]

# b) 	
tabela[,4] 
# lub 
tabela["Waga.dziecka..w.kg."]

# c) 	
wzrost = tabela$Wzrost.dziecka.w.cm / 100
waga = tabela$'Waga.dziecka..w.kg.'
ilosc_dzieci = length(tabela[,1])
bmi = waga/(wzrost*wzrost)

# d)	
sort(bmi, decreasing = TRUE)

# Zadanie 2.

library(foreign)

dane = read.spss("Employee data.sav", to.data.frame = TRUE, add.undeclared.levels = "no")
ilosc_rekordow = length(dane[,1])

# a)	
pensja = dane$salary
poziom_zarobkow = rep(0, length.out = ilosc_rekordow)
	
for(i in 1:ilosc_rekordow) {
	if(pensja[i] <= 25000)
		poziom_zarobkow[i] = "niskie"
	else if(pensja[i] > 35000)
		poziom_zarobkow[i] = "wysokie"
	else poziom_zarobkow[i] = "przecietne"
}
poziom_zarobkow
	
# b)	
table(poziom_zarobkow)

# c)
write.table(poziom_zarobkow, file = "zarobki.txt", sep = ";")

# Zadanie 3.

kx = function(x, k) {

	n = length(x)
	avg = mean(x)
	sum = 0

	for(i in 1:n) {
		sum = sum + ((x[i] - avg)^k)
	}
	return(sum * (1/n))
}

