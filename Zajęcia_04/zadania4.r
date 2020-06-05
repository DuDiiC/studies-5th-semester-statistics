# Zadanie 1.

ankieta = read.csv("Ankieta.csv", header=T, sep=";")
attach(ankieta)

# a)
  boxplot(wiek~plec, horizontal=T)
# b)
  hist(cisnienie.skurczowe, main="Cisnienie skurczowe", col=rainbow(6))
# c)
  hist(cisnienie.skurczowe, main="Cisnienie skurczowe", col=rainbow(6), prob = T)
# d)
  barplot(table(plec))
# e)
  srednie_M = mean(cisnienie.skurczowe[plec == "M"])
  srednie_K = mean(cisnienie.skurczowe[plec == "K"])
# f)
  barplot(c(srednie_M, srednie_K), xlab = "", ylab = "srednie cisnienie skurczowe", col = rainbow(2))
    legend("bottom", c("mezczyzni", "kobiety"), cex=1, fill=rainbow(2))
    
# Zadanie 2.

# a)
  dane = read.csv("UOF_gs.txt", header=T, sep=" ")
  attach(dane)
  # i)  
    dane$gender = factor(dane$gender, levels=c(0, 1), labels=c("kobieta", "mezczyzna"))
  # ii) 
    dane$college = factor(dane$college, levels=c(1,2,3,4,5,6,7,8), 
		labels=c("rolnictwo", "architektura", "budownictwo", "administracja", 
		"lesnictwo", "pedagogika", "inzynieria", "sztuki piekne"))
  
library(ggplot2)
# a)  
	qplot(dane$salary, data = dane, fill=dane$gender, alpha=I(.5), main="Rozrzut pensji wzgledem plci", xlab="Pensja", ylab="Ilosc osob")  
    qplot(dane$salary, data = dane, fill=dane$gender, alpha=I(.5), main="Rozrzut pensji wzgledem plci", xlab="Pensja", ylab="Gestosc", geom="density")
    
# b)
	qplot(dane$college, data = dane, geom="bar", fill= dane$college, xlab="Kierunek", ylab="Liczba absolwentow", main="Liczba apsolwentow danego kierunku uczelni")

# c)  
	qplot(dane$salary, dane$college, data = dane, color=dane$gender, geom="point", alpha = I(.5), main="Zaleznosc ukonczonych studiow od zarobkow", xlab="Kierunek", ylab="Zarobki")

# d) 
	qplot(dane$salary, dane$college, data = dane, shape=dane$gender, color=dane$grad_date, geom="point", alpha = I(.5), main="Zaleznosc ukonczonych studiow od zarobkow", xlab="Kierunek", ylab="Zarobki")

