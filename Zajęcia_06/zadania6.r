# Zadanie.1

liczba_ocen_ndst = c(14, 18, 28, 12, 4, 22, 14, 16, 10, 8, 18, 6, 12)

test_ndst = chisq.test(liczba_ocen_ndst)
test_ndst$p.value

# p < alfa, odrzucamy H0

# Zadanie.2

bledy = read.table("bledy.txt")
srednia = mean(bledy$x)
opis = names(table(bledy))
y1 = c()

for(i in 1:length(opis)) {
  y1[i] = strtoi(opis[i], 10L)
}

y2 = c()

for(i in 1:length(opis)) {
  y2[i] = dpois(y1[i], srednia)
}

chisq.test(table(bledy), p=y2, rescale.p=T)

# p < alfa, odrzucamy H0

# Zadanie.3

random_val_E2 = c(0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.06, 0.11, 0.11, 0.16, 0.18, 0.22, 0.24, 0.26, 0.27, 0.36, 0.44, 0.46, 0.46, 0.60, 0.65, 0.70, 0.80, 0.85, 0.90, 0.95, 1.20, 1.50, 2.00)

ks.test(random_val_E2, "pexp", 2)

# p > alfa, nie ma podstaw do odrzucenia Ho
# w tescie Kolmogorowa-Smirnowa nie powinno być powtorzen w danych

# Zadanie.4

dane = c(4.5, 3.6, 6.0, 6.4, 7.9, 6.9, 6.1, 7.4, 9.0, 4.3, 6.1, 8.2, 4.9, 7.5, 5.8)

# do N(0, 1)
dane = dane - 6.3
dane = dane / 1.5

shapiro.test(dane)

# p > alfa, nie odrzucamy H0

# Zadanie.5 

zmienne = read.table("zmienne.txt")
zmienna1 = zmienne$zmienna1
zmienna2 = zmienne$zmienna2
zmienna3 = zmienne$zmienna3

# a)
ks.test(zmienna1, zmienna2)
ks.test(zmienna1, zmienna3)
ks.test(zmienna2, zmienna3)

# w kazdym tescie p-value jest bardzo mala, wiec zadna para zbiorow zmiennych nie ma tego 
# samego rozkladu

# b)
shapiro.test(zmienna1)
# wysoka p-value, zatem zmienna1 ma rozklad normalny

ks.test(zmienna2, "pexp", 3.4)
# wysoka p-value, zatem zmienna2 ma rozklad wykladniczy E(3.4)

# dla zmienna3 nie udalo mi sie wyznaczyc rozkladu, nie jest to rozklad:
# - normalny
# - wykladniczy
# - jednostajny
