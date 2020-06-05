# Zadanie.1

niedziele = c(2.9, 3.3, 3.2, 3.2, 3.2, 3.0, 2.9, 3.1)

#a)

testA = t.test(niedziele, mu=3.2, alternative="l")
testA$p.value

# p < alfa, odrzucamy H0

#b)

testB = t.test(niedziele, mu=3.2)
testB$p.value

# p > alfa, nie odrzucamy H0

# Zadanie.2

zapalki = read.table("zapalki.txt", sep=" ")
zapalki = zapalki$x

T = sqrt(length(zapalki))*(mean(zapalki)-64)/sd(zapalki)
qnorm(0.95)

# T nalezy do obszaru krytycznego, zatem odrzucamy H0

#lub p-value

t.test(zapalki, mu=64, alternative="g")

# p < alfa, odrzucamy H0

# Zadanie.3

wyprane_platkami = c(74.4, 75.1, 73.0, 72.8, 76.2, 74.6, 76.0, 73.4, 72.9, 71.6)
wyprane_plynem = c(56.8, 57.8, 54.6, 59.0, 57.1, 58.2, 57.6)

test_prania = t.test(wyprane_platkami, wyprane_plynem, paired = FALSE, alternative="g")
test_prania$p.value

# p < alfa, odrzucamy H0 na rzecz H1 - platki piora lepiej niz plyn

# Zadanie.4

tramwaje_sroda = read.table("tramwaje_sroda.txt", sep=" ")
predkosc_sroda = tramwaje_sroda$x

tramwaje_niedziela = read.table("tramwaje_niedziela.txt", sep=" ")
predkosc_niedziela = tramwaje_niedziela$x

test_predkosci = t.test(predkosc_sroda, predkosc_niedziela, alternative="l")
test_predkosci$p.value

# p < alfa, odrzucamy H0

# Zadanie.5

cisnienie_przed = c(210, 180, 260, 270, 190, 250, 180, 200)
cisnienie_po = c(180, 160, 220, 260, 200, 230, 180, 190)

test_cisnienia = t.test(cisnienie_przed, cisnienie_po, paired=T, alternative = "g")
test_cisnienia$p.value

# p < alfa, odrzucamy H0

# Zadanie.6 

sondaz_tab = read.table("sondaz.txt")
sondaz = sondaz_tab$x
wynik = 0
for(i in 1:length(sondaz)) {
  if(sondaz[i] == 'nie')
    wynik = c(wynik, 1)
  else wynik = c(wynik, 0)
}
test_sondaz = t.test(wynik, mu=0.99)
test_sondaz$p.value

# p > alfa, nie odrzucamy H0