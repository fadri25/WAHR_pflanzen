
#Wasserbedarf denfinieren pro Pflanze
aloevera_wassererhalt <- rpois(100, lambda = 4)
baumfreund_wassererhalt <- rpois(1000, lambda = 12)

#Sonnenstunden von Januar bis Dezember und gesamt in Kloten, ZH
jahr2022 <- c(84,120,	241,	201,	239,	263,	323,	278,	146,	112,	50,	27,	2085)
jahr2021 <- c(32, 103,	169,	221,	170,	240,	175,	162,	215,	122,	31,	31,	1669)
jahr2020 <- c(90, 101,	172,	291,	246,	189,	292,	223,	193,	69,	64,	16,	1945)

combined <- cbind(jahr2020, jahr2021, jahr2022)
combined[1,]
mean (combined[12,])

N <- 100
Wohlfühlscore_aloevera <- c(rep(NA,N))
gesamt <- c(rep(NA,N))

for (i in 1:N){
  sonnenstunde_matrix <- cbind(jahr2020, jahr2021, jahr2022)
  for (monat in 1:12){
    #pro Monat wird die durchschnittliche Anzahl Sonnenstunden berechnet uafgrund von der matrix
    mean_sonnenstunden <- mean (sonnenstunde_matrix[monat,])
    sonnenstunden_aloevera <- rnorm(30, mean = mean_sonnenstunden, sd = 10)
    mean (sonnenstunden_aloevera)
    jahreszeit <- ifelse((monat < 6), "sommer", "winter")

    aloevera_wassererhalt <- rpois(1, lambda = 4)
    baumfreund_wassererhalt <- rpois(1, lambda = 12)
    
    #wenn es Sommer ist
    if (jahreszeit == "sommer"){
      # 4 ist optimal im Sommer
      if(aloevera_wassererhalt == 4) Wohlfühlscore_aloevera [i]<- 10
      #wenn erhalt 5 oder 3 ist
      else if (aloevera_wassererhalt == 5 | aloevera_wassererhalt == 3) Wohlfühlscore_aloevera [i] <- 7
      else Wohlfühlscore_aloevera [i] <- 4}
    
    #wenn es winter ist, hat es anderen Wsserbedarf
    else {
      # 2 ist optimal im Winter
      if(aloevera_wassererhalt == 2) Wohlfühlscore_aloevera [i]<- 10
      #wenn erhalt 1 oder 3 ist -> halbwegs gut für Pflanze
      else if (aloevera_wassererhalt == 1 | aloevera_wassererhalt == 3) Wohlfühlscore_aloevera [i] <- 7
      else Wohlfühlscore_aloevera [i] <- 4}
  durchschnitt <- mean (Wohlfühlscore_aloevera)
  gesamt [i] <- durchschnitt
  }
  
}
gesamt
mean (gesamt)
