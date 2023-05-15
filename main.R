#Wasserbedarf denfinieren pro Pflanze
aloevera_wassererhalt <- rpois(100, lambda = 4)
baumfreund_wassererhalt <- rpois(1000, lambda = 12)

#Sonnenstunden
2022 <- c(84,120,	241,	201,	239,	263,	323,	278,	146,	112,	50,	27,	2085)
2021 <- c(32, 103,	169,	221,	170,	240,	175,	162,	215,	122,	31,	31,	1669)

N <- 1000
c(rep(NA,N))
N <- 100
Wohlfühlscore_aloevera <- c(rep(NA,N))
gesamt <- 
for (i in 1:N){
  for (monat in 1:12){
    sonnenstunden_aloevera <- rnorm(30)
    jahreszeit <- ifelse(monat <= 6, "sommer", "winter")
    #jahreszeit <- sample(x = c("sommer", "winter"), size = 1)
    aloevera_wassererhalt <- rpois(1, lambda = 4)
    baumfreund_wassererhalt <- rpois(1, lambda = 12)
    if (jahreszeit == "sommer"){
      ifelse(aloevera_wassererhalt == 4, Wohlfühlscore_aloevera [i]<- 10, Wohlfühlscore_aloevera [i] <- 5)}
    else {
      ifelse(aloevera_wassererhalt == 2, Wohlfühlscore_aloevera [i]<- 9, Wohlfühlscore_aloevera [i] <- 4)
    }
  }
  gesamt 
}
Wohlfühlscore_aloevera
mean (Wohlfühlscore_aloevera)
