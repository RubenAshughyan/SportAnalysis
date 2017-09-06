install.packages("devtools")
library(devtools)
library(ggplot2)
install_github("Habet/CSE270")
library(SportsAnalytics270)
help(package="SportsAnalytics270")
library(SportsAnalytics270)
View(f_data_sm)

#ԼԱ ԼԻԳԱ	16.09   18:15	ԽԵՏԱՖԵ	- : -	ԲԱՐՍԵԼՈՆԱ

Spain_2017 <- f_data_sm[f_data_sm$COUNTRY=="Spain" & f_data_sm$SEASON==2017 ,]

fttg <- Spain_2017$FTTG
ggplot(fttg, aes(x=FTTG))+geom_histogram(binwidth = 0.5)

summary(fttg)

l = 2.942

table(fttg)


rel_fr <- as.vector(table(fttg))


rel_fr <- rel_fr/sum(rel_fr)

#------------------------------------

g <-   f_data_sm[
  f_data_sm$COUNTRY == "Spain" &
  f_data_sm$SEASON == 2016 &
  f_data_sm$HOMETEAM == "Getafe",
  "FTHG"
  ] 


b <-   f_data_sm[
  f_data_sm$COUNTRY == "Spain" &
    f_data_sm$SEASON == 2016 &
    f_data_sm$AWAYTEAM == "Barcelona",
  "FTHG"
  ] 


g
summary(g)
summary(b)

g_l <- 1.211
b_l <- 0.7895

goal_probs_getafe <- dpois(c(0:10), lambda = g_l)
goal_probs_barca <-  dpois(c(0:10), lambda = b_l)

goal_probs_getafe

options(scipen = 999, digits = 3)


M <- goal_probs_getafe %*% t(goal_probs_barca)

print(round(M,digits = 4))



sum(diag(M))
sum(M[lower.tri(M,diag = F)])
sum(M[upper.tri(M,diag = F)])


