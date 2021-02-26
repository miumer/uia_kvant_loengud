# Lineaarne regressioonanalüüs 
# Sotsiaalse analüüsi meetodid: kvantitatiivne lähenemine (SVUH.00.087)
# Ave Roots
# Ühiskonnateaduste instituut
# Tartu Ülikool
# 26.02.2021

#Tõmbame alla ESS 2018. aasta Eesti andmed

library(essurvey)
library(summarytools)
library(GGally)
library(psych)
library(ggpubr)
library(ggplot2)
library(TAM)
library(forcats)
library(olsrr)
library(car)
library(xlsx)
library(broom)


set_email("siimpoldre@gmail.com")


essee18 <-
  import_country(
    country = "Estonia",
    rounds = c(9)
  )


# Ülesanne 1
# Kuidas on seotud rahulolu riigi institutsioonidega vanuse ja sooga?


# Loome institutsionaalse usalduse indeksi


instrahul <- na.omit(subset(essee18, select=c(stfeco, stfgov, stfdem, stfedu, stfhlth, gndr)))

str(instrahul)

summary(instrahul)

descr(instrahul)

hist(instrahul$stfeco)

hist(instrahul$stfgov)

hist(instrahul$stfdem)

hist(instrahul$stfedu)

hist(instrahul$stfhlth)

instrahul$gndr <- as.factor(instrahul$gndr)

ggpairs(instrahul)


# Teeme andmestiku ainult rahulolu tunnustega

instrahul1 <- subset(essee18, select=c(stfeco, stfgov, stfdem, stfedu, stfhlth))

cor(instrahul1)

alpha(instrahul1)

essee18$indekssum <- as.numeric(instrahul1$stfeco) + as.numeric(instrahul1$stfgov) + as.numeric(instrahul1$stfdem) + as.numeric(instrahul1$stfedu) + as.numeric(instrahul1$stfhlth)

hist(essee18$indekssum)

weighted_skewness(essee18$indekssum, essee18$pspwght)

weighted_kurtosis(essee18$indekssum, essee18$pspwght)

ggqqplot(essee18$indekssum, weights=essee18$pspwght)


# Teeme loodud indeksiga lineaarse regressioonimudeli


indmudel <- lm(indekssum ~ agea, data=essee18, weights=pspwght)

indmudel

summary(indmudel)


# Kontrollime eeldusi

par(mfrow=c(2,2))

# Siit alates ei toimi

plot(indmudel)

par(mfrow=c(1,1))


plot(indmudel, 4)



ggplot(essee18, aes(x=agea, y=indekssum))+ # andmestik ja mis tunnus x ja mis y teljel
  geom_point() +                         # joonise tüüp
  geom_smooth(method="lm")+ # joonistame regressioonijoone, lm - linear model
  stat_regline_equation()+  # paneb regressiooijoone valemi
  labs(x='Vanus', y='Institutsioonidega rahulolu')


# lisame mudelisse hariduse

essee18$Haridus <- car::recode(as.numeric(essee18$edulvlb), "0:213='Kuni_põhi'; 229='Kutse'; 313='Kesk'; 321:423='Kutse'; 520:800='Kõrg'; 5555=NA")

essee18$Haridus <- fct_relevel(essee18$Haridus, "Kuni_põhi", "Kutse")

table(essee18$edulvlb, essee18$Haridus)


indmudel2 <- lm(indekssum ~ agea + Haridus, data=essee18, weights=pspwght)

summary(indmudel2)

# Kontrollime eeldusi

par(mfrow=c(2,2))

plot(indmudel2)

par(mfrow=c(1,1))


plot(indmudel2, 4)


# multikollineaarsus 

ols_vif_tol(indmudel2)


# Lisame mudelisse eluga rahulolu


indmudel3 <- lm(indekssum ~ agea + Haridus + stflife, data=essee18, weights=pspwght)

summary(indmudel3)

# Kontrollime eeldusi

par(mfrow=c(2,2))

plot(indmudel3)

par(mfrow=c(1,1))


plot(indmudel2, 4)


# multikollineaarsus 


ols_vif_tol(indmudel3)


# Lisame kuulumise rahvusvähemusse


essee18$Rahvusvahemus <- car::recode(as.numeric(essee18$blgetmg), "1='jah'; 2='ei'")


table(essee18$Rahvusvahemus, essee18$blgetmg)


indmudel4 <- lm(indekssum ~ agea + Haridus + as.numeric(stflife) + Rahvusvahemus, data=essee18, weights=pspwght)


summary(indmudel4)

# Kontrollime eeldusi

par(mfrow=c(2,2))

plot(indmudel4)

par(mfrow=c(1,1))


plot(indmudel2, 4)


# multikollineaarsus 


ols_vif_tol(indmudel4)


# Leiame standardiseeritud regressioonikoefitsiendid

install.packages("lm.beta")

library(lm.beta)

lm.beta(indmudel4)

summary(lm.beta(indmudel4))



# Salvestame tulemused

# Tabelina

install.packages('stargazer')

library(stargazer)

stargazer(indmudel2, indmudel4, type="text", out="indeks_reg1.txt")

stargazer(indmudel2, indmudel4, type="text", dep.var.labels="Riigiinstitutsioonidega rahulolu", 
          covariate.labels=c("Vanus","Kutseharidus","Keskharidus","Kõrgharidus", "Eluga rahulolu", "Vähemusrahvus", "Vabaliige"), out="indeks_reg2.txt")




# Salvestame viimase mudeli excelisse


indeksimud <- tidy(indmudel4) #paneme õigesse formaati

indeksimud <- as.data.frame(indeksimud) # teeme andmetabeliks

write.xlsx(indeksimud, "indeks_reg.xlsx") 


# Teeme joonise

install.packages('ggeffects')

library(ggeffects)

dat <- ggpredict(indmudel4, terms = c("stflife", "Rahvusvahemus"))


intern <- plot(dat) + labs(title="", x="Eluga rahulolu", y="Rahulolu riigiinstitutsioonidega")



intern

# Koosmõjudega mudel


indmudel5 <- lm(indekssum ~ agea * Haridus + as.numeric(stflife) + Rahvusvahemus, data=essee18, weights=pspwght)

summary(indmudel5)

dat2 <- ggpredict(indmudel5, terms = c("agea", "Haridus"))

intern2 <- plot(dat2) + labs(title="", x="Vanus", y="Rahulolu riigiinstitutsioonidega")


intern2

# Salvestame joonise

ggsave("instrahul.jpg", plot = intern2, device = "jpg", 
       width = 12, height = 8, units = "in", dpi = 600)



# Ülesanne 2

#Palun koostage regessioonimudel, kus netosissetulekut (netinum) prognoositakse soo, vanuse, hariduse, töötundide, rahvusvähemusse kuulumise ja elukoha regiooni järgi?

# Vaatame sõltuva tunnuse jaotust

hist(essee18$netinum)

hist(log(essee18$netinum+0.1))

hist(sqrt(essee18$netinum))

# Vaatame asümmeetriakordajat

weighted_skewness(essee18$netinum, essee18$pspwght)

weighted_skewness(log(essee18$netinum+0.1), essee18$pspwght)

weighted_skewness(sqrt(essee18$netinum), essee18$pspwght)


# Vaatame järskuskordajat

weighted_kurtosis(essee18$netinum, essee18$pspwght)

weighted_kurtosis(log(essee18$netinum+0.1), essee18$pspwght)

weighted_kurtosis(sqrt(essee18$netinum), essee18$pspwght)


# Vaatame kvantiilide joonist

ggqqplot(essee18$netinum, weights=essee18$pspwght)

ggqqplot(log(essee18$netinum + 0.1), weights=essee18$pspwght)


ggqqplot(sqrt(essee18$netinum), weights=essee18$pspwght)


# Võtame sõltuvast tunnusest ruutjuure

essee18$juursiss <- sqrt(essee18$netinum)



# Kodeerime ümber regiooni tunnuse


essee18$Regioon <- fct_recode(essee18$region, Pohja="EE001", Laane="EE004", Kesk="EE006", Kirde="EE007",
                              Louna="EE008")

table(essee18$Regioon, essee18$region)


table(essee18$Regioon)

# Teeme lineaarse regressioonimudeli

essee18$vanusruut <- essee18$agea * essee18$agea

summary(lm(juursiss ~ agea + vanusruut + Haridus + wkhtot + Rahvusvahemus + Regioon, data=essee18, weights=pspwght))

sissmudel <- lm(juursiss ~ agea + vanusruut + Haridus + wkhtot + Rahvusvahemus + Regioon, data=essee18, weights=pspwght)

# Kontrollime eeldusi

par(mfrow=c(2,2))

plot(sissmudel)

par(mfrow=c(1,1))


plot(sissmudel, 4)

# Kontrollime multikollineaarsust

ols_vif_tol(sissmudel)


# Teeme joonise


dat3 <- ggpredict(sissmudel, terms = c("wkhtot", "Haridus"))


joonississ <- plot(dat3) + labs(title="", x="Vanus", y="Sissetulek (ruutjuur)")

joonississ

# koosmõjudega

sissmudel2 <- lm(juursiss ~ agea + Haridus + vanusruut + Rahvusvahemus + wkhtot*Regioon, data=essee18, weights=pspwght)

summary(sissmudel2)

dat4 <- ggpredict(sissmudel2, terms = c("wkhtot", "Regioon"))


joonississ2 <- plot(dat4) + labs(title="", x="Töötunnid", y="Sissetulek (ruutjuur)")

joonississ2




'# Ülesanded iseseisvaks lahendamiseks


# Ülesanne 3. 
# Kas see, kui palju aega inimene tavalisel päeval veedab internetti kasutades arvutis, tahvelarvutis, nutitelefonis või muus seadmes (ankeedis küsimus A2, lk 2, andmestikus netustm) võiks olla seotud tema vanusega(andmestikus tunnus agea)? Palun kasutage Euroopa Sotsiaaluuringu Eesti 2018. aasta andmeid!


# Ülesanne 4. 
# Palun koostage regressioonimudel, kus uurite eluga rahulolu (Küsimus B27, lk 9, tunnus stflife) seost hinnanguga leibkonna toimetulekule (Küsimus F 42, lk 52, tunnus hincfel), tervisehinnanguga (küsimus C7 ankeedis lk 16, tunnus health), sooga (Küsimus F2, lk 42, tunnus gndr), vanusega (agea) ja haridusega (kasutame eelmises ülesandes ümberkodeeritud tunnust). Palun kasutage Euroopa Sotsiaaluuringu Eesti 2018. aasta andmeid!




