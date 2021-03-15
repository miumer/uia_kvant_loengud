# Sotsiaalse analüüsi meetodid: kvantitatiivne lähenemine (SVUH.00.087)
# Logistiline regressioonanalüüs
# Ave Roots
# Ühiskonnateaduste instituut
# Tartu ülikool
# 12.03.2021


library(TAM)
library(ggpubr)
library(ggplot2)
library(forcats)
library(olsrr)
library(ggeffects)
library(summarytools)


# Lineaarne regressioon


#Palun koostage regessioonimudel, kus netosissetulekut (netinum) prognoositakse soo, vanuse, hariduse, töötundide, rahvusvähemusse kuulumise ja elukoha regiooni järgi?

# Laeme alla andmed

library(essurvey)

set_email("your@email.com")


essee18 <-
  import_country(
    country = "Estonia",
    rounds = c(9)
  )


# Vaatame sõltuva tunnuse jaotust

hist(essee18$netinum)

hist(log(essee18$netinum+0.1))

hist(sqrt(essee18$netinum))

# Vaatame asümmeetriakordajat

weighted_skewness(essee18$netinum, essee18$pspwght)

weighted_skewness(log(essee18$netinum+0.1), essee18$pspwght)

weighted_skewness(sqrt(essee18$netinum), essee18$pspwght)


# Vaatame kvantiilide joonist

ggqqplot(essee18$netinum, weights=essee18$pspwght)

ggqqplot(log(essee18$netinum + 0.1), weights=essee18$pspwght)


ggqqplot(sqrt(essee18$netinum), weights=essee18$pspwght)


# Võtame sõltuvast tunnusest ruutjuure

essee18$juursiss <- sqrt(essee18$netinum)

# kodeerime ümber hariduse tunnuse

essee18$Haridus <- car::recode(as.numeric(essee18$edulvlb), "0:213='Kuni_põhi'; 229='Kutse'; 313='Kesk'; 321:423='Kutse'; 520:800='Kõrg'; 5555=NA")

essee18$Haridus <- fct_relevel(essee18$Haridus, "Kuni_põhi")

# Kodeerime ümber regiooni tunnuse


essee18$Regioon <- fct_recode(essee18$region, Pohja="EE001", Laane="EE004", Kesk="EE006", Kirde="EE007",
                              Louna="EE008")

table(essee18$Regioon, essee18$region)


table(essee18$Regioon)

# Kodeerime ümber rahvusvähemuse tunnuse

essee18$Rahvusvahemus <- car::recode(as.numeric(essee18$blgetmg), "1='jah'; 2='ei'")

# võtame vanuse tunnuse ruutu

essee18$vanusruut <- essee18$agea * essee18$agea

essee18$vanuskuup <- essee18$agea * essee18$agea * essee18$agea

# Teeme lineaarse regressioonimudeli


sissmudel <- lm(juursiss ~ agea + vanusruut + vanuskuup + Haridus + wkhtot + Rahvusvahemus + Regioon, data=essee18, weights=pspwght)

summary(sissmudel)

# Kontrollime eeldusi

par(mfrow=c(2,2))

plot(sissmudel)

par(mfrow=c(1,1))


plot(sissmudel, 4)

# Kontrollime multikollineaarsust

ols_vif_tol(sissmudel)



## Binaarne logistiline regressioon


# teeme sissetuleku tunnuse kaheväärtuseliseks

#install.packages('sjmisc')

#library(sjmisc)

#essee18 <- dicho(essee18, netinum)

#frq(essee18, netinum_d)

#essee18$siss41 <- split_var(essee18$netinum, n = 4)

#frq(essee18$siss41)

#freq(essee18$siss41, weights=essee18$pspwght)

descr(essee18$netinum, weight=essee18$pspwght)

essee18$siss2 [essee18$netinum <= 600] <- 0
essee18$siss2 [essee18$netinum > 600] <- 1

freq(essee18$siss2, weights=essee18$pspwght)


# teeme binaarse logistilise regressioonimudeli

siss_bilog <- glm(siss2 ~ agea + vanusruut + Haridus + wkhtot + Rahvusvahemus + Regioon, family=binomial(link="logit"), data=essee18, weights=pspwght)


summary(siss_bilog)

exp(coefficients(summary(siss_bilog)))

library(DescTools)

PseudoR2(siss_bilog, which=c("Nagelkerke", "CoxSnell", "McFadden"))


## Multinomiaalne logistiline regressioon


# Teeme sõltuva tunnuse 4-väärtuseliseks

install.packages('reldist')
library(reldist)

wtd.quantile(essee18$netinum, q=0.25 , weight= essee18$pspwght)



essee18$siss4 <- cut(essee18$netinum, 
                   breaks=c(0,420,600,1050,Inf), 
                   labels=c("I","II","III","IV"))

freq(essee18$siss4, weights=essee18$pspwght)

install.packages('nnet')

library(nnet)

sisse_multilog <- multinom(siss4 ~ agea + Haridus + wkhtot + Rahvusvahemus + Regioon, weights=pspwght, data=essee18)


summary(sisse_multilog)

exp(coef(sisse_multilog))



library(stargazer)

stargazer(sisse_multilog, type="text", dep.var.labels=c("II", "III", "IV"), 
          covariate.labels=c("Vanus", "Kesk", "Kutse", "Kõrg", "Töötunnid","Rahvusvähemus", "Lääne-Eesti", "Kesk-Eesti", "Kirde-Eesti", "Lõuna-Eesti", "Vabaliige"),
          out="siss4.txt")

PseudoR2(sisse_multilog, which=c("Nagelkerke", "CoxSnell", "McFadden"))


## Ordinaalne regressioon

install.packages('MASS')

library(MASS)

siss_ordreg <- polr( siss4 ~ agea + Haridus + wkhtot + Rahvusvahemus + Regioon, weights=pspwght, data = essee18, Hess=TRUE)

summary(siss_ordreg)

(sissordtabel <- coef(summary(siss_ordreg)))


p <- round(pnorm(abs(sissordtabel[, "t value"]), lower.tail = FALSE) * 2, 3)



(sissordtabel <- cbind(sissordtabel, "p value" = p))

sissordtabel

exp(coef(summary((siss_ordreg))))

(sissordtabel1 <- exp(coef(summary(siss_ordreg))))

(sissordtabel1 <- cbind(sissordtabel1, "p value" = p))


PseudoR2(siss_ordreg, which=c("Nagelkerke", "CoxSnell", "McFadden"))


# Ülesanne iseseisvaks lahendamiseks

#Ülesanne 1

#Kas eluga rahulolu (stflife) võiks olla seotud vanuse (agea), soo (gndr), leibkonna toimetuleku (hincfel) ja tervisega (health). Palun lahendage see ülesanne erinevate regressioonimudelitega (lineaarne, binaarge logistiline, multinomiaalne logistiline, ordinaalne)!

Tunnuste kohta leiate infot:
  
https://www.europeansocialsurvey.org/docs/round9/survey/ESS9_appendix_a7_e03_1.pdf

https://www.yti.ut.ee/sites/default/files/www_ut/ess9_ee_questionnaire.pdf







