### Praktikum 6: Jadade analüüs
### Sotsiaalse analüüsi meetodid: kvantitatiivne lähenemine (SVUH.00.087)
### Ave Roots
### 23.04.2021
### Ühiskonnateaduste instituut
### Tartu Ülikool
###################
###################
###################

install.packages('TraMineR')
install.packages('TraMineRextras')
install.packages('factoextra')


library(foreign)
library(ggplot2)
library(TraMineR)
library(TraMineRextras)
library(factoextra)
library(cluster)
library(nnet)
library(stargazer)
library(essurvey)
library(readxl)
library(forcats)
library(car)


# Loeme sisse Euroopa Sotsiaaluuringu 2018. aasta Eesti andmed

set_email("your@email.com")


essee18 <-
  import_country(
    country = "Estonia",
    rounds = c(9)
  )



# ankeet
# https://www.yti.ut.ee/sites/default/files/www_ut/ess9_ee_questionnaire.pdf

# Tunnused, mida kasutame

#yrbrn - sünniaasta

#lvpntyr - aasta kui lahkus vanemate kodust (ankeedis lk 26-27)
#lvptnyr: aasta, mis esimest korda elas abikaasa või partneriga koos 3 kuud või enam 
#maryr: Abiellus esimest korda
#fcldbrn: Esimese lapse sünniaasta
#ycldbyr: Noorima lapse sünniaasta

# Vanus, kui lahkus vanematekodust

table(essee18$lvpntyr)

essee18$lahvan <- recode(essee18$lvpntyr, "0=NA; 1111=NA")

table(essee18$lahvan)

essee18$kodlah <- essee18$lahvan - essee18$yrbrn

table(essee18$kodlah)

# Partneriga elama asumise vanus

table(essee18$lvptnyr)

essee18$partnervan <- essee18$lvptnyr - essee18$yrbrn

table(essee18$partnervan)

# Abiellumise vanus

table(essee18$maryr)


essee18$abvan <- essee18$maryr - essee18$yrbrn

table(essee18$abvan)

# Vastaja vanus esimese lapse sünnil

table(essee18$fcldbrn)

essee18$laps1 <- essee18$fcldbrn - essee18$yrbrn

table(essee18$laps1)

# Vanus noorima lapse sünnil


table(essee18$ycldbyr)

essee18$lapsn <- essee18$ycldbyr - essee18$yrbrn

table(essee18$lapsn)

##### Vanuste vahemikud

# kodus elamine

essee18$algus <- 0

table(essee18$algus)


essee18$kodlopp <- essee18$kodlah - 1


# staatus

essee18$kodus <- 'kodus'


# vanematest eraldi

# algus on kodlah

# lõpp

essee18$kodara <- essee18$partnervan-1

# staatus

essee18$eraldi <- 'eraldi'

# kooselu

# algus on partnervan

# Lõpp 

essee18$kooslopp <- essee18$abvan-1

# staatus

essee18$kooselu <- 'kooselu'

# Abielu

# algus on abvan

# lõpp 

essee18$ablopp <- essee18$laps1-1

# staatus

essee18$abielu <- 'abielu'


# Esimene laps

# algus laps1

# lõpp 

essee18$laps1lopp <- essee18$lapsn-1


# staatus 

essee18$laps1st <- 'laps1'


# Teisest lapsesst praeguse vanuseni

#algus lapsn


# lõpp praegune vanus agea

# staatus

essee18$lapsnlopp <- 'noorimlaps'


# Võtan analüüsi 40aastased ja vanemad ning uurin kuni 40. eluaastani toimuvaid elusündmusi.

e40 <- subset(essee18, agea>=40) 


# andmete teisendamine

elusynd <- HSPELL_to_STS(e40, begin=c("algus", "kodlah", "partnervan", "abvan", "laps1", "lapsn"), 
                             end=c("kodlopp", "kodara", "kooslopp", "ablopp", "laps1lopp", "agea"),
                             status=c("kodus", "eraldi", "kooselu", "abielu", "laps1st", "lapsnlopp"))


# Vaatame loodud objekti

View(elusynd)



# Vaatame järgi, millised on staatused ja nende nimetused 


elusynd.seq <- seqdef(elusynd, 0:40)


# loome staatuste ja nimetuste nimekirja

is_seq.lab <- c ("abielu", "eraldi", "kodus", "kooselu", "laps1", "noorimlaps")
is_seq.plab <- c ("abielu", "eraldi", "kodus", "kooselu", "laps1", "noorimlaps")


# Loome jadade objekti koos staatuste nimedega

elusynd.seq <- seqdef(elusynd, 0:40, states = is_seq.lab, labels=is_seq.plab)



# Joonis kõigi eluteedega

seqdplot(elusynd.seq, border=NA, na.rm=F, withlegend=F, xlab="Aasta")

seqlegend(elusynd.seq)



#### sorteerime kõige sagdasemate jadade järgi


dist.mostf.elusynd <- seqdist(elusynd.seq, method="LCS", refseq=0, with.missing=T)

seqdplot(elusynd.seq, border=NA, sortv=dist.mostf.elusynd)

seqIplot(elusynd.seq, border=NA, sortv=dist.mostf.elusynd, xlab="Aasta")


# Uurime, mitu klastrit oleks kõige parem teha

is.dist <- seqdist(elusynd.seq, method="OM", indel=1, sm="CONSTANT", with.missing=T)

is.clustarv <- fviz_nbclust(is.dist, pam, method = "silhouette")

is.clustarv

# Loome klastrid

elusynd.clusterward <- agnes(is.dist , diss=T, method="ward")


elusynd.klast <- cutree(elusynd.clusterward, k = 3)

elusynd.klast.factor <- factor(elusynd.klast, levels=1:3)

seqdplot (elusynd.seq, group=elusynd.klast.factor)


elusyndklast.labels <- c ("Lastega", "Yks_laps", "Lasteta")


elusynd.klast.factor <- factor(elusynd.klast, levels=1:3, labels=elusyndklast.labels)

seqdplot (elusynd.seq, group=elusynd.klast.factor)

seqdplot (elusynd.seq, group=elusynd.klast.factor, border=NA)

seqdplot (elusynd.seq, group=e40$region)


##########entroopia indeks ####



seqHtplot(elusynd.seq)

seqHtplot(elusynd.seq, group=e40$gndr)


### Klastrid sõltuvateks tunnusteks regressiooni jaoks

# binaarsed tunnused logistilise regressiooni jaoks


lastega <- elusynd.klast==1

yks_laps <- elusynd.klast==2

lasteta <- elusynd.klast==3


# kõik ühte tunnusesse multinomiaalse regerssiooni või sõltumatu tunnusena kasutamiseks

e40$klaster[elusynd.klast==1] <- 'lastega'
e40$klaster[elusynd.klast==2] <- 'yks_laps'
e40$klaster[elusynd.klast==3] <- 'lasteta'

table(e40$klaster)



# regressioonanalüüs

# kodeerime ümber sõltumatud tunnused

#Regioon

e40$Regioon <- fct_recode(e40$region, Pohja="EE001", Laane="EE004", Kesk="EE006", Kirde="EE007",
                              Louna="EE008")

# Kas vanemad sündinud Eestis

e40$vansyn[e40$facntr==1 & e40$mocntr==1] <- 'molemad_EE'
e40$vansyn[e40$facntr==2 & e40$mocntr==2] <- 'molemad_mujal'
e40$vansyn[(e40$facntr==2 & e40$mocntr==1) | (e40$facntr==1 & e40$mocntr==2)] <- 'yks_EE'

table(e40$facntr, e40$mocntr)

table(e40$vansyn)

# binaarne

summary(glm(lastega ~ as.factor(gndr) + agea + mocntr + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght))

exp(coef(glm(lastega ~ as.factor(gndr) + agea + mocntr + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght)))


summary(glm(yks_laps ~ as.factor(gndr) + agea + blgetmg + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght))

exp(coef(glm(yks_laps ~ as.factor(gndr) + agea + mocntr + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght)))

summary(glm(lasteta ~ as.factor(gndr) + agea + blgetmg + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght))

exp(coef(glm(lasteta ~ as.factor(gndr) + agea + mocntr + Regioon + vansyn, family=binomial(link=logit), data=e40, weights=pspwght)))



# multinomiaalne

klastmud <- multinom(klaster ~ as.factor(gndr) + agea + Regioon + vansyn, weights=pspwght, data=e40)

stargazer(klastmud, type="text", dep.var.labels=c("Lasteta", "Üks laps"), 
          covariate.labels=c("Naine", "Vanus",  
                             "Lääne-Eesti", "Kesk-Eesti","Kirde-Eesti","Lõuna-Eesti", "Vanemad sünd mujal", "Üks vanem sünd Eestis", "Vabaliige"))




# klastri tunnus sõltumatu tunnusena

# prognoosime brutopalka

summary(lm(grspnum ~ klaster + gndr + agea + eduyrs + Regioon + vansyn, data=e40, weights=pspwght))


