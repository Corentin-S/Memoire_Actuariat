

## On initialise le sexe et la cause désirés.(voir les 5 causes dans le mémoire)
## Dans le mémoire nous traitons principalement le cas des hommes, c'est à dire sexe = 1.

cause <- 1

sexe <- 1

##=================================================================================================##
# Gestion des données
##=================================================================================================##





## Récupération des données importées, création de notre tableau.
tab1 <- FRATNP_d_short_idr
tab2 <- FRATNP_m_short_idr
tab3 <- Exposures



## Rassemblement des 16 causes dans 5 plus petites catégories
##=================================================================================================##

identif_cause <- function(cause) {
  # Cancers
  if (cause == 1) {
    tab_cause <- c(2)
  }

  # Maladies de l'appareil circulatoire et avc
  if (cause == 2) {
    tab_cause <- c(7, 8, 9)
  }

  # Maladies de l'appareil respiratoire et appareil digestif
  if (cause == 3) {
    tab_cause <- c(10, 11, 12)
  }

  # Causes externes
  if (cause == 4) {
    tab_cause <- c(16)
  }

  # Autres
  if (cause == 5) {
    tab_cause <- c(1, 3, 4, 5, 6, 13, 14, 15)
  }
  return(tab_cause)
}

tab_cause <- identif_cause(cause)

##=================================================================================================##


## On met les exposures dans un format adéquat
new_exposures <- matrix(0, 111, 16)
if (sexe == 3) {
  d <- 5
}
if (sexe == 1) {
  d <- 4
}
if (sexe == 2) {
  d <- 3
}
for (i in 1:16) {
  for (j in 1:111) {
    new_exposures[j, i] <- as.integer(tab3[(i - 1) * 111 + j, d])
  }
}

sel <- c(97:111)
new_exposures <- new_exposures[-sel, ]
rownames(new_exposures) <- c(0:95)
colnames(new_exposures) <- c(2000:2015)
data_e <- data.frame(new_exposures)

## On élimine deux colonnes peu intéressantes, les 85ans et plus et les 90ans et plus.
tab1 <- subset(tab1, select = -c(26:26))
tab1 <- subset(tab1, select = -c(27:27))
tab2 <- subset(tab2, select = -c(25:25))
tab2 <- subset(tab2, select = -c(26:26))

## On récupère les données souhaitées.
N <- 816
extraction <- function(tab, cause, sexe, N) {
  Notre_table <- matrix(0, 16, 22)
  tab_cause <- identif_cause(cause)
  for (i in 1:N) {
    if ((is.element(tab[i, "cause"], tab_cause)) & (tab[i, "sex"] == sexe)) {
      b <- tab[i, ]
      x <- as.numeric(tab[i, "year"] - 1999)
      Notre_table[x, ] <- Notre_table[x, ] + as.numeric(b[7:28])
    }
  }
  return(Notre_table)
}

## Construction du nouveau tableau en fonction de l'âge et de la cause.
constr_d_short <- function(cause, sexe) {
  new_d_short <- extraction(tab1, cause, sexe, N)
  rownames(new_d_short) <- c(2000:2015)
  colnames(new_d_short) <- c("total", "m0", "m1", "m5", "m10", "m15", "m20", "m25", "m30", "m35", "m40", "m45", "m50", "m55", "m60", "m65", "m70", "m75", "m80", "m85", "m90", "m95p")
  return(new_d_short)
}

## Construction du tableau des taux de mortalité par âge
constr_m_short <- function(cause, sexe) {
  new_m_short <- extraction(tab2, cause, sexe, N)
  new_m_short <- new_m_short[, -22]
  rownames(new_m_short) <- c(2000:2015)
  colnames(new_m_short) <- c("m0", "m1", "m5", "m10", "m15", "m20", "m25", "m30", "m35", "m40", "m45", "m50", "m55", "m60", "m65", "m70", "m75", "m80", "m85", "m90", "m95p")
  return(new_m_short)
}

## Application des fonctions
new_d_short <- constr_d_short(cause, sexe)
new_m_short <- constr_m_short(cause, sexe)
data_m <- data.frame(new_m_short)
data_d <- data.frame(new_d_short)

new_m_shortH <- constr_m_short(cause, 1) / 1000000
new_m_shortF <- constr_m_short(cause, 2) / 1000000



##=================================================================================================##
# Stastistiques descriptives
##=================================================================================================##


## Histogrammes
histo <- subset(new_d_short, select = -c(1:1))
barplot(histo[16, ], xlab = "Tranche d'âge", ylab = "Nombre de décès")
barplot(new_exposures[, 16] / 1000, xlab = "âge", ylab = "expositions au risque (en milliers)")

## Graphique des taux de mortalités
t <- 5 * (c(8:21) - 2)
plot(t, log(new_m_shortH[16, (t / 5) + 2]),
  xlim = c(30, 95), pch = 16, ylim=c(-11,-2),
  xlab = "Tranche d'âge", ylab = "Logarithme du taux de mortalité", type = "o", col = "blue",
  main = "Taux de mortalité pour les hommes et les femmes"
)
par(new = T)
plot(t, log(new_m_shortF[16, (t / 5) + 2]),
  pch = 15, xlab = "", ylab = "",ylim=c(-11,-2),
  axes = F, xlim = c(30, 95), type = "o", col = "red"
)
legend(x = "topleft", legend = c("Hommes", "Femmes"), text.col = c("blue", "red"), pch = c(16, 15), col = c("black", "red"))


# Calcul de moyennes
moy <- histo[16, 1] * 0.5 + histo[16, 2] * 3
for (i in 1:19) {
  moy <- histo[16, i + 2] * (7.5 + 5 * (i - 1)) + moy
}
moy <- moy / new_d_short[16, 1]
print(moy)


## Création de la table de mortalité
annee <- 2015
Mort <- matrix(0, 22, 4)
Mort[1, 1] <- new_d_short[annee - 1999, 1]

for (i in 2:21) {
  Mort[i, 1] <- round(Mort[i - 1, 1] - new_d_short[annee - 1999, i], 0)
}

for (i in 1:21) {
  Mort[i, 2] <- round(Mort[i, 1] - Mort[i + 1, 1], 0)
  Mort[i, 4] <- round((Mort[1, 1] - Mort[i + 1, 1]) / Mort[1, 1], 4)
  Mort[i, 3] <- round(1 - Mort[i, 4], 4)
  Mort[i, 1] <- round(Mort[i, 1], 0)
}

Mort <- Mort[-22, ]
rownames(Mort) <- c("m0", "m1", "m5", "m10", "m15", "m20", "m25", "m30", "m35", "m40", "m45", "m50", "m55", "m60", "m65", "m70", "m75", "m80", "m85", "m90", "m95p")
colnames(Mort) <- c("lx", "dx", "px", "qx")
data2 <- data.frame(Mort)
write.csv(data2, file = "Table_de_mortalite_neoplasmes.xls")



##=================================================================================================##
# Interpolation
##=================================================================================================##



library("Brobdingnag")

### Recherche des valeurs des parametres b et c dans la loi de Gombertz
parametre_Gombertz <- function(t, x1, x2,data) { # t= 1,2..16 où 1=2000, 2=2001,..., 16=2015 et x1 doit etre plus petit que x2 et ils doivent etre divisible par 5
  # exemple: si x1= 10 et x2 = 15, on cherche les parametres entre m10 et m15 donc entre les indices 16 et 21 pour data_m
  # attention ne fonctionne pas pour x1=0 et x1=1
  X <- c()
  mu_1 <- as.brob(data[t, (x1 / 5) + 2])
  mu_2 <- as.brob(data[t, (x2 / 5) + 2])
  b <- (((mu_1)^(x2)) / ((mu_2)^x1))^(1 / (x2 - x1))
  c_para <- (mu_2 / mu_1)^(1 / (x2 - x1))
  X <- c(X, as.numeric(b), as.numeric(c_para))
  return(X)
}

# Loi Gompertz
Gombertz <- function(x, parametres) { # renvoie le mu en fonction du x
  return(parametres[1] * (parametres[2])^x)
}

# Creation de la data mort_interp avec les valeurs de mu entre 0 et 95, pour toutes les années avec la loi de Gombertz

constr_m_interp <- function(data) {
  Mort_interp <- matrix(0, 16, 96)
  rownames(Mort_interp) <- c(2000:2015)
  colnames(Mort_interp) <- c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", "m26", "m27", "m28", "m29", "m30", "m31", "m32", "m33", "m34", "m35", "m36", "m37", "m38", "m39", "m40", "m41", "m42", "m43", "m44", "m45", "m46", "m47", "m48", "m49", "m50", "m51", "m52", "m53", "m54", "m55", "m56", "m57", "m58", "m59", "m60", "m61", "m62", "m63", "m64", "m65", "m66", "m67", "m68", "m69", "m70", "m71", "m72", "m73", "m74", "m75", "m76", "m77", "m78", "m79", "m80", "m81", "m82", "m83", "m84", "m85", "m86", "m87", "m88", "m89", "m90", "m91", "m92", "m93", "m94", "m95p")
  Mort_interp[, 1] <- data[, 1] # pour m0
  Mort_interp[, 2] <- data[, 2] # pour m1

  for (i in 1:19) {
    Mort_interp[, (i * 5 + 1)] <- data[, (i + 2)]
  }


  for (t in 1:16) {
    for (i in 1:95) {
      if ((i - 1) %% 5 == 0) {
        parametres <- parametre_Gombertz(t, i - 1, i + 4,data ) # on établit les paramètres entre deux points divisibles par 5
      }

      else {
        Mort_interp[t, i] <- as.integer(as.numeric(Gombertz(i - 1, parametres))) # on applique ces paramètres aux points entre ces deux points.
      }
    }
  }
  return(Mort_interp)
}

data_m_interp <- data.frame(constr_m_interp(data_m))

new_m_interp <- t(constr_m_interp(data_m))

## Histogrammes
barplot(subset(t(new_m_interp) / 1000000, select = -c(1:1)), xlab = "âge", ylab = "Taux de mortalité (échelle log)", log = "y")
barplot(subset(t(new_d_interp), select = -c(1:1)), xlab = "âge", ylab = "Nombre de décès")


## Utilisation de la formule pour retrouver le nombre de morts pour toutes les années.
new_d_interp <- matrix(0, 96, 16)
new_d_interp <- (new_m_interp * new_exposures) / 1000000
for (i in 1:96) {
  for (j in 1:16) {
    new_d_interp[i, j] <- as.integer(new_d_interp[i, j])
  }
}



##=================================================================================================##
# Copules de Clayton et de Franck, calcul des taux nets
##=================================================================================================##



Brut <- list()

for (i in 1:5) {
  Brut[[i]] <- t(constr_m_interp(data.frame(constr_m_short(i, sexe)))) / 1000000
                                                                                  
}

## Les différents taux brutes pour chaque cause.

B1 <- Brut[[1]]
B2 <- Brut[[2]]
B3 <- Brut[[3]]
B4 <- Brut[[4]]
B5 <- Brut[[5]]


## Premières sommes(intégrales) avec les exponentielles

generator <- function(s, psi_prime) {
  if (s==1){
    alpha <- B1[s, ] + B2[s, ] + B3[s, ] + B4[s, ] + B5[s, ]
  }else{
    alpha <- (apply(B1[0:s, ] + B2[0:s, ] + B3[0:s, ] + B4[0:s, ] + B5[0:s, ], 2, sum))
  }
  return(((exp(-alpha)) / (psi_prime((exp(-alpha))))))
}

## Seconde somme(intégrale) en t

generator2 <- function(t, j, psi, psi_prime) {
  B <- Brut[[j]] ## Taux brute pour la cause choisie
  v <- matrix(0, t, 16)
  for (i in 1:t) {
    v[i, ] <- generator(i, psi_prime)
  }
  omega <- apply((v * B[0:t, ]), 2, sum)
  return(psi(-omega))
}

## On calcule les taux nets pour chaque âge t entre 0 et 95 ans.

brut_to_net <- function(cause, psi, psi_prime) {
  s <- matrix(0, 96, 16)
  for (i in 1:96) {
    s[i, ] <- generator2(i, cause, psi, psi_prime)
  }
  return(s)
}

## On calcule les tableaux de taux nets pour chacune des 5 causes i

constr_taux_net <- function(psi, psi_prime) {
  taux_net <- list()
  Survie <- list()
  for (i in 1:5) {
    net <- matrix(0, 96, 16)
    survie <- brut_to_net(i, psi, psi_prime)
    Survie[[i]] <- survie
    net[1,] <- -log(survie[1,])
    for (j in 1:95){
      net[j+1,] <- -(log(survie[j+1,]) + apply(net,2,sum))
    }
    taux_net[[i]] <- net
    colnames(taux_net[[i]]) <- c(2000:2015)
    rownames(taux_net[[i]]) <- c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", "m26", "m27", "m28", "m29", "m30", "m31", "m32", "m33", "m34", "m35", "m36", "m37", "m38", "m39", "m40", "m41", "m42", "m43", "m44", "m45", "m46", "m47", "m48", "m49", "m50", "m51", "m52", "m53", "m54", "m55", "m56", "m57", "m58", "m59", "m60", "m61", "m62", "m63", "m64", "m65", "m66", "m67", "m68", "m69", "m70", "m71", "m72", "m73", "m74", "m75", "m76", "m77", "m78", "m79", "m80", "m81", "m82", "m83", "m84", "m85", "m86", "m87", "m88", "m89", "m90", "m91", "m92", "m93", "m94", "m95p")
  }
  
  return(c(taux_net,Survie))
}



## Copule de Clayton
##=================================================================================================##

## Fonction génératrice de la copule de Clayton

theta_clayton <- 0.5
psiC <- function(t) {
  return(1 / ((1 + t)^(theta_clayton)))
}

## Inverse de la fonction génétratrice

psi_inverseC <- function(t){
  return(t^{(-1/theta_clayton)} - 1)
}


## C'est la fonction Psi_prime composée avec psi^(-1)

psi_primeC <- function(t) {
  return (-theta_clayton * (t^{(1/theta_clayton)+1}))
}

L <- constr_taux_net(psiC, psi_primeC)
taux_net_clayton <- L[1:5]
survie_clayton <- L[6:10]


## Copule de Franck
##=================================================================================================##

## Fonction génératrice

theta_franck <- 0.5
psiF <- function(t) {
  return(-(1 / theta_franck ) * (log(1 + exp(-t) * (exp(-theta_franck ) - 1))))
}

## Inverse de la fonction génétratrice

psi_inverseF <- function(t){
  return(-log((exp(-theta_franck*t)-1)/(exp(-theta_franck )-1)))
}

## C'est la fonction Psi_prime composée avec psi^(-1)

psi_primeF <- function(t) {
  return((1/theta_franck)*(1-exp(theta_franck * t)))
}

L <- constr_taux_net(psiF, psi_primeF)
taux_net_franck <- L[1:5]
survie_franck <- L[6:10]



##=================================================================================================##
# Modèle de Lee Carter
##=================================================================================================##



## Utilisation des fonctions de bases
library("StMoMo")
library("demography")
library("forecast")

StMoMo(
  link = c("log", "logit"), staticAgeFun = TRUE, periodAgeFun = "NP",
  cohortAgeFun = NULL, constFun = function(ax, bx, kt, b0x, gc, wxt, ages) {
    list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
  }
)


constLC <- function(ax, bx, kt, b0x, gc, wxt, ages) {
  c1 <- mean(kt[1, ], na.rm = TRUE)
  c2 <- sum(bx[, 1], na.rm = TRUE)
  list(ax = ax + c1 * bx, bx = bx / c2, kt = c2 * (kt - c1))
}

LC <- lc(link = "logit")

## Création de notre objet de type StMoMoData

FRData <- demogdata(new_m_interp / 1000000, new_exposures, c(0:95), c(2000:2015), "mortality", "France", "total")
FRData <- StMoMoData(FRData, series = "total")

## Implémentation du modèle

FRIniData <- central2initial(FRData)
ages.fit <- 0:95
wxt <- genWeightMat(ages = ages.fit, years = FRIniData$years, clip = 3)
LCfit <- fit(LC, data = FRIniData, ages.fit = ages.fit, wxt = wxt)

plot(LCfit, nCol = 3)
LCres <- residuals(LCfit)
plot(LCres)
plot(LCres, type = "colourmap", reslim = c(-3.5, 3.5))

LCfor <- forecast(LCfit, h = 50)
LCforArima <- forecast(LCfit,
  h = 50, kt.method = "iarima",
  kt.order = c(1, 1, 2)
)
plot(LCfor, only.kt = TRUE)
plot(LCforArima, only.kt = TRUE)

set.seed(1434)
nsim <- 500
LCsim <- simulate(LCfit, nsim = nsim, h = 50)

library("fanplot")
probs <- c(2.5, 10, 25, 50, 75, 90, 97.5)
qxt <- FRIniData$Dxt / FRIniData$Ext
matplot(LCfit$years, t(qxt[c("65", "75", "85"), ]),
  xlim = c(2000, 2060), ylim = c(0.002, 0.04), pch = 20, col = "black",
  log = "y", xlab = "year", ylab = "mortality rate (log scale)"
)

fan(t(LCsim$rates["65", , ]),
  start = 2014, probs = probs, n.fan = 4,
  fan.col = colorRampPalette(c("black", "white")), ln = NULL
)
fan(t(LCsim$rates["75", , ]),
  start = 2014, probs = probs, n.fan = 4,
  fan.col = colorRampPalette(c("red", "white")), ln = NULL
)
fan(t(LCsim$rates["85", , ]),
  start = 2014, probs = probs, n.fan = 4,
  fan.col = colorRampPalette(c("blue", "white")), ln = NULL
)
text(2000, qxt[c("65", "75", "85"), "2000"],
  labels = c("x = 65", "x = 75", "x = 85")
)


plot(30:45, extractCohort(fitted(LCfit, type = "rates"), cohort = 1970),
        type = "l", log = "y", xlab = "age", ylab = "q(x)",
        main = "Mortality rates for the 1970 cohort",
        xlim = c(30,95), ylim = c(0.0001, 0.05))
lines(46:95, extractCohort(LCfor$rates, cohort = 1970), lty = 2)



##=================================================================================================##
# Modèles de Lee-Carter avec les différentes copules
##=================================================================================================##


## Création d'une fonction qui renvoie les taux projetés

lee_carter <- function(taux,i){
  FRData <- demogdata(taux, new_exposures, c(0:95), c(2000:2015), "mortality", "France", "total")
  FRData <- StMoMoData(FRData, series = "total")
  FRIniData <- central2initial(FRData)
  ages.fit <- 0:95
  
  wxt <- genWeightMat(ages = ages.fit, years = FRIniData$years, clip = 3)
  LCfit <- fit(LC, data = FRIniData, ages.fit = ages.fit, wxt = wxt)
  LCfor <- forecast(LCfit, h = 50)
  return(LCfor$rates)
}




## Création d'une fonction qui trace des courbes de mortalité pour chaque cause
##=================================================================================================##


cohorte <- function(taux,i){
  FRData <- demogdata(taux, new_exposures, c(0:95), c(2000:2015), "mortality", "France", "total")
  FRData <- StMoMoData(FRData, series = "total")
  FRIniData <- central2initial(FRData)
  ages.fit <- 0:95
  
  wxt <- genWeightMat(ages = ages.fit, years = FRIniData$years, clip = 3)
  LCfit <- fit(LC, data = FRIniData, ages.fit = ages.fit, wxt = wxt)
  LCfor <- forecast(LCfit, h = 50)
  
  if (i==1){
    couleur='blue'
      }
  if(i==2){
    couleur='red'
  }
  if(i==3){
    couleur='purple'
  }
  if(i==4){
    couleur='green'
  }
  if(i==5){
    couleur='black'
  }
  plot(30:45, extractCohort(fitted(LCfit, type = "rates"), cohort = 1970),
       type = "l", log = "y", xlab = "age", ylab = "q(x)",
       main = "Taux de mortalité pour la cohorte 1970", col = couleur,
       xlim = c(30,95), ylim = c(0.00005, 0.16))
  lines(46:95, extractCohort(LCfor$rates, cohort = 1970), lty = 2,col=couleur)
  par(new = T)
  return(LCfor$rates)
}

taux_projetes=list()
for (i in 1:5){
  taux_projetes[[i]] <- cohorte(Brut[[i]],i)
}
legend(x = "topleft", legend = c("Cancers", "Maladies circulatoires","Maladies respiratoires","Causes externes","Autres"), text.col = c("blue", "red","purple","green","black"), pch = c(16, 15), col = c("blue", "red","purple","green","black"))



## Création d'une fonction qui trace les coefficients kappa pour chaque cause
##=================================================================================================##

f_kappa <- function(taux,i){
  FRData <- demogdata(taux, new_exposures, c(0:95), c(2000:2015), "mortality", "France", "total")
  FRData <- StMoMoData(FRData, series = "total")
  FRIniData <- central2initial(FRData)
  ages.fit <- 0:95
  
  wxt <- genWeightMat(ages = ages.fit, years = FRIniData$years, clip = 3)
  LCfit <- fit(LC, data = FRIniData, ages.fit = ages.fit, wxt = wxt)
  LCfor <- forecast(LCfit, h = 50)
  
  if (i==1){
    couleur='blue'
  }
  if(i==2){
    couleur='red'
  }
  if(i==3){
    couleur='purple'
  }
  if(i==4){
    couleur='green'
  }
  if(i==5){
    couleur='black'
  }
  plot(2000:2015,LCfit$kt,ylim=c(-180,20),xlim=c(2000,2065),type='l',
       ylab='kappa',xlab='annee',col=couleur,main = "Coefficient kappa par annee pour chaque cause.")
  lines(2016:2065,LCfor$kt.f$mean, lty = 2, col=couleur)
  par(new = T)
}


for (i in 1:5){
  f_kappa(taux_net_clayton[[i]],i)
}
legend(x = "bottomleft", legend = c("Cancers", "Maladies circulatoires","Maladies respiratoires","Causes externes","Autres"), text.col = c("blue", "red","purple","green","black"), pch = c(16, 15), col = c("blue", "red","purple","green","black"))



##=================================================================================================##
# Traitement des espérances de vie
##=================================================================================================##



mort_to_survie <- function(tab){
    survie <- matrix(0, 96, length(tab[1,]))
      for (j in 1:95){
        survie[j,] <- exp(-apply(tab*c(matrix(1,j),matrix(0,96-j)),2,sum))
      }
    survie[96,] <- exp(-apply(tab,2,sum))
    return(survie)
  }




# Espérance passée
##=================================================================================================##



survie_brut = list()
for (i in 1:5){
  survie_brut[[i]] <- mort_to_survie(Brut[[i]])
}

survie_passe = survie_brut[[1]]*survie_brut[[2]]*survie_brut[[3]]*survie_brut[[4]]*survie_brut[[5]]
colnames(survie_passe) <- c(2000:2015)
Esperance_passee=apply(survie_passe,2,sum)



# Copule indépendante
##=================================================================================================##


## Calcul de l'espérance de vie future

survie_future_brut_par_cause = list()
for (i in 1:5){
  survie_future_brut_par_cause[[i]] <- mort_to_survie(taux_projetes[[i]])
}
survie_future_brut=survie_future_brut_par_cause[[1]]*survie_future_brut_par_cause[[2]]*survie_future_brut_par_cause[[3]]*survie_future_brut_par_cause[[4]]*survie_future_brut_par_cause[[5]]
colnames(survie_future_brut) <- c(2016:2065)
Esperance_brut_future <- apply(survie_future_brut,2,sum)



# Copule de Clayton
##=================================================================================================##


survie_future = list()
for (i in 1:5){
  survie_future[[i]] <- mort_to_survie(lee_carter(taux_net_clayton[[i]],i))
}

survie_future_clayton <- psiC(psi_inverseC(survie_future[[1]]) + psi_inverseC(survie_future[[2]]) + psi_inverseC(survie_future[[3]]) +psi_inverseC(survie_future[[4]]) + psi_inverseC(survie_future[[5]]))
colnames(survie_future_clayton ) <- c(2016:2065)
Esperance_clayton_future <- apply(survie_future_clayton ,2,sum)

# Copule de Franck
##=================================================================================================##


survie_future_franck_par_cause = list()
for (i in 1:5){
  survie_future_franck_par_cause[[i]] <- mort_to_survie(lee_carter(taux_net_franck[[i]],i))
}


survie_future_franck <- psiF(psi_inverseF(survie_future_franck_par_cause[[1]]) + psi_inverseF(survie_future_franck_par_cause[[2]]) + psi_inverseF(survie_future_franck_par_cause[[3]]) +psi_inverseF(survie_future_franck_par_cause[[4]]) + psi_inverseF(survie_future_franck_par_cause[[5]]))
colnames(survie_future_franck) <- c(2016:2065)
Esperance_franck_future <- apply(survie_future_franck,2,sum)

plot(2000:2015,Esperance_passee,ylim=c(72,85),xlim=c(2000,2065),type='l',
     ylab='',xlab='annee',main = "Projection de l'espérance de vie")
lines(2016:2065,Esperance_franck_future, lty = 2,ylim=c(72,85),col='red')
lines(2016:2065,Esperance_clayton_future, lty = 2,ylim=c(72,85),col='blue')
lines(2016:2065,Esperance_brut_future, lty = 2,ylim=c(72,85))
legend(x = "topleft", legend = c("Copule de Clayton","Copule de Franck", "Copule indépendante"), text.col = c("blue", "red","black"), pch = c(16, 15), col = c("blue", "red","black"))


##=================================================================================================##
# Elimination complète ou partiel d'une cause
##=================================================================================================##


## Paramètre de la fonction reduce, (voir Kaishev), A=1 et B=1 ==> élimination brutale et complète d'une cause
cause = 1
A = 0.3
B = 0.8
C = 55
D = 75

reduce <- function(l,a,b,c,d){
  return((l<c)*a + (c < l & l < d) * (a + ((b-a)/(d-c))*(l-c)) + (l>d) * b)
}

elimination <- function(cause,liste,a,b,c,d){
  tab <- liste[[cause]]
  survie_reduce <- matrix(1,96,50)
  q <- matrix(0,96,50)
  survie_reduce[1,] <- tab[1,]
  for (i in 1:95){
    q[i,] <- (1 - (tab[i+1,] / tab[i,])) * (1 - reduce(i,a,b,c,d))
    survie_reduce[i+1,] <- (1 - q[i,]) * survie_reduce[i,]
  }
  return(survie_reduce)
}




# Copule indépendante, graphique pour une élimination avec paramètre A,B,C,D
##=================================================================================================##



plot(2000:2015,Esperance_passee,ylim=c(72,86),xlim=c(2000,2065),type='l',
     ylab='',xlab='annee',main = "Projection avec réduction d'une des causes")

survie_future = list()
for (j in 1:5){
  survie_future[[j]] <- mort_to_survie(lee_carter(Brut[[j]],j))
}

survie_future_brut_reduce  = list()

for (i in 1:5){
  save= survie_future
  survie_future[[i]] <- elimination(i,survie_future,A,B,C,D)
  survie_future_reduce <- survie_future[[1]]*survie_future[[2]]*survie_future[[3]]*survie_future[[4]]*survie_future[[5]]
  colnames(survie_future_reduce) <- c(2016:2065)
  Esperance_future_reduce <- apply(survie_future_reduce,2,sum)
  
  survie_future_brut_reduce[[i]] <- survie_future_reduce
  survie_future=save
  
  if (i==1){
    couleur='blue'
  }
  if(i==2){
    couleur='red'
  }
  if(i==3){
    couleur='purple'
  }
  if(i==4){
    couleur='green'
  }
  if(i==5){
    couleur='orange'
  }
  
  lines(2016:2065,Esperance_brut_future, lty = 2,ylim=c(72,86))
  lines(2016:2065,Esperance_future_reduce,lty = 2, ylim=c(72,86),col=couleur)
  par(new = T)
}


legend(x = "topleft", legend = c("Cancers", "Maladies circulatoires","Maladies respiratoires","Causes externes","Autres"), text.col = c("blue", "red","purple","green","orange"), pch = c(16, 15), col = c("blue", "red","purple","green","orange"))



# Copule de Clayton, graphique pour une élimination avec paramètre A,B,C,D
##=================================================================================================##



plot(2000:2015,Esperance_passee,ylim=c(70,90),xlim=c(2000,2065),type='l',
     ylab='',xlab='annee',main = "Projection avec réduction d'une des causes")

survie_future = list()
for (j in 1:5){
  survie_future[[j]] <- mort_to_survie(lee_carter(taux_net_clayton[[j]],j))
}

survie_future_clayton_reduce  = list()

for (i in 1:5){
  save= survie_future
  survie_future[[i]] <- elimination(i,survie_future,A,B,C,D)
  survie_future_reduce <- psiC(psi_inverseC(survie_future[[1]]) + psi_inverseC(survie_future[[2]]) + psi_inverseC(survie_future[[3]]) + psi_inverseC(survie_future[[4]]) + psi_inverseC(survie_future[[5]]))
  colnames(survie_future_reduce) <- c(2016:2065)
  Esperance_future_reduce <- apply(survie_future_reduce,2,sum)
  
  survie_future_clayton_reduce[[i]] <- survie_future_reduce
  survie_future=save
  
  if (i==1){
    couleur='blue'
  }
  if(i==2){
    couleur='red'
  }
  if(i==3){
    couleur='purple'
  }
  if(i==4){
    couleur='green'
  }
  if(i==5){
    couleur='orange'
  }
  
  lines(2016:2065,Esperance_brut_future, lty = 2,ylim=c(70,90))
  lines(2016:2065,Esperance_future_reduce,lty = 2, ylim=c(70,90),col=couleur)
  par(new = T)
}


legend(x = "topleft", legend = c("Cancers", "Maladies circulatoires","Maladies respiratoires","Causes externes","Autres"), text.col = c("blue", "red","purple","green","orange"), pch = c(16, 15), col = c("blue", "red","purple","green","orange"))



# Copule de Franck, graphique pour une élimination avec paramètre A,B,C,D
##=================================================================================================##



plot(2000:2015,Esperance_passee,ylim=c(72,86),xlim=c(2000,2065),type='l',
     ylab='',xlab='annee',main = "Projection avec réduction d'une des causes")

survie_future = list()
for (j in 1:5){
  survie_future[[j]] <- mort_to_survie(lee_carter(taux_net_franck[[j]],j))
}

survie_future_franck_reduce  = list()

for (i in 1:5){
  save= survie_future
  survie_future[[i]] <- elimination(i,survie_future,A,B,C,D)
  survie_future_reduce <- psiF(psi_inverseF(survie_future[[1]]) + psi_inverseF(survie_future[[2]]) + psi_inverseF(survie_future[[3]]) + psi_inverseF(survie_future[[4]]) + psi_inverseF(survie_future[[5]]))
  colnames(survie_future_reduce) <- c(2016:2065)
  Esperance_future_reduce <- apply(survie_future_reduce,2,sum)
  
  survie_future_franck_reduce[[i]] <- survie_future_reduce
  survie_future=save
  
  if (i==1){
    couleur='blue'
  }
  if(i==2){
    couleur='red'
  }
  if(i==3){
    couleur='purple'
  }
  if(i==4){
    couleur='green'
  }
  if(i==5){
    couleur='orange'
  }
  
  lines(2016:2065,Esperance_brut_future, lty = 2,ylim=c(72,86))
  lines(2016:2065,Esperance_future_reduce,lty = 2, ylim=c(72,86),col=couleur)
  par(new = T)
}


legend(x = "topleft", legend = c("Cancers", "Maladies circulatoires","Maladies respiratoires","Causes externes","Autres"), text.col = c("blue", "red","purple","green","orange"), pch = c(16, 15), col = c("blue", "red","purple","green","orange"))



##=================================================================================================##
# Produit de rente
##=================================================================================================##
#les k_p_x sont égaux à P(T_x >k, T_x > 0) / P(T_x>0), cela correspond a: 

x = 65
v=1/(1)
annee_debut=2016


#annee_debut correspond a l'annee a laquelle on commence quand la personne est age de x annees.

construct_pi <- function(x,annee_debut,fonction_survie){
  indice_annee = annee_debut-2015
  k_p_x = c()
  for (k in x : 96){
    t = k - x
    k_p_x = c( k_p_x, fonction_survie[k,indice_annee+t] / fonction_survie[x,indice_annee])
  }
  prime = 0
  for (i in 1: length(k_p_x)){
    prime = prime + v^(i)*k_p_x[i]
  }
  return(as.numeric(prime))
}

prime_indep = construct_pi(x,annee_debut,survie_future_brut) #pour la copule indépendantes
prime_F = construct_pi(x,annee_debut,survie_future_franck) #pour la copule de Franck
prime_C = construct_pi(x,annee_debut,survie_future_clayton) #pour la copule de Clayton
prime_indep_reduce = construct_pi(x,annee_debut,survie_future_brut_reduce[[1]]) # copule indépendantes sans les cancers
prime_C_reduce = construct_pi(x,annee_debut,survie_future_clayton_reduce[[1]]) # Copule de Clayton sans les cancers
prime_F_reduce = construct_pi(x,annee_debut,survie_future_franck_reduce[[1]]) # Copule de Franck sans les cancers
print(prime_indep)
print(prime_C)#clayton
print(prime_F)
print(prime_indep_reduce)
print(prime_C_reduce)#clayton
print(prime_F_reduce)
