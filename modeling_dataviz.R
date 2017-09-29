
setwd(dir="D:/Documents/cours R/TD_R")
#chargement des données
mesDonnees<-read.csv(file="Paris.csv", sep=";",dec=",",header=TRUE)

#il peut être intéressant d'utiliser l expression ci dessous pour visualiser la structure du data frame
print(str(mesDonnees))

#Publicité en fonction du jour de la semaine
#Recuperation des mois où il y a eu de la publicité à la tele avant le match
MoisPub<-mesDonnees$Mois[mesDonnees$PubliciteTV == "Oui"]
(MoisPubFrequence<-table(x=MoisPub))
pie(x = MoisPubFrequence)

#histogramme de la fréquentation en fonction de la température
hist(x = mesDonnees$NombreSpectateurs, xlab= "Nombre de spectateurs",ylab= "Fréquence", main = "Histogramme de la Fréquentation", col = "red")



#Fréquentation du stade en fonction du jour de la semaine
boxplot(formula = mesDonnees$NombreSpectateurs ~ mesDonnees$JourSemaine, xlab = "Jour de la semaine",
        ylab="Nombre de spectateurs", main = "Fréquentation en fonction du jour de la semaine")

#Fréquentation du stade en fonction du jour de la semaine : version avec le nom des jours 
mesDonnees$jourOrdonne<- factor(x = mesDonnees$JourSemaine, levels = 1:7, labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"))
boxplot(formula = mesDonnees$NombreSpectateurs ~ mesDonnees$jourOrdonne, xlab = "Jour de la semaine",
        ylab="Nombre de spectateurs", col = "green", main = "Fréquentation en fonction du jour de la semaine")


#Fréquentation du stade en fonction du nombre de jours
#on crée une fonction qui nous renvoie le numéro du mois
NumeroMois<-function(mois_)
{
  ifelse(mois_ == "Avril", 4,
  ifelse (mois_ == "Mai", 5,
  ifelse (mois_ == "Juin",6,
  ifelse (mois_ == "Juillet", 7,
  ifelse (mois_ == "Aout",8,
  ifelse (mois_ == "Sept.", 9,
  ifelse(mois_ == "Oct.", 10,
  -1)))))))
}
#on attache le facteur au mois
mesDonnees$moisOrdonne <- NumeroMois(mesDonnees$Mois)
mesDonnees$moisOrdonne <- factor(x = mesDonnees$moisOrdonne, levels = 4:10, labels = c("Avril", "Mai", "Juin","Juil.","Aout", "Sept.","Oct."))
boxplot(formula = mesDonnees$NombreSpectateurs ~ mesDonnees$moisOrdonne, xlab = "Mois",
        ylab="Nombre de spectateurs", col = "light blue", main = "Fréquentation en fonction du mois")



#fréquentation en fonction de la température
plot(formula= mesDonnees$NombreSpectateurs ~ mesDonnees$Temperature, xlab= "température", ylab = "Nombre de spectateurs", main = "Fréquentation en fonction de la température", col = "blue")


#chargement du package lattice
library(lattice)  # graphics 

#graphique avec xyplot : Fréquentation en fonction de la température, de l'heure et de la météo
xyplot(x = NombreSpectateurs ~ Temperature | Ciel + JourNuit,
       data = mesDonnees, xlab = "Température", ylab = "Nombre de spectateurs", 
       main = "Fréquentation en fonction de la température, de l'heure et de la météo",
       pch = 15
       , col = "blue")

#différenciation des données suivant si il y a eu un feu d'artifice ou pas
monGroupe.Noms <- c("Sans Feu d'artifice","Avec Feu d'artifice") #attention à l'ordre
xyplot(x = NombreSpectateurs ~ Temperature | Ciel + JourNuit,
              data = mesDonnees, xlab = "Température", ylab = "Nombre de spectateurs", 
              main = "Fréquentation en fonction de la température, de l'heure et de la météo",
               groups = FeuxDartifice,
              auto.key = list( text = monGroupe.Noms))


#graphique xyplot : fréquentation en fonction de l'adversaire avec/sans publicité TV. On echange les x et y pour la lisibilité
xyplot(x = Adversaire ~ NombreSpectateurs | PubliciteTV, data =mesDonnees
        , ylab = "Adversaires", xlab = "Nombre de spectateurs",
       main = "Fréquentation en fonction de l'adversaire avec ou sans publicité", 
       pch = 16 , col = "brown", type=c("p","g"))


#Modèlisation de la fréquentation
mesDonnees$numeroMois <- NumeroMois(mesDonnees$Mois) #on convertit en nombre
monModele <- { NombreSpectateurs ~ JourSemaine + numeroMois + PubliciteTV}

#méthodologie de training & test
# pour pouvoir répéter les calculs on choisi la graine du générateur de nombres aléatoires
set.seed(9)

training_test <- c(rep(1,length=trunc((2/3)*nrow(mesDonnees))),
                   rep(2,length=(nrow(mesDonnees) - trunc((2/3)*nrow(mesDonnees)))))
mesDonnees$training_test <- sample(training_test) # permutations aléatoires

monModele.echantillonApprentissage <- mesDonnees[mesDonnees$training_test == 1,]
monModele.echantillonTest <- mesDonnees[mesDonnees$training_test == 2,]

#regression
monModele.fit<-lm(formula = monModele, data = monModele.echantillonApprentissage)
monModele.fit
summary(monModele.fit)

#prediction sur l echantillon d apprentissage
monModele.echantillonApprentissage$Prediction <- predict(monModele.fit)
#prediction sur l echantillon de test
monModele.echantillonTest$Prediction <- predict(monModele.fit, newdata = monModele.echantillonTest)

#???	Tracer les valeurs prédites en fonction du nombre de spectateurs attendu
layout(mat = matrix(1:2,2,1))
layout.show(2)
plot(formula = monModele.echantillonApprentissage$Prediction ~ monModele.echantillonApprentissage$NombreSpectateurs
      ,xlab = "Nombre de spectateurs"
      ,ylab = "Prédiction Nombre de spectateurs"
      ,col = "blue"
     , main = "Echantillon : apprentissage")
plot(formula = monModele.echantillonTest$Prediction ~ monModele.echantillonTest$NombreSpectateurs
     ,xlab = "Nombre de spectateurs"
     ,ylab = "Prédiction Nombre de spectateurs"
     ,col = "red"
     ,main = "Echantillon : test")

#regression sur toutes les observations
monModele.fitfull<-lm(formula = monModele, data = mesDonnees)
monModele.fitfull
summary(monModele.fitfull)

#graphiques de base sur la régression linéaire dont qqplot
plot(monModele.fitfull)





