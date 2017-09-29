
setwd(dir="D:/Documents/cours R/TD_R")
#chargement des donn�es
mesDonnees<-read.csv(file="Paris.csv", sep=";",dec=",",header=TRUE)

#il peut �tre int�ressant d'utiliser l expression ci dessous pour visualiser la structure du data frame
print(str(mesDonnees))

#Publicit� en fonction du jour de la semaine
#Recuperation des mois o� il y a eu de la publicit� � la tele avant le match
MoisPub<-mesDonnees$Mois[mesDonnees$PubliciteTV == "Oui"]
(MoisPubFrequence<-table(x=MoisPub))
pie(x = MoisPubFrequence)

#histogramme de la fr�quentation en fonction de la temp�rature
hist(x = mesDonnees$NombreSpectateurs, xlab= "Nombre de spectateurs",ylab= "Fr�quence", main = "Histogramme de la Fr�quentation", col = "red")



#Fr�quentation du stade en fonction du jour de la semaine
boxplot(formula = mesDonnees$NombreSpectateurs ~ mesDonnees$JourSemaine, xlab = "Jour de la semaine",
        ylab="Nombre de spectateurs", main = "Fr�quentation en fonction du jour de la semaine")

#Fr�quentation du stade en fonction du jour de la semaine : version avec le nom des jours 
mesDonnees$jourOrdonne<- factor(x = mesDonnees$JourSemaine, levels = 1:7, labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"))
boxplot(formula = mesDonnees$NombreSpectateurs ~ mesDonnees$jourOrdonne, xlab = "Jour de la semaine",
        ylab="Nombre de spectateurs", col = "green", main = "Fr�quentation en fonction du jour de la semaine")


#Fr�quentation du stade en fonction du nombre de jours
#on cr�e une fonction qui nous renvoie le num�ro du mois
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
        ylab="Nombre de spectateurs", col = "light blue", main = "Fr�quentation en fonction du mois")



#fr�quentation en fonction de la temp�rature
plot(formula= mesDonnees$NombreSpectateurs ~ mesDonnees$Temperature, xlab= "temp�rature", ylab = "Nombre de spectateurs", main = "Fr�quentation en fonction de la temp�rature", col = "blue")


#chargement du package lattice
library(lattice)  # graphics 

#graphique avec xyplot : Fr�quentation en fonction de la temp�rature, de l'heure et de la m�t�o
xyplot(x = NombreSpectateurs ~ Temperature | Ciel + JourNuit,
       data = mesDonnees, xlab = "Temp�rature", ylab = "Nombre de spectateurs", 
       main = "Fr�quentation en fonction de la temp�rature, de l'heure et de la m�t�o",
       pch = 15
       , col = "blue")

#diff�renciation des donn�es suivant si il y a eu un feu d'artifice ou pas
monGroupe.Noms <- c("Sans Feu d'artifice","Avec Feu d'artifice") #attention � l'ordre
xyplot(x = NombreSpectateurs ~ Temperature | Ciel + JourNuit,
              data = mesDonnees, xlab = "Temp�rature", ylab = "Nombre de spectateurs", 
              main = "Fr�quentation en fonction de la temp�rature, de l'heure et de la m�t�o",
               groups = FeuxDartifice,
              auto.key = list( text = monGroupe.Noms))


#graphique xyplot : fr�quentation en fonction de l'adversaire avec/sans publicit� TV. On echange les x et y pour la lisibilit�
xyplot(x = Adversaire ~ NombreSpectateurs | PubliciteTV, data =mesDonnees
        , ylab = "Adversaires", xlab = "Nombre de spectateurs",
       main = "Fr�quentation en fonction de l'adversaire avec ou sans publicit�", 
       pch = 16 , col = "brown", type=c("p","g"))


#Mod�lisation de la fr�quentation
mesDonnees$numeroMois <- NumeroMois(mesDonnees$Mois) #on convertit en nombre
monModele <- { NombreSpectateurs ~ JourSemaine + numeroMois + PubliciteTV}

#m�thodologie de training & test
# pour pouvoir r�p�ter les calculs on choisi la graine du g�n�rateur de nombres al�atoires
set.seed(9)

training_test <- c(rep(1,length=trunc((2/3)*nrow(mesDonnees))),
                   rep(2,length=(nrow(mesDonnees) - trunc((2/3)*nrow(mesDonnees)))))
mesDonnees$training_test <- sample(training_test) # permutations al�atoires

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

#???	Tracer les valeurs pr�dites en fonction du nombre de spectateurs attendu
layout(mat = matrix(1:2,2,1))
layout.show(2)
plot(formula = monModele.echantillonApprentissage$Prediction ~ monModele.echantillonApprentissage$NombreSpectateurs
      ,xlab = "Nombre de spectateurs"
      ,ylab = "Pr�diction Nombre de spectateurs"
      ,col = "blue"
     , main = "Echantillon : apprentissage")
plot(formula = monModele.echantillonTest$Prediction ~ monModele.echantillonTest$NombreSpectateurs
     ,xlab = "Nombre de spectateurs"
     ,ylab = "Pr�diction Nombre de spectateurs"
     ,col = "red"
     ,main = "Echantillon : test")

#regression sur toutes les observations
monModele.fitfull<-lm(formula = monModele, data = mesDonnees)
monModele.fitfull
summary(monModele.fitfull)

#graphiques de base sur la r�gression lin�aire dont qqplot
plot(monModele.fitfull)




