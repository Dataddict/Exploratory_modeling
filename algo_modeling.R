#chargement des donnees
mesDonnees <- read.csv(file = "marketData.csv", dec = ".", sep=",", header = TRUE)
#ces données sont issues du package ISLR

names(mesDonnees)
str(mesDonnees)

#statistiques de base
summary(mesDonnees)

#matrice de corrélation : attention les valeurs doivent être numériques. La première est l'id
m <- cor(mesDonnees[,-c(1,10)])

#install.packages("corrplot")
library(corrplot)
corrplot(corr = m)
# correlation entre les rendements proches de 0

#régression logistique
reglogit.fit <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = mesDonnees, family = "binomial")
summary(reglogit.fit)

#plus petite p-value : Lag1
#le signe du coefficient de Lag1 est négatif donc a priori si l'action a cloturé en hausse hier elle cloturera en baise aujourd'hui

#prediction
reglogit.probs <- predict(object = reglogit.fit, type = "response")
reglogit.probs

contrasts(mesDonnees$Direction) #Up correspond à 1

#classification
reglogit.predClasse <- rep("DOWN",1250)
reglogit.predClasse[reglogit.probs>0.5] <- "UP"

attach(mesDonnees)#permet de directement avoir accès aux champs de mesDonnees
#résultats de la classification
table(reglogit.predClasse,Direction)

#taux d'erreur
(tauxErreur <- (141+457)/1250)
(tauxBonnePrevision <- 1-tauxErreur)

#le taux de bonnes prévisions est à peine plus grand que 50% et le modèle et le test ont été
#effectués sur les mêmes données...


#################################
#################################
#validation du modèle Train & Test

monModele <- {Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume}

#méthodologie de training & test
# pour pouvoir répéter les calculs on choisi la graine du générateur de nombres aléatoires
set.seed(3)

training_test <- c(rep(1,length=trunc((3/4)*nrow(mesDonnees))),
                   rep(2,length=(nrow(mesDonnees) - trunc((3/4)*nrow(mesDonnees)))))
mesDonnees$training_test <- sample(training_test) # permutations aléatoires

monModele.echantillonApprentissage <- mesDonnees[mesDonnees$training_test == 1,]
monModele.echantillonTest <- mesDonnees[mesDonnees$training_test == 2,]


#régression logistique
monModele.fit <- glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = monModele.echantillonApprentissage, family = "binomial")
summary(monModele.fit)
monModele.probs <- predict(object = monModele.fit, type = "response",  newdata = monModele.echantillonTest)


#classification
monModele.predClasse <- rep("DOWN",nrow(monModele.echantillonTest))
monModele.predClasse[monModele.probs>0.5] <- "UP"

#résultats de la classification
(tbl <- table(monModele.predClasse,monModele.echantillonTest$Direction))

#taux d'erreur
(tauxErreur <- (tbl["UP","Down"]+tbl["DOWN","Up"])/sum(tbl))
(tauxBonnePrevision <- 1-tauxErreur)



#Linear Discriminant Analysis
library(MASS)
  lda.fit <- lda(monModele, data = monModele.echantillonApprentissage)
  lda.fit
#cf prior probabilities. Dans l'échantillon d'apprentissage 48% des jours correspondent à des baisses


#le champ group means correspond à la moyenne de chacun des prédicteurs dans chacune des classes
#on voit que le marché a tendance à être en hausse lorsque la veille il était en baisse (rdt négatif)
#le marché à tendance à être en Baisse lorsqu'il a été 2 jours d'affilé en hausse (rdt positifs)

plot(lda.fit)

#prediction sur l'échantillon de test
lda.pred <- predict(lda.fit, newdata = monModele.echantillonTest)
#attributs disponibles dans lda.pred
names(lda.pred)
#les classes prédites sont disponibles dans l'attribut $class
(tbl_lda <- table(lda.pred$class, monModele.echantillonTest$Direction))

#taux d'erreur
(tauxErreur_LDA <- (tbl_lda["Up","Down"]+tbl_lda["Down","Up"])/sum(tbl_lda))

#calcul du nombre de Up suivant une proba a posteriori P(Y=1|X) > 50%
sum(lda.pred$posterior[,2]>=0.5)
#calcul du nombre de Down
sum(lda.pred$posterior[,1]>0.5)
#ou 
#sum(lda.pred$posterior[,2]<0.5)

#on retrouve les résultats donnés dans l'attribut class de la fonction predict : 27 Down et 286 Up

sum(lda.pred$posterior[,2]>=0.75)
#il y a 0 jour dans l'echantillon de test avec une probabilité a posteriori au moins égale à 75%


##################################
##################################
#K nearest neighbors
library(class)

?knn
#k-nearest neighbour classification for test set from training set. 
#For each row of the test set, the k nearest (in Euclidean distance) 
#training set vectors are found, and the classification is decided by 
#majority vote, with ties broken at random. If there are ties for the
#kth nearest vector, all candidates are included in the vote.

#arguments utilisés dans la fonction
#train : matrice contenant les observations des prédicteurs de l'échantillon d'apprentissage
#test : matrice conteant les observations qui serviront à tester le modèle
#cl : le vecteur contenant la classification sur l'échantillon d'apprentissage
#k : le nombre de voisins à considérer

col_classification <-which(colnames(monModele.echantillonApprentissage)=="Direction")
col_lag1 <-which(colnames(monModele.echantillonApprentissage)=="Lag1")
col_lag2 <-which(colnames(monModele.echantillonApprentissage)=="Lag2")


cl_ <- monModele.echantillonApprentissage[,col_classification]
train_ <- monModele.echantillonApprentissage[,c(col_lag1,col_lag2)]
test_ <- monModele.echantillonTest[,c(col_lag1,col_lag2)]

k_ = 2

#Pour les égalités un tirage au sort doit être fait, on choisit une graine
#pour le générateur de nombres aléatoires de manière à rendre les calculs reproductibles
set.seed(9)
knn.prediction <- knn(train = train_, test = test_,cl = cl_,k = k_)

#tableau de comparaison des classes entre modèle et observations sur l'échantillon de test
(tbl_knn <- table(knn.prediction,monModele.echantillonTest[,col_classification]))

#taux d'erreur
(tauxErreur_knn <- (tbl_knn["Up","Down"]+tbl_knn["Down","Up"])/sum(tbl_knn))


k_ = 4
set.seed(9)
knn.prediction <- knn(train = train_, test = test_,cl = cl_,k = k_)
#tableau de comparaison des classes entre modèle et observations sur l'échantillon de test
(tbl_knn <- table(knn.prediction,monModele.echantillonTest[,col_classification]))
#taux d'erreur
(tauxErreur_knn <- (tbl_knn["Up","Down"]+tbl_knn["Down","Up"])/sum(tbl_knn))
#le résultat est moins précis qu'avec 2