# Importer les packages n�c�ssaires
library(kernlab)
data(spam)

# Afficher la base de donn�es
print(spam)

# D�tecter les valeurs manquantes
is.na(spam)

# V�rifier ma base de donn�es
str(spam)
# 1.Proposer quelques analyses descriptives univari�es permettant d'appr�hender le jeu de donn�es.
# faisons un r�sum� statistique
summary(spam, na.rm=TRUE)

# Trouvons la dimension de la base de donn�es
dim(spam)
table(spam$type)

# Tracer le tableau de contingence pour la variable type
table(spam$type)
# Calculer la proportion 
prop.table(table(spam$type))

# Tracer le diagramme en baton
barplot(table(spam$type), main = "Diagramme � barre", col="blue", border="white")

# Extraire la variable qualitative
data_numeric<- spam[, sapply(spam, is.numeric)]

# Afficher les valeurs num�riques
print(data_numeric)
str(data_numeric)
dim(data_numeric)

# Tracer la boite � moustache
boxplot(data_numeric)

# v�rifier � nouveau ma base donn�e
data(spam)
print(spam)
str(spam)

# Cr�er un �chantillon d'apprentissage sur lequel on fera tourner les diff�rentes m�thodes de data mining
# Tout d'abord d�finir la proportion qu'on veut tester avec les variables num�riques
Trainset <- sample(nrow(data_numeric))[1:floor(80/100*nrow(data_numeric))]
print(TrainSet)

# cr�er les �chatillons � nouveaux
TestSet<- c(1:nrow(data_numeric))[-Trainset]
data_numeric_Train <- data_numeric[Trainset,]
data_numeric_Test <- data_numeric[TestSet,]

print(TestSet)
print(data_numeric_Train)
print(data_numeric_Test)
length(TestSet)
length(data_numeric_Train)
length(data_numeric_Test)

# Parie II Data Minig non supervis�e
# Utiliser le k-means sur l'�chantillon d'apprentissage pour cr�er des groupes de mails.

# Charger les biblioth�ques n�c�ssaires
library(factoextra)
library(cluster)

# v�rification des valeurs manquantes
sum(is.na(data_numeric))

# standardiser les donn�es
data_numeric <- scale(data_numeric,center=T,scale=T)
# Appliquer k-means avec un nombre initial de clusters pour k=3
kmean <- kmeans(data_numeric, centers=3)
head(kmean$cluster)

# Pour le cluster 1
names(which(kmean$cluster==1))
# Pour le cluster 2
names(which(kmean$cluster==2))
# pour le cluster 3
names(which(kmean$cluster==3))

# Visualiser les clusters
fviz_cluster(kmean, data = data_numeric)

# visualiser les r�sultats
kmean$centers 
kmean$size 

# Choix du nombre de clusters avec la m�thode coude
fviz_nbclust(data_numeric, kmeans, method = "wss") + 
  labs(title = "M�thode du coude pour d�terminer le nombre de clusters")

# Utiliser la m�thode de CAH
# Calcul de la matrice de distances
distance_matrix <- dist(data_numeric)

# Appliquer la CAH avec m�thode de regroupement (Ward.D2 est souvent utilis�e)
cah_model <- hclust(distance_matrix, method = "ward.D2")

# Visualiser le dendrogramme
plot(cah_model, main = "Dendrogramme", xlab = "", sub = "")
rect.hclust(cah_model, k = 3, border = "red") # S�paration en 3 clusters

# D�couper l'arbre pour obtenir les clusters
clusters_cah <- cutree(cah_model, k = 3)
table(clusters_cah) # Taille des clusters
# la longueur de mes variables num�riques
length(data_numeric)
table(data_numeric)

# Comparer les affectations des clusters
table(Kmeans_Clusters = kmean$cluster, CAH_Clusters = clusters_cah)

# Visualisation conjointe des clusters
library(ggplot2)
data_numeric_df <- as.data.frame(data_numeric)
data_numeric_df$Kmeans_Clusters <- kmean$cluster
data_numeric_df$CAH_Clusters <- clusters_cah

ggplot(data_numeric_df, aes(x = Kmeans_Clusters, y = CAH_Clusters)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Comparaison des clusters entre k-means et CAH")

# Utiliser ACP
# T�l�charger les biblioth�ques n�c�ssaires
library(FactoMineR)
pca <- PCA(data_numeric,graph=FALSE)

pca$var$contrib # contribution des variables
# Standardiser les donn�es
data_scaled <- scale(data_numeric)

# Appliquer l'ACP
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# R�sum� de l'ACP
summary(pca_result)

# Proportion de variance expliqu�e
variance_expliquee <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(variance_expliquee)

# Graphique de la variance expliqu�e
barplot(variance_expliquee[1:10], main = "Variance expliqu�e par les premi�res composantes",
        xlab = "Composantes principales", ylab = "Proportion de variance expliqu�e", col = "steelblue")

# Ajout des deux premi�res composantes principales
pca_data <- as.data.frame(pca_result$x[, 1:2])  # Extraire PC1 et PC2

# Visualisation : Nuage de points des deux premi�res composantes
# Ajoutez une variable cat�gorielle, si disponible (par exemple, 'type' pour diff�rencier les classes)
if (exists("spam$type")) {
  pca_data$type <- spam$type
  library(ggplot2)
  ggplot(pca_data, aes(x = PC1, y = PC2, color = type)) +
    geom_point(alpha = 0.7) +
    labs(title = "Projection selon les deux premi�res composantes principales",
         x = "Premi�re composante principale (PC1)",
         y = "Deuxi�me composante principale (PC2)") +
    theme_minimal()
} else {
  # Si aucune variable cat�gorielle n'existe, affichez un simple nuage de points
  plot(pca_data$PC1, pca_data$PC2,
       main = "Projection selon les deux premi�res composantes principales",
       xlab = "Premi�re composante principale (PC1)",
       ylab = "Deuxi�me composante principale (PC2)", col = "blue", pch = 16)
}

# Partie 3
# Charger les packages n�cessaires
library(class)
library(caret) # Pour les m�triques de performance

# Supposons que `data_numeric` contient les variables explicatives
# Et que `type` est la variable cible (spam ou non-spam)

# Diviser les donn�es en apprentissage (80%) et test (20%)
set.seed(123) # Pour reproduire les r�sultats
indices <- sample(1:nrow(data_numeric), size = 0.8 * nrow(data_numeric))
train_data <- data_numeric[indices, ]
train_labels <- type[indices]
test_data <- data_numeric[-indices, ]
test_labels <- type[-indices]
print(type)
type<- spam$type
# Divisez les donn�es correctement
train_labels <- type[indices]
test_labels <- type[-indices]
# Appliquer kNN (choisissez k = 4 comme point de d�part)
k <- 4
predictions <- knn(train = train_data, test = test_data, cl = train_labels, k = k)

# Afficher la matrice de confusion
confusion_matrix <- table(Predicted = predictions, Actual = test_labels)
print("Matrice de confusion :")
print(confusion_matrix)

# �valuation des performances
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Pr�cision :", round(accuracy * 100, 2), "%"))

# Calculer des m�triques suppl�mentaires
eval_metrics <- confusionMatrix(confusion_matrix)
print(eval_metrics)

# Question 2
# Charger les packages
library(rpart)
library(rpart.plot)
library(caret) # Pour �valuer les performances

# Supposons que `data_numeric` contient vos donn�es explicatives
# Et `type` est votre variable cible (spam ou non-spam)

set.seed(123) # Pour rendre les r�sultats reproductibles
indices <- sample(1:nrow(data_numeric), size = 0.8 * nrow(data_numeric))
train_data <- data_numeric[indices, ]
train_labels <- type[indices]
test_data <- data_numeric[-indices, ]
test_labels <- type[-indices]

# Construire l'arbre de d�cision
decision_tree <- rpart(train_labels ~ ., data = data.frame(train_data, train_labels),
                       method = "class", # Pour une classification
                       control = rpart.control(minsplit = 20, cp = 0.01)) # Param�tres ajustables

# Visualiser l'arbre
rpart.plot(decision_tree, type = 2, extra = 104, fallen.leaves = TRUE,
           main = "Arbre de d�cision")

# Convertir train_data en data frame
train_data <- as.data.frame(train_data)

# Convertir test_data en data frame
test_data <- as.data.frame(test_data)

class(train_data) # Devrait retourner "data.frame"
class(test_data)  # Devrait retourner "data.frame"

# Combiner les donn�es explicatives et la cible
train_data <- data.frame(train_data, train_labels)

# Construire l'arbre de d�cision
decision_tree <- rpart(train_labels ~ ., data = train_data,
                       method = "class", control = rpart.control(minsplit = 20, cp = 0.01))

# Pr�dictions sur l'�chantillon de test
predictions <- predict(decision_tree, newdata = test_data, type = "class")

# Matrice de confusion
confusion_matrix <- table(Predicted = predictions, Actual = test_labels)
print("Matrice de confusion :")
print(confusion_matrix)

# Pr�cision
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Pr�cision :", round(accuracy * 100, 2), "%"))

# �valuation des m�triques avec le package caret
eval_metrics <- confusionMatrix(confusion_matrix)
print(eval_metrics)

# derni�re partie
ls()
str(spam)
capitalTotal<- spam$capitalTotal
head(capitalTotal)
head(data_numeric)
head(capitalTotal)

# Charger les donn�es
# Supposons que `data_numeric` contient les variables explicatives
# Et `capitalTotal` est la variable cible

set.seed(123)  # Pour des r�sultats reproductibles

# V�rifiez que capitalTotal existe
if (!"capitalTotal" %in% colnames(data_numeric)) {
  stop("La variable 'capitalTotal' n'existe pas dans 'data_numeric'.")
}

# Diviser les donn�es en apprentissage (80%) et test (20%)
indices <- sample(1:nrow(data_numeric), size = 0.8 * nrow(data_numeric))
train_data <- data_numeric[indices, ]
test_data <- data_numeric[-indices, ]
train_target <- data_numeric$capitalTotal[indices]
test_target <- data_numeric$capitalTotal[-indices]

# Conversion en data frame si n�cessaire
data_numeric <- as.data.frame(data_numeric)
colnames(data_numeric)  # Liste des noms des colonnes

head(capitalTotal)
data_numeric$capitalTotal
head(data_numeric)

set.seed(123)  # Pour garantir des r�sultats reproductibles

# Diviser les donn�es en apprentissage (80%) et test (20%)
indices <- sample(1:nrow(data_numeric), size = 0.8 * nrow(data_numeric))
train_data <- data_numeric[indices, ]
test_data <- data_numeric[-indices, ]
train_target <- train_data$capitalTotal  # Extraire la cible pour l'apprentissage
test_target <- test_data$capitalTotal    # Extraire la cible pour le test


# Construire le mod�le lin�aire
model <- lm(capitalTotal ~ ., data = train_data)

# R�sum� du mod�le
summary(model)
