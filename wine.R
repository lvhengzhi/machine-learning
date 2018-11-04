# mclust
install.packages("fastICA")
library(mclust)
library(caret)
library(fastICA)



# read wine quality data
wine <- read.csv("wineTrainingset1.csv")
# set seed
set.seed(100)
# use kmeans
winekmeans <- kmeans(wine[,-12], centers = 2, nstart = 20)
table(winekmeans$cluster, wine$qualitylevel)
# use EM
wineem <- Mclust(wine[,-12])

# read diabetes data
diabetes <- read.csv("diabetesTrainingset.csv")
# use kmeans
diabeteskmeans <- kmeans(diabetes[,-9], centers = 2, nstart = 20)
table(diabeteskmeans$cluster, diabetes$outcome)
# use EM
diabetesem <- Mclust(diabetes[,-9])

# wine PCA
winepca <- prcomp(wine[,-12],scale. = TRUE)
winepca$rotation[1:5,1:4]
# compute variance
std_dev <- winepca$sdev
winevar <- std_dev^2
#propotion of variance explained
winevarex <- winevar/sum(winevar)

# scree plot
plot(winevarex, xlab = "Principal Component",
     ylab = "Propotion of Variance Explained",
     type = "b")
plot(cumsum(winevarex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

# kmeans with pca
winepcakmeans <- kmeans(winepca$x[,1:9], centers = 2, nstart = 20)
table(winepcakmeans$cluster, wine$qualitylevel)

# neural network with pca
#set CrossValidation
cv <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

wine_pca_nnet <- train(x = winepca$x[,1:8], y = as.factor(wine$qualitylevel), 
                   method = 'nnet',
                   trControl = cv,
                   tuneGrid = expand.grid(.size = c(3, 6, 9, 12),
                                        .decay = c(0.001, 0.1, 0.7)),
                   trace = FALSE,
                   maxit = 1000)
wine_pca_nnet


#wine ICA
wineica <- fastICA(wine[,-12], n.comp = 2)
#colnames(wineica$S) <- c("IC1","IC2","IC3","IC4","IC5","IC6","IC7","IC8")
# kmeans with pca
wineicakmeans <- kmeans(wineica$S, centers = 2, nstart = 20)
table(wineicakmeans$cluster, wine$qualitylevel)
#neural network with ica
wine_ica_nnet <- train(x = wineica$S, y = as.factor(wine$qualitylevel), 
                   method = 'nnet',
                   trControl = cv,
                   tuneGrid = expand.grid(.size = c(3, 6, 9, 12),
                                          .decay = c(0.001, 0.1, 0.7)),
                   trace = FALSE,
                   maxit = 1000)
wine_ica_nnet


#random projection
winerp <- RandomProjection(wine[,-12], 8)
colnames(winerp$RP) <- c("RP1","RP2","RP3","RP4","RP5","RP6","RP7","RP8")
# kmeans with pca
winerpkmeans <- kmeans(winerp$RP, centers = 2, nstart = 20)
table(winerpkmeans$cluster, wine$qualitylevel)

#neural network with rp
wine_rp_nnet <- train(x = winerp$RP, y = as.factor(wine$qualitylevel), 
                       method = 'nnet',
                       trControl = cv,
                       tuneGrid = expand.grid(.size = c(3, 6, 9, 12),
                                              .decay = c(0.001, 0.1, 0.7)),
                       trace = FALSE,
                       maxit = 1000)
wine_rp_nnet