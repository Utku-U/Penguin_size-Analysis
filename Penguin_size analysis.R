
# My goal is to create a good classifier model for the dataset using an SVM application. And in order for me to create this model,
# I first need to be able to observe how different the means of my independent variable groups are in the dependent
# variable or variables. The more different the group means, the more visible, 
# distinguishable the classes mean, and I will also observe the group means with a plot graph.
# And to find out in which dependent variables the group means are different, I wanted to apply MANOVA. 
# But the assumptions for MANOVA need to be met. If not met, as another method, using Box-Plot, the independent
# variable groups' graphs on the dependent variables are examined. The variables with the most different group means can be considered.

# Let's say you found the variables where you can best observe the separation of the classes. And you decided on one of the (linear, radial, polynomial, sigmoid) 
# hyperplanes. That is when we can start building the SVM model.

# Our independent variable = Mission Type
# Dependent variables = ...


# Data Set
# This dataset contains information on penguin species and some characteristics associated with those species.

df <- read.csv("C:/Users/Utku/Downloads/penguins_size.csv")
View(df)
head(df)


# In the SVM model, I only included the continuous variables that I could classify because working 
# with continuous variable values is more efficient.

table(df$species)

names(df)
library(dplyr)
df <- df %>% select(species,culmen_length_mm,culmen_depth_mm,flipper_length_mm,body_mass_g)
View(df)

# I removed the incomplete observations that had no value other than the species. There were only two anyway.


missing_rows <- c()

for (i in 1:nrow(df)) {
  if (any(is.na(df[i, ]))) {
    missing_rows <- c(missing_rows,i)
  }
}

df <- df[-missing_rows,]
View(df)





######## Group means check for multiple dependent variables 

# With MANOVA, I will look at which multiple dependent variables the independent variable groups have high 
# mean differences with. For this, I first need to satisfy the MANOVA assumptions. 
# (Multivariate normality, Var-Cov homogeneity, Independence of observations, Absence of multicollinearity)

# MANOVA assumptions check:
# Normality, var-cov homogeneity, Multicollinearity. From these, we will check normality and var-cov homogeneity.



library(tidyverse)
# ------------- Some summary statisctics ------------- 

library(rstatix)
df %>%
  group_by(species) %>%
  get_summary_stats(culmen_length_mm, culmen_depth_mm,
                    flipper_length_mm, body_mass_g,type = "mean_sd")

# ------------- Groups count------------- 
df %>%
  group_by(species) %>%
  summarise(N = n())


#------------- Outliers and Normality test------------- 

# First, let's plot the histogram graphs of each variable individually to see if they follow a normal distribution and 
# if there are any outliers.

# 1. Way
library(ggplot2)
library(gridExtra)
data <- df[,2:5]

h1 <- ggplot(data,aes(x=culmen_length_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "culmen_length_mm")

h2 <- ggplot(data,aes(x=culmen_depth_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "culmen_depth_mm")

h3 <- ggplot(data,aes(x=flipper_length_mm))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "flipper_length_mm")

h4 <- ggplot(data,aes(x=body_mass_g))+
  geom_histogram(fill="blue",color="black")+
  labs(title = "body_mass_g")


grid.arrange(h1,h2,h3,h4,ncol=2)


# 2. Way
shapiro.test(data$culmen_length_mm)
shapiro.test(data$culmen_depth_mm)
shapiro.test(data$flipper_length_mm)  # H0: There is normality.
shapiro.test(data$body_mass_g)       # Ha: None.       H0 can be rejected.


# Our data does not follow a normal distribution. One of MANOVA's assumptions is not satisfied.






#----------- Var-Kov homogeneity -----------

# To assess var-kov homogeneity, we use the Box-M test.
# install.packages("biotools")
library(biotools)
boxM_result <- boxM(df[2:5], df$species)  # H0: The variances-kov are homogeneous.
print(boxM_result)                        # Ha: The variances-kov are not homogeneous. H0 RED. 










# Var-cov equality is not satisfied. One more assumption of MANOVA is not met.


# Since the assumptions are not met, MANOVA cannot be used; instead, its non-parametric alternative, Kruskal-Wallis, is used.
# It is done one by one for the dependent variables. Or
# If we want to look at the non-parametric MANOVA test statistic for multiple independent variables at the same time:
# PERMANOVA is used. We use this with the adonis2 function.


## One by one Kruskall Wallis
kruskal.test(culmen_length_mm ~ species, data = df)
kruskal.test(culmen_depth_mm ~ species, data = df)
kruskal.test(flipper_length_mm ~ species, data = df)
kruskal.test(body_mass_g ~ species, data = df)


## Multi kruskall wallis 

# install.packages("vegan")
library(vegan)

names(df)
# Rank-based distance calculation (e.g., Euclidean + rank)
rank_dist <- dist(apply(df[, c("culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g")],
                        2, rank))  # Multi depented varibales 


# Permanova as an alternative to Manova, depending on the group variable.
adonis2(rank_dist ~ species, data = df, permutations = 999)  # We checked whether the distance rankings changed according to the groups
# H0: The distances of the dependent variables do not vary between groups.
# Ha: They do vary. H0 is rejectable. The differences between species are highly significant. 


disp <- betadisper(rank_dist, df$species) 
TukeyHSD(disp)    # There is no statistically significant difference between the hinstrap and Adelie groups based on the independent variable values.


########## This is why, regardless of which independent variables we use in our SVM model, we will not be able to achieve 100% discrimination.




#----------- Control of BOX-Plot with mean of groups -----------
names(df)

library(ggpubr)
g1 <- ggboxplot(
  df, x = "species", y = c("culmen_length_mm"), 
  merge = TRUE, color = "blue")

g2 <- ggboxplot(
  df, x = "species", y = c("culmen_depth_mm"), 
  merge = TRUE, color = "orange")

g3 <- ggboxplot(
  df, x = "species", y = c("flipper_length_mm"), 
  merge = TRUE, color = "lightblue")

g4 <- ggboxplot(
  df, x = "species", y = c("body_mass_g"), 
  merge = TRUE, color = "red")

grid.arrange(g1,g2,g3,g4,ncol=2)


# What I want to see here is that for all corresponding dependent variables, the independent variable groups
# have different means.
# Here, they are all close to each other. In these cases, it is quite difficult to observe class separation using plots.

# If I could see a single dependent variable where the group means are different. Based on a single dependent variable,
# I will do a plot analysis and perform classification.
# If I could see multiple variables where the group means are different. Based on multiple independent variables,
# I will do a plot analysis and perform classification.








####### Plot graphs of single variables

#------------- Culmen length -------------

# Görev süresi değişkeni değerlerimizin. görev tiplerine göre plot grafiğini çizdirdim.
plot(df$culmen_length_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)]) # lineer, polinomal, redia ya da sigmoid ilişki gözükmüyor.



# Görev süresi değişkenimiz ve bu değişkenin karesini alarak 2 boyuta çıkarttım ve exp bir artış yakalayarak 
# görev tiplerine göre görev yılları arasında görünür bir ayrım yakalamaya çalışıyorum. 
culmen2 <- (df$culmen_length_mm)**2

plot(df$culmen_length_mm,culmen2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)]) # Burada da lineer, polinomal, redia ya da sigmoid ilişki gözükmüyor.



# Bu sefer 3D incelemek için bu değişkenin küpünü de alıyorum. Böylelikle 3 boyutlu epx bir grafik yapalamaya çalışıyorum.
culmen3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$culmen_length_mm,
  y = culmen2,
  z = culmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)

#------------- Culmen depth -------------

# Tek boyutlu
plot(df$culmen_depth_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# iki boyutlu
Culmend2 <- (df$culmen_depth_mm)**2

plot(df$culmen_depth_mm,Culmend2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# üç boyutlu
Culmend3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$culmen_depth_mm,
  y = Culmend2,
  z = Culmend3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)




#------------- Flipper Length -------------

# One dimention
plot(df$flipper_length_mm,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# Two dimention 
flipper2 <- (df$flipper_length_mm)**2

plot(df$flipper_length_mm,flipper2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])

# three dimention
flipper3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$flipper_length_mm,
  y = flipper2,
  z = flipper3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)



# ------------- Body mass -------------

plot(df$body_mass_g,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])


body2 <- (df$body_mass_g)**2

plot(df$body_mass_g,body2,pch=19,
     col=c("blue","orange","lightblue")[as.factor(df$species)])


body3 <- c(1:nrow(df))

library(plotly)
plot_ly(
  x = df$body_mass_g,
  y = body2,
  z = body3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)





######### Plot graphs of multi variables

# ------------- Culmen length & Culmen depth -------------

# One dimention
plot(df$culmen_length_mm,df$culmen_depth_mm,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)])  


# Two dimentions
Mculmen3 <- c(1:nrow(df))
library(plotly)

plot_ly(
  x = df$culmen_length_mm,
  y = df$culmen_depth_mm,
  z = Mculmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)
# ------------- Culmen length & flipper length ------------- !!!!!!!!

plot(df$culmen_length_mm,df$flipper_length_mm,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)])  


# three dimentions
Mculmen3 <- c(1:nrow(df))
library(plotly)

plot_ly(
  x = df$culmen_length_mm,
  y = df$flipper_length_mm,
  z = Mculmen3,
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)


# ------------- flipper length & body mass -------------

# Tek boyutlu
plot(df$flipper_length_mm,df$body_mass_g,pch=19,
     col=c("blue","orange","pink")[as.factor(df$species)]) 

# Üç boyutlu
library(plotly)

plot_ly(
  x = df$flipper_length_mm,
  y = df$body_mass_g,
  z = 1:nrow(df),
  type = 'scatter3d',
  mode = 'markers',
  color = df$species,
  colors = c('blue', 'orange', 'lightblue')
)












# Making model

library(caret)
library(e1071)
library(caTools)
library(ggplot2)

df$species <- as.factor(df$species)
class(df$species)

table(df$species)


# Train ve Test data split
set.seed(312)
TrainIndex <- sample(1:nrow(df), size= 0.8*nrow(df))

TrainSet <- df[TrainIndex,]
TestSet <- df[-TrainIndex,]

table(TrainSet$species)
table(TestSet$species)




# culmen_length_mm & flipper_length_mm
# Model 1
library(e1071)
library(ggplot2)

# Model 1
SVM11 <- svm(species ~ culmen_length_mm + flipper_length_mm, data = TrainSet, kernel = "linear")
summary(SVM11)
SVM11$SV
plot(SVM11, TrainSet, formula = culmen_length_mm ~ flipper_length_mm)

# Linear Model
# Tahminler
predLinear1 <- predict(SVM11, TestSet)
predLinear1
# Comparing prediction values with y values 
caret::confusionMatrix(predLinear1, TestSet$species)
# Adding F1 score
caret::confusionMatrix(predLinear1, TestSet$species, mode="prec_recall")



# Model 2
SVM12 <- svm(species ~ culmen_length_mm + flipper_length_mm, data = TrainSet, kernel = "radial")
summary(SVM12)
SVM12$SV
plot(SVM12, TrainSet, formula = culmen_length_mm ~ flipper_length_mm)

# Radial Model
# Prediction values
predRedial1 <- predict(SVM12, TestSet)
# Comparing
caret::confusionMatrix(predRedial1, TestSet$species)
# F1 score
caret::confusionMatrix(predRedial1, TestSet$species,mode="prec_recall")







# culmen_length_mm ve culmen_depth_mm 

# Model 1
SVM21 <- svm(species ~ culmen_length_mm + culmen_depth_mm, data = TrainSet, kernel = "linear")
summary(SVM21)
SVM21$SV

plot(SVM21, TrainSet, formula = culmen_length_mm ~ culmen_depth_mm)

# Predicitons
predLinear2 <- predict(SVM21, TestSet)
predLinear2
# Comparing
caret::confusionMatrix(predLinear2, TestSet$species)
# F1 
caret::confusionMatrix(predLinear2, TestSet$species, mode="prec_recall")



# Model 2
SVM22 <- svm(species ~ culmen_length_mm + culmen_depth_mm, data = TrainSet, kernel = "radial")
summary(SVM22)
SVM22$SV
plot(SVM22, TrainSet, formula = culmen_length_mm ~ culmen_depth_mm)


# predictions
predRedial2 <- predict(SVM22, TestSet)
# Comparing
caret::confusionMatrix(predRedial2, TestSet$species)
# F1 
caret::confusionMatrix(predRedial2, TestSet$species,mode="prec_recall")







# MODEL TUNİNG

# culmen_length_mm and flipper_length_mm modellerinin Tuning (Optimal Gamma ve C parametreleri)

SVM11Tune <- tune(svm, species ~ culmen_length_mm + flipper_length_mm , data = TrainSet, 
                  kernel = "linear",
                  ranges = list(cost= 2^(-4:2)),               # Gamma paramater doesnt in linear kernel
                  tunecontrol = tune.control(cross = 5))

SVM12Tune <- tune(svm, species ~ culmen_length_mm + flipper_length_mm , data = TrainSet, 
                  kernel = "radial",
                  ranges = list(gamma = 2^(-2:2), cost= 2^(-4:2)),
                  tunecontrol = tune.control(cross = 5))

SVM11Tune
SVM12Tune


SVM11Tune$performances # Parameters that apparently produce the fewest errors, cost= 1
SVM12Tune$performances # Parameters that apparently produce the fewest errors., gamma= 4, cost= 4

SVM11Tune$best.model  # Our model yielded 56 support vectors.
SVM12Tune$best.model  # Our model yielded 90 support vectors.


# predict
predLinearTune1 <- predict(SVM11Tune$best.model, TestSet)
predRedialTune1 <- predict(SVM12Tune$best.model, TestSet)

caret::confusionMatrix(predLinearTune1, TestSet$species)
caret::confusionMatrix(predRedialTune1, TestSet$species)
# With the goodness of model prediction results after obtaining optimal gamma and c parameters
# The prediction values obtained before finding the optimal parameter values are almost equally good.








# culmen_length_mm ve culmen_depth_mm Modellerinin Tuning İşlemleri (Optimal Gamma ve C parametreleri)
library(e1071)

SVM21Tune <- tune(svm, species ~ culmen_length_mm + culmen_depth_mm , data = TrainSet, 
                  kernel = "linear",
                  ranges = list(cost= 2^(-4:2)),  # linear karnel da gamma parametresi olmaz.
                  tunecontrol = tune.control(cross = 5))

SVM22Tune <- tune(svm, species ~ culmen_length_mm + culmen_depth_mm , data = TrainSet, 
                  kernel = "radial",
                  ranges = list(gamma = 2^(-2:2), cost= 2^(-4:2)),
                  tunecontrol = tune.control(cross = 5))

SVM21Tune
SVM22Tune


SVM21Tune$performances # Parameters that apparently produce the fewest errors, cost= 1
SVM22Tune$performances # Parameters that apparently produce the fewest errors., gamma= 0.5, cost= 0.25

SVM21Tune$best.model  # Our model yielded 41 support vectors.
SVM22Tune$best.model  # Our model yielded 57 support vectors.


# predict
predLinearTune2 <- predict(SVM21Tune$best.model, TestSet)
predRedialTune2 <- predict(SVM22Tune$best.model, TestSet)

caret::confusionMatrix(predLinearTune2, TestSet$species)
caret::confusionMatrix(predRedialTune2, TestSet$species)
# With the goodness of model prediction results after obtaining optimal gamma and c parameters
# The prediction values obtained before finding the optimal parameter values are almost equally good.












































######### Decision Tree 

# Bu veri setimiz için SVM uyguladım iyi sonuçlar elde ettim. Fakat SVM uygularken yalnızca sürekli bağımsız değişkenleri
# kullandım. Şimdi ise decision tree ile bütün bağımsımsız değişkenlerin kullanıldığı bir model oluşturmak istiyorum.
# Bunun için öncelikle karakter türüne sahip olan değişkenlerimi factore çevireceğim ya da dummy değişkenler elde edeceğim.
# Daha sonrasında decision tree modeli uygulamaya koyulabilirim.

df <- read.csv("C:/Users/Utku/Downloads/penguins_size.csv")
View(df)
names(df)


# Öncelikle ben SVM ile güzel bir sınıflandırma modeli elde ettim. Fakat Decision tree ile daha iyi veya aynı kalitede bir sınıflandırma modeli yapılabilir mi buna bakıyorum. 
# Decision tree kullanmak istememin sebebi sınıflandırma modelime SVM deki gibi yalnızca sürekli değişkenleri değil, cinsiyet ve lokasyon kategorik değişkenlerini de
# hesaba katmak istiyorum. Fakat bunun için YALNIZCA kategorik değişkenlerimde rslaycağım NA değerli satırları tespit edip bu NA değerlerine atamalar yapmak istiyorum.
# (neredeyse tüm değerleri NA olan gözlemleri kastetmiyorum.) O NA değerlerini doldurayım ki iyi bir decision tree modeli elde edebileyim.

# NA değerlerini nasıl dolduracağım ? SVM modeli ile elde ettiğim sınıflandırma modelimi kullanarak.

# Kayıp gözlem var mı?
library(mice)
md.pattern(df)

na <- is.na(df$sex)
table(na)



# Sürekli değişkenlere karşılık gelen satırlarda bütün değerleri na olanlar var ise onları tespit edeceğim. Ve veriden atacağım.

numeric_col <- sapply(df, is.numeric)
numeric_col

numeric_data <- df[,numeric_col]
numeric_data

na_rows <- apply(numeric_data, 1, function(x) all(is.na(x)))
na_rows

df_cleaned <- df[!na_rows,]

df_cleaned
View(df_cleaned)

# artık elimde yalnızca kategorik değişkenlerdeki na değerleri kaldı. Bunlara düzgün atamalar yapacağım.
library(dplyr)
df_cleaned <- df_cleaned %>%
  mutate(across(where(is.character), ~na_if(., ".")))

table(df_cleaned$sex)
View(df_cleaned)

sum(is.na(df$sex)) # 10 adet NA değerimiz var.




######## NA değeri olan ve olmayan satırlar olarak veriyi ikiye ayıracağım. 
# Amacım, önceden türlerin sınıflandırması için oluşturduğum SVM modelini cinsiyet için oluşturacağım ki buna göre, NA değerlerine bir atama yapabileyim.

df_cleaned_train <- df_cleaned[!is.na(df_cleaned$sex), ]  # Sınıfı bilinenler
df_cleaned_train
df_cleaned_test <- df_cleaned[is.na(df_cleaned$sex), ]    # Sınıfı bilinmeyenler
df_cleaned_test


library(e1071)
names(df_cleaned)
df_cleaned_train$sex <- as.factor(df_cleaned_train$sex)

model <- svm(sex ~ culmen_length_mm + culmen_length_mm + flipper_length_mm + body_mass_g, 
             data= df_cleaned_train,
             karnel="radial")

summary(model)

pred <- predict(model, df_cleaned_test)
pred


library(caret)
caret::confusionMatrix(pred, df_cleaned_test$sex)














# NA değerleri doldurulmuş olan kategorik değişkenlerim için Dummy encoding oluşturacağım bunun için 
# model.matrix fonksiyonu işimi görmez onun yerine dummy_cols fonksyionunu kullanmam daha pratik.

#install.packages("fastDummies")
library(fastDummies)
df <- fastDummies::dummy_columns(df, select_columns = c("island","sex"), remove_selected_columns = T)
View(df)

# kategorik tüm kategorik değişkenleri factore çevireceğim.
ifelse()

# 
df$island <- as.factor(df$island)
df$island
table(df$island)

class(df$island)

















