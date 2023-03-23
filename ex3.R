# 3) Importare il dataset cereal. Effettuare una analisi
# descrittiva di tale dataset per familiarizzare con esso 
# (una descrizione delle variabili puo' essere trovata qui:
# https://www.kaggle.com/datasets/crawford/80-cereals).
# Selezionare la variabile risposta che si ritiene piu' 
# sensata e provare a stimare un modello di regressione
# lineare semplice scegliendo una tra le possibili esplicative
# quantitative presenti nel dataset. Interpetare tutti i 
# risultati ottenuti.
library(dplyr)
rm(list= ls())
cereal <- read.csv("Cereal_Original.csv", sep = ";" ,header= T)
View(cereal)
str(cereal)
summary(cereal)

# let's either throw out of the matrix the qualitative variabiles that we dont need
row.names(cereal) <- cereal$name 
cereal <- cereal[,- c(1,2,3)]

# let's convert the other in numeric 
cereal <- mutate_if(cereal, is.character, function(x) gsub(",", ".", x))  
# substitutes "," with "."
cereal <- mutate_if(cereal, is.character, as.numeric)  
# converts characters in numbers

scatterplotMatrix(cereal)
#to many variables let's focus on 2 in particular 


plot(calories ~ sugars , data= cereal)

# there seems to be a positive correlation between the two

abline(v  = 0, lty=2)
# there is one negative value , let's remove it 
cereal$sugars[which(cereal$sugar < 0)] <- NA


plot(calories ~ sugars , data= cereal)
#we successfully removed the outlier

mod <- lm(calories  ~ sugars , data= cereal)

abline(a= mod$coefficients[1], b=mod$coefficients[2])

(rsq <- cor(cereal$calories , cereal$sugars, use="pairwise.complete.obs")^2)
# there is a slight positive correlation between sugar and calories



#-------verifying some descriptive stastistics --------

#let's analyse the relationship bewteen sugar and ratings

plot(rating~ sugars , data= cereal) # there seem to be a negative correlation 

mod2 <- lm(rating~ sugars , data= cereal)

abline(a= mod2$coefficients[1], b=mod2$coefficients[2], col = "red")


sum(residuals(mod2)) # verifying the non-systematic errors 

mean(fitted.values(mod2))

mean(cereal$rating)


#------------
