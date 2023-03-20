# Homework:
# 1) Effettuare una analisi descrittiva sul dataset "Davis" per l'altezza dichiarata e l'altezza vera

# 2) Correggere l'errore che ha causato l'outlier e rieffettuare le analisi

# 3) Importare il dataset cereal. Effettuare una analisi
# descrittiva di tale dataset per familiarizzare con esso 
# (una descrizione delle variabili puo' essere trovata qui:
# https://www.kaggle.com/datasets/crawford/80-cereals).
# Selezionare la variabile risposta che si ritiene piu' 
# sensata e provare a stimare un modello di regressione
# lineare semplice scegliendo una tra le possibili esplicative
# quantitative presenti nel dataset. Interpetare tutti i 
# risultati ottenuti. # not done in this file 

library(car)
data("Davis")
View(Davis)
str(Davis)
summary(Davis)


all(is.na(c(Davis$height, Davis$repht)) == F)
# some of the values are NA so we have to proceed conscientiously, 
sum(is.na(Davis$repht)) # 17 missing values
sum(is.na(Davis$height)) # no missing values
#I decide that for this analysis to disegard any pair of obs that isn't complete.


plot(repht ~ height , data=Davis , xlab="height" , ylab= "reported height")
#It's clear that there is an outlier in this data set. it's very unlikely that someone who is
#60 cm tall reports to be 163 cm 


# if we procede with the analysis without excluding the outlier the model isn't as precise as it could be
mod <- lm ( repht ~ height , data=Davis )
abline(a = mod$coefficients[1], b=mod$coefficients[2], col="blue") #the model clearly could be fitted better

summary(mod)
# we still see that it's still a significant model B0 and B1 are relevant because p-value: < 2.2e-16
Rsq <- cor(Davis$height,Davis$repht, use="pairwise.complete.obs")^2 
# the goodness of fit is not that high


# now let's exclude the outlier from our analysis 

(n_outlier <- which(Davis$height < 80 ))
# the 12th record should be the outlier 

#let's see it 
Davis[12,]
#   sex weight height repwt repht
#12   F    166     57    56   163
# probably the weight was switched with the height
# so let's exlude this record 


Davis2 <- Davis[-12,]

#and now let's run the same code as before 


mod2 <- lm ( repht ~ height , data=Davis2 )

abline(a = mod2$coefficients[1], b=mod2$coefficients[2], col="red") 

summary(mod)

(Rsq2 <- cor(Davis2$height,Davis2$repht, use="pairwise.complete.obs")^2) 
# Now the R^2 is almost one, which indicates an almost perfect correlation  
# Furthermore the line maps much better on the data







