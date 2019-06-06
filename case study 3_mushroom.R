library(bnlearn)
# source("http://bioconductor.org/biocLite.R")
# biocLite(c("graph", "RBGL", "Rgraphviz"))
library(Rgraphviz)
library(caret)
library(rpart)
library(klaR)
#library(bnclassify)


setwd("~/Desktop/CSC 529/case study 3")
mushroom <- read.table("agaricus-lepiota.data", sep = ",", header = F)
head(mushroom)
str(mushroom)

#data cleaning
#detect missing value
apply(is.na(mushroom),2,sum)

#add varible names
colnames(mushroom)[1] <- "class"
colnames(mushroom)[2] <- "cap_shape"
colnames(mushroom)[3] <- "cap_surface"
colnames(mushroom)[4] <- "cap_color"
colnames(mushroom)[5] <- "bruises"
colnames(mushroom)[6] <- "odor"
colnames(mushroom)[7] <- "gill_attachment"
colnames(mushroom)[8] <- "gill_spacing"
colnames(mushroom)[9] <- "gill_size"
colnames(mushroom)[10] <- "gill_color"
colnames(mushroom)[11] <- "stalk_shape"
colnames(mushroom)[12] <- "stalk_root"
colnames(mushroom)[13] <- "stalk_sf_ab_r"
colnames(mushroom)[14] <- "stalk_sf_bl_r"
colnames(mushroom)[15] <- "stalk_col_ab_r"
colnames(mushroom)[16] <- "stalk_col_bl_r"
colnames(mushroom)[17] <- "veil_type"
colnames(mushroom)[18] <- "veil_color"
colnames(mushroom)[19] <- "ring_no"
colnames(mushroom)[20] <- "ring_type"
colnames(mushroom)[21] <- "spore_col"
colnames(mushroom)[22] <- "population"
colnames(mushroom)[23] <- "habitat"


#since variable 17, veil-color only has 1 level, and bayesian network need at least 2 levels to work with, I removed variable 17 here.
mushroom$veil_type <- NULL

#exploratory analysis
str(mushroom)
summary(mushroom)

#plot frequency bar charts
par(mfcol=c(1, 1))
#cap shape grouped by class
capshape_class <- mushroom[ , c(1, 2)]
head(capshape_class)
tab_capshape_class <- table(capshape_class)
tab_capshape_class
barplot(tab_capshape_class, main="Mushroom cap shape by class", xlab="cap shape", ylab="Count",
                names.arg=c("b","c","f","k","s","x"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,2500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#cap surface grouped by class
capsurface_class <- mushroom[ , c(1, 3)]
head(capsurface_class)
tab_capsurface_class <- table(capsurface_class)
tab_capsurface_class
barplot(tab_capsurface_class, main="Mushroom cap surface by class", xlab="cap surface", ylab="Count",
        names.arg=c("f","g","s","y"),font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,2500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#cap color grouped by class
capcolor_class <- mushroom[ , c(1, 4)]
head(capcolor_class)
tab_capcolor_class <- table(capcolor_class)
tab_capcolor_class
barplot(tab_capcolor_class, main="Mushroom cap color by class", xlab="cap color", ylab="Count",
        names.arg=c("b","c","e","g","n","p","r","u","w","y"),font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,1500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#bruises grouped by class
bruises_class <- mushroom[ , c(1, 5)]
head(bruises_class)
tab_bruises_class <- table(bruises_class)
tab_bruises_class
barplot(tab_bruises_class, main="Mushroom bruises by class", xlab="bruises", ylab="Count",
        names.arg=c("f","t"),font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#odor grouped by class
odor_class <- mushroom[ , c(1, 6)]
head(odor_class)
tab_odor_class <- table(odor_class)
tab_odor_class
barplot(tab_odor_class, main="Mushroom odor by class", xlab="odor", ylab="Count",
        names.arg=c("a","c","f","l","m","n","p","s","y"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#gillattachment grouped by class
gillattachment_class <- mushroom[ , c(1, 7)]
head(gillattachment_class)
tab_gillattachment_class <- table(gillattachment_class)
tab_gillattachment_class
barplot(tab_gillattachment_class, main="Mushroom gillattachment by class", xlab="gillattachment", ylab="Count",
        names.arg=c("a","f"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,5000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#gillspacing grouped by class
gillspacing_class <- mushroom[ , c(1, 8)]
head(gillspacing_class)
tab_gillspacing_class <- table(gillspacing_class)
tab_gillspacing_class
barplot(tab_gillspacing_class, main="Mushroom gillspacing by class", xlab="gillspacing", ylab="Count",
        names.arg=c("c","w"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,4000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#gillsize grouped by class
gillsize_class <- mushroom[ , c(1, 9)]
head(gillsize_class)
tab_gillsize_class <- table(gillsize_class)
tab_gillsize_class
barplot(tab_gillsize_class, main="Mushroom gillsize by class", xlab="gillsize", ylab="Count",
        names.arg=c("b","n"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,4000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#gillcolor grouped by class
gillcolor_class <- mushroom[ , c(1, 10)]
head(gillcolor_class)
tab_gillcolor_class <- table(gillcolor_class)
tab_gillcolor_class
barplot(tab_gillcolor_class, main="Mushroom gillcolor by class", xlab="gillcolor", ylab="Count",
        names.arg=c("b","e","g","h","k","n","o","p","r","u","w","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,2000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalkshape grouped by class
stalkshape_class <- mushroom[ , c(1, 11)]
head(stalkshape_class)
tab_stalkshape_class <- table(stalkshape_class)
tab_stalkshape_class
barplot(tab_stalkshape_class, main="Mushroom stalkshape by class", xlab="stalkshape", ylab="Count",
        names.arg=c("e","t"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalkroot grouped by class
stalkroot_class <- mushroom[ , c(1, 12)]
head(stalkroot_class)
tab_stalkroot_class <- table(stalkroot_class)
tab_stalkroot_class
barplot(tab_stalkroot_class, main="Mushroom stalkroot by class", xlab="stalkroot", ylab="Count",
        names.arg=c("?","b","c","e","r"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,2000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalksfabovering grouped by class
stalksfabovering_class <- mushroom[ , c(1, 13)]
head(stalksfabovering_class)
tab_stalksfabovering_class <- table(stalksfabovering_class)
tab_stalksfabovering_class
barplot(tab_stalksfabovering_class, main="Mushroom stalk surface above ring by class", xlab="stalk surface above ring", ylab="Count",
        names.arg=c("f","k","s","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,4000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalksfbelowring grouped by class
stalksfbelowring_class <- mushroom[ , c(1, 14)]
head(stalksfbelowring_class)
tab_stalksfbelowring_class <- table(stalksfbelowring_class)
tab_stalksfbelowring_class
barplot(tab_stalksfbelowring_class, main="Mushroom stalk surface below ring by class", xlab="stalk surface below ring", ylab="Count",
        names.arg=c("f","k","s","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,4000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalkclabovering grouped by class
stalkclabovering_class <- mushroom[ , c(1, 15)]
head(stalkclabovering_class)
tab_stalkclabovering_class <- table(stalkclabovering_class)
tab_stalkclabovering_class
barplot(tab_stalkclabovering_class, main="Mushroom stalk color above ring by class", xlab="stalk color above ring", ylab="Count",
        names.arg=c("b","c","e","g","n","o","p","w","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#stalkclbelowring grouped by class
stalkclbelowring_class <- mushroom[ , c(1, 16)]
head(stalkclbelowring_class)
tab_stalkclbelowring_class <- table(stalkclbelowring_class)
tab_stalkclbelowring_class
barplot(tab_stalkclbelowring_class, main="Mushroom stalk color below ring by class", xlab="stalk color below ring", ylab="Count",
        names.arg=c("b","c","e","g","n","o","p","w","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#veilcolor grouped by class
veilcolor_class <- mushroom[ , c(1, 17)]
head(veilcolor_class)
tab_veilcolor_class <- table(veilcolor_class)
tab_veilcolor_class
barplot(tab_veilcolor_class, main="Mushroom veilcolor by class", xlab="veilcolor", ylab="Count",
        names.arg=c("n","o","w","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,5000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#ringno grouped by class
ringno_class <- mushroom[ , c(1, 18)]
head(ringno_class)
tab_ringno_class <- table(ringno_class)
tab_ringno_class
barplot(tab_ringno_class, main="Mushroom ring number by class", xlab="ring no", ylab="Count",
        names.arg=c("n","o","t"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,4500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#ringtype grouped by class
ringtype_class <- mushroom[ , c(1, 19)]
head(ringtype_class)
tab_ringtype_class <- table(ringtype_class)
tab_ringtype_class
barplot(tab_ringtype_class, main="Mushroom ring type by class", xlab="ring type", ylab="Count",
        names.arg=c("e","f","l","n","p"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,4000))
legend(x="topright", legend=c("edible", "poisonuus"), fill=c("blue", "yellow"))

#sporecolor grouped by class
sporecolor_class <- mushroom[ , c(1, 20)]
head(sporecolor_class)
tab_sporecolor_class <- table(sporecolor_class)
tab_sporecolor_class
barplot(tab_sporecolor_class, main="Mushroom spore color by class", xlab="spore color", ylab="Count",
        names.arg=c("b","h","k","n","o","r","u","w","y"), font.main=2, col=c("blue", "yellow"), 
        beside=T, ylim = c(0,2500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#population grouped by class
population_class <- mushroom[ , c(1, 21)]
head(population_class)
tab_population_class <- table(population_class)
tab_population_class
barplot(tab_population_class, main="Mushroom population by class", xlab="population", ylab="Count",
        names.arg=c("a","c","n","s","v","y"), 
        font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,3500))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))

#habitat grouped by class
habitat_class <- mushroom[ , c(1, 22)]
head(habitat_class)
tab_habitat_class <- table(habitat_class)
tab_habitat_class
barplot(tab_habitat_class, main="Mushroom habitat by class", xlab="habitat", ylab="Count",
        names.arg=c("d","g","l","m","p","u","w"), font.main=2, col=c("blue", "yellow"), beside=T, ylim = c(0,2000))
legend(x="topright", legend=c("edible", "poisonous"), fill=c("blue", "yellow"))



#create training and test dataset
ind <- sample(2, nrow(mushroom), replace=TRUE, prob=c(0.8, 0.2))
train <- mushroom[ind==1,]
test <- mushroom[ind==2,]

#create gs bayesian network
mushroom.gs <- gs(train)
mushroom.gs
plot(mushroom.gs, main = "gs algorithms")
modelstring(mushroom.gs)

#fit parameters of gs bayesian network
fit.gs <- bn.fit(mushroom.gs, data=train, method="mle")
summary(fit.gs)
fit.gs
mushroom.gs
pred.fit.gs <- predict(fit.gs, data=test, node="class")
pred.fit.gs
t.fit.gs <- table(pred.fit.gs,test$class)
confusionMatrix(t.fit.gs)
# mushroom.gs.bic <- bn.cv(train, bn="gs", loss="pred", loss.args = list(target="class"))

# mushroom.iamb <- iamb(mushroom)
# mushroom.fast <- fast.iamb(mushroom)
# mushroom.inter<- inter.iamb(mushroom)
# compare(mushroom.gs, mushroom.iamb)
# compare(mushroom.gs, mushroom.fast)
# compare(mushroom.gs, mushroom.inter)

#create hc bayesian network
mushroom.hc <- hc(train, score="bic")
mushroom.hc
plot(mushroom.hc, main = "hc algorithms")
modelstring(mushroom.hc)

compare(target=mushroom.gs, current=mushroom.hc, arcs=F)

#build hc bayesian network using 10-fold cross validation
mushroom.hc.bic <- bn.cv(mushroom, bn="hc", loss="pred", loss.args = list(target="class"))
#mushroom.gs <- bn.cv(mushroom, bn="gs", loss="pred", loss.args = list(target="class"))
mushroom.hc.bic
# mushroom.hc.bde <- bn.cv(mushroom, bn="hc", algorithm.args = list(score="bde", iss=1))
# mushroom.hc.bde

#fit parameters of hc bayesian network
fit.hc <- bn.fit(mushroom.hc, data=train, method="mle")
pred.fit.hc <- predict(fit.hc, data=test, node="class")
pred.fit.hc
t.fit.hc <- table(pred.fit.hc,test$class)
confusionMatrix(t.fit.hc)

#naive bayes
train_control <- trainControl(method="cv", number=10)
nb.cv <- train(class~., data=train, trControl=train_control, method="nb")
nb.cv
pred.nb.cv <- predict(nb.cv, data=test)
t.nb.cv <- table(pred.nb.cv,test$class)
confusionMatrix(t.nb.cv)

#naive bayes model
nb <- naive.bayes(train, "class")
nb
plot(nb, main = "naive bayes algorithms")

#fit parameters of nb bayesian network
fit.nb <- bn.fit(nb, data=train, method="mle")
pred.fit.nb <- predict(fit.nb, data=test)
t.fit.nb <- table(pred.fit.nb,test$class)
confusionMatrix(t.fit.nb)

sample1 <- cpdist(fit.nb, nodes="class", evidence=(habitat=="g")&(odor=="n"))
prop.table(table(sample1))

nb
fit.nb
bn.fit.dotplot(fit.nb$odor, main="odor comditional probabilities")
bn.fit.dotplot(fit.nb$ring_type, main="ring type comditional probabilities")
bn.fit.dotplot(fit.nb$spore_col, main="spore color comditional probabilities")
bn.fit.dotplot(fit.nb$habitat, main="habitat comditional probabilities")


compare(mushroom.gs, nb)

#Tree Augmented Naive Bayes Classifier
#tan <- train(class~., data=train, trControl=train_control, method="tan", tuneGrid=grid)
tan <- tree.bayes(mushroom, "class")
tan
pred.tan <- predict(tan, test)
t.tan <- table(pred.tan,test$class)
confusionMatrix(t.tan)
plot(tan, main = "tan algorithms")
modelstring(tan)

#fit parameters of TAN bayesian network
fit.tan <- bn.fit(tan, data=train, method="mle")
summary(fit.tan)
fit.tan
pred.fit.tan <- predict(fit.tan, data=test)
t.fit.tan <- table(pred.fit.tan,test$class)
confusionMatrix(t.fit.tan)

compare(nb, tan)

#create a decision tree
tree <- train(class~., data=train, trControl=train_control, method="rpart")
tree
pred.tree <- predict(tree, test)
t.tree <- table(pred.tree,test$class)
confusionMatrix(t.tree)
