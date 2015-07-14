# load data
ourdata <- read.csv(file.choose())

# check number of obs.
dim(ourdata)
dim(na.omit(ourdata))

# split dataset into training & test dataset
n_origin = nrow(ourdata)
train_index = sample(1:n_origin, n_origin/100*60)
train = ourdata[train_index,]
test = ourdata[-train_index,]
dim(train)
dim(test)

write.csv(train_index, file = file.choose()) # when reload, use option 'row.names=1' suggested.
write.csv(train, file = file.choose())
write.csv(test, file = file.choose())

train_index <- as.vector(read.csv(file.choose(), row.names=1))
train = ourdata[train_index[,1],]
test = ourdata[-train_index[,1],]

# draw plots
pairs(ourdata)

# fit logistic reg.
ourfit01 <- glm(EMPLOYED ~ GENDER + AGE + GPA + ENG_SCORE + INTERN_DUMMY
                , data=ourdata, subset=train_index[,1], family=binomial(logit))
summary(ourfit01)

# stepwise selection
ourfit02 <- step(ourfit01, direction='both')
summary(ourfit02)

# Odds Ratios
exp(cbind(OR = coef(ourfit02), confint(ourfit02)))

# test
library(logistf)
testfit <- logistf(EMPLOYED ~ GENDER + AGE + ENG_SCORE + INTERN_DUMMY, data = ourdata)
summary(testfit)

# missclassification rate
fitprobs = predict(ourfit02, newdata=test)
pred = rep(0, nrow(test))
pred[fitprobs > 0.5] = 1
table(pred, test$EMPLOYED)
1 - sum(test$EMPLOYED == pred)/nrow(test)



###################################
# Quadratic Discriminant Analysis #
###################################

library(MASS)

fit.qda = qda(EMPLOYED ~ GENDER + AGE + GPA + ENG_SCORE + INTERN_DUMMY, data=ourdata, subset=train_index[,1])
fit.qda

# Test error
fitpred = predict(fit.qda, newdata=test)
1 - sum(fitpred$class == test$EMPLOYED)/nrow(test)

library(ggplot2)
library(grid)

# plot
Y_HAT <- fitpred$class

ggplot(test, aes(GPA, AGE)) +
  #aes(shape = factor(Y_HAT)) +
  #facet_grid(GEN ~ INTERN, scales = "free") +
  geom_point(aes(colour=Y_HAT), size=4) +
  scale_color_manual(values=c("#00bfc4", "#f8766d")) +
  geom_point(colour="grey90", size = 1.5) +
  labs(title='Prediction Plot') +
  theme(text = element_text(size=14), legend.position = c(.12, .9))



########################
# Classification Trees #
########################

library(tree)

fit.tree = tree(EMPLOYED ~ GENDER + AGE + GPA + INTERN_DUMMY + ENG_SCORE
                , data=ourdata, subset=train_index[,1]
                , split='gini')
summary(fit.tree)

fit.tree.prune = prune.tree(fit.tree, best=10, newdata=test)
summary(fit.tree.prune)

# show results
fit.tree.prune
plot(fit.tree.prune, type="uniform")
text(fit.tree.prune, pretty=0)
title('Result of Classification Tree')

# plot results with ggplot2
library(ggplot2)
library(ggdendro)

tree_data <- dendro_data(fit.tree.prune)

temp <- label(tree_data)
temp_2 <- fit.tree.prune$frame$splits[fit.tree.prune$frame$splits!='']
temp$value = temp_2[1:(length(temp_2)/2)]

temp2 <- leaf_label(tree_data)
temp2$label2 <- ifelse(temp2$label>0.5, 'Employed', 'Unemployed')

ggplot(segment(tree_data)) +
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend, size=n), colour='lightblue') +
  scale_size("n") +
  geom_text(data=label(tree_data), aes(x=x, y=y, label=label), vjust=-0.5, size=6) +
  geom_text(data=temp, aes(x=x, y=y, label=value), vjust=1, size=5) +
  geom_text(data=temp2, aes(x=x, y=y, label=label2), vjust=0.5, size=4) +
  theme_dendro() +
  theme(text=element_text(size=18), legend.position = c(.9, .75)) +
  annotate('text', label='Results of Classification Tree', x = mean(temp$x), y = max(temp$y)+1.4, size = 10)

#ggdendrogram(tree_data, rotate=F) + labs(title="Results of Classification Tree")

# Test error
fitprobs = predict(fit.tree.prune, newdata=test)
pred = rep(0, nrow(test))
pred[fitprobs > 0.5] = 1
table(pred, test$EMPLOYED)
1 - sum(test$EMPLOYED == pred)/nrow(test)

