## ---- include=F---------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(corrplot)
library(polycor)
library(car)
library(DMwR)
library(stats)
library(purrr)
library(broom)
library(gam)
library(psych)
library(e1071)


## -----------------------------------------------------------------------------
test = read.csv("./loan_testx.csv", stringsAsFactors = T)
train = read.csv("./loan_train.csv", stringsAsFactors = T)
str(train)


## -----------------------------------------------------------------------------
str(test)


## ----warning=FALSE------------------------------------------------------------
train_n = train %>%
  select(-default) %>%
  keep(is.numeric)
test_n = test %>% 
  keep(is.numeric)

train_n %>%
  gather() %>%
  ggplot() + 
  facet_wrap(~ key, scales = 'free') + 
  stat_ecdf(aes(value), geom = "step", col = 'red') + 
  stat_ecdf(data = test_n %>% gather(), aes(value), geom = "step", col = 'black')


## ----warning=FALSE------------------------------------------------------------
result <- colnames(train_n) %>%
  set_names() %>% 
  map(~ ks.test(train_n[, .x], test_n[, .x])) %>%
  map_dfr(., broom::tidy, .id = "parameter") %>%
  arrange(p.value)
result


## ----warning=FALSE------------------------------------------------------------
train_f = train %>%
  dplyr::select(-default) %>%
  keep(is.factor)

test_f = test %>%
  keep(is.factor)

train_f %>%
  gather() %>%
  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = 'free') +
  geom_histogram(data = test_f %>% gather(), fill = 'black', stat = 'count') + 
  geom_histogram(stat = 'count', fill = 'red', alpha = .75) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


## -----------------------------------------------------------------------------
lm.fit = lm(default ~ ., data = train)
vifplot <- function(vif){
  data.frame(name = rownames(vif), vif) %>%
  arrange(-GVIF) %>%
  mutate(name = factor(name, levels = name)) %>%
  ggplot(aes(y=GVIF, x = name)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


## -----------------------------------------------------------------------------
fit = vif(glm(default~., data = train, family = 'binomial'))
fit[,'GVIF'] = log(fit[,'GVIF'])
vifplot(fit)


## ----warning=FALSE------------------------------------------------------------
hetcor = hetcor(train)
cors = hetcor$correlations
corrplot(cors,
         pch.cex = .9,
         method = 'ellipse', 
         addrect = 12, 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 60)


## -----------------------------------------------------------------------------
train_s = train %>% select(-total_cc,
                           -amount,
                           -out_prncp,
                           -pymnt_rec,
                           -funded,
                           -monthly_payment,
                           -initial_list_status
                           )


## ----warning=FALSE------------------------------------------------------------
fit2 = glm(default~. + quality * interest - quality - interest, data = train_s, family = 'binomial')
anova(fit2, fit)


## -----------------------------------------------------------------------------
vifplot(vif(fit2))


## ----warning=FALSE------------------------------------------------------------
hetcor = hetcor(train_s)
cors = hetcor$correlations
corrplot(cors,
         pch.cex = .9,
         method = 'ellipse', 
         addrect = 12, 
         order = "hclust", 
         tl.col = "black",
         tl.srt = 60)


## -----------------------------------------------------------------------------
ggplot(train_s, aes(y = recover, x = coll_fee)) + geom_point()


## -----------------------------------------------------------------------------
ggplot(train_s, aes(y = interest, x = quality)) + geom_point()


## -----------------------------------------------------------------------------
ggplot(train_s, aes(y = last_payment, x = prin_rec)) + geom_point()

## -----------------------------------------------------------------------------
ggplot(train_s, aes(y = total_acc, x = ncc)) + geom_point()


## -----------------------------------------------------------------------------
apply(train_s, 2, function(x) sum(is.na(x)))


## -----------------------------------------------------------------------------
apply(test, 2, function(x) sum(is.na(x)))


## -----------------------------------------------------------------------------
train_si = knnImputation(train_s)
apply(train_si, 2, function(x) sum(is.na(x)))
train_si %>% group_by(employment) %>% summarize(count = n()) %>% select(employment, count) %>% 
  ggplot(aes(y = count, x = employment)) + geom_bar(stat = 'identity')


## -----------------------------------------------------------------------------
test_i = test
test_i$reason[test_i$reason == 'educ'] <- NA
test_i = knnImputation(test_i)
test_i %>% group_by(employment) %>% summarize(count = n()) %>% select(employment, count) %>% 
  ggplot(aes(y = count, x = employment)) + 
  geom_bar(stat = 'identity') + 
  geom_bar(data = train_si %>% group_by(employment) %>% summarize(count = n()) %>% select(employment, count),
           stat = 'identity', fill = 'darkred') 


## ----warning=FALSE------------------------------------------------------------
train %>%
  keep(is.numeric) %>%
  mutate(default = as.factor(default)) %>%
  gather(-default, key = 'key', value = 'value') %>%
  ggplot(aes(value, fill = default)) + 
  facet_wrap(~ key, scales = 'free') + 
  geom_density(position = 'fill',alpha = .5)


## ----warning=FALSE------------------------------------------------------------
train %>%
  mutate(default = as.factor(default)) %>%
  keep(is.factor) %>%
  gather(-default, key = 'key', value = 'value') %>%
  ggplot(aes(fill = default, y= 1, x=value)) + 
  geom_bar(position = "fill",stat="identity") +
  facet_wrap(~ key, scales = 'free') + 
  theme(axis.text.x = element_text( angle = 60,  hjust = 1 ) )


## -----------------------------------------------------------------------------
set.seed(123)
index = sample(nrow(train_si), .7*nrow(train_si))
dev = train_si[-index,]
train_sit = train_si[index,]


## ----warning=FALSE------------------------------------------------------------
base = glm(default~., data = train_sit, family = 'binomial')
preds = predict(base, newdata = dev, type = 'response')
class = rep(0, length(preds))
class[preds>.5] = 1
mean(dev$default == class)


## -----------------------------------------------------------------------------
summary(base)


## ----warning=FALSE------------------------------------------------------------
interaction = glm(default~.+ quality * interest + last_payment * prin_rec, 
           data = train_sit, family = 'binomial')
preds = predict(interaction, newdata = dev, type = 'response')
class = rep(0, length(preds))
class[preds>.5] = 1
mean(dev$default == class)


## -----------------------------------------------------------------------------
train_knn = train_sit
test_knn = dev

train_knn$term = dummy.code(train_knn$term)
train_knn$employment = dummy.code(train_knn$employment)
train_knn$status = dummy.code(train_knn$status)
train_knn$reason = dummy.code(train_knn$reason)
train_knn$quality = dummy.code(train_knn$quality)

test_knn$term = dummy.code(test_knn$term)
test_knn$employment = dummy.code(test_knn$employment)
test_knn$status = dummy.code(test_knn$status)
test_knn$reason = dummy.code(test_knn$reason)
test_knn$quality = dummy.code(test_knn$quality)


## -----------------------------------------------------------------------------
set.seed(1)
library(class)
knn_error = c()
for(k in 1:60){
  knn.pred = knn(train_knn[,-1], test_knn[,-1], as.factor(train_sit[,1]), k = k)
  knn_error[k] = mean(knn.pred!=dev[,1])
}
plot(knn_error)


## -----------------------------------------------------------------------------
knn.pred = knn(train_knn[,-1], test_knn[,-1], as.factor(train_sit[,1]), k = 9)
mean(knn.pred==dev[,1])


## -----------------------------------------------------------------------------
set.seed(1)
library(kknn)
kknn_error = c()
for(k in 1:100){
  kknn = kknn(as.factor(default) ~ ., train_knn, test_knn, kernel = 'epanechnikov', k = k)
  kknn.pred = kknn$fitted.values
  kknn_error[k] = mean(kknn.pred!=dev[,1])
}
plot(kknn_error)


## -----------------------------------------------------------------------------
which.min(kknn_error)


## -----------------------------------------------------------------------------
kknn = kknn(as.factor(default) ~ ., train_sit, dev, kernel = 'epanechnikov', k = 46)
kknn.pred = kknn$fitted.values
mean(kknn.pred==dev[,1])


## -----------------------------------------------------------------------------
nb <- naiveBayes(default ~ ., data = train_sit)
preds = apply(predict(nb, dev, type = "raw"), 1, which.max)-1
mean(preds==dev[,1])


## ----warning=FALSE------------------------------------------------------------
set.seed(1)
svm.linear <- svm(default ~ ., data = train_sit, kernel = "linear", cross = 10)
svm.linear$tot.MSE
svm.poly <- svm(default ~ ., data = train_sit, kernel = "polynomial", cross = 10)
svm.poly$tot.MSE
svm.rad <- svm(default ~ ., data = train_sit, kernel = "radial", cross = 10)
svm.rad$tot.MSE
svm.sig <- svm(default ~ ., data = train_sit, kernel = "sigmoid", cross = 10)
svm.sig$tot.MSE


svm.rad.preds <- as.numeric(predict(svm.rad, newdata = dev))
svm.rad.class = rep(0, length(svm.rad.preds))
svm.rad.class[svm.rad.preds>.5] = 1
mean(dev$default == svm.rad.class)


## -----------------------------------------------------------------------------
# Select all predictors that are numeric with more than 2 distinct values
ind = apply(train_sit %>% keep(is.numeric), 2, function(x) length(unique(x)))
ind = ind > 2
s_predictors = names(apply(train_sit %>% keep(is.numeric), 2, function(x) length(unique(x))))[ind]
# reason is omitted because of incapability with test set
s_predictors = s_predictors[s_predictors != "reason"]
mid = paste(s_predictors, collapse = ") + s(")
form = as.formula(paste('default ~ . + s(', mid, ')'))
form


## ----warning=FALSE------------------------------------------------------------
gam = gam(form, family = binomial, data = train_sit)


## -----------------------------------------------------------------------------
summary(gam)


## ----warning=FALSE------------------------------------------------------------
preds = predict(gam, newdata = dev)
class = rep(0, length(preds))
class[preds>.5] = 1
mean(dev$default == class)


## -----------------------------------------------------------------------------
set.seed(123)
# Reimpute since we are using all the variables now
train_i = knnImputation(train)
index = sample(nrow(train_i), .7*nrow(train_i))
dev = train_i[-index,]
train_it = train_i[index,]


## ----warning = FALSE----------------------------------------------------------
gam.fit = gam(default ~ . + interest*quality + last_payment*prin_rec, family = "binomial", data = train_it)
  
stepGAM = step.Gam(gam.fit, scope = list(
  # For factor variables and variables that have less than two distinct values, include options of not retaining the variable
  # or removing a dummy variable into the model.
  "reason" = ~1 + reason,
  "n_collect" = ~1 + n_collect,
  "initial_list_status" = ~1 + initial_list_status,
  "recover" = ~1 + recover,
  "coll_fee" = ~1 + coll_fee,
  "term" = ~1 + term,
  "total_acc" = ~1 + total_acc,
  "amount" = ~1 + amount,
  "monthly_payment" = ~1 + monthly_payment,
  "status" = ~1 + status,
  "pymnt_rec" = ~1 + pymnt_rec,
  "quality" = ~1 + quality,
  "violations" = ~1 + violations,
  "del" = ~1 + del,
  "employment" = ~1 + employment,
  # These are the variables that were signnificantly different b/n test and train. Cap their DGFs to 4
  "interest" = ~1 + interest + s(interest),
  "out_prncp_inv" = ~1 + out_prncp_inv + s(out_prncp_inv),
  "req" = ~1 + req + s(req),
  "prin_rec" = ~1 + prin_rec + s(prin_rec),
  "total_cc" = ~1 + total_cc + s(total_cc),
  "out_prncp" = ~1 + out_prncp + s(out_prncp),
  "last_payment" = ~1 + last_payment + s(last_payment),
  "inc" = ~1 + inc + s(inc),
  # For all other variables, consider nonlinear terms of varying degrees of freedom
  "funded" = ~1 + funded + s(funded) + s(funded,10) + s(funded, 20),
  "v1" = ~1 + v1 + s(v1) + s(v1, 10) +  s(v1, 20),
  "int_rec" = ~1 + int_rec + s(int_rec) + s(int_rec, 10) + s(int_rec, 20) ,
  "credit_bal" = ~1 + credit_bal + s(credit_bal)  + s(credit_bal, 10) + s(credit_bal, 20),
  "ncc" = ~1 + ncc + s(ncc)  + s(ncc, 10)  + s(ncc, 20),
  "fees_rec" = ~1 + fees_rec + s(fees_rec) + s(fees_rec, 10) + s(fees_rec, 20),
  "credit_ratio" = ~1 + credit_ratio + s(credit_ratio, 10) + s(credit_ratio, 20)),
  direction = "forward", 
  trace = FALSE)

stepGAM$formula


## -----------------------------------------------------------------------------
summary(stepGAM)


## -----------------------------------------------------------------------------
pred = predict(stepGAM, newdata = dev, type = "response")
class = rep(0, length(pred))
class[pred>.5] = 1
mean(dev$default == class)


## -----------------------------------------------------------------------------
class = rep(0, length(stepGAM$fitted.values))
class[stepGAM$fitted.values>.5] = 1
mean(train_it$default == class)


## ----warning = FALSE----------------------------------------------------------
test_preds = predict(stepGAM, newdata = test_i, type = "response")
class = rep(0, length(test_preds))
class[test_preds>.5] = 1
sum(class)/length(class)


## ----warning=FALSE------------------------------------------------------------
test_preds = data.frame(x = test_preds); train_preds = data.frame(x = stepGAM$fitted.values)
ggplot() +
  geom_histogram(data = test_preds, aes(x), stat="bin", bins = 30, col = 'blue', fill = "blue", alpha = .50)+
  geom_histogram(data = train_preds, aes(x), stat="bin", bins = 30, col = 'red', fill = "red", alpha = .35)



## -----------------------------------------------------------------------------
names(test_preds) = "Predictions"
write.csv(test_preds, "./loan_testy.csv", row.names = FALSE, col.names = FALSE)

