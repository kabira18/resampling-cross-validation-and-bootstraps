library(tidymodels)
theme_set(theme_test())
library(ISLR)



dim(Auto)


auto<-Auto %>% 
  mutate(origin=case_when(
    origin==1~"american",
    origin==2~"european",
    origin==3~"japanese"
  ))


m<-lm(mpg~origin,auto)

library(sjPlot)

plot_model(m,type="pred")




###spliting of data

library(rsample)

set.seed(18)     ##for reproducability

auto_split<-initial_split(auto,prop = 0.80,strata = origin)

auto_train<-training(auto_split)
auto_test<-testing(auto_split)



library(janitor)

tabyl(auto$origin)
tabyl(auto_train$origin)




library(ggridges)
ggplot(auto, aes(x=horsepower,origin))+
  stat_density_ridges(quantile_lines = TRUE)




##

set.seed(18)


auto_split<-initial_split(auto,prop = 0.80,strata = horsepower,breaks = 10)

auto_train<-training(auto_split)
auto_test<-testing(auto_split)



ggplot()+
  geom_density(data = auto,aes(horsepower))+
  geom_density(data = auto_train,aes(horsepower),colour = "green")+
  geom_density(data = auto_test,aes(horsepower),colour = "red")





### fit linear model

lm_fit<-
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>% 
  fit(mpg~origin*horsepower,data=auto_train)

##test performance of the model


test_results<-
  predict(lm_fit,new_data = auto_test) %>% 
  bind_cols(auto_test)


ames_metrics<-metric_set(rsq)
ames_metrics(test_results,truth = mpg,estimate = .preds)


m<-lm(mpg~origin*horsepower,data = auto)


library(performance)
r2(m)





set.seed(18)

folds<-vfold_cv(auto_train,v=4)       ## default fold is (10) ....
folds$splits[[1]]


###setup a model

lm_fit<-
  linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')


##create a workflow

mpg_wf<-
  workflow() %>% 
  add_model(lm_fit) %>% 
  add_formula(mpg~origin*horsepower)


###fit models to folds
trained_models<-fit_resamples(object = mpg_wf,resamples = folds)


##get performance metrics

trained_models %>% 
  collect_metrics(summarize = FALSE)



##get repeated cross-validation objects
set.seed(18)
folds<-vfold_cv(auto_train,strata = origin,repeats = 10)
folds





###specify random forest model
library(ranger)

rf_spec<-
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


###specify workflow

origin_wf<-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_formula(factor(origin)~mpg*horsepower)


##fit models to folds

trained_models<-fit_resamples(object = origin_wf,resamples = folds)


trained_models %>% 
  collect_metrics()




##bootstraping

set.seed(7)
boot_samp<-bootstraps(auto_train,times = 10)


lm_spec<-
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


###specify workflow

mpg_wf<-
  workflow() %>% 
  add_model(lm_spec) %>% 
  add_formula(factor(origin)~mpg*horsepower)


trained_models<-
  fit_resamples(object = mpg_wf,resamples=boot_samp)

trained_models %>% 
  collect_metrics(summarize=TRUE)


