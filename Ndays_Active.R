library(reshape)
library(reshape2)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(Metrics)
library(hydroGOF)
library(e1071)
# library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(kernlab)
library(rpart)
library(randomForest)
library(gbm)

# # create example data
# X <- c("A", "B", "C", "A", "B", "C")
# Y <- c(10, 20, 30, 15, 25, 35)
# 
# # create dummy variables
# X_dummies <- model.matrix(~ X - 1)
# 
# # generate linear regression model
# model <- lm(Y ~ X_dummies)
# 
# # display summary of model
# summary(model)



#Property Data 
total_data <- read.csv(file.choose())

#total_data <- read_csv("sdmproject.csv")
total_df = as.data.frame(total_data)
total_df = total_df[,!(names(total_df) %in% c("...1","gender"))]
# total_df.drop("...1")
new_df = total_df
#outliers removal 
clean_df = total_df
#clean_df = drop_na(clean_df) # gender is not being used , 
#incomplete_flag
clean_df = clean_df[clean_df$incomplete_flag==0 ,]
#gender - o is being removed as we have only 4 observations
#clean_df = subset(clean_df, clean_df$gender %in% c("m", "f"))
# viewed = 0 , explored = 1 
clean_df = clean_df[!(clean_df$viewed == 0 & clean_df$explored == 1),]
#Explored , n_chapters
# Assumpotion - atleast one of students accessed all chapters in coursework
Agg = aggregate(nchapters ~ course_id, data = clean_df, FUN = max)
Agg$chapter_limit = round(Agg$nchapters/2,0)
Agg[Agg$course_id==1,2]/2
Agg = Agg[,!(names(Agg) %in% c('nchapters'))]
clean_df = merge(clean_df,Agg, by = 'course_id')
clean_df =  clean_df[(clean_df$explored == 1 & clean_df$nchapters >= clean_df$chapter_limit) | (clean_df$explored == 0 & clean_df$nchapters < clean_df$chapter_limit), ]
clean_df = clean_df[,!(names(clean_df) %in% c('chapter_limit'))]
#date 
clean_df = clean_df[(clean_df$last_event_DI >= clean_df$start_time_DI),]
#Grade, certified 
Agg1 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = max )
Agg2 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = min )
Agg_m = merge(Agg1,Agg2, by = c('course_id','certified') )
Agg_f = Agg_m[Agg_m$certified == 0 ,c('course_id','grade.x')]
clean_df = merge(clean_df,Agg_f, by = 'course_id')
clean_df = clean_df[(clean_df$certified == 0 & clean_df$grade <= clean_df$grade.x) | (clean_df$certified == 1 & clean_df$grade > clean_df$grade.x),]
clean_df = clean_df[,!(names(clean_df) %in% c('grade.x'))]
Agg1 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = max )
Agg2 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = min )
Agg_m = merge(Agg1,Agg2, by = c('course_id','certified') )

#age >= 9  - can be implemented
# adding days_diff
clean_df$n_days = as.numeric(difftime(clean_df$last_event_DI, clean_df$start_time_DI, units = "days")+1)

test_df = clean_df[clean_df$ndays_act > clean_df$n_days,c('start_time_DI','last_event_DI','ndays_act','n_days')]

# ndays_act <= n_day
clean_df = clean_df[clean_df$ndays_act <= clean_df$n_days,]


# anymore outlier conditions should be implemented



# Non Numeric columns are being factorised for correlation calculation
colnames(total_df)

#course_id 
L = factor(clean_df$course_id)
K = as.numeric(L)
clean_df$course_id = K
# [1] "14.73x" "2.01x"  "3.091x" "6.002x" "6.00x"  "7.00x"  "8.02x"  "8.MReV" "CB22x" 
#[10] "CS50x"  "ER22x"  "PH207x" "PH278x"

# institute - need to be needed as course and institute are directly related
L = factor(clean_df$institute, levels = c("MITx","HarvardX"))
K = as.numeric(L)
clean_df$institute = K
#"HarvardX" "MITx"


# Semester
L = factor(clean_df$semester)
K = as.numeric(L)
clean_df$semester = K
# "Fall"   "Spring" "Summer"

# LoE_DI
L = factor(clean_df$LoE_DI, levels = c("Less than Secondary","Secondary" ,"Bachelor's","Master's","Doctorate"))
levels(L)
K = as.numeric(L)
clean_df$LoE_DI = K

# gender
# L = factor(clean_df$gender)
# levels(L)
# K = as.numeric(L)
# clean_df$gender = K
#"f" "m" "o"

# final_cc_cname_DI
L = factor(clean_df$final_cc_cname_DI)
levels(L)
K = as.numeric(L)
clean_df$final_cc_cname_DI = K
# clean_df$final_cc_cname_DI = as.numeric(new_df$final_cc_cname_DI)
# [1] "Australia"                              "Bangladesh"                            
# [3] "Brazil"                                 "Canada"                                
# [5] "China"                                  "Colombia"                              
# [7] "Egypt"                                  "France"                                
# [9] "Germany"                                "Greece"                                
# [11] "India"                                  "Indonesia"                             
# [13] "Japan"                                  "Mexico"                                
# [15] "Morocco"                                "Nigeria"                               
# [17] "Other Africa"                           "Other East Asia"                       
# [19] "Other Europe"                           "Other Middle East/Central Asia"        
# [21] "Other North & Central Amer., Caribbean" "Other Oceania"                         
# [23] "Other South America"                    "Other South Asia"                      
# [25] "Pakistan"                               "Philippines"                           
# [27] "Poland"                                 "Portugal"                              
# [29] "Russian Federation"                     "Spain"                                 
# [31] "Ukraine"                                "United Kingdom"                        
# [33] "United States"                          "Unknown/Other" 

str(clean_df)

work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]

set.seed(80)

k_test<-sample(1:nrow(work_df),nrow(work_df)*.10)
sampled_df<-work_df[k_test,]


# n_rows <- nrow(work_df)
# sample_size <- floor(0.05 * n_rows)
# 
# sampled_indices <- sample(n_rows, size = sample_size)
# sampled_df <- work_df[sampled_indices, ]

# Correlation such as Spearman and Kendall can be used for comparision between
#numeric and non- numeric valued columns.

#Spearman_correlation

cor_work_data = cor(work_df,method = 'spearman')

#plotting heat map
uppertri_mean_pearson = cor_work_data
uppertri_mean_pearson[lower.tri(cor_work_data)] =NA 
uppertri_melted = melt(uppertri_mean_pearson, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Spearman Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))

#kendall_correlation

cor_work_data_Kendall = cor(sampled_df,method = 'kendall')
cor_work_data_Kendall
#plotting heat map
uppertri_mean_kendall = cor_work_data_Kendall
uppertri_mean_kendall[lower.tri(cor_work_data)] =NA 
uppertri_melted = melt(uppertri_mean_kendall, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data_Kendall, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Kendall Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))

#Pearson_correlation - plotiing it as kendall is taking a lot of time for proccesing

cor_work_data_Pearson = cor(work_df,method = 'pearson')

#plotting heat map
uppertri_mean_Pearson = cor_work_data_Pearson
uppertri_mean_Pearson[lower.tri(cor_work_data_Pearson)] =NA 
uppertri_melted = melt(uppertri_mean_Pearson, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data_Pearson, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Pearson Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))


vpost<-work_df

#vpost <- vpost[vpost$nforum_posts >= 1 & vpost$nplay_video >= 1,]
#Linear Regression
vpost_cor<-round(cor(vpost,method="spearman"),digits=2)
vpost_cor

corrplot(vpost_cor, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "spearman Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))


vpost<-work_df
#vpost <- work_df[work_df$nforum_posts>0,]
#Identify the most correlated feature and use that for LR
#For cars data, wt is most correlated (both from cor and VIP)
data_lr<-as.data.frame(scale(vpost[,c("ndays_act","viewed",'grade','nevents','n_days')]))
#data_lr<-as.data.frame(scale(vpost[,c("ndays_act",'grade','nevents','n_days')]))

#Divide into training and testing data
set.seed(508)
mtcars_datasort_lr<-sample(1:nrow(data_lr),nrow(data_lr)*.8)
train_lr<-data_lr[mtcars_datasort_lr,]
test_lr<-data_lr[-mtcars_datasort_lr,]
test_lr
#Build the model and predict the values
mdl_lr<-lm(ndays_act~.,data=train_lr)
pred_train_lr<-predict(mdl_lr,train_lr)
pred_train_lr
pred_test_lr<-predict(mdl_lr,test_lr)
pred_test_lr
#Get RMSE values
rmse_lr_train<-rmse(pred_train_lr,train_lr$ndays_act)
rmse_lr_train
rmse_lr_test<-rmse(pred_test_lr,test_lr$ndays_act)
rmse_lr_test
#R2 value for training data
sst<-sum((train_lr$ndays_act-mean(train_lr$ndays_act))^2)
sse<-sum((pred_train_lr-train_lr$ndays_act)^2)
rsq<-1-sse/sst
rsq
plot(train_lr$ndays_act,pred_train_lr,xlab="Actual",ylab="Predicted")
summary(mdl_lr)



vpost<-work_df

#vpost <- vpost[, c('ndays_act',"viewed", "grade",'nevents','n_days')]


vpost <- vpost[, c('ndays_act', "grade",'nevents','n_days')]


vpost 

#Ridge Regression
#data(mtcars)
set.seed(508)


datasort<-sample(1:nrow(vpost),nrow(vpost)*0.8)
train_ridge<-vpost[datasort,]
test_ridge<-vpost[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("ndays_act")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("ndays_act")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=0)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=0)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=0,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
pred_train_ridge<-as.data.frame(pred_train_ridge)
pred_test_ridge<-as.data.frame(pred_test_ridge)
rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$ndays_act)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$ndays_act)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$ndays_act-mean(train_ridge$ndays_act))^2)
sse<-sum((pred_train_ridge-train_ridge$ndays_act)^2)
rsq<-1-sse/sst
rsq

summary((mdl_ridge_cv))



vpost<-work_df



#vpost <- vpost[, c('ndays_act',"viewed", "grade",'nevents','n_days')]

vpost <- vpost[, c('ndays_act', "grade",'nevents','n_days')]


vpost 

##Lasso Regression
#data(mtcars)
set.seed(508)


datasort<-sample(1:nrow(vpost),nrow(vpost)*0.8)
train_ridge<-vpost[datasort,]
test_ridge<-vpost[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("ndays_act")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("ndays_act")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=1)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=1)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$ndays_act,alpha=1,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
pred_train_ridge<-as.data.frame(pred_train_ridge)
pred_test_ridge<-as.data.frame(pred_test_ridge)
rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$ndays_act)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$ndays_act)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$ndays_act-mean(train_ridge$ndays_act))^2)
sse<-sum((pred_train_ridge-train_ridge$ndays_act)^2)
rsq<-1-sse/sst
rsq

summary((mdl_ridge_cv))



