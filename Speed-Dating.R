data = read.csv('./Rdataproject/Speed Dating Data.csv')
head(data)
str(data)
dim(data)
#8389row 195col 


################Preprocessing################

colnames(data)

#Remove variable

#by file "Rdataproject/Speed Dating Data Key.doc"
#Remove variable given after matching (num 120-195)
#Because it is predicting whether to match or not
data <- data[,-c(120:195)]

#Check and remove variables with more than 50% missing values
for (i in 1:length(data)){
  if (sum(is.na(data[i])) > (8378/2)){
    print(colnames(data[i]))
  }
}

#After checking the table, if it is determined that it cannot be replaced, it is deleted.
#If the data distribution is skewed to one side, it is judged to be replaceable.
table(data$expnum) #Remove
table(data$attr1_s) #Remove
table(data$sinc1_s) #Remove
table(data$intel1_s) #Remove
table(data$fun1_s) #Remove
table(data$amb1_s) #Remove
table(data$shar1_s) #Remove
table(data$attr3_s) #Remove
table(data$sinc3_s) #Remove
table(data$intel3_s) #Remove
table(data$fun3_s) #Remove
table(data$amb3_s) #Remove

data <- subset(data, select=-c(expnum, attr1_s,
                               sinc1_s ,intel1_s, fun1_s, amb1_s,shar1_s,
                               attr3_s, sinc3_s, intel3_s, fun3_s, amb3_s))


#Check the number of categories in categorical variables
for (i in 1:length(data)){
  if (is.character(data[,i])){
    print(colnames(data[i]))
    print(length(table(data[i])))
  }
}

length(table(data$from)) #270 
#If there are many categories, there is a risk of overfitting -> Remove
data <- subset(data, select=-from)

length(table(data$undergra)) #A lot of categories
sum(data$undergra == "") #More than 40% missing values "" -> Remove

table(data$mn_sat) 
data$mn_sat <- as.numeric(gsub(",","",data$mn_sat))
table(is.na(data$mn_sat)) #More than 50% missing values -> Remove
library(lattice)
bwplot(mn_sat~as.factor(match), data = data, notch = TRUE)
#No difference

table(data$tuition) 
sum(data$tuition == "") #More than 50% missing values "" -> Remove

table(data$zipcode) 
#Judged to have no effect -> Remove

table(data$income)
data$income <- as.numeric(gsub(",","",data$income))
table(is.na(data$income)) #More than 50% missing values -> Remove
bwplot(income~as.factor(match), data = data, notch = TRUE)
#No difference

data <- subset(data, select=-c(undergra,zipcode,tuition,mn_sat,income))

#Other variables to remove

#Variables judged to have no effect on favorable sensitivity
#position
#positin1
#attr2_1,sinc2_1,intel2_1,fun2_1,amb2_1,shar2_1
#attr4_1,sinc4_1,intel4_1,fun4_1,amb4_1,shar4_1
#attr5_1,sinc5_1,intel5_1,fun5_1,amb5_1
#wave, condtn
#id, partner, ldg, iid, pid


#Substitutable variables exist

#variable "samerace" -> Remove race, race_o 
#variable "career_c", "field" -> Remove career, field


data <- subset(data, select=-c(position, positin1,
                               race ,race_o, 
                               career, field, wave,
                               id, partner, idg, iid, pid,
                               attr4_1,sinc4_1,intel4_1,
                               fun4_1,amb4_1,shar4_1,
                               attr2_1,sinc2_1,intel2_1,
                               fun2_1,amb2_1,shar2_1,
                               attr5_1,sinc5_1,intel5_1,
                               fun5_1,amb5_1,condtn))


#Outlier handling

#met : 1 or 2
table(data$met) #Too many outliers -> Remove
table(data$met_o) #Use this variable

#histogram
ggplot(data , aes(x=met)) +
  geom_bar()+ labs(title='met')

data <- subset(data, select=-c(met))

colnames(data)
#Check outliers
table(data$gender) #0 or 1
table(data$condtn) #1 or 2
table(data$int_corr) #-1 ~ 1
table(data$samerace) #0 or 1
#Like this..

#0~10
table(data$attr_o)
table(data$sinc_o)
table(data$intel_o)
table(data$fun_o)
table(data$amb_o)
table(data$shar_o)
table(data$like_o)
table(data$prob_o)

#values greater than 10 -> 10
data$attr_o[data$attr_o>10] <- 10
data$fun_o[data$fun_o>10] <- 10

#Expected number of successful matches is an integer
table(data$match_es)
#decimals are rounded off
data$match_es[data$match_es==0.5] <- 1
data$match_es[data$match_es==1.5] <- 2
data$match_es[data$match_es==2.5] <- 3
data$match_es[data$match_es==3.4] <- 3


#1 or 2
table(data$met_o)
#values greater than 2 -> 2
data$met_o[data$met_o>2] <- 2


#Missing Value Handling

#Check the number of missing values by row
count = 0
removelist = list()

for (i in 1:length(data[,1])){
  if (sum(is.na(data[i,]))>3){
    count = count+1
    removelist[count] = i
  }
}
count
#Remove 637 rows with more than 3 missing values
data <- data[-c(unlist(removelist)),]
dim(data)


colnames(data)
#Check Variables with Missing Values
for (i in 1:length(data)){
  if (sum(is.na(data[i])) != 0){
    print(sum(is.na(data[i])))
    print(colnames(data[i]))
  }
}

#Variables with ranges ex) from 0 to 10 -> replace with median
data$pf_o_fun[is.na(data$pf_o_fun)] <- median(data$pf_o_fun, na.rm = TRUE)
data$pf_o_amb[is.na(data$pf_o_amb)] <- median(data$pf_o_amb, na.rm = TRUE)
data$pf_o_sha[is.na(data$pf_o_sha)] <- median(data$pf_o_sha, na.rm = TRUE)
data$attr_o[is.na(data$attr_o)] <- median(data$attr_o, na.rm = TRUE)
data$sinc_o[is.na(data$sinc_o)] <- median(data$sinc_o, na.rm = TRUE)
data$intel_o[is.na(data$intel_o)] <- median(data$intel_o, na.rm = TRUE)
data$fun_o[is.na(data$fun_o)] <- median(data$fun_o, na.rm = TRUE)
data$amb_o[is.na(data$amb_o)] <- median(data$amb_o, na.rm = TRUE)
data$shar_o[is.na(data$shar_o)] <- median(data$shar_o, na.rm = TRUE)
data$like_o[is.na(data$like_o)] <- median(data$like_o, na.rm = TRUE)
data$prob_o[is.na(data$prob_o)] <- median(data$prob_o, na.rm = TRUE)
data$fun1_1[is.na(data$fun1_1)] <- median(data$fun1_1, na.rm = TRUE)
data$amb1_1[is.na(data$amb1_1)] <- median(data$amb1_1, na.rm = TRUE)
data$shar1_1[is.na(data$shar1_1)] <- median(data$shar1_1, na.rm = TRUE)
data$attr[is.na(data$attr)] <- median(data$attr, na.rm = TRUE)
data$sinc[is.na(data$sinc)] <- median(data$sinc, na.rm = TRUE)
data$intel[is.na(data$intel)] <- median(data$intel, na.rm = TRUE)
data$fun[is.na(data$fun)] <- median(data$fun, na.rm = TRUE)
data$amb[is.na(data$amb)] <- median(data$amb, na.rm = TRUE)
data$shar[is.na(data$shar)] <- median(data$shar, na.rm = TRUE)
data$like[is.na(data$like)] <- median(data$like, na.rm = TRUE)
data$prob[is.na(data$prob)] <- median(data$prob, na.rm = TRUE)
data$match_es[is.na(data$match_es)] <- median(data$match_es, na.rm = TRUE)


#Not many missing values -> Remove that row
#age_o,met_o,age,field_cd,date,career_c,exphappy
dim(data)
data <- na.omit(data)
dim(data)


#Derived Variable

#Interesting activity
data$activ = data$exercise + data$sports +data$hiking + data$yoga +data$clubbing
data$culture = data$museums + data$art + data$theater + data$concerts + data$movies + data$music +data$shopping
data$static = data$dining + data$gaming + data$tvsports + data$reading + data$tv
#Remove used variable
data <- subset(data, select=-c(sports,  tvsports,
                               exercise, dining,  museums,  art,     
                               hiking,   gaming,   clubbing, reading, 
                               tv,    theater,  movies, concerts,
                               music,   shopping ,yoga))

#Age difference variable
data$age_diff = abs(data$age - data$age_o)
data <- subset(data, select=-age_o)

#Average Matching Success Variable
data$match_mean = data$match_es/data$round
data <- subset(data, select=-match_es)


#data variable type setting 
str(data)
data$gender <- as.factor(data$gender)
data$match <- as.factor(data$match)
data$samerace <- as.factor(data$samerace)
data$field_cd <- as.factor(data$field_cd)
data$goal <- as.factor(data$goal)
data$career_c <- as.factor(data$career_c)
data$dec <- as.factor(data$dec)


###############EDA####################
library(ggplot2)
#By match
bwplot(age_diff~match, data = data, notch = TRUE)
bwplot(int_corr~match, data = data, notch = TRUE)
bwplot(like~match, data = data, notch = TRUE)
bwplot(like_o~match, data = data, notch = TRUE)
bwplot(shar~match, data = data, notch = TRUE)
bwplot(shar_o~match, data = data, notch = TRUE)

table(data$shar)
table(data$shar_o)

ggplot(data=data , aes(x=shar, y=shar_o)) +
  geom_point() +
  labs(title='shar vs shar_o', 
       x = 'shar' , y = 'shar_o')

par(mfrow=c(2,3)) 
boxplot(attr~dec, data = data, notch = TRUE)
boxplot(sinc~dec, data = data, notch = TRUE)
boxplot(intel~dec, data = data, notch = TRUE)
boxplot(fun~dec, data = data, notch = TRUE)
boxplot(amb~dec, data = data, notch = TRUE)
boxplot(shar~dec, data = data, notch = TRUE)


par(mfrow=c(2,3)) 
boxplot(attr_o~dec_o, data = data, notch = TRUE)
boxplot(sinc_o~dec_o, data = data, notch = TRUE)
boxplot(intel_o~dec_o, data = data, notch = TRUE)
boxplot(fun_o~dec_o, data = data, notch = TRUE)
boxplot(amb_o~dec_o, data = data, notch = TRUE)
boxplot(shar_o~dec_o, data = data, notch = TRUE)

ggplot(data , aes(x=activ , y=culture, color=match)) +
  geom_point()
#Not related to the activity you are interested in


data <- subset(data, select=-c(dec,dec_o))

colnames(data)
str(data)


#############modeling#############

table(data$match)

set.seed(123) # use set.seed 
library(caret)
idx <- createDataPartition(data$match, p=c(0.7,0.3), list = FALSE)
train <- data[idx,]
test <- data[-idx,]

rownames(train)=NULL
rownames(test)=NULL

table(train$match)
table(test$match)

# check the proportion of class variable
prop.table(table(train$match))
prop.table(table(test$match))

## Step 3: Training a model on the data ----
# build the simplest decision tree
# install.packages("C50")
library(C50)
colnames(train[4])
# Before tuning
model <- C5.0(train[-4], as.factor(train$match))

# display simple facts about the tree
model

# display detailed information about the tree
summary(model)

sum(predict(model, train)==train$match)/length(train$match)
sum(predict(model, test)==test$match)/length(test$match)

##################parameter tuning##################
c5_options <- C5.0Control(winnow = TRUE, noGlobalPruning = TRUE, CF=0.005)
#There are many variables, to avoid overfitting -> cf=0.005, winnow = TRUE, noGlobalPruning = TRUE 

control_model <- C5.0(train[-4], as.factor(train$match),
                      control=c5_options)

pred = predict(control_model, test , type='class')
confusionMatrix(pred,
                as.factor(test$match))


library(gmodels)
CrossTable(test$match, pred, prop.chisq=FALSE, 
           prop.c=FALSE, prop.r=FALSE, dnn=c('act','pred'))
pred_prob = predict(control_model, test , type='prob')

plot(control_model)
summary(control_model)
#install.packages("Epi")
library(Epi)
a = as.vector(test$match==0)
b = as.vector(pred_prob[,1])
par(mfrow=c(1,1))
ROC(form=a~b, plot="ROC")
