adult<-read.csv('adult_sal.csv',stringsAsFactors=T)
head(adult)
library(dplyr)
adult<-select(adult,-X)
head(adult,1)
str(adult)
summary(adult)
table(adult$type_employer)
unemp<-function(job){
  job<-as.character(job)
  if(job=='Without-pay'| job=='Never-worked')
  {
    return('Unemployed')
  }
  else
  {
    return(job)
  }
}

adult$type_employer<-sapply(adult$type_employer,unemp)
table(adult$type_employer)
str(adult)

group_emp<-function(jobs){
  if(jobs=='State-gov' | jobs=='Local-gov')
  {
    return('SL-gov')
  }else if(jobs=='Self-emp-inc' || jobs=='Self-emp-not-inc')
  {
    return('self-emp')
  }else
  {
    return(jobs)
  }
}

adult$type_employer<-sapply(adult$type_employer,group_emp)
table(adult$type_employer)

str(adult)
table(adult$marital)

matrinomy<-function(status)
{
  status<-as.character(status)
  if(status=='Married-AF-spouse' | status=='Married-civ-spouse' | status=='Married-spouse-absent')
  {
    return("Married")
  }else if(status=='Never-married'){
    return('Never-Married')
  }else{
    return("Not-Married")
  }
}


adult$marital<-sapply(adult$marital,matrinomy)
table(adult$marital)


table(adult$country)
levels(adult$country)


Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')



group_country<-function(country){
  if(country %in% Asia)
  {
    return('Asia')
  }else if(country %in% North.America)
  {
    return("North.America")
  }else if(country %in% Europe)
  {
    return('Europe')
  }else if(country %in% Latin.and.South.America)
  {
  
    return('Latin.and.South.America')
  }else{
    return('Other')
  }
  
}


adult$country<-sapply(adult$country,group_country)
table(adult$country)
str(adult)


adult$type_employer<-factor(adult$type_employer)
adult$marital<-factor(adult$marital)
adult$country<-sapply(adult$country,factor)

str(adult)
library(Amelia)
adult[adult=='?']<-NA
adult=='?'
table(adult$type_employer)
adult$type_employer<-sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

missmap(adult,y.at=c(1),labels=c(''),color=c('yellow','red'))
adult<-na.omit(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
str(adult)
library(ggplot2)
library(dplyr)
ggplot(adult,aes(x=age))+geom_histogram(aes(fill=income),color='black',bins=40)
ggplot(adult,aes(x=hr_per_week))+geom_histogram()
str(adult)

#Renaming Country column to Region Column
names(adult)[names(adult)=='country']<-'region'
#or

str(adult)
ggplot(adult,aes(x=region))+geom_bar(aes(fill=income),color='black')+
  theme_bw()+theme(axis.text.x=element_text(angle=90,hjust=1))
head(adult)

library(caTools)
set.seed(101)
sample<-sample.split(adult$income,SplitRatio = 0.7)
train_dataset=subset(adult,sample==T)
test_dataset=subset(adult,sample==F)

smodel<-glm(income ~ . ,family=binomial(link='logit'),data = train_dataset)
summary(smodel)

#Lets use step to remove the unnecessary features from our dataset as there are too many features in our dataset
#new.step.model<-step(model)
#new.step.model
#summary(new.step.model)


#Create a confusion matrix using the predict function with type='response' as an argument inside of that function.

test_dataset$predictedincome<-predict(model,test_dataset,interval = 'confidence',type='response')
table(test_dataset$income,test_dataset$predicted.income>0.5)
length(test_dataset$income)
length(test_dataset$predicted.income)