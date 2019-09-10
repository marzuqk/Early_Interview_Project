#inputting data and using as a dataframe
library(readr)
voterfile <- read_csv("voterfile/voterfile.csv")
View(voterfile)
VF.df=data.frame(voterfile)
str(VF.df)

#Converting any non-numerical values into numerical
VF.df$party<-as.numeric(factor(VF.df$party))
VF.df$ethnicity<-as.numeric(factor(VF.df$ethnicity))
VF.df$maritalstatus<-as.numeric(factor(VF.df$maritalstatus))
VF.df$dwellingtype<-as.numeric(factor(VF.df$dwellingtype))
VF.df$income<-as.numeric(factor(VF.df$income))
VF.df$education<-as.numeric(factor(VF.df$education))
VF.df$dma<-as.numeric(factor(VF.df$dma))
VF.df$occupationindustry<-as.numeric(factor(VF.df$occupationindustry))
VF.df$net_worth<-as.numeric(factor(VF.df$net_worth))
VF.df$petowner_dog<-as.numeric(factor(VF.df$petowner_dog))
VF.df$intrst_nascar_in_hh<-as.numeric(factor(VF.df$intrst_nascar_in_hh))
VF.df$intrst_musical_instruments_in_hh<-as.numeric(factor(VF.df$intrst_musical_instruments_in_hh))
VF.df$donates_to_conservative_causes<-as.numeric(factor(VF.df$donates_to_conservative_causes))
VF.df$home_owner_or_renter<-as.numeric(factor(VF.df$home_owner_or_renter))
str(VF.df)

summary(VF.df)
#all data points under donates_to_liberal_causes are NA so best to just remove

VF.df <- VF.df[,-31]

#Cleaning omitted data points will help with further analysis
VF.df.omitted<-na.omit(VF.df)
cat(paste("VF.df.omitted: ",nrow(VF.df.omitted)," obs. of ",ncol(VF.df.omitted)," variables."))
#Removed 22 data points
write.csv(VF.df.omitted, file = "voterfile/VF_df_omitted.csv")

#Linear Regression
VF.lm=lm(vh14p ~ ., data=VF.df)
summary(VF.lm)

#according to linear regression when alpha=0.10 these variables are significant
#id, age, ethnicity, dwellingtype, income, cd, dma, vh12g, vh12p, vh10p, vh08g, vh08p,
#vh06g, vh06p, vh04p, vh02p, vh00g, vh00p, donates_to_conservative_causes,
#home_owner_or_renter, g08_precinct_turnout, p08_precinct_turnout, p12_precinct_turnout
#23 significant variables
#r-squared=0.3662 which is honestly not great showing that there really isn't a linear 
#relationship

#Remove id because it is a label
VF.df.omitted <- VF.df.omitted[,-1]

#Logistic Regression
set.seed(12345)
training <- sample(1:nrow(VF.df.omitted), 0.6*nrow(VF.df.omitted))
VF.training <- VF.df.omitted[training,-11]
VF.training.results <- VF.df.omitted[training,11]
VF.test = VF.df.omitted[-training,-11]
VF.test.results = VF.df.omitted[-training,11]

VF.lr <- glm(vh14p ~ ., family=binomial(link='logit'),data=VF.df.omitted[training,])
summary(VF.lr)


VF.test.probabilities <- predict(VF.lr,VF.test,type="response")
VF.lr.classifications <- round(VF.test.probabilities,0)
sum(VF.lr.classifications == VF.test.results) / length(VF.test.results)
1-sum(VF.lr.classifications == VF.test.results) / length(VF.test.results)
table(VF.lr.classifications,VF.test.results)
#About a 7% error rate on the test set, seems to be pretty good

#Plots
install.packages("ggplot2")
library("ggplot2")
ggplot(VF.df.omitted,aes(y=vh14p,x=age)) + geom_point()

library(lattice) 
hist(VF.df.omitted$age)
hist(VF.df.omitted$vh14p)

#setup for vh12g variable
VF.df.omitted$vh14p=NA
View(VF.df.omitted)
VF.df.omitted$vh14p=predict(VF.lr, VF.df.omitted, type="response")
View(VF.df.omitted)
write.csv(VF.df.omitted, file = "voterfile/VF_1.csv")

#Second
set.seed(12345)
training <- sample(1:nrow(VF.df.omitted), 0.6*nrow(VF.df.omitted))
VF.training <- VF.df.omitted[training,-12]
VF.training.results <- VF.df.omitted[training,12]
VF.test = VF.df.omitted[-training,-12]
VF.test.results = VF.df.omitted[-training,12]

VF.lr <- glm(vh12g ~ . -vh14p, family=binomial(link='logit'),data=VF.df.omitted[training,])
summary(VF.lr)

#More Plots
library("ggplot2")
ggplot(VF.df.omitted,aes(y=vh12g, x=age)) + geom_point()
library(lattice)
hist(VF.df.omitted$vh12g)

VF.test.probabilities <- predict(VF.lr,VF.test,type="response")
VF.lr.classifications <- round(VF.test.probabilities,0)
sum(VF.lr.classifications == VF.test.results) / length(VF.test.results)
1-sum(VF.lr.classifications == VF.test.results) / length(VF.test.results)
table(VF.lr.classifications,VF.test.results)
#About a 12% error rate on the test set, isn't too bad

VF.df.omitted$vh12g=NA
View(VF.df.omitted)
VF.df.omitted$vh12g=predict(VF.lr, VF.df.omitted, type="response")
View(VF.df.omitted)
write.csv(VF.df.omitted, file = "voterfile/VF_2.csv")

#creation of the vh14g variable to input prediction results
clnames <- c("vh14p","vh12g")
VF.df.omitted$vh14g <- rowMeans ( VF.df.omitted[clnames] )
write.csv(VF.df.omitted, file = "voterfile/VF_3.csv")
VF.df.omitted$vh14g=round((VF.df.omitted$vh14g),0)
View(VF.df.omitted)
write.csv(VF.df.omitted, file = "voterfile/VF_F.csv")

(sum(VF.df.omitted$vh14g))/49978
#There will probably be around a 12.88% voter turnout 