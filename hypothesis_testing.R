## Cutlet hypothesis testing 
attach(Cutlets)
shapiro.test(Unit.A) ## p-value = 0.32 this is normalized 
shapiro.test(Unit.B) ## p-value = 0.5225 this is normalized
var.test(Unit.A,Unit.B) ## p-value = 0.3136 variance are equal
t.test(Unit.A,Unit.B,alternative = 'two.sided',correct = TRUE, conf.level = 0.95)
## Null Hypothesis-> Diameter of the cutlet are same from both the unit 
## Alternative Hypothesis -> Diameter are not same from both the unit 
## p-value = 0.4723 which illustrate that there is no enough evidence to reject the null hypothesis 

## Tat hypothesis testing 
attach(LabTAT)
shapiro.test(Laboratory.1) ## p-value = 0.5508 it is normalized 
shapiro.test(Laboratory.2) ## p-value = 0.8637 it is normalized 
shapiro.test(Laboratory.3) ## p-value = 0.4205 it is normalized 
shapiro.test(Laboratory.4) ## p-value = 0.6619 it is normalized 
stack_data <- stack(LabTAT)
head(stack_data)
bartlett.test(values~ind,data = stack_data) ## p-value = 0.1069 hence variance are equal 
tat_result <- aov(values~ind,data = stack_data)
summary(tat_result) ## p value 2e-16 < 0.05 means enough evidence to reject the Null hypothesis 
## Null Hypothesis -> All average tat is same

## Buyer Ration Hypothesis testing 
str(BuyerRatio)
BuyerRatio_chop <- BuyerRatio[-1]
BuyerRatio_chop
chisq.test(BuyerRatio_chop) ## p-value = 0.6603, means there is no enough evidance to reject null hypothysis 
## Null Hypothysis -> Proportion for M and F in different regions are same 
#chisq.test(BuyerRatio[,2:5])

## TeleCall Hypothesis testing 
table(Costomer)
str(Costomer)
Costomer_numeric <- as.data.frame(NULL)
Costomer$Phillippines <- ifelse(Costomer$Phillippines == 'Error Free',0,1)
Costomer$Indonesia <- ifelse(Costomer$Indonesia == 'Error Free',0,1)
Costomer$Malta <- ifelse(Costomer$Malta == 'Error Free',0,1)
Costomer$India <- ifelse(Costomer$India == 'Error Free',0,1)
dim(Costomer)
total_result <- data.frame(
  'Phillippines' =NULL,
  'Indonesia' = NULL,
  'Malta' = NULL,
  'India' = NULL
)

Phillippines_error_free <- nrow(as.data.frame(Costomer$Phillippines[which(Costomer$Phillippines == 0)]))
Phillippines_error <- nrow(Costomer) - Phillippines_error_free
Indonesia_error_free <- nrow(as.data.frame(Costomer$Indonesia[which(Costomer$Indonesia == 0)]))
Indonesia_error <- nrow(Costomer) - Indonesia_error_free
Malta_error_free <- nrow(as.data.frame(Costomer$Malta[which(Costomer$Malta == 0)]))
Malta_error <- nrow(Costomer) - Malta_error_free
India_error_free <- nrow(as.data.frame(Costomer$India[which(Costomer$India == 0)]))
India_error <- nrow(Costomer) - India_error_free

temp.data <- data.frame(Phillippines_error_free,Indonesia_error_free,Malta_error_free,India_error_free)
names(temp.data) <- c('Phillippines','Indonesia','Malta','India')
total_result <- rbind(total_result,temp.data)

temp.data <- data.frame(Phillippines_error,Indonesia_error,Malta_error,India_error)
names(temp.data) <- c('Phillippines','Indonesia','Malta','India')
total_result <- rbind(total_result,temp.data)

#rm(total_result)
total_result
chisq.test(total_result) ## p-value = 0.2771, means there is no enough evidence to reject null hypothesis 

## simple way 
stack_data <- stack(Costomer)
stack_data
table_tele <- table(stack_data$values,stack_data$ind)
table_tele
chisq.test(table_tele)
## Null Hypothysis -> error detection percent is same for all centers 

## Fantaloons hypothesis testing 
table <- table(Faltoons$Weekdays,Faltoons$Weekend)
table
fresult <- prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = 'two.sided')
fresult ## P-value of p1-p2 = -.135 which is < 0.05 hence we reject H0 hypothesis 
##Null Hypothesis -> Both having same proportion so there mean is equal and 
##M1 = M2 means M1-M2 = 0
##prop 1 prop 2 
##0.2825 0.4175 

