getwd()
setwd("C:/Users/LefMel/Documents/Publications-Analyses/Diphtheritida Papagiannis")
list.files()
#################
?read.xlsx
library(openxlsx)
draft = read.xlsx("Data.xlsx", sheet=2)
View(draft)

!is.na(draft$`Pertussis.IgG.(U/ml)`)
data <- draft[!is.na(draft$`Pertussis.IgG.(U/ml)`),]
data <- data[,c(3,4,5,7)]

names <- c("Age", "Gender", "Region", "Test")
colnames(data) <- names

unique(data$Gender)
data$Gender <- ifelse(data$Gender=="ÁÍÄÑÁÓ", "Male", "Female")
unique(data$Gender)
data$Test <- as.numeric(data$Test)

str(data)
summary(data$Test)
hist(data$Test)

unique(data$Region)
library(dplyr)

data <- data %>%
  mutate(Region_c = recode(Region,
                           "ÌÁÃÍÇÓÉÁ" = "Thessaly",
                           "ËÁÑÉÓÁ" = "Thessaly",
                           "ÊÁÑÄÉÔÓÁ" = "Thessaly",
                           "ÊÏÑÉÍÈÉÁ" = "Peloponnisos",
                           "ÌÅÓÓÇÍÉÁ" = "Peloponnisos",
                           "ËÁÊÙÍÉÁÓ" = "Peloponnisos",
                           "ÁÑÊÁÄÉÁÓ" = "Peloponnisos",
                           "ÁÑÃÏËÉÄÁ" = "Peloponnisos",
                           "ÐÁÔÑÁ" = "Peloponnisos",
                           "ÁÍÁÔÏËÉÊÇ ÌÁÊÅÄÏÍÉÁ" = "Macedonia",
                           "ÊÅÍÔÑ.ÌÁÊÅÄÏÍÉÁ" = "Macedonia",
                           "ÄÕÔÉÊÇ ÌÁÊÅÄÏÍÉÁ " = "Macedonia",
                           "Ä ÌÁÊÅÄÏÍÉÁ ËÉÁÐÇÓ" = "Macedonia",
                           "Ä ÌÁÊÅÄÏÍÉÁ ÄÏÕÃÊÁÓ" = "Macedonia",
                           "Ä ÌÁÊÅÄÏÍÉÁ ÓÖÅÔÓÏÓ" = "Macedonia",
                           "ÓÔÅÑÅÁ ÅËËÁÄÁ" = "St. Ellada",
                           "ÊÑÇÔÇ" = "Crete",
                           "ÂÏÑÅÉÏ ÁÉÃÁÉÏ " = "Aegean",
                           "ÉÏÍÉÁ ÍÇÓÉÁ" = "Ionian",
                           "ÇÐÅÉÑÏÓ" = "Epirus",
                           "ÁÔÔÉÊÇ" = "St. Ellada",
                           ))

data[is.na(data$Region_c),]$Region_c = "Thessaly"

sum(is.na(data$Region_c))
sum(is.na(data$Test))

unique(data$Region_c) # 8

range(data$Test)
(126.6 - 1.46)/300 # 0.42

data = data[!is.na(data),]
last_obs_index <- max(which(!is.na(data$Age)))
data <- data[1:last_obs_index, ]
save(data, file="data.Rdata")
# Skip code above, just download data and load with commands below
#################

#################
load("data.Rdata")


# Covariate data formation
factor(data$Gender, levels=c("Male", "Female"), labels=c(1,2)) # 1 - Male, 2 - Female
factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8)))

boxplot(data$Test ~ data$Gender)
ggplot(data, aes(x=Gender, y=Test)) +
  geom_boxplot(fill="lightblue", outlier.color="red", outlier.shape=16) +
  labs(title="Box plot of Gender and Test Value", x="Gender", y="Test value (U/ml)")


plot(data$Age, data$Test)
ggplot(data, aes(x=Age, y=Test)) + 
  geom_point(aes(color=Age), size=3, alpha=0.7) +
  labs(title="Scatter Plot of Age and test Value", x="Age", y="Test value (U/ml)") +
  theme_minimal()
#################################
### Main Bayesian Mixture model
#################################
library(runjags)
runjags.options(force.summary=TRUE)
results <- run.jags("normal_model", 
                    monitor=c("AUC", "delta", "lambda", "sigma", "delta", "se", "sp", "alpha", "beta", "U", "random_sd", "J"),
                    data=list(Test = data$Test,
                              N=length(data$Test), Age = data$Age, Gender = factor(data$Gender, levels=c("Male", "Female"), labels=c(1,2)), Region=factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8)))),
                    n.chains=2)


posterior <- as.data.frame(summary(results))
str(posterior)
posterior$index = rownames(posterior)

View(round(posterior[,1:3],4))

round(sqrt(posterior[grepl("sigma", posterior$index),][,(1:3)]),2)

which.max(posterior[grepl("J", posterior$index),]$Median)
posterior["se[45",]
posterior["sp[45",]
# J[45]
((1.46+0.42*45))#  - 20.36

#J[37]
((1.46+0.42*37))#  - 17
posterior["se[37",]
posterior["sp[37",]

#J[38]
((1.46+0.42*38))#  - 17.42
posterior["se[38",]
posterior["sp[38",]

# ROC plot


thresholds <- numeric(300)
initial_value <- 1.46 

# Increment the value by 0.42 a total of 300 times and store in the vector
for (i in 1:300) {
  initial_value <- initial_value +0.42
  thresholds[i] <- initial_value
}


se_est <- combine.mcmc(results_add, vars='se')
sp_est <- combine.mcmc(results_add, vars='sp')
ses_mu <- apply(se_est, 2, mean)
sps_mu <- apply(sp_est, 2, mean)
ses_lower <- apply(se_est, 2, FUN = quantile, prob = 0.025)
sps_lower <- apply(sp_est, 2, FUN = quantile, prob = 0.025)
ses_upper <- apply(se_est, 2, FUN = quantile, prob = 0.975)
sps_upper <- apply(sp_est, 2, FUN = quantile, prob = 0.975)


roc_data <- data.frame(thresholds, ses_mu, sps_mu, ses_lower, sps_lower, ses_upper, sps_upper)

summary_stats <- roc_data %>%
  group_by(thresholds) %>%
  summarize(ses_mu,
            sps_mu,
            ses_upper,
            sps_upper,
            ses_lower,
            sps_lower)

library(ggplot2)
ggplot(summary_stats, aes(x=1-sps_mu, y=ses_mu))+
  geom_line() +
  #geom_ribbon(aes(ymin=ses_lower, ymax=ses_upper))+
  #geom_ribbon(aes(ymin=1-sps_lower, ymax=1-sps_upper))+
  labs(x = "1- Specificity", y = "Sensitivity")+
  coord_fixed(ratio=1) +
  theme_minimal()

auc_est <- combine.mcmc(results_add, vars='AUC')
hist(auc_est, breaks=50, col="orange", main="AUC", xlab="Arean Under the Curve")

mean_est <- combine.mcmc(results_add, vars='lambda')
boxplot(as.matrix(mean_est), col="red")
mu_lambda <- apply(mean_est, 2, mean)
mu_lambda

sigma_est <- sqrt(combine.mcmc(results_add, vars='sigma'))
mu_sigma <- (apply(sigma_est, 2, mean))
mu_sigma
boxplot(as.matrix(sigma_est), col="red")


density1 <- dnorm(data$Test, mean = mu_lambda[1], sd = mu_sigma[1])
density2 <- dnorm(data$Test, mean = mu_lambda[2], sd = mu_sigma[2])

df <- data.frame(data$Test, density1, density2)
ggplot(df, aes(data$Test)) +
  geom_line(aes(y = density1, color = "Healthy group")) +
  geom_line(aes(y = density2, color = "Infected group")) +
  ggtitle("Densities of mixture distributions") +
  labs(x = "Test Value", y = "Density") +
  xlim(-25,150) + 
  geom_vline(aes(xintercept = 20.36), linetype="dashed", color= "black") + 
  scale_color_manual(values = c("Healthy group" = "blue", "Infected group" = "red"), 
                     name = "Distributions")



######################
# posterior without age
######################
results <- run.jags("normal_model_age_rm", 
                    monitor=c("AUC", "delta", "lambda", "sigma", "P", "delta", "se", "sp", "J", "beta", "U", "random_sd"),
                    data=list(Test = data$Test,
                              N=length(data$Test), Gender = factor(data$Gender, levels=c("Male", "Female"), labels=c(1,2)), Region=factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8)))),
                    n.chains=2)


posterior_age <- as.data.frame(summary(results))
str(posterior_age)
posterior_age$index = rownames(posterior_age)


View(round(posterior_age[,1:3],4))
round(posterior_age[grepl("lambda", posterior_age$index),][,(1:3)],2)
round(sqrt(posterior_age[grepl("sigma", posterior_age$index),][,(1:3)]),2)

round(posterior_age[grepl("delta", posterior_age$index),][,(1:3)],2)


which.max(posterior_age[grepl("J", posterior_age$index),]$Median)
posterior_age["se[45",]
posterior_age["sp[45",]
# J[45]
((1.46+0.42*45))#  - 20.36

#J[37]
((1.46+0.42*37))#  - 17
posterior_age["se[37",]
posterior_age["sp[37",]

#J[38]
((1.46+0.42*38))#  - 17.42
posterior_age["se[38",]
posterior_age["sp[38",]






######################
# Simple Linear regression model
######################
summary(data$Age)
summary(data$Test)
summary(factor(data$Gender, levels=c("Male", "Female"), labels=c(1,2)))
summary(factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8))))
round(summary(factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8))))/1168,2)

library(lme4)
model <- lmer(data$Test ~ data$Age + 
              factor(data$Gender, levels=c("Male", "Female"), labels=c(1,2))
              + (1|factor(data$Region_c, levels= c("Thessaly", "Peloponnisos","Macedonia","St. Ellada","Crete","Aegean","Ionian","Epirus"), labels = labels(seq(1,8)))))
summary_model = summary(model)
confint(model)


######################
# No covariates
######################
library(runjags)
runjags.options(force.summary=TRUE)
results <- run.jags("normal_model_no_cov", 
                    monitor=c("AUC", "delta", "lambda", "sigma", "P", "delta", "se", "sp", "J"),
                    data=list(Test = data$Test,
                              N=length(data$Test), alpha=c(1,1)),
                    n.chains=2)

posterior_no_cov = as.data.frame(summary(results))
posterior_no_cov$index = rownames(posterior_no_cov)
View(round(posterior_no_cov[,1:3],4))

round(posterior_no_cov[grepl("lambda", posterior_no_cov$index),][,(1:3)],2)
round(sqrt(posterior_no_cov[grepl("sigma", posterior_no_cov$index),][,(1:3)]),2)

which.max(posterior_no_cov[grepl("J", posterior_no_cov$index),]$Median)
posterior_no_cov["se[45",]
posterior_no_cov["sp[45",]
# J[45]
((1.46+0.42*45))#  - 20.36

#J[37]
((1.46+0.42*37))#  - 17
posterior_no_cov["se[37",]
posterior_no_cov["sp[37",]

#J[38]
((1.46+0.42*38))#  - 17.42
posterior_no_cov["se[38",]
posterior_no_cov["sp[38",]


##############################################
## Covariates both on infection + test
##############################################



