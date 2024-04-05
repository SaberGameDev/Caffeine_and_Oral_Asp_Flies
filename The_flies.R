install.packages("tidyverse")
install.packages("car")
install.packages("ggthemes")
library(tidyverse)
library(car)
library(ggthemes)

flies <- read.csv("the_flies.csv")

# We are dealing with a categorical explanatory variable, with two categorical 
# covariates: sex (F or M) and age of treatment (24 hours old or 48 hours old).
# Our response variable (lifespan, in unit of days) is numerical and discrete.
# To decide which statistical tests we should conduct, we need to see if our
# data is equally varied and normally distributed
#-------------Code to Show variance------------------
leveneTest(lifespan ~ trmt, flies)
  # result is P-value of 0.7178, we do not reject H_0 of equal variance

leveneTest(lifespan ~ sex, flies)
  # result is P-value of 0.05231, we do not reject H_0 of equal variance

leveneTest(lifespan ~ as.factor(age_at_trmt), flies)
 # result is P-value of 0.1084, we do not reject H_0 of equal variance



#--------------Normal Distribution Testing and Covariate effects--------------
#ANOVA Model Fit for correlation between mean lifespan and treatment v control
trmtmodel <- aov(data = flies, lifespan ~ trmt)
summary(trmtmodel)
#Shapiro-Wilk Test on residuals to test for data normality.
shapiro.test(residuals(trmtmodel))

#ANOVA Model Fit for correlation between mean lifespan and treatment and sex as
#covariate
sexcovmodel <-aov(data = flies, lifespan ~ trmt + sex)
summary(sexcovmodel)
#Shapiro-Wilk Test on residuals to test for data normality.
shapiro.test(residuals(sexcovmodel))

#ANOVA Model Fit for correlation between mean lifespan and the interaction
#between the treatment and sex of flies.
sexintmodel <- aov(data = flies, lifespan ~ trmt * sex)
summary(sexintmodel)
shapiro.test(residuals(sexintmodel))

#ANOVA Model Fit for correlation between mean lifespan and the interaction
#between the treatment and age of flies.
ageintmodel <- aov(data = flies, lifespan ~ trmt*as.factor(age_at_trmt))
summary(ageintmodel)

#ANOVA Model Fit for correlation between mean lifespan and treatment and age at
# treatment as covariate.
agecovmodel <-aov(data = flies, lifespan ~ trmt + as.factor(age_at_trmt))
summary(agecovmodel)
#Shapiro-Wilk Test on residuals to test for data normality.
shapiro.test(residuals(agecovmodel))


#------------Histograms II------------------
ggplot(flies, aes(x = lifespan, fill = sex, color = sex)) + 
  geom_histogram(bins = 7, alpha = 0.3, position="identity") +
  theme_bw() + labs(x = "Lifespan (Days)", y = "Individual Count", fill = "Sex") + 
  scale_fill_discrete(labels=c('Female', 'Male')) +
  scale_color_discrete(guide = "none") +
  theme(text=element_text(size=18))

ggsave("block_sex_overlap.png", width = 7.6, height = 7.6)

ggplot(flies, aes(x = lifespan, fill = trmt, color = trmt)) + 
  geom_histogram(bins = 7, alpha = 0.3, position = "identity") +
  theme_bw() + labs(x = "Lifespan (Days)", y = "Individual Count", fill = "Treatment") + 
  scale_fill_manual(values = c("#353436", "darkgoldenrod1"), labels=c('Control', 'Treatment')) +
  scale_color_manual(values = c("#353436", "darkgoldenrod1"), guide = "none") +
  theme(text=element_text(size=18))

ggsave("block_trmt_overlap.png", width = 7.6, height = 7.6)

ggplot(flies, aes(x = lifespan, fill = as.factor(age_at_trmt), color = as.factor(age_at_trmt))) + 
  geom_histogram(bins = 7, alpha = 0.3, position = "identity") +
  theme_bw() + labs(x = "Lifespan (Days)", y = "Individual Count", fill = "Age at Treatment") + 
  scale_fill_manual(values = c("darkorchid2", "chartreuse3"), labels=c('24H', '48H')) +
  scale_color_manual(values = c("darkorchid2", "chartreuse3"), guide = "none") +
  theme(text=element_text(size=18))
  
ggsave("block_age_overlap.png", width = 7.6, height = 7.6)
#-------------Boxplots---------------------
ggplot(flies, aes(x = sex, y = lifespan, fill = sex)) + geom_boxplot() + 
  facet_wrap(~ age_at_trmt, labeller = labeller(age_at_trmt = c("1" = "24H", "2" = "48H"))) +
  theme_bw() +
  geom_jitter(alpha = 0.3) +
  theme(legend.position = "none") + ggtitle("The Lifespan of Coffee-Aspirated Flies by Sex") +
  labs(x = "Sex", y = "Lifespan (Days)") +
  scale_x_discrete(label = c("Female", "Male"))

ggsave("box_age.png", width = 7.6, height = 7.6)

ggplot(flies, aes(x = sex, y = lifespan, fill = sex)) + geom_boxplot() + 
  facet_wrap(~ trmt, labeller = labeller(trmt = c("con" = "Control", "trmt" = "Treatment"))) +
  theme_bw() +
  geom_jitter(alpha = 0.3) +
  theme(legend.position = "none") + ggtitle("The Lifespan of Coffee-Aspirated Flies by Sex") +
  labs(x = "Sex", y = "Lifespan (Days)") +
  scale_x_discrete(label = c("Female", "Male"))

ggsave("box_trmt.png", width = 7.6, height = 7.6)

#-----------------------Stats Testing----------------------------------
#------------------------Wilcox-------------------
wilcox.test(lifespan~trmt, data = flies)
wilcox.test(lifespan~sex, data = flies)
wilcox.test(lifespan~age_at_trmt, data = flies)


#------------------------Kruskal-Wallis------------
kruskal.test(lifespan~trmt, data = flies)
kruskal.test(lifespan~sex, data = flies)
kruskal.test(lifespan~age_at_trmt, data = flies)
