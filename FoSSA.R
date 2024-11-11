# Get working directory ----
getwd()
setwd("C:/Users/Hon.Olayinka/Desktop/Data Science/THE GRAPH COURSES/FoSSA")

# Load dataset ----

whitehall <- read.csv("Whitehall_fossa(in).csv")

# Standard Error ----

# Calculate the Sample size, remember to exclude missing values!

SBP.n <- sum(!is.na(whitehall$sbp))

# Calculate SD, noting again that we are rounding to 3 significant figures and excluding NA values.

SBP.sd <- sd(whitehall$sbp, na.rm=T)
round(SBP.sd, digits=3)

#  Calculate SE

SBP.se <- SBP.sd/sqrt(SBP.n)

round (SBP.se, 3)

# 95% confidence interval ----

# Z0.975 is the 97.5% percentile of the standard normal distribution (~1.96 SD from the mean). We can determine this precisely in R using qnorm(0.975), as previously demonstrated.

Za <- qnorm(0.975)

SBP.mean <- mean(whitehall$sbp, na.rm=TRUE)

ci.Z_SBP <- c((SBP.mean - (SBP.se*Za)), (SBP.mean + (SBP.se*Za)))
ci.Z_SBP


# Calculate a 95% confidence interval for mean BMI ----

# Calculate the Sample Size

BMI.n <- sum(!is.na(whitehall$bmi))

# Calculate the SD

BMI.sd <- sd(whitehall$bmi, na.rm = T)

round(BMI.sd, 3)

# Calculate the SE

BMI.se <- BMI.sd/sqrt(BMI.n)

round(BMI.se, 3)

# Calculate the mean BMI

BMI.mean <- mean(whitehall$bmi, na.rm = T)

# Calculate the SE

Za <- qnorm(0.975)

ci.z_BMI <- c((BMI.mean - (BMI.se*Za)), (BMI.mean + (BMI.se*Za)))
ci.z_BMI

# 95% of sample means for BMI lie between 25.12 and 25.31

# Calculate 99% CI for BMI

ZA <- qnorm (0.995)

ci.z99_BMI <- c((BMI.mean - (BMI.se*ZA)), (BMI.mean + (BMI.se*ZA)))
ci.z99_BMI

# The 99% CI is wider. This is because only 1% of your sample means will fall outside this range. Therefore, to be more certain, the range of values will increase to allow for this.


# Recode BMI into a binary variable so that one group has a BMI below 25, and the other group has a BMI of 25 and above. Perform a t-test to compare the mean SBP in those with BMI<25 and those with BMI≥. ----

whitehall$bmi_grp4 <- NA
whitehall$bmi_grp4[whitehall$bmi < 25] <- 1
whitehall$bmi_grp4[whitehall$bmi >= 25] <- 2

table(whitehall$bmi_grp4)

# What is the mean SBP where BMI <25 (x1) ?

x1 <- aggregate(sbp~bmi_grp4, mean, subset = bmi_grp4 == 1, 
                data = whitehall)
x1

# What is the mean SBP where BMI ≥25 (x2) ?

x2 <- aggregate(sbp~bmi_grp4, mean, subset = bmi_grp4 == 2, 
                data = whitehall)
x2

# What is the mean difference (x1 - x2) ?

mean_diff <- x1 - x2
mean_diff

# What is the test statistic t?

t <- t.test(sbp~bmi_grp4, data = whitehall)
t

# Transforming Variables ----

whitehall$hdlc.log <- log(whitehall$hdlc)

# Histogram

hist(whitehall$hdlc.log, breaks = 20)


# Custom function to find the mode
get_mode <- function(v) {
  uniq_vals <- unique(v)  # Get unique values
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]  # Find the most frequent value
}
x <- c(25, 24, 31, 38, 18, 14, 16, 25, 22, 25, 24, 27, 42, 30, 32)

# Calculate mode
mode_value <- get_mode(x)
mode_value

# The power package ----

install.packages('pwr')
library('pwr')

# Cohen's d 
d <- 5/15
d

# Use the ‘pwr.t.test’ command to assess the sample size needed to detect this effect size.
power1<-pwr.t.test(d=d, power=0.9, sig.level =0.05 )
power1

# If we want to estimate the power of a given sample size, we omit the ‘power’ option, and instead use the ‘n=’ option:
power2<-pwr.t.test(n=190, d=d, sig.level =0.05 )
power2

# how much power would we have ended up with in our study if we only managed to recruit 150 participants in each group, but the variance of our study sample was smaller than what we anticipated (so SD=12)?
d1 <- 5/12
d1

power3 <- pwr.t.test(n=150, d=d1, sig.level = 0.05)
power3

# We recruited fewer participants, which would decrease our power, but since our variance was lower our power actually increased overall to 95%.

# Power calculations for two proportions----
# Estimate the sample size needed to compare the proportion of people who smoke in two populations. From previous work, you think that 10% of the people in population A smoke, and that an absolute increase of 5% in population B (compared to population A) would be clinically significant. You want 90% power, and a 5% significance level.
# In this scenario we use the ‘pwr.2p.test’ command in the power package.
# alpha = sig.level option and is equal to 0.05
# power = 0.90
# p1 = 0.10
# p2 = 0.15

power4<-pwr.2p.test(h=ES.h(p1=0.1, p2=0.15), sig.level=0.05, power=0.9)
power4

# You estimate that you need 911 participants from each population, with a total sample of around 1,821.
#  If you wanted different sample sizes in each group, you would use the command ‘pwr.2p2n.test’ instead.

# If we type ‘plot(power4)’ we can see how the power level changes with varying sample sizes:
plot(power4)

# Unfortunately, the funding body has informed you, you only have enough resources to recruit a fixed number of people. Can you estimate the power of a study if you only had 500 people in total (with even numbers in each group)?
power5 <- pwr.2p.test(h=ES.h(p1=0.1, p2=0.15), n = 250, sig.level = 0.05)
power5

# In this scenario, the power of the study would be only 0.40.  Most people would regard such a study as under-powered as there is only a 40% chance that the effect will be detected if one truly exists.

# Estimating sample size for a hypothesis in a cross-sectional study ----

# You have been asked to help with a power calculation for a cross-sectional study, to estimate the point prevalence of obesity within a population. A study five years ago in this population found that 30% of people were obese, but the government thinks this has increased by 10% (to a point prevalence of 40%). Estimate the sample size needed for this study, assuming that the previous point prevalence of 30% is your `null hypothesis’. You want 80% power.

# You are calculating a sample size for one proportion here.

power8<-pwr.p.test(h=ES.h(p1=0.3, p2=0.4), power=0.8, sig.level=0.05)
power8

# You need about 178 participants in your study to estimate this prevalence.

# One researcher has suggested that the proportion of the population who is obese may actually have decreased by 10% in the last five years (i.e. to 20%). How would this change your estimate for the sample size needed?

power9<-pwr.p.test(h=ES.h(p1=0.3, p2=0.2), power=0.8, sig.level=0.05)
power9

# The estimated sample size has now reduced slightly, to 146.

# Based the outputs above, we can conclude that more data are needed to detect a change in proportion from 0.3 to 0.4 than from 0.3 to 0.2. For a fixed absolute difference (here the absolute difference in proportions is 0.1), larger sample sizes are needed to obtain a given level of power as the proportions approach 0.5.

# Estimating sample size for case-control studies ----

# how many people do you need to recruit into your study if your odds ratio is less than 0.8 and you want to have power of at least 80%?

power10 <- power.t.test(delta = 6.0 - 5.5, sd = 1, power = 0.9, sig.level = 0.05, type = "two.sample", alternative = "two.sided")
power10

# Scatterplots ----
# In this section we will use correlations and scatter plots to examine the relationship between two continuous variables.
# Let us now assess if there is a relationship between BMI and SBP. A scatterplot is a quick way to have a first impression on how variables may relate to each other.

plot(whitehall$bmi, whitehall$sbp, xlab="BMI (kg/m2)",ylab="Systolic blood pressure (mm Hg)", cex=0.8)

#  Interpret the correlation coefficient between SBP and BMI.  
corrbp_bmi <- cor(whitehall[, 6:7], use="complete.obs")
round(corrbp_bmi, 2)

# The correlation between SBP and BMI is 0.09 and it is a weak positive correlation.

# ANOVA ----
whitehall$bmi_fact<-factor(whitehall$bmi_grp4)
fit3<-aov(sbp~bmi_fact, data=whitehall)
summary(fit3)

# The result indicates there are differences in SBP across the four categories of BMI as the p-value of 0.0001 is highly significant.
