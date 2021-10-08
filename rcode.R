library(apaTables)

# Upload the dataset
data <- read.csv("data_Oyserman2018.csv", header = TRUE)

# Rename the first column
names(data)[1] <- c("conditions")

# Change the variable of gender to the factor type
data$gender_fac <- factor(x=data$gender,
                        levels = 0:1,
                        labels = c("Male", "Female"))

# Descriptive Statistics
apa.2way.table(iv1 = gender_fac,iv2 = conditions, dv = scores, 
               data = data, 
               filename = "Table1_APA.doc", 
               show.marginal.means = TRUE,
               table.number = 1)

# Assumption Check: Normality
spssSkewKurtosis=function(x) {
  w=length(x)
  m1=mean(x)
  m2=sum((x-m1)^2)
  m3=sum((x-m1)^3)
  m4=sum((x-m1)^4)
  s1=sd(x)
  skew=w*m3/(w-1)/(w-2)/s1^3
  sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
             dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}

spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 2.244032 < 3.09, Pass Normality Check
spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Female"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 0.2530942 < 3.09, Pass Normality Check
spssSkewKurtosis(data[which(data$conditions=="importance" & data$gender_fac=="Male"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 0.3832331 < 3.09, Pass Normality Check
spssSkewKurtosis(data[which(data$conditions=="importance" & data$gender_fac=="Female"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 0.3056062 < 3.09, Pass Normality Check
spssSkewKurtosis(data[which(data$conditions=="impossibility" & data$gender_fac=="Male"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 0.933647 < 3.09, Pass Normality Check
spssSkewKurtosis(data[which(data$conditions=="impossibility" & data$gender_fac=="Female"),"scores"])[1,1]/spssSkewKurtosis(data[which(data$conditions=="control" & data$gender_fac=="Male"),"scores"])[1,2]
## -3.09 < 0.7098966 < 3.09, Pass Normality Check

# Assumption Check: Homoscedasticity
data$groups <- paste(data$conditions,data$gender_fac)
library(lawstat)
levene.test(data[,"scores"], data[,c("groups")])
## p-value = .06991 > .05, No evidence for heteroscedasticity, Pass Homoscedasticity Check.

## The data meet both the assumptions for a ANOVA test, the ANOVA test can be carried out from here on.

# Change the default handling of unordered factors to deviation coding (contr.sum)
options(contrasts=c("contr.sum", "contr.poly"))

# Fit the linear regression model to conduct ANCOVA, DV = scores, IVs = gender*condition, COVs = effort, grade, words, and sentences
lm1 <- lm(formula = scores ~ conditions*gender_fac + z_effort + z_grade + z_words + z_sentences, data = data)

# The ANOVA Source Table
apa.aov.table(lm1, filename = "Table2_APA.doc", table.number = 2)
# interaction is not significant, p-value = .15

######################################################################################
# The Interaction
library(emmeans)
emm1<-emmeans(object = lm1, spec=~conditions*gender_fac)
ip1 <- emmip(object=emm1, formula = gender_fac ~ conditions, lty = 2, xlab=c("Mindset Interventions"), ylab=c("Writing Scores"), CIs = TRUE, ylim=c(0,4))
ip1$labels$colour <-  "Gender"
print(ip1)

# Pairwise comparisons
emm1.m<-emmeans(object = lm1, spec=~conditions) # the main effect of conditions
pairs(emm1.m, adjust="none")
## The only significant pair is "control - importance". p-value = .0165 < .05/1, the Shaffer's Planned Post-Omnibus

# Linear contrasts
contrast(emm1.m, method=list(c(1/2,-1,1/2))) # importance vs others
## Significant, p-value = .0136 < .05/2, the Holm-Bonferroni Correction
contrast(emm1.m, method=list(c(1/2,1/2,-1))) # impossibility vs others
## Not significant, p-value = .4520