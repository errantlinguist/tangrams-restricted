#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)
if(length(args) < 1)
{
	stop("Usage: <scriptname> INFILE")
}

infile <-args[1]
if (!file_test("-f", infile)) 
{
	stop(sprintf("No file found at \"%s\".", infile));
}

library(lmerTest)
library(MASS)
library(MuMIn)

cvResults <- read.table(infile, sep="\t", header=TRUE)
origSampleSize <- nrow(cvResults)
print(sprintf("Read %d cross-validation samples.", origSampleSize), quote=FALSE)

#Take out the observation(s) with token count over 200 (in this data: one data point)
cvResults[!cvResults$TOKEN_COUNT>200,] -> cvResults
sampleSizeWithoutOutliers <- nrow(cvResults)
print(sprintf("Removed %d outlier.", origSampleSize - sampleSizeWithoutOutliers), quote=FALSE)

refLevel <- "ALL_NEG"
#Set the reference level for Training
relevel(cvResults$Training, ref=refLevel) -> cvResults$Training
print(sprintf("Set training reference level to \"%s\".", refLevel), quote=FALSE)

#Linear Mixed Model where RANK is the dependent variable (the stuff you predict), TOKEN_COUNT, Training and 
#SESSION_ORDER are your fixed effects (the stuff you are interested in that you think might have an effect on RANK),
#and a single random effect which is adjusting your intercept (the constant b in y = b + xz) by the effect
#the participants in a sessions might have introduced. The assumption is effect is random, i.e. following a Gaussian.

#Model m.additive is an additive model (only main effects)
m.additive <- lmer(RANK ~ TOKEN_COUNT + Training + SESSION_ORDER +  (1|DYAD), data=cvResults, REML=FALSE)
#Model m.interaction is an interaction model (interaction btw token count and trainng)
m.interaction <- lmer(RANK ~ TOKEN_COUNT * Training + SESSION_ORDER +  (1|DYAD), data=cvResults, REML=FALSE)
#Model m.cubicNonlinear includes a cubic nonlinear term for token count, does not significantly improve the model
#m.cubicNonlinear <- lmer(RANK ~ I(TOKEN_COUNT^2) * Training + SESSION_ORDER +  (1|DYAD), data=cvResults, REML=FALSE)

#This is a test for whether the interaction model improved anything over the additive model.
p <- anova(m.additive, m.interaction)
p$Chisq
p$`Pr(>Chisq)`

#Data: cvResults
#Models:
#  object: RANK ~ TOKEN_COUNT + Training + SESSION_ORDER + (1 | DYAD)
#..1: RANK ~ TOKEN_COUNT * Training + SESSION_ORDER + (1 | DYAD)
# Df   AIC   BIC logLik deviance Chisq Chi        Df    Pr(>Chisq)  
# object  7 42923 42972 -21455    42909                          
# ..1     9 42919 42981 -21450    42901 8.858      2    0.01193 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#plot(cvResults$RANK~cvResults$Training|cvResults$DYAD)

# http://mindingthebrain.blogspot.se/2014/02/three-ways-to-get-parameter-specific-p.html
# extract coefficients
#coefs <- data.frame(coef(summary(m.interaction)))
# get Satterthwaite-approximated degrees of freedom
#coefs$df.Satt <- coef(summary(m.interaction))[, 3]
# get approximate p-values
#coefs$p.Satt <- coef(summary(m.interaction))[, 5]

#Now you print the estimates etc. of the best model you found.
summary(m.interaction)

# Linear mixed model fit by maximum likelihood t-tests use Satterthwaite approximations to degrees of
# freedom [lmerMod]
# Formula: RANK ~ TOKEN_COUNT * Training + SESSION_ORDER + (1 | DYAD)
# Data: cvResults
# 
# AIC      BIC   logLik deviance df.resid 
# 42918.6  42980.6 -21450.3  42900.6     7200 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.0493 -0.7085 -0.3553  0.5164  3.5080 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# DYAD      (Intercept)  2.001   1.414   
# Residual             22.335   4.726   
# Number of obs: 7209, groups:  DYAD, 13
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                   4.495e+00  4.639e-01  2.500e+01   9.691 7.26e-10 ***
#   TOKEN_COUNT                   6.727e-02  2.481e-02  7.201e+03   2.711  0.00672 ** 
#   TrainingDIALOGIC              3.017e-01  3.019e-01  7.196e+03   0.999  0.31770    
# TrainingONE_NEG               1.225e+00  2.370e-01  7.196e+03   5.168 2.42e-07 ***
#   SESSION_ORDER                -8.700e-03  2.198e-03  7.192e+03  -3.959 7.60e-05 ***
#   TOKEN_COUNT:TrainingDIALOGIC -6.737e-02  2.943e-02  7.197e+03  -2.289  0.02209 *  
#   TOKEN_COUNT:TrainingONE_NEG  -1.298e-02  2.670e-02  7.196e+03  -0.486  0.62704    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#qqnorm(resid(m.interaction))

#r.squaredGLMM(m.interaction)

#library(ggplot2)
#ggplot(aes(x=factor(TOKEN_COUNT), y=RANK, fill=factor(TOKEN_COUNT)), data=cvResults)+geom_boxplot()+theme_jlre+facet_wrap(~Training)
#ggplot(aes(x=factor(TOKEN_COUNT), y=RANK, fill=factor(TOKEN_COUNT)), data=cvResults)+geom_boxplot()+facet_wrap(~Training)
