library(lavaan)

#A CFA model
HS.model <- ' visual  =~ x1 + start(0.5) * x2 + x3 
              textual =~ x4 + x5 + a_label * x6 + start(0.1) * x6
              speed   =~ x7 + v * x8 + v * x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939, orthogonal = TRUE, std.lv= TRUE)

coef(fit)
summary(fit, fit.measures = TRUE)
#I fixed the starting value for the second indicator of the "visual" variable at 0.5
#I labeled x6 the third indicator for the "textual" factor "a_label" and fixed its starting value at 0.1
#I set the two indicators x8 and x9 to have the same parameter
#Here I made all the factors to be independent and their variances to be equal to 1, the model is standardized
#Note here that because variances equal to 1, factor loading of first indicators are different than 1


## Alternatively: 
HS.model.alt <- ' visual  =~ x1 + start(0.5) * x2 + x3 
                  textual =~ x4 + x5 + a_label * x6 + start(0.1) * x6
                  speed   =~ x7 + v * x8 + v * x9 

                  #To make latent variables orthogonal
                  visual ~~ 0 * speed
                  textual ~~ 0 * speed
                  visual ~~ 0 * textual

                  #To set variances to unity
                  visual  ~~ 1 * visual
                  textual ~~ 1 * textual
                  speed   ~~ 1 * speed
                  
                  #fixing intercepts values
                  x1 + x3 + x8 + x9 ~ 1
                  x2 + x4 ~ 0.2 * 1
                  x6 + x7 ~ 0.5 * 1 
                  x5 ~ 0 * 1'

fit <- cfa(HS.model.alt, data = HolzingerSwineford1939)

coef(fit)
summary(fit, fit.measures = TRUE)
#Here I set all observed variables to be intercepts (except x5)
#I fixed the value of intercepts x2 and x4 to 0.2
#I fixed the value of intercepts x6 and x7 to 0.5


#An SEM Model
PD.model <- ' # measurement model
                ind60 =~ x1 + x2 + x3
                dem60 =~ y1 + y2 + y3 + y4
                dem65 =~ y5 + y6 + y7 + y8
        
              # regressions
                dem60 ~ ind60
                dem65 ~ ind60 + dem60
      
              # residual correlations
                y1 ~~ y5
                y2 ~~ y4 + y6
                y3 ~~ y7
                y4 ~~ y8
                y6 ~~ y8 '

fit <- sem(PD.model, data = PoliticalDemocracy)

summary(fit, standardized = TRUE)       


#A group model
HS.model <- ' visual  =~ x1 + 0.5 * x2 + c(0.6, 0.8) * x3
              textual =~ x4 + start(c(1.2, 0.6)) * x5 + c(a1, a2) * x6
              speed   =~ x7 + x8 + c(b, b) * x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939, group = "school", group.equal = c("intercepts"))

summary(fit)
#Here factor loading of x2 is 0.5 in both groups
#Factor loading of x3 is 0.6 in the first group and 0.8 in the second
#Starting value of x3 in first group is 1.2 and 0.6 in second group
#I imposed the same label on x9 for both groups and therefore the parameters are also constrained to be equal
#In the fitting function, I made intercepts to be equal across groups


#Measurement Invariance Testing
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

    # configural invariance
    fit1 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school")
    
    # weak invariance: factor loadings constrained to be equal across all groups
    fit2 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school", group.equal = "loadings")
    
    # strong invariance: factor loadings and intercepts constrained to be equal across all groups
    fit3 <- cfa(HS.model, data = HolzingerSwineford1939, group = "school", group.equal = c("intercepts", "loadings"))
    
    # model comparison tests
    lavTestLRT(fit1, fit2, fit3)
