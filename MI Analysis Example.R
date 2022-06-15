## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#The factor loadings for csr9r and csr14r (-0.211 and - 0.057 respectively) are below this limit,
#therefore, we remove them from our model as they are poorly correlated with the csr factor.
#We consider all other indicators to be adequate
#We also note that csr3, csr6, csr8, csr 12, csr13 have large loadings (> 0.6) 
#This means that the csr latent variable has a strong influence on these variables.


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#the chi squares for both groups are significant, however, we know that chi square is sensitive to sample size
#therefore, given our large sample size, we will rely on fit indices (CFI, RMSEA, SRMR) to assess the fit of the model
#The model shows a good fit for the Peruvian group (CFI=0.96, RMSEA=0.05, SRMR=0.05) as well as the Venezuelan group (CFI=0.97, RMSEA=0.05, SRMR=0.05)

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces adequate fit as demonstrated by fit indices (CFI=0.97, RMSEA=0.05 and SRMR=0.05)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.96, RMSEA=0.04 and SRMR=0.05)
#the threshold model produces an adequate fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to an excellent range (CFI=0.97, RMSEA=0.04, SRMR=0.05)
#the loading model shows a good fit


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold
#We notice a slight decrease in fit from the configural model to the threshold model 
#demonstrated by a difference in CFI of - 0.001, a difference in RMSEA of -0.001
#(smaller CFI and RMSEA values were found in the threshold model)
#there was no change observed in the value of SRMR
#The chi square difference test was statistically non-significant (chi-square difference= 56.909, p= 0.092)
#The differences in fit indices were also non-significant
#the threshold model provides a slightly worse fit model
#however, this change in fit from the configural to the threshold model is non-significant 
#Since we previously established that the threshold model provides a good fit
#We conclude that threshold invariance is satisfied


#Threshold vs Loading
#We notice a slight improvement in fit from the threshold model to the loading model 
#demonstrated by a difference in CFI of 0.001 and an improvement in the RMSEA of 0.001
#No change in SRMR was recorded 
#(a larger CFI, a smaller RMSEA)
#The chi square difference test was statistically non-significant (chi-square difference= 9.359, p= 0.745)
#the changes observed in the values of fit indices between the two models are considered to be non-significant
#The loading model provides a slightly improved fit  
#however, this change in fit from the threshold to the loading model is non-significant 
#Since we previously established that the loading model provides a good fit
#We conclude that loading invariance is satisfied

#The loading model -presenting the most constraints- demonstrates the best fit with the highest CFI=0.97, lowest RMSEA=0.04, and SRMR=0.05
#Since loading invariance (strong invariance) was achieved, 
#we can conclude that any observed differences in the Child Self-Regulation factor between the Peruvian and Venezuelan groups are a reflection of true differences and not due to bias (or variation in item functioning)