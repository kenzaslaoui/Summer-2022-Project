###################################################
#                                                 #
#  Peruvian and Venezuelan Measurement Invariance #
#               Grades 3 and 7                    #
#                                                 #
###################################################

# -----------   Initial Set-up   -------------

# Loading packages and custom functions
source("MI_PnF.R")

#### Importing data ####
## caching from osf - occasionally need to check for an updated version
# osfr::osf_auth(token = "")
# p1p2.meta.dta <- osfutils::osfcache_get(guid = "2v9p5", cache_dir = "MI/osfcache_p1p2", conflicts = "overwrite") # .dta; 
# p1p2.meta.tsv <- osfutils::osfcache_get(guid = "kt3mu", cache_dir = "MI/osfcache_p1p2", conflicts = "overwrite") # .tsv
# p1p2.dat <- read.delim(osfutils::osf_path(p1p2.meta))

## loading cached data
p1p2.orig <- haven::read_dta("osfcache_p1p2/assets/combined_p1p2-nightly.dta")  # dta - a stata file with metadata


## transforming grouping variables to factors
p1p2.orig$mnat <- haven::as_factor(p1p2.orig$mnat) # nationality
p1p2.orig$mgrade <- haven::as_factor(p1p2.orig$mgrade) # grade

## checking values
# only want Peruvian and Venezuelan kids in grades 3 and 7, but data includes other cases
table(p1p2.orig$mnat, p1p2.orig$mgrade, useNA = "always")
# filtering dataset 
p1p2.dat <- p1p2.orig %>%
  filter(mgrade %in% c(3, 7) & mnat %in% c("Peruvian", "Venezuelan"))
# table(p1p2.dat$mnat, p1p2.dat$mgrade, useNA = "always") # post-filter check


#### Defining Scales ####
## The scales that should be present and corresponding items (and item name prefix).
## These are based on the PRSEL_data-maping_p1-4_20220518.xlsx and combined_p1p2-nightly.html
## Notes. 

## Child self-regulation ("csr")
g3.csr.items <- c(paste0("csr", 1:8), "csr9r", paste0("csr", 10:13), "csr14r", "csr15") 
g7.csr.items <- paste0("csr", c(1, 4, 5, 8, 10, 12, 13, 15:19))

## Child self-regulated learning ("csrl")
# in data-mapping, only panel 2 has r suffix (panel 1 does not); response option direction appears to align though except panel 2 is 0-3 and panel 1 is 1-4
table(p1p2.dat$mnat, p1p2.dat$csrl5r, useNA = "always") # coding all appears to be 0 - 3 in data though, with frequencies similarly distributed by panel
g3.csrl.items <- paste0("csrl", 1:15)                      
g3.csrl.items[c(5:7, 13)] <- paste0(g3.csrl.items[c(5:7, 13)], "r") # items 5, 6, 7, and 13 were reversed and have suffix "r"
g7.csrl.items <- g3.csrl.items  # same items in grades 3 and 7

## Child internalizing - Child report ("cyips")
# not asked grade 3
g7.cyips.items <- paste0("cyips", 1:10)

## Child externalizing - Child report ("cyeps")
# not asked in grade 3
g7.cyeps.items <- paste0("cyeps", 1:10)

## Child internalizing - Caregiver report ("aint")
# not asked in grade 7
g3.aint.items <- paste0("aint", c(1:14))

## Child externalizing - Caregiver report ("aext") - Not asked to these panels

## Child emotional processes - Caregiver report ("aemo")
g3.aemo.items <- c(paste0("aemo", c(1:9)), "aemo10r")
g7.aemo.items <- g3.aemo.items    # same items in grades 3 and 7

## Caregiver relationship - Caregiver report ("arel") - NEED TO CHECK
g3.arel.items <- paste0("arel", 1:8)
g7.arel.items <- g3.arel.items    # same items in grades 3 and 7

## Caregiver relationship - child report ("crel")
# Listed in data-mapping for grade 7, but not present in the dataset

## Caregiver anxiety and depression ("aanxdep")
g3.aanxdep.items <- paste0("aanxdep", c(2:6))
g7.aanxdep.items <- g3.aanxdep.items


# # check for scale responses by nationality and grade
# datcheck <- p1p2.dat %>%
#   select(cid_1:mnat, all_of(g3.csr.items))
# lapply(g3.csr.items, function(x) table(datcheck$mnat, is.na(datcheck[[x]])))
# lapply(g3.csr.items, function(x) table(datcheck$mgrade, is.na(datcheck[[x]])))

##########################################################

## model identification settings - will be used for all measurement invariance models run with semTools::measEq.syntax
mi.settings <- list(ordered = TRUE, meanstructure = TRUE,
                    parameterization = "delta", ID.fac = "std.lv",
                    ID.cat = "Mplus") #, return.fit = TRUE


#############################################
####        Child Self-Regulation        ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.csr <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.csr.items)

## check response values for miscodes or lack of variance
lapply(g3.csr.items, function(x) class(g3.csr[[x]]))
lapply(g3.csr.items, function(x) table(g3.csr[[x]], useNA = "always"))

## Defining base structure
g3.csr.base <- paste("csr =~", paste(g3.csr.items, collapse = " + "))

# ## three ways to identify the configural model
# # W&E is equivalent to Mplus except the latter keeps latent response intercepts constrained to 0 when thresholds are constrained 
# config.settings <- list(configural.model = g3.csr.base, data = g3.csr, group = "mnat", group.equal = "configural",
#                         ordered = TRUE, meanstructure = TRUE)
# compare.config <- list(Wu_Estabrook = do.call(semTools::measEq.syntax,
#                                               args = c(list(parameterization = "delta", ID.fac = "std.lv",
#                                                             ID.cat = "Wu.Estabrook.2016"), config.settings)),
#                        Millsap_Tein = do.call(semTools::measEq.syntax,
#                                               args = c(list(parameterization = "theta", ID.fac = "marker",
#                                                             ID.cat = "millsap.tein.2004"), config.settings)),
#                        Mplus_lavaan = do.call(semTools::measEq.syntax,
#                                               args = c(list(parameterization = "delta", ID.fac = "std.lv",
#                                                             ID.cat = "Mplus"), config.settings)))
# 
# cat(as.character(compare.config$Wu_Estabrook))
# cat(as.character(compare.config$Millsap_Tein))
# cat(as.character(compare.config$Mplus_lavaan))
# all.equal(as.character(compare.config$Wu_Estabrook), as.character(compare.config$Mplus_lavaan))


## Generating model syntax
g3.csr.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.csr.base, data = g3.csr,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                       as.character(),
                     Threshold = do.call(semTools::measEq.syntax,
                                         args = c(list(configural.model = g3.csr.base, data = g3.csr, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                       sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                     Loading = do.call(semTools::measEq.syntax,
                                       args = c(list(configural.model = g3.csr.base, data = g3.csr, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .)) # keeping latent response variance constrained to 1


## Running models
g3.csr.mods <- list(`Peruvian Only` = cfa(g3.csr.base, data = g3.csr[g3.csr$mnat == "Peruvian",], ordered = TRUE),
                    `Venezuelan Only` = cfa(g3.csr.base, data = g3.csr[g3.csr$mnat == "Venezuelan",], ordered = TRUE),
                    Configural = cfa(g3.csr.syntax$Configural, data = g3.csr, group = "mnat"),
                    Threshold = cfa(g3.csr.syntax$Threshold, data = g3.csr, group = "mnat"),
                    Loading = cfa(g3.csr.syntax$Loading, data = g3.csr, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.csr.comps <- bind_rows(compare_mods(g3.csr.mods$Configural, g3.csr.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g3.csr.mods$Threshold, g3.csr.mods$Loading) %>% mutate(Model = "Loading")) %>%
                  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
                         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.csr.fits <- purrr::map_dfr(g3.csr.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
                 rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
                 mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
                 mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
                 select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.csr.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.csr.loads <- extract_lavaan_parameters(g3.csr.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.csr.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.csr.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.csr.means <- extract_lavaan_parameters(g3.csr.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "csr")

## unstandardized variance difference
g3.csr.var <- extract_lavaan_parameters(g3.csr.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "csr")

## Compiling results into list object
g3.csr.results <- list(Base = g3.csr.base,
                       Models = g3.csr.mods,
                       Fits = g3.csr.fits,
                       Loadings = g3.csr.loads,
                       Means = g3.csr.means,
                       Variances = g3.csr.var)

# Conclusions:
#  - model fit, configural vs threshold vs loading comparisons, and factor loadings
print(g3.csr.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#The factor loadings for csr9r and csr14r (-0.211 and - 0.057 respectively) are below this limit,
#therefore, we should consider dropping these items from our model as they are poorly correlated with the csr factor.
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
#We notice a slight change in fit from the configural model to the threshold model 
#demonstrated by a difference in CFI of - 0.001, a difference in RMSEA of -0.001
#(smaller CFI and RMSEA values were found in the threshold model)
#there was no change observed in the value of SRMR
#The chi square difference test was statistically non-significant (chi-square difference= 56.909, p= 0.092)
#The differences in fit indices were also non-significant
#therefore, the change in fit from the configural to the threshold model is non-significant 
#Since we previously established that the threshold model provides a good fit
#We conclude that threshold invariance is satisfied


#Threshold vs Loading
#We notice a slight decrease in fit from the threshold model to the loading model 
#demonstrated by a difference in CFI of -0.003 and an change in the RMSEA of 0.004
#No change in SRMR was recorded 
#(a smaller CFI and larger RMSEA)
#The chi square difference test was statistically non-significant (chi-square difference= 14.310, p= 0.427)
#the changes observed in the values of fit indices between the two models are considered to be non-significant
#The loading model provides a slightly worse fit  
#however, this change in fit from the threshold to the loading model is non-significant 
#Since we previously established that the loading model provides a good fit
#We conclude that loading invariance is satisfied

#The loading model -presenting the most constraints- demonstrates the best fit with the highest CFI=0.97, lowest RMSEA=0.04, and SRMR=0.05
#Since loading invariance (strong invariance) was achieved, 
#we can conclude that any observed differences in the Child Self-Regulation factor between the Peruvian and Venezuelan Grade 3 groups are a reflection of true differences and not due to bias (or variation in item functioning)

#Standardized means and variances of the Peruvian and Venezuelan Grade 3 groups on the Child Self Regulation factor
group <- c('Peruvian', 'Venezuelan')
mean.est <- g3.csr.means$est
mean.se <- g3.csr.means$se
variance.est <- g3.csr.var$est
variance.se <- g3.csr.var$se

g3.csr.mean.var.data <- data.frame(group, mean.est, mean.se, variance.est, variance.se)
print(g3.csr.mean.var.data)

# -----------      Grade 7      -----------

## Obtaining sample cases and scale items
g7.csr <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.csr.items)

## check response values for miscodes or lack of variance
lapply(g7.csr.items, function(x) class(g7.csr[[x]]))
lapply(g7.csr.items, function(x) table(g7.csr[[x]], useNA = "always"))

## Defining base structure
g7.csr.base <- paste("csr =~", paste(g7.csr.items, collapse = " + "))


## Generating model syntax
g7.csr.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g7.csr.base, data = g7.csr,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                       as.character(),
                     Threshold = do.call(semTools::measEq.syntax,
                                         args = c(list(configural.model = g7.csr.base, data = g7.csr, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                       sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                     Loading = do.call(semTools::measEq.syntax,
                                       args = c(list(configural.model = g7.csr.base, data = g7.csr, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.csr.mods <- list(`Peruvian Only` = cfa(g7.csr.base, data = g7.csr[g7.csr$mnat == "Peruvian",], ordered = TRUE),
                    `Venezuelan Only` = cfa(g7.csr.base, data = g7.csr[g7.csr$mnat == "Venezuelan",], ordered = TRUE),
                    Configural = cfa(g7.csr.syntax$Configural, data = g7.csr, group = "mnat"),
                    Threshold = cfa(g7.csr.syntax$Threshold, data = g7.csr, group = "mnat"),
                    Loading = cfa(g7.csr.syntax$Loading, data = g7.csr, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.csr.comps <- bind_rows(compare_mods(g7.csr.mods$Configural, g7.csr.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g7.csr.mods$Threshold, g7.csr.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.csr.fits <- purrr::map_dfr(g7.csr.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.csr.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.csr.loads <- extract_lavaan_parameters(g3.csr.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.csr.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.csr.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.csr.means <- extract_lavaan_parameters(g7.csr.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "csr")

## unstandardized variance difference
g7.csr.var <- extract_lavaan_parameters(g7.csr.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "csr")

## Compiling results into list object
g7.csr.results <- list(Base = g7.csr.base,
                       Models = g7.csr.mods,
                       Fits = g7.csr.fits,
                       Loadings = g7.csr.loads,
                       Means = g7.csr.means,
                       Variances = g7.csr.var)


# Conclusions:
print(g7.csr.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#All indicators are considered to be adequate
#We note large factor loadings for indicators csr8, csr13, csr15 
#This means that these observed variables are strongly influenced by the csr factor


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)

#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#the chi squares for both groups are significant, which is an expected result
#chi square is sensitive to sample size, therefore we will rely on fit indices to determine goodness of fit
#The model shows a good fit for the Peruvian group (CFI=0.95, RMSEA=0.06, SRMR=0.06) 
#The model fits the Venezuelan group slightly worse, however, the fit indices are still in their respective acceptable ranges (CFI=0.91, RMSEA=0.07, SRMR=0.07)

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces reasonable fit as demonstrated by fit indices (CFI=0.94, RMSEA=0.07 and SRMR=0.06)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.06 and SRMR=0.06)
#the threshold model produces an adequate fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to an acceptable range (CFI=0.92, RMSEA=0.06, SRMR=0.07)
#the loading model shows a good fit


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold
#We notice a slight decrease in fit from the configural model to the threshold model 
#demonstrated by a difference in CFI of - 0.009, a difference in RMSEA of 0.001
#(smaller CFI and larger RMSEA values were found in the threshold model)
#there was no change observed in the value of SRMR
#These differences in fit indices between the two models are considered to be non-significant
#However, the chi square difference test was found to be statistically significant (chi-square difference= 68.856, p= 0.001)
#the threshold model provides a good fit
#however, the change in fit from the configural to the threshold model is significant
#Therefore, we can't establish threshold invariance 

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.csr.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint to achieve partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.csr.part.inv.n <- 0
for (value in lavTestScore(g7.csr.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.csr.part.inv.n <- g7.csr.part.inv.n + 1 }}
print(g7.csr.part.inv.n)

#List of parameters with MI > 4
g7.csr.part.inv.int <- arrange(lavTestScore(g7.csr.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.csr.thr.part.inv.data <- data.frame(plabel = g7.csr.part.inv.int$plabel, lhs = vector('character', length = g7.csr.part.inv.n),
                            op = vector('character', length = g7.csr.part.inv.n), rhs = vector('character', length = g7.csr.part.inv.n),
                            label = vector('character', length = g7.csr.part.inv.n), X2 = g7.csr.part.inv.int$X2)

i <- 1
for (param in g7.csr.part.inv.int$plabel){
  row <- filter(parTable(g7.csr.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.csr.thr.part.inv.data$lhs[i] <- row$lhs
  g7.csr.thr.part.inv.data$op[i] <- row$op
  g7.csr.thr.part.inv.data$rhs[i] <- row$rhs
  g7.csr.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 7 Child Self Regulation Measure
print(g7.csr.thr.part.inv.data)

######################################################

#####################################################
####        Child Self-Regulated Learning        ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.csrl <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.csrl.items)

## check response values for miscodes or lack of variance
lapply(g3.csrl.items, function(x) class(g3.csrl[[x]]))
lapply(g3.csrl.items, function(x) table(g3.csrl[[x]], useNA = "always"))

## Defining base structure
g3.csrl.base <- paste("csrl =~", paste(g3.csrl.items, collapse = " + "))


## Generating model syntax
g3.csrl.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.csrl.base, data = g3.csrl,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                       as.character(),
                     Threshold = do.call(semTools::measEq.syntax,
                                         args = c(list(configural.model = g3.csrl.base, data = g3.csrl, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                       sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                     Loading = do.call(semTools::measEq.syntax,
                                       args = c(list(configural.model = g3.csrl.base, data = g3.csrl, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                       as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g3.csrl.mods <- list(`Peruvian Only` = cfa(g3.csrl.base, data = g3.csrl[g3.csrl$mnat == "Peruvian",], ordered = TRUE),
                    `Venezuelan Only` = cfa(g3.csrl.base, data = g3.csrl[g3.csrl$mnat == "Venezuelan",], ordered = TRUE),
                    Configural = cfa(g3.csrl.syntax$Configural, data = g3.csrl, group = "mnat"),
                    Threshold = cfa(g3.csrl.syntax$Threshold, data = g3.csrl, group = "mnat"),
                    Loading = cfa(g3.csrl.syntax$Loading, data = g3.csrl, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.csrl.comps <- bind_rows(compare_mods(g3.csrl.mods$Configural, g3.csrl.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g3.csrl.mods$Threshold, g3.csrl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.csrl.fits <- purrr::map_dfr(g3.csrl.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.csrl.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.csrl.loads <- extract_lavaan_parameters(g3.csrl.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.csrl.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.csrl.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.csrl.means <- extract_lavaan_parameters(g3.csrl.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "csrl")

## unstandardized variance difference
g3.csrl.var <- extract_lavaan_parameters(g3.csrl.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "csrl")

## Compiling results into list object
g3.csrl.results <- list(Base = g3.csrl.base,
                       Models = g3.csrl.mods,
                       Fits = g3.csrl.fits,
                       Loadings = g3.csrl.loads,
                       Means = g3.csrl.means,
                       Variances = g3.csrl.var)

# Conclusions:
print(g3.csrl.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#The factor loadings for csrl5r csrl6r and csrl7r (0.170, 0.139 and 0.170 respectively) are below this limit
#therefore, we should consider dropping these items from our model because of their poor correlation with the csrl factor.
#We consider all other indicators to be adequate
#We also note that csrl15 has a large loading (> 0.6) - denoting a strong correlation between the indicator and the csrl factor


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)

#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#the chi squares for both groups are significant, this is an expected result as chi square is sensitive to sample size
#To assess goodness of fit of the model we will rely heavily on the results of the fit indices (CFI, RMSEA, SRMR)
#While a good fit is shown by CFI (CFI>0.95), RMSEA (RMSEA<0.06) and SRMR (SRMR<0.08)
#values of CFI between 0.9<CFI<0.95 and values of RMSEA belonging to this range 0.06<RMSEA<0.10 are acceptable
#However, in this case the values of CFI for both groups fall outside of that range
#with CFI= 0.82 for the Peruvian group and CFI = 0.87 for the Venezuelan group
#The RMSEAs (0.08 for the Peruvian Group and 0.07 for the Venezuelan Group) and SRMR=0.08 for both groups can be accepted 
#Because of the low CFIs and high RMSEAs, we conclude that the csrl measure model fits poorly both the Peruvian and Venezuelan grade 3 groups


# -----------      Grade 7      -----------

## Obtaining sample cases and scale items
g7.csrl <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.csrl.items)

## check response values for miscodes or lack of variance
lapply(g7.csrl.items, function(x) class(g7.csrl[[x]]))
lapply(g7.csrl.items, function(x) table(g7.csrl[[x]], useNA = "always"))

## Defining base structure
g7.csrl.base <- paste("csrl =~", paste(g7.csrl.items, collapse = " + "))

## Generating model syntax
g7.csrl.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.csrl.base, data = g7.csrl,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g7.csrl.base, data = g7.csrl, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g7.csrl.base, data = g7.csrl, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.csrl.mods <- list(`Peruvian Only` = cfa(g7.csrl.base, data = g7.csrl[g7.csrl$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g7.csrl.base, data = g7.csrl[g7.csrl$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g7.csrl.syntax$Configural, data = g7.csrl, group = "mnat"),
                     Threshold = cfa(g7.csrl.syntax$Threshold, data = g7.csrl, group = "mnat"),
                     Loading = cfa(g7.csrl.syntax$Loading, data = g7.csrl, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.csrl.comps <- bind_rows(compare_mods(g7.csrl.mods$Configural, g7.csrl.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g7.csrl.mods$Threshold, g7.csrl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.csrl.fits <- purrr::map_dfr(g7.csrl.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.csrl.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.csrl.loads <- extract_lavaan_parameters(g7.csrl.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.csrl.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.csrl.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.csrl.means <- extract_lavaan_parameters(g7.csrl.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "csrl")

## unstandardized variance difference
g7.csrl.var <- extract_lavaan_parameters(g7.csrl.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "csrl")

## Compiling results into list object
g7.csrl.results <- list(Base = g7.csrl.base,
                        Models = g7.csrl.mods,
                        Fits = g7.csrl.fits,
                        Loadings = g7.csrl.loads,
                        Means = g7.csrl.means,
                        Variances = g7.csrl.var)

# Conclusions:
print(g7.csrl.results)

## Factor Loadings

#We consider any factor loading ( |factor loading| > 0.30 ) to be adequate
#In this case, all factor loadings meet this criteria and are considered to be adequate (even those that are reverse coded)
#We also note that csrl10 and csrl15 have large loadings (> 0.6) 
#This means that the csrl factor has a strong influence on these variables.


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#As expected, we obtain significant chi squares for both groups
#the RMSEA (=0.06 in both groups) and the SRMR (=0.07 in both groups) belong to their respective ranges for adequate fit
#the CFI values (0.91 in the Peruvian group and 0.90 in the Venezuelan group) are acceptable
#We can conclude that the model produces a reasonable fit for the two groups 


#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces adequate fit as demonstrated by fit indices (CFI=0.91, RMSEA=0.06 and SRMR=0.07)
#we conclude that the fit of the configural model is reasonable enough
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the RMSEA nd SRMR belong to their respective adequate fit ranges (RMSEA=0.06 and SRMR=0.07)
#However, because the CFI value (CFI= 0.89) is outside of its acceptable fit range [0.90, 1.00]
#The fit of the threshold model is ambiguous
#Therefore, we can not proceed to test threshold invariance (#Note also that the difference in chi square test is significant when comparing the configural and threshold models)

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.csrl.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint for a better fit of the threshold model and potentially achieve partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.csrl.part.inv.n <- 0
for (value in lavTestScore(g7.csrl.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.csrl.part.inv.n <- g7.csrl.part.inv.n + 1 }}
print(g7.csrl.part.inv.n)

#List of parameters with MI > 4
g7.csrl.part.inv.int <- arrange(lavTestScore(g7.csrl.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.csrl.thr.part.inv.data <- data.frame(plabel = g7.csrl.part.inv.int$plabel, lhs = vector('character', length = g7.csrl.part.inv.n),
                                       op = vector('character', length = g7.csrl.part.inv.n), rhs = vector('character', length = g7.csrl.part.inv.n),
                                       label = vector('character', length = g7.csrl.part.inv.n), X2 = g7.csrl.part.inv.int$X2)

i <- 1
for (param in g7.csrl.part.inv.int$plabel){
  row <- filter(parTable(g7.csrl.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.csrl.thr.part.inv.data$lhs[i] <- row$lhs
  g7.csrl.thr.part.inv.data$op[i] <- row$op
  g7.csrl.thr.part.inv.data$rhs[i] <- row$rhs
  g7.csrl.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to improve the fit of the threshold model 
#and potentially achieve Threshold Partial Invariance on the Grade 7 Child Self Regulated Learning Measure
print(g7.csrl.thr.part.inv.data)


######################################################

#####################################################
####     Child internalizing - Child report      ####

# -----------      Grade 7      -----------

g7.cyips <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.cyips.items)

## check response values for miscodes or lack of variance
lapply(g7.cyips.items, function(x) class(g7.cyips[[x]]))
lapply(g7.cyips.items, function(x) table(g7.cyips[[x]], useNA = "always"))

## Defining base structure
g7.cyips.base <- paste("cyips =~", paste(g7.cyips.items, collapse = " + "))

## Generating model syntax
g7.cyips.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.cyips.base, data = g7.cyips,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g7.cyips.base, data = g7.cyips, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g7.cyips.base, data = g7.cyips, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.cyips.mods <- list(`Peruvian Only` = cfa(g7.cyips.base, data = g7.cyips[g7.cyips$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g7.cyips.base, data = g7.cyips[g7.cyips$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g7.cyips.syntax$Configural, data = g7.cyips, group = "mnat"),
                     Threshold = cfa(g7.cyips.syntax$Threshold, data = g7.cyips, group = "mnat"),
                     Loading = cfa(g7.cyips.syntax$Loading, data = g7.cyips, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.cyips.comps <- bind_rows(compare_mods(g7.cyips.mods$Configural, g7.cyips.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g7.cyips.mods$Threshold, g7.cyips.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.cyips.fits <- purrr::map_dfr(g7.cyips.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.cyips.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.cyips.loads <- extract_lavaan_parameters(g7.cyips.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.cyips.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.cyips.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.cyips.means <- extract_lavaan_parameters(g7.cyips.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "cyips")

## unstandardized variance difference
g7.cyips.var <- extract_lavaan_parameters(g7.cyips.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "cyips")

## Compiling results into list object
g7.cyips.results <- list(Base = g7.cyips.base,
                        Models = g7.cyips.mods,
                        Fits = g7.cyips.fits,
                        Loadings = g7.cyips.loads,
                        Means = g7.cyips.means,
                        Variances = g7.cyips.var)


# Conclusions:
print(g7.cyips.results)

## Factor Loadings

#All factor loading are adequate as they meet the criteria ( |factor loading| > 0.30 ) 
#We also note that cyips5 and cyips9 - having factor loading that exceed 0.6 are strongly correlated with the latent variable cyips.


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
#The model shows a good fit for the Peruvian group (CFI=0.98, RMSEA=0.05, SRMR=0.05) 
#The model's fit decreased for the Venezuelan group as compared to the Peruvian group 
#However, the fit indices produced for the Venezuelan group are still acceptable (CFI=0.92, RMSEA=0.08, SRMR=0.07)

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model shows a good fit as demonstrated by fit indices (CFI=0.95, RMSEA=0.06 and SRMR=0.06)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.07 and SRMR=0.06)
#the threshold model produces a reasonable fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to a adequate fit ranges (CFI=0.93, RMSEA=0.06, SRMR=0.06)
#the Loading model shows an acceptable fit


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold

#We notice a slight decrease in fit from the configural model to the threshold model 
#demonstrated by a difference in CFI of - 0.017, a difference in RMSEA of 0.010
#(smaller CFI and a larger RMSEA values were found in the threshold model)
#no change in SRMR was recorded 
#These changes in fit indices are considered to be non-significant
#However, the chi square difference test was statistically significant (chi-square difference= 76.493, p= 0.000)
#Although we previously established that the threshold model provides a good fit
#We conclude that threshold invariance is not satisfied
#The measure is threshold non-invariant


##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.cyips.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint for a better fit of the threshold model and potentially achieve partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.cyips.part.inv.n <- 0
for (value in lavTestScore(g7.cyips.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.cyips.part.inv.n <- g7.cyips.part.inv.n + 1 }}
print(g7.cyips.part.inv.n)

#List of parameters with MI > 4
g7.cyips.part.inv.int <- arrange(lavTestScore(g7.cyips.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.cyips.thr.part.inv.data <- data.frame(plabel = g7.cyips.part.inv.int$plabel, lhs = vector('character', length = g7.cyips.part.inv.n),
                                        op = vector('character', length = g7.cyips.part.inv.n), rhs = vector('character', length = g7.cyips.part.inv.n),
                                        label = vector('character', length = g7.cyips.part.inv.n), X2 = g7.cyips.part.inv.int$X2)

i <- 1
for (param in g7.cyips.part.inv.int$plabel){
  row <- filter(parTable(g7.cyips.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.cyips.thr.part.inv.data$lhs[i] <- row$lhs
  g7.cyips.thr.part.inv.data$op[i] <- row$op
  g7.cyips.thr.part.inv.data$rhs[i] <- row$rhs
  g7.cyips.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to achieve Threshold Partial Invariance on the Grade 7 Child Internalizing(Child Report) Measure
print(g7.cyips.thr.part.inv.data)


######################################################

#####################################################
####     Child externalizing - Child report      ####

# -----------      Grade 7      -----------

g7.cyeps <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.cyeps.items)

## check response values for miscodes or lack of variance
lapply(g7.cyeps.items, function(x) class(g7.cyeps[[x]]))
lapply(g7.cyeps.items, function(x) table(g7.cyeps[[x]], useNA = "always"))

## Defining base structure
g7.cyeps.base <- paste("cyeps =~", paste(g7.cyeps.items, collapse = " + "))

## Generating model syntax
g7.cyeps.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                            args = c(list(configural.model = g7.cyeps.base, data = g7.cyeps,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                         as.character(),
                       Threshold = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.cyeps.base, data = g7.cyeps, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                         as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                         sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                       Loading = do.call(semTools::measEq.syntax,
                                         args = c(list(configural.model = g7.cyeps.base, data = g7.cyeps, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                         as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.cyeps.mods <- list(`Peruvian Only` = cfa(g7.cyeps.base, data = g7.cyeps[g7.cyeps$mnat == "Peruvian",], ordered = TRUE),
                      `Venezuelan Only` = cfa(g7.cyeps.base, data = g7.cyeps[g7.cyeps$mnat == "Venezuelan",], ordered = TRUE),
                      Configural = cfa(g7.cyeps.syntax$Configural, data = g7.cyeps, group = "mnat"),
                      Threshold = cfa(g7.cyeps.syntax$Threshold, data = g7.cyeps, group = "mnat"),
                      Loading = cfa(g7.cyeps.syntax$Loading, data = g7.cyeps, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.cyeps.comps <- bind_rows(compare_mods(g7.cyeps.mods$Configural, g7.cyeps.mods$Threshold) %>% mutate(Model = "Threshold"),
                            compare_mods(g7.cyeps.mods$Threshold, g7.cyeps.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.cyeps.fits <- purrr::map_dfr(g7.cyeps.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.cyeps.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.cyeps.loads <- extract_lavaan_parameters(g7.cyeps.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.cyeps.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.cyeps.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.cyeps.means <- extract_lavaan_parameters(g7.cyeps.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "cyeps")

## unstandardized variance difference
g7.cyeps.var <- extract_lavaan_parameters(g7.cyeps.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "cyeps")

## Compiling results into list object
g7.cyeps.results <- list(Base = g7.cyeps.base,
                         Models = g7.cyeps.mods,
                         Fits = g7.cyeps.fits,
                         Loadings = g7.cyeps.loads,
                         Means = g7.cyeps.means,
                         Variances = g7.cyeps.var)

# Conclusions:
print(g7.cyeps.results)

## Factor Loadings
#All factor loading are considered to be adequate as they all meet the criteria ( |factor loading| > 0.30 )
#We note that the cyeps latent variable has consistently large factor loadings as all factor loadings > 0.5
#This implies a strong influence of the factor on all the items 


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#similar to the results of the cyips factor, we notice a significant decrease in model fit from the Peruvian to the Venezuelan group
#the chi square is significant for both groups, which is an expected result
#the fit indices for the Peruvian group are in the excellent range (CFI= 0.98, RMSEA= 0.05, SRMR= 0.05)
#The fit of the model on the Peruvian group is excellent
#When the model was applied to the Venezuelan group, the fit decreased significantly as demonstrated by a smaller CFI and higher RMSEA and SRMR values
#The fit indices for the Venezuelan group are as follows (CFI= 0.91, RMSEA= 0.09, SRMR= 0.08)
#These values belong to an acceptable fit range
#Therefore the fit of the model on the Venezuelan group is acceptable


#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces adequate fit as demonstrated by fit indices (CFI=0.95, RMSEA=0.07 and SRMR=0.06)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.07 and SRMR=0.06)
#the threshold model produces a reasonable fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.07, SRMR=0.07)
#the fit of the loading model is reasonable enough


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold
#We notice that the fit decreases from the configural to the threshold model 
#This is demonstrated by a difference in CFI of -0.016 and a difference in RMSEA of 0.009, no change in SRMR was noted
#These changes in fit indices between the two models are non-significant
#however, there is a significant change in the chi square test (chi-square difference= 81.955, p= 0.000)
#Therefore, although we accepted the fit of the threshold model, we can not establish threshold invariance
#This model is threshold non-invariant

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.cyeps.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint for a better fit of the threshold model and potentially achieve partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.cyeps.part.inv.n <- 0
for (value in lavTestScore(g7.cyeps.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.cyeps.part.inv.n <- g7.cyeps.part.inv.n + 1 }}
print(g7.cyeps.part.inv.n)

#List of parameters with MI > 4
g7.cyeps.part.inv.int <- arrange(lavTestScore(g7.cyeps.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.cyeps.thr.part.inv.data <- data.frame(plabel = g7.cyeps.part.inv.int$plabel, lhs = vector('character', length = g7.cyeps.part.inv.n),
                                         op = vector('character', length = g7.cyeps.part.inv.n), rhs = vector('character', length = g7.cyeps.part.inv.n),
                                         label = vector('character', length = g7.cyeps.part.inv.n), X2 = g7.cyeps.part.inv.int$X2)

i <- 1
for (param in g7.cyeps.part.inv.int$plabel){
  row <- filter(parTable(g7.cyeps.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.cyeps.thr.part.inv.data$lhs[i] <- row$lhs
  g7.cyeps.thr.part.inv.data$op[i] <- row$op
  g7.cyeps.thr.part.inv.data$rhs[i] <- row$rhs
  g7.cyeps.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 7 Child Externalizing(Child Report) Measure
print(g7.cyeps.thr.part.inv.data)


######################################################

#####################################################
####  Child internalizing - Caregiver report     ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.aint <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.aint.items)

## check response values for miscodes or lack of variance
lapply(g3.aint.items, function(x) class(g3.aint[[x]]))
lapply(g3.aint.items, function(x) table(g3.aint[[x]], useNA = "always"))

## Defining base structure
g3.aint.base <- paste("aint =~", paste(g3.aint.items, collapse = " + "))

## Generating model syntax
g3.aint.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g3.aint.base, data = g3.aint,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.aint.base, data = g3.aint, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g3.aint.base, data = g3.aint, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g3.aint.mods <- list(`Peruvian Only` = cfa(g3.aint.base, data = g3.aint[g3.aint$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g3.aint.base, data = g3.aint[g3.aint$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g3.aint.syntax$Configural, data = g3.aint, group = "mnat"),
                     Threshold = cfa(g3.aint.syntax$Threshold, data = g3.aint, group = "mnat"),
                     Loading = cfa(g3.aint.syntax$Loading, data = g3.aint, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.aint.comps <- bind_rows(compare_mods(g3.aint.mods$Configural, g3.aint.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g3.aint.mods$Threshold, g3.aint.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.aint.fits <- purrr::map_dfr(g3.aint.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.aint.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.aint.loads <- extract_lavaan_parameters(g3.aint.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.aint.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.aint.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.aint.means <- extract_lavaan_parameters(g3.aint.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "aint")

## unstandardized variance difference
g3.aint.var <- extract_lavaan_parameters(g3.aint.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "aint")

## Compiling results into list object
g3.aint.results <- list(Base = g3.aint.base,
                        Models = g3.aint.mods,
                        Fits = g3.aint.fits,
                        Loadings = g3.aint.loads,
                        Means = g3.aint.means,
                        Variances = g3.aint.var)

# Conclusions:
print(g3.aint.results)

## Factor Loadings

#We consider a factor loading to be adequate if ( |factor loading| > 0.30 ) 
#In this case, all factor loadings are adequate
#We also note that aint9 and aint10 have large loadings (> 0.6) - denoting a strong correlation between these items and the aint variable


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)

#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#As expected, the chi square result for both groups was significant, we rely more on the values of fit indices produced
#values of CFI between 0.9<CFI<0.95, values of RMSEA between 0.06<RMSEA<0.10 and values of SRMR between 0.08<SRMR<0.10 are considered acceptable
#However, in this case the values of CFI for both groups fall outside of their acceptable range
#with CFI= 0.87 for the Peruvian group and CFI = 0.81 for the Venezuelan group
#The RMSEAs (0.08 for the Peruvian Group and 0.09 for the Venezuelan Group) and 
#the SRMRs (0.08 for the Peruvian Group and 0.10 for the Venezuelan Group) can be accepted 
#Because of the low CFIs and high RMSEAs and SRMRs, we conclude that the model for the aint factor has a poor fit on both the Peruvian and Venezuelan grade 3 groups


######################################################

#####################################################
#### Child emotional processes - Caregiver report ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.aemo <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.aemo.items)

## check response values for miscodes or lack of variance
lapply(g3.aemo.items, function(x) class(g3.aemo[[x]]))
lapply(g3.aemo.items, function(x) table(g3.aemo[[x]], useNA = "always"))

## Defining base structure
g3.aemo.base <- paste("aemo =~", paste(g3.aemo.items, collapse = " + "))

## Generating model syntax
g3.aemo.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g3.aemo.base, data = g3.aemo,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.aemo.base, data = g3.aemo, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g3.aemo.base, data = g3.aemo, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g3.aemo.mods <- list(`Peruvian Only` = cfa(g3.aemo.base, data = g3.aemo[g3.aemo$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g3.aemo.base, data = g3.aemo[g3.aemo$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g3.aemo.syntax$Configural, data = g3.aemo, group = "mnat"),
                     Threshold = cfa(g3.aemo.syntax$Threshold, data = g3.aemo, group = "mnat"),
                     Loading = cfa(g3.aemo.syntax$Loading, data = g3.aemo, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.aemo.comps <- bind_rows(compare_mods(g3.aemo.mods$Configural, g3.aemo.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g3.aemo.mods$Threshold, g3.aemo.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.aemo.fits <- purrr::map_dfr(g3.aemo.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.aemo.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.aemo.loads <- extract_lavaan_parameters(g3.aemo.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.aemo.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.aemo.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.aemo.means <- extract_lavaan_parameters(g3.aemo.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "aemo")

## unstandardized variance difference
g3.aemo.var <- extract_lavaan_parameters(g3.aemo.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "aemo")

## Compiling results into list object
g3.aemo.results <- list(Base = g3.aemo.base,
                        Models = g3.aemo.mods,
                        Fits = g3.aemo.fits,
                        Loadings = g3.aemo.loads,
                        Means = g3.aemo.means,
                        Variances = g3.aemo.var)

# Conclusions:
print(g3.aemo.results)

## Factor Loadings

##We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#The factor loading for aemo10r (0.282) is below this limit 
#the factor loadings of all other indicators are considered to be adequate, in fact, they all have large factor loadings (>0.55)


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)

#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#The chi square test results are significant for both groups - a result we expected
#We rely on fit indices CFI, RMSEA, and SRMR
#The fit of the model on the Peruvian group is reasonable (CFI= 0.95, RMSEA= 0.09, SRMR= 0.06) #(The RMSEA is high but still in its acceptable range)
  
#The fit indices for the Venezuelan group (CFI= 0.89, RMSEA= 0.14, SRMR= 0.09) 
#The CFI and RMSEA values fall outside of their respective acceptable fit ranges (0.9<CFI<0.95, 0.06<RMSEA<0.10)
#The model for the aemo measure fits the Venezuelan grade 3 group poorly 


# -----------      Grade 7      -----------

## Obtaining sample cases and scale items
g7.aemo <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.aemo.items)

## check response values for miscodes or lack of variance
lapply(g7.aemo.items, function(x) class(g7.aemo[[x]]))
lapply(g7.aemo.items, function(x) table(g7.aemo[[x]], useNA = "always"))

## Defining base structure
g7.aemo.base <- paste("aemo =~", paste(g7.aemo.items, collapse = " + "))

## Generating model syntax
g7.aemo.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.aemo.base, data = g7.aemo,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g7.aemo.base, data = g7.aemo, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g7.aemo.base, data = g7.aemo, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.aemo.mods <- list(`Peruvian Only` = cfa(g7.aemo.base, data = g7.aemo[g7.aemo$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g7.aemo.base, data = g7.aemo[g7.aemo$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g7.aemo.syntax$Configural, data = g7.aemo, group = "mnat"),
                     Threshold = cfa(g7.aemo.syntax$Threshold, data = g7.aemo, group = "mnat"),
                     Loading = cfa(g7.aemo.syntax$Loading, data = g7.aemo, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.aemo.comps <- bind_rows(compare_mods(g7.aemo.mods$Configural, g7.aemo.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g7.aemo.mods$Threshold, g7.aemo.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.aemo.fits <- purrr::map_dfr(g7.aemo.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.aemo.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.aemo.loads <- extract_lavaan_parameters(g7.aemo.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.aemo.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.aemo.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.aemo.means <- extract_lavaan_parameters(g7.aemo.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "aemo")

## unstandardized variance difference
g7.aemo.var <- extract_lavaan_parameters(g7.aemo.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "aemo")

## Compiling results into list object
g7.aemo.results <- list(Base = g7.aemo.base,
                        Models = g7.aemo.mods,
                        Fits = g7.aemo.fits,
                        Loadings = g7.aemo.loads,
                        Means = g7.aemo.means,
                        Variances = g7.aemo.var)

# Conclusions:
print(g7.aemo.results)

## Factor Loadings

#All factor loadings are considered to be adequate (even aemo10r which is reverse coded)
#as they all meet the criteria ( |factor loading| > 0.30 ) 
#with the exception of aemo10r, all indicators have quite large factor loadings (>0.55)


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)

#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#As expected, the results of the chi square test for both groups are significant
#The fit of the model on the Peruvian group is acceptable (although not great) with the following fit indices (CFI= 0.92, RMSEA= 0.10, SRMR= 0.07)
#However, the fit of the model on the Venezuelan group is ambiguous (CFI= 0.93, RMSEA= 0.12, SRMR= 0.07) 
#the RMSEA, being quite large, falls outside of the accepted fit range (0.06<RMSEA<0.10)


#####################################################
#### Caregiver relationship - Caregiver report  ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.arel <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.arel.items)

## check response values for miscodes or lack of variance
lapply(g3.arel.items, function(x) class(g3.arel[[x]]))
lapply(g3.arel.items, function(x) table(g3.arel[[x]], useNA = "always"))

## Defining base structure
g3.arel.base <- paste("arel =~", paste(g3.arel.items, collapse = " + "))

## Generating model syntax
g3.arel.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g3.arel.base, data = g3.arel,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.arel.base, data = g3.arel, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g3.arel.base, data = g3.arel, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g3.arel.mods <- list(`Peruvian Only` = cfa(g3.arel.base, data = g3.arel[g3.arel$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g3.arel.base, data = g3.arel[g3.arel$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g3.arel.syntax$Configural, data = g3.arel, group = "mnat"),
                     Threshold = cfa(g3.arel.syntax$Threshold, data = g3.arel, group = "mnat"),
                     Loading = cfa(g3.arel.syntax$Loading, data = g3.arel, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.arel.comps <- bind_rows(compare_mods(g3.arel.mods$Configural, g3.arel.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g3.arel.mods$Threshold, g3.arel.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.arel.fits <- purrr::map_dfr(g3.arel.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.arel.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.arel.loads <- extract_lavaan_parameters(g3.arel.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.arel.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.arel.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.arel.means <- extract_lavaan_parameters(g3.arel.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "arel")

## unstandardized variance difference
g3.arel.var <- extract_lavaan_parameters(g3.arel.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "arel")

## Compiling results into list object
g3.arel.results <- list(Base = g3.arel.base,
                        Models = g3.arel.mods,
                        Fits = g3.arel.fits,
                        Loadings = g3.arel.loads,
                        Means = g3.arel.means,
                        Variances = g3.arel.var)

# Conclusions:
print(g3.arel.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#The factor loading for arel7 (0.286) is below this limit
#We can consider dropping this item from our model 
#All other factor loadings are adequate
#note the large factor loading for arel2 - showing a strong correlation between this item and the arel factor


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#The results of the chi square test are significant in both groups - which was expected
#The fit indices in the Peruvian group belong to their excellent ranges (CFI= 0.97, RMSEA= 0.05, SRMR= 0.05)
#The fit of the model on the Peruvian group is good
#The model also shows a good fit for the Venezuelan group (CFI= 0.96, RMSEA= 0.04, SRMR = 0.06)

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces a good fit as demonstrated by fit indices (CFI=0.97, RMSEA=0.05 and SRMR=0.05)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.06 and SRMR=0.05)
#the fit of the threshold model is adequate
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.92, RMSEA=0.06, SRMR=0.06)
#the fit of the loading model is reasonable


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold
#We notice a significant decrease in fit from the configural model to the threshold model 
#this is demonstrated by a significant change in CFI of -0.029 
#and a significant difference in chi square test result (chi-square difference= 64.763, p= 0.000)
#there was no change in SRMR and the difference in RMSEA (0.016) was non-significant
#Because of the significant p-value and CFI differences 
#we can conclude that the change in fit from the configural to the threshold model is significant
#As such we can not establish threshold invariance (Although we proved that the threshold model provides a good fit)

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g3.arel.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint to obtain partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g3.arel.part.inv.n <- 0
for (value in lavTestScore(g3.arel.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g3.arel.part.inv.n <- g3.arel.part.inv.n + 1 }}
print(g3.arel.part.inv.n)

#List of parameters with MI > 4
g3.arel.part.inv.int <- arrange(lavTestScore(g3.arel.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g3.arel.thr.part.inv.data <- data.frame(plabel = g3.arel.part.inv.int$plabel, lhs = vector('character', length = g3.arel.part.inv.n),
                                         op = vector('character', length = g3.arel.part.inv.n), rhs = vector('character', length = g3.arel.part.inv.n),
                                         label = vector('character', length = g3.arel.part.inv.n), X2 = g3.arel.part.inv.int$X2)

i <- 1
for (param in g3.arel.part.inv.int$plabel){
  row <- filter(parTable(g3.arel.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g3.arel.thr.part.inv.data$lhs[i] <- row$lhs
  g3.arel.thr.part.inv.data$op[i] <- row$op
  g3.arel.thr.part.inv.data$rhs[i] <- row$rhs
  g3.arel.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 3 Caregiver Relationship Measure
print(g3.arel.thr.part.inv.data)


# -----------      Grade 7      -----------

## Obtaining sample cases and scale items
g7.arel <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.arel.items)

## check response values for miscodes or lack of variance
lapply(g7.arel.items, function(x) class(g7.arel[[x]]))
lapply(g7.arel.items, function(x) table(g7.arel[[x]], useNA = "always"))

## Defining base structure
g7.arel.base <- paste("arel =~", paste(g7.arel.items, collapse = " + "))

## Generating model syntax
g7.arel.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.arel.base, data = g7.arel,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g7.arel.base, data = g7.arel, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g7.arel.base, data = g7.arel, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.arel.mods <- list(`Peruvian Only` = cfa(g7.arel.base, data = g7.arel[g7.arel$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g7.arel.base, data = g7.arel[g7.arel$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g7.arel.syntax$Configural, data = g7.arel, group = "mnat"),
                     Threshold = cfa(g7.arel.syntax$Threshold, data = g7.arel, group = "mnat"),
                     Loading = cfa(g7.arel.syntax$Loading, data = g7.arel, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.arel.comps <- bind_rows(compare_mods(g7.arel.mods$Configural, g7.arel.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g7.arel.mods$Threshold, g7.arel.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.arel.fits <- purrr::map_dfr(g7.arel.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.arel.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.arel.loads <- extract_lavaan_parameters(g7.arel.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.arel.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.arel.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.arel.means <- extract_lavaan_parameters(g7.arel.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "arel")

## unstandardized variance difference
g7.arel.var <- extract_lavaan_parameters(g7.arel.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "arel")

## Compiling results into list object
g7.arel.results <- list(Base = g7.arel.base,
                        Models = g7.arel.mods,
                        Fits = g7.arel.fits,
                        Loadings = g7.arel.loads,
                        Means = g7.arel.means,
                        Variances = g7.arel.var)

# Conclusions:
print(g7.arel.results)


## Factor Loadings
#All factor loadings are considered to be adequate as they meet the criteria ( |factor loading| > 0.30 ) 
#Note that the values of the factor loadings in the grade 7 model are close to those obtained in the grade 3 model for the same factor arel
#Note the large factor loading on arel2 

## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#We ignore the significant p value results as chi square is known to be sensitive to large sample sizes
#The model produces excellent fit on the Peruvian group (CFI= 0.97, RMSEA= 0.06, SRMR= 0.05)
#The fit decreases for the Venezuelan group 
#this is demonstrated by the lower CFI value and higher RMSEA and SRMR results (CFI= 0.94, RMSEA= 0.07, SRMR= 0.07)
#however, these values are acceptable and so we conclude that the fit is reasonable

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces a good fit as demonstrated by fit indices (CFI=0.96, RMSEA=0.06 and SRMR=0.06)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.93, RMSEA=0.06 and SRMR=0.06)
#the threshold model produces an adequate fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the values of the fit indices are acceptable (CFI=0.92, RMSEA=0.07, SRMR=0.07)
#the loading model's fit is reasonable enough


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold
#There is a decrease in fit from the configural to the threshold model
#although the changes in fit indices are non-significant (difference in CFI= -0.013, difference in RMSEA= 0.004, no difference in SRMR)
#the chi square difference test result is significant (chi-square difference= 49.566, p= 0.001)
#Therefore, threshold invariance is not satisfied

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.arel.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint to obtain partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.arel.part.inv.n <- 0
for (value in lavTestScore(g7.arel.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.arel.part.inv.n <- g7.arel.part.inv.n + 1 }}
print(g7.arel.part.inv.n)

#List of parameters with MI > 4
g7.arel.part.inv.int <- arrange(lavTestScore(g7.arel.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.arel.thr.part.inv.data <- data.frame(plabel = g7.arel.part.inv.int$plabel, lhs = vector('character', length = g7.arel.part.inv.n),
                                        op = vector('character', length = g7.arel.part.inv.n), rhs = vector('character', length = g7.arel.part.inv.n),
                                        label = vector('character', length = g7.arel.part.inv.n), X2 = g7.arel.part.inv.int$X2)

i <- 1
for (param in g7.arel.part.inv.int$plabel){
  row <- filter(parTable(g7.arel.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.arel.thr.part.inv.data$lhs[i] <- row$lhs
  g7.arel.thr.part.inv.data$op[i] <- row$op
  g7.arel.thr.part.inv.data$rhs[i] <- row$rhs
  g7.arel.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 7 Caregiver Relationship Measure
print(g7.arel.thr.part.inv.data)


#####################################################
####    Caregiver anxiety and depression    ####

# -----------      Grade 3      -----------

## Obtaining sample cases and scale items
g3.aanxdep <- p1p2.dat %>%
  get_sampscale(grade = "3", items = g3.aanxdep.items)

## check response values for miscodes or lack of variance
lapply(g3.aanxdep.items, function(x) class(g3.aanxdep[[x]]))
lapply(g3.aanxdep.items, function(x) table(g3.aanxdep[[x]], useNA = "always"))

## Defining base structure
g3.aanxdep.base <- paste("aanxdep =~", paste(g3.aanxdep.items, collapse = " + "))

## Generating model syntax
g3.aanxdep.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g3.aanxdep.base, data = g3.aanxdep,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                        as.character(),
                      Threshold = do.call(semTools::measEq.syntax,
                                          args = c(list(configural.model = g3.aanxdep.base, data = g3.aanxdep, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                        sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                      Loading = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = g3.aanxdep.base, data = g3.aanxdep, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                        as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g3.aanxdep.mods <- list(`Peruvian Only` = cfa(g3.aanxdep.base, data = g3.aanxdep[g3.aanxdep$mnat == "Peruvian",], ordered = TRUE),
                     `Venezuelan Only` = cfa(g3.aanxdep.base, data = g3.aanxdep[g3.aanxdep$mnat == "Venezuelan",], ordered = TRUE),
                     Configural = cfa(g3.aanxdep.syntax$Configural, data = g3.aanxdep, group = "mnat"),
                     Threshold = cfa(g3.aanxdep.syntax$Threshold, data = g3.aanxdep, group = "mnat"),
                     Loading = cfa(g3.aanxdep.syntax$Loading, data = g3.aanxdep, group = "mnat"))


## comparing configural, Threshold, and Loading models
g3.aanxdep.comps <- bind_rows(compare_mods(g3.aanxdep.mods$Configural, g3.aanxdep.mods$Threshold) %>% mutate(Model = "Threshold"),
                           compare_mods(g3.aanxdep.mods$Threshold, g3.aanxdep.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.aanxdep.fits <- purrr::map_dfr(g3.aanxdep.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.aanxdep.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g3.aanxdep.loads <- extract_lavaan_parameters(g3.aanxdep.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.aanxdep.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.aanxdep.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g3.aanxdep.means <- extract_lavaan_parameters(g3.aanxdep.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "aanxdep")

## unstandardized variance difference
g3.aanxdep.var <- extract_lavaan_parameters(g3.aanxdep.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "aanxdep")

## Compiling results into list object
g3.aanxdep.results <- list(Base = g3.aanxdep.base,
                        Models = g3.aanxdep.mods,
                        Fits = g3.aanxdep.fits,
                        Loadings = g3.aanxdep.loads,
                        Means = g3.aanxdep.means,
                        Variances = g3.aanxdep.var)

# Conclusions:
print(g3.aanxdep.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#All rhe factor loadings are above this limit and are therefore considered to be adequate
#Note that all items have corresponding factor loadings that are quite large (> 0.50)


## Model Fit

#We use the following indices to determine adequate fit
#Chi Square: Non significant -  p-value (p>0.05)
#CFI (CFI>0.95)
#RMSEA (RMSEA<0.06)
#SRMR (SRMR<0.08)
#(Kline, 2016)


#We first test our model separately on each of the groups ‘Peruvian’ and ‘Venezuelan’
#As expected, the chi square results are significant and we rely more heavily on the fit indices
#The fit indices of the Peruvian group (CFI= 0.99, RMSEA= 0.07, SRMR= 0.03) are excellent (the RMSEA is a little high but still acceptable)
#The model shows a satisfactory fit for the Peruvian group

#The fit indices of the Venezuelan group are as follows (CFI= 0.98, RMSEA= 0.12, SRMR= 0.05) 
#The RMSEA is outside of its acceptable fit range 0.06<RMSEA<0.10
#The fit of the model on the Venezuelan group is ambiguous

#If we choose to accept the fit of the model on the Venezuelan group, 
#we can proceed to run a multi-group CFA and compare the three models (Configural, Threshold and Loading)

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces adequate fit as demonstrated by fit indices (CFI=0.99, RMSEA=0.09 and SRMR=0.04)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.97, RMSEA=0.08 and SRMR=0.04)
#the threshold model produces an adequate fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.97, RMSEA=0.07, SRMR=0.05)
#the loading model shows a good fit


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold

#We notice a significant change in fit from the configural model to the threshold model 
#We obtain non-significant changes in fit indices 
#(difference in CFI of -0.004, a difference in RMSEA of -0.008, no change observed in the value of SRMR)
#However, the chi square difference test was statistically significant (chi-square difference= 32.719, p= 0.003)
#Therefore, we are unable to establish threshold invariance


##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g3.aanxdep.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint to obtain partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g3.aanxdep.part.inv.n <- 0
for (value in lavTestScore(g3.aanxdep.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g3.aanxdep.part.inv.n <- g3.aanxdep.part.inv.n + 1 }}
print(g3.aanxdep.part.inv.n)

#List of parameters with MI > 4
g3.aanxdep.part.inv.int <- arrange(lavTestScore(g3.aanxdep.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g3.aanxdep.thr.part.inv.data <- data.frame(plabel = g3.aanxdep.part.inv.int$plabel, lhs = vector('character', length = g3.aanxdep.part.inv.n),
                                           op = vector('character', length = g3.aanxdep.part.inv.n), rhs = vector('character', length = g3.aanxdep.part.inv.n),
                                           label = vector('character', length = g3.aanxdep.part.inv.n), X2 = g3.aanxdep.part.inv.int$X2)

i <- 1
for (param in g3.aanxdep.part.inv.int$plabel){
  row <- filter(parTable(g3.aanxdep.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g3.aanxdep.thr.part.inv.data$lhs[i] <- row$lhs
  g3.aanxdep.thr.part.inv.data$op[i] <- row$op
  g3.aanxdep.thr.part.inv.data$rhs[i] <- row$rhs
  g3.aanxdep.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 3 Caregiver Anxiety and Depression Measure
print(g3.aanxdep.thr.part.inv.data)

# -----------      Grade 7      -----------

## Obtaining sample cases and scale items
g7.aanxdep <- p1p2.dat %>%
  get_sampscale(grade = "7", items = g7.aanxdep.items)

## check response values for miscodes or lack of variance
lapply(g7.aanxdep.items, function(x) class(g7.aanxdep[[x]]))
lapply(g7.aanxdep.items, function(x) table(g7.aanxdep[[x]], useNA = "always"))

## Defining base structure
g7.aanxdep.base <- paste("aanxdep =~", paste(g7.aanxdep.items, collapse = " + "))

## Generating model syntax
g7.aanxdep.syntax <-list(Configural = do.call(semTools::measEq.syntax,
                                              args = c(list(configural.model = g7.aanxdep.base, data = g7.aanxdep,group = "mnat", group.equal = "configural"), mi.settings)) %>%
                           as.character(),
                         Threshold = do.call(semTools::measEq.syntax,
                                             args = c(list(configural.model = g7.aanxdep.base, data = g7.aanxdep, group = "mnat", group.equal = c("thresholds")), mi.settings)) %>%
                           as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .) %>% # keeping latent response variance constrained to 1
                           sub("c\\(0, 0\\)\\*1 \\+ c\\(alpha", "c(0, NA)*1 + c(alpha", .), # freeing latent trait mean in group 2
                         Loading = do.call(semTools::measEq.syntax,
                                           args = c(list(configural.model = g7.aanxdep.base, data = g7.aanxdep, group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)) %>%
                           as.character() %>% gsub("~\\*~ c\\(1, NA\\)", "~*~ c(1, 1)", .))


## Running models
g7.aanxdep.mods <- list(`Peruvian Only` = cfa(g7.aanxdep.base, data = g7.aanxdep[g7.aanxdep$mnat == "Peruvian",], ordered = TRUE),
                        `Venezuelan Only` = cfa(g7.aanxdep.base, data = g7.aanxdep[g7.aanxdep$mnat == "Venezuelan",], ordered = TRUE),
                        Configural = cfa(g7.aanxdep.syntax$Configural, data = g7.aanxdep, group = "mnat"),
                        Threshold = cfa(g7.aanxdep.syntax$Threshold, data = g7.aanxdep, group = "mnat"),
                        Loading = cfa(g7.aanxdep.syntax$Loading, data = g7.aanxdep, group = "mnat"))


## comparing configural, Threshold, and Loading models
g7.aanxdep.comps <- bind_rows(compare_mods(g7.aanxdep.mods$Configural, g7.aanxdep.mods$Threshold) %>% mutate(Model = "Threshold"),
                              compare_mods(g7.aanxdep.mods$Threshold, g7.aanxdep.mods$Loading) %>% mutate(Model = "Loading")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g7.aanxdep.fits <- purrr::map_dfr(g7.aanxdep.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
  rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
  mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
  mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
  select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g7.aanxdep.comps, by = "Model")


## extracting unstandardized factor loadings for each group
# Constrained standardized factor loadings will not necessarily be equal across groups
g7.aanxdep.loads <- extract_lavaan_parameters(g7.aanxdep.mods$Threshold, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g7.aanxdep.mods$Loading, std = "no", params = "=~") %>%
               select(item = rhs, Constrained = est) %>%
               mutate(Constrained = round(Constrained, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g7.aanxdep.items)) %>%
  arrange(item)

## extracting standardized mean difference from model constraining thresholds and loadings
g7.aanxdep.means <- extract_lavaan_parameters(g7.aanxdep.mods$Loading, std = "std.all", params = "~1") %>%
  filter(lhs == "aanxdep")

## unstandardized variance difference
g7.aanxdep.var <- extract_lavaan_parameters(g7.aanxdep.mods$Loading, std = "no", params = "~~") %>%
  filter(lhs == "aanxdep")

## Compiling results into list object
g7.aanxdep.results <- list(Base = g7.aanxdep.base,
                           Models = g7.aanxdep.mods,
                           Fits = g7.aanxdep.fits,
                           Loadings = g7.aanxdep.loads,
                           Means = g7.aanxdep.means,
                           Variances = g7.aanxdep.var)

# Conclusions:
print(g7.aanxdep.results)

## Factor Loadings

#We fix the cutoff for adequate factor loadings to ( |factor loading| > 0.30 ) 
#All factor loadings are adequate
#We note that all factor loadings are quite large


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
#The model shows a satisfactory fit for the Peruvian group (CFI=0.99, RMSEA=0.08, SRMR=0.03) 
#and an acceptable fit for the Venezuelan group (CFI=0.98, RMSEA=0.10, SRMR=0.05)
#note that the RMSEA is quite high for both groups

#Multi-Group CFA is performed 
#First, with no constraints:
#the chi square result is significant (p<0.05) - as expected
#the model produces adequate fit as demonstrated by fit indices (CFI=0.99, RMSEA=0.09 and SRMR=0.04)
#we conclude that the fit of the configural model is satisfactory
#Second, we constrain the indicator thresholds to be equal across the two groups (to test weak invariance):
#the chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.98, RMSEA=0.08 and SRMR=0.04)
#the threshold model produces an adequate fit
#Third, we constrain both indicator thresholds and factor loadings to be equal across the two groups (to test strong invariance):
#The chi square result is significant (p<0.05) - as expected
#the fit indices belong to their respective acceptable fit ranges (CFI=0.98, RMSEA=0.07, SRMR=0.04)
#the loading model shows a good fit


## Configural vs Threshold vs Loading

#To compare the 3 models, we consider a change in model fit to be significant
#between configural and threshold models, if: Difference in CFI= 0.02 , Difference in RMSEA= 0.03, Difference in SRMP= 0.03
#between threshold and loading models, if: Difference in CFI= 0.01, Difference in RMSEA= 0.01, Difference in SRMP= 0.01
#Chen (2007) 
#Rutkowski and Svetina (2014)

#Configural vs Threshold

#We notice a significant change in fit from the configural model to the threshold model 
#We obtain non-significant changes in fit indices 
#(difference in CFI of - 0.003, a difference in RMSEA of -0.007, no change observed in the value of SRMR)
#However, the chi square difference test was statistically significant (chi-square difference= 28.742, p= 0.011)
#Therefore, we are unable to establish threshold invariance

##We proceed to investigate partial invariance in the Threshold model
lavTestScore(g7.aanxdep.mods$Threshold, cumulative = TRUE)$uni

#We would like to know which thresholds to free from the equality constraint to obtain partial invariance
#the lavTestScore identifies the Modification Index for each of our restricted parameters 
#(in this case, our restricted parameters are the indicator thresholds that are constrained to be equal in the two groups)
#The Modification Index describes the decrease in chi square that would be achieved provided that we free the parameter restricted
#We identify the thresholds with significant Modification indices (MI > 4)  

#Number of parameters with MI > 4
g7.aanxdep.part.inv.n <- 0
for (value in lavTestScore(g7.aanxdep.mods$Threshold, cumulative = TRUE)$uni$X2){
  if (value > 4){
    g7.aanxdep.part.inv.n <- g7.aanxdep.part.inv.n + 1 }}
print(g7.aanxdep.part.inv.n)

#List of parameters with MI > 4
g7.aanxdep.part.inv.int <- arrange(lavTestScore(g7.aanxdep.mods$Threshold, cumulative = TRUE)$uni, desc(X2)) %>%
  filter(X2 > 4) %>%
  select(lhs, X2) %>%
  rename(plabel = lhs)

g7.aanxdep.thr.part.inv.data <- data.frame(plabel = g7.aanxdep.part.inv.int$plabel, lhs = vector('character', length = g7.aanxdep.part.inv.n),
                                        op = vector('character', length = g7.aanxdep.part.inv.n), rhs = vector('character', length = g7.aanxdep.part.inv.n),
                                        label = vector('character', length = g7.aanxdep.part.inv.n), X2 = g7.aanxdep.part.inv.int$X2)

i <- 1
for (param in g7.aanxdep.part.inv.int$plabel){
  row <- filter(parTable(g7.aanxdep.mods$Threshold), plabel == param) %>%
    select(lhs, op, rhs, label)
  
  g7.aanxdep.thr.part.inv.data$lhs[i] <- row$lhs
  g7.aanxdep.thr.part.inv.data$op[i] <- row$op
  g7.aanxdep.thr.part.inv.data$rhs[i] <- row$rhs
  g7.aanxdep.thr.part.inv.data$label[i] <- row$label
  i <- i + 1
}

#Consider freeing the equality constraint on the following thresholds to obtain Threshold Partial Invariance on the Grade 7 Caregiver Anxiety and Depression Measure
print(g7.aanxdep.thr.part.inv.data)

save(g3.csr.fits, g3.csrl.fits, g3.aint.fits, g3.aemo.fits, g3.arel.fits, 
     g3.aanxdep.fits, g7.csr.fits, g7.csrl.fits, g7.cyips.fits, g7.cyeps.fits,
     g7.aemo.fits, g7.arel.fits, g7.aanxdep.fits, g3.arel.thr.part.inv.data,
     g3.aanxdep.thr.part.inv.data, g7.csr.thr.part.inv.data, g7.csrl.thr.part.inv.data, 
     g7.cyips.thr.part.inv.data, g7.cyeps.thr.part.inv.data, g7.arel.thr.part.inv.data, 
     g7.aanxdep.thr.part.inv.data,
  file= "Archived/MI_Results.RData")
