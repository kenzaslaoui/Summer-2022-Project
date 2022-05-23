###################################################
#                                                 #
#  Peruvian and Venezuelan Measurement Invariance #
#            Packages and Functions               #
#                                                 #
###################################################

library(dplyr)
library(lavaan)
# library(purrr)

## gather item names from dataset
# Note. Assumes naming structure is constant and does not account for separate scales using the same prefix
get_items <- function(data, prefix){
  nums <- sub(prefix, "", names(data)[grepl(paste0(prefix,"[0-9]+"), names(data))])
  if(length(nums) == 0){
    return(nums)
  } else {
    return(paste0(prefix, nums))
  }
}


#### Shortcut for gathering sample and scale data ####
## data - a data.frame
## grade - character indicating grade. Must be a level in mgrade variable
## items - character vector of column names indicated items in the scale
get_sampscale <- function(data, grade, items){
  
  ss <- data %>%
    select(cid_1:mnat, all_of(items)) %>% # cid_1:mnat are demographic variables; can change this if needed
    filter(mgrade == grade) %>%           # assumes grade variable is mgrade
    filter(rowSums(is.na(.[items])) != length(items)) %>% # removing cases with NA for all items
    mutate(across(.cols = all_of(items), .fns = as.numeric)) # ensuring items are numeric (rather than a factor or character)
  
  return(ss)
}

#### Extract model information and fit statistics ####
get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(object@Fit@converged == 0){
    stop("Model did not converge and fit statistics cannot be produced.")
  }
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
  } else if(measures == "robust"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.robust',  'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    
  } else if(measures == "naive"){
    
    indices <- c('npar', 'chisq', 'df', 'pvalue',
                 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')
  }
  
  
  
  fits <- as.data.frame(t(c(lavaan::fitMeasures(object, fit.measures = indices))))
  
  fits <- fits %>% mutate(ntotal = lavaan::inspect(object, "ntotal"),
                          ngroups = lavaan::inspect(object, "ngroups")) %>%
    select(ntotal, ngroups, everything())
  
  if(!is.na(object@loglik$loglik)){
    
    fits <- fits %>%
      mutate(AIC = round(AIC(object), 1),
             BIC = round(BIC(object), 1))
    
  }
  
  return(fits)
}

#### Likelihood ratio test comparing measurement invariance models ####
compare_mods <- function(mod0, mod1, method = "default", measures = c("cfi", "rmsea", "srmr"), digits = 3){
  
  aov01 <- lavTestLRT(mod0, mod1, method = method)[2,c(5:7)]
  
  # Uses non-scaled version
  ind0 <- get_lavaan_fits(mod0, measures = measures)
  ind1 <- get_lavaan_fits(mod1, measures = measures)
  ind.diff <- ind1 - ind0
  
  mi.com <- format(round(cbind(aov01, ind.diff), digits), nsmall = digits)
  row.names(mi.com) <- NULL
  # names(mi.com) <- c("delta.chisq", "delta.df", "delta.pvalue", "delta.cfi", "delta.rmsea", "delta.srmr")
  
  return(mi.com)
}

#### Estimating model-based reliability ####
get_relis <- function(object, wave){
  
  relis <- suppressWarnings(semTools::reliability(object))
  
  df <- data.frame(wave = wave,
                   alpha = relis[[1]],
                   omega = relis[[4]])
  return(df)
}


#### Extract standardized or unstandardized parameter estimates ####
extract_lavaan_parameters <- function(object, std = "no", params = "all", ...){
  
  if("all" %in% params){
    
    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")
    
  }
  
  if(std == "no"){
    
    TheParams <- parameterEstimates(object, standardized = FALSE, ...) %>%
      filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- standardizedSolution(object, type = std) %>%
      filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  
  return(TheParams)
}


