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

## model identification settings - will be used for all measurement invariance models run with semTools::measEq.syntax
mi.settings <- list(ordered = TRUE, parameterization = "delta", ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016", return.fit = TRUE)


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

## Running models
g3.csr.mods <- list(`Peruvian Only` = cfa(g3.csr.base, data = g3.csr[g3.csr$mnat == "Peruvian",], ordered = TRUE),
                    `Venezuelan Only` = cfa(g3.csr.base, data = g3.csr[g3.csr$mnat == "Venezuelan",], ordered = TRUE),
                    Configural = do.call(semTools::measEq.syntax, args = c(list(configural.model = g3.csr.base, data = g3.csr,
                                                                                group = "mnat", group.equal = "configural"), mi.settings)),
                    Metric = do.call(semTools::measEq.syntax, args = c(list(configural.model = g3.csr.base, data = g3.csr,
                                                                            group = "mnat", group.equal = c("thresholds")), mi.settings)),
                    Scalar = do.call(semTools::measEq.syntax, args = c(list(configural.model = g3.csr.base, data = g3.csr,
                                                                            group = "mnat", group.equal = c("thresholds", "loadings")), mi.settings)))

## comparing configural, metric, and scalar models
g3.csr.comps <-bind_rows(compare_mods(g3.csr.mods$Configural, g3.csr.mods$Metric) %>% mutate(Model = "Metric"),
                           compare_mods(g3.csr.mods$Metric, g3.csr.mods$Scalar) %>% mutate(Model = "Scalar")) %>%
                  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
                         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr)

## gathering model fit for each model and joining model comparison statistics
g3.csr.fits <- map_dfr(g3.csr.mods, ~get_lavaan_fits(.x), .id = "Model") %>%
                 rename_with(.cols = ends_with(".scaled"), .fn = ~gsub("\\.scaled", "", .)) %>%
                 mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
                 mutate(RMSEA.CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
                 select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, RMSEA.CI95, SRMR = srmr) %>%
  left_join(g3.csr.comps, by = "Model")


## extracting factor loadings for each group
g3.csr.loads <- extract_lavaan_parameters(g3.csr.mods$Metric, std = "no", params = "=~") %>%
  mutate(group = recode(group, `1` = "Peruvian", `2` = "Venezuelan")) %>%
  select(group, item = rhs, est) %>%
  tidyr::spread(group, est) %>%
  inner_join(extract_lavaan_parameters(g3.csr.mods$Scalar, std = "no", params = "=~") %>%
               select(item = rhs, Scalar = est) %>%
               mutate(Scalar = round(Scalar, 5)) %>%
               unique(), by = "item") %>%
  mutate(item = factor(item, levels = g3.csr.items)) %>%
  arrange(item)

## Compiling results into list object
g3.csr.results <- list(Base = g3.csr.base,
                       Models = g3.csr.mods,
                       Fits = g3.csr.fits)


# -----------      Grade 7      -----------

######################################################

#####################################################
####        Child Self-Regulated Learning        ####


