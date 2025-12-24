

library(mgcv)


# import ssf data
load("./data/ssf/ssf_data_w_fire_enlarged.rda")
ssf_data_2023 <- ssf_data[ssf_data$year == "2023",]
# ssf_data_2023$log_dist_escape_25m <- log(ssf_data_2023$dist_escape_25m + 1)
# ssf_data_2023$log_dist_to_fire <- log(ssf_data_2023$dist_to_fire + 1)


#..........................................
### hierarchical smooths = non-linear pattern of selection ----

# cbind(times, stratum) = the relative probability of a step being chosen given the covariates in the model COMPARED to random steps in the same stratum
# `stratum` = identifier for each observed location and its associated random points, 
# `obs` = binary variable to identify whether the location is observed or random; `obs = 1` if observed, and `obs = 0` if random, 
# `step` = the step length, and 
# `angle` = the turning angle. 
# The response needs to be a combination of a column of `times` (all same value, i.e. 1) and stratum ID (as a factor). df$times <- 1
# "times" is defined as a constant variable, .-. add dummy time column as a single value for all

# For step lengths -> use the default thin plate regression splines (TPRS). 
# Turning angles = circular distribution .-. use a cyclic spline.
# include a smooth effect for covariates, using TPRS. 
# using log -> log(covariate + 1) will represent values closer to said parameter, e.g. distance to fire, will weigh/be more critical biologically, like if youre 1m to a fire is more serious than if youre 100m from a fire
# We specify `family = cox.ph` (identity link function) and 
# use the `weights` argument to specify whether each row is an observed or random location (`obs`). 

# hierarchical smooths -> accounts for inter-individual variability in selection AND 
# allows individuals to have non-linear patterns of selection
# linear model with non-linear terms
# 


#.............................................................................
# model checks for selection ----

# base model (only movement and habitat covariates)
tictoc::tic()
base_model <- gam(cbind(times, stratum) ~ 
                    step + log(step) + cos(angle) +
                    s(elev_25m_scaled, k = 5) + 
                    s(log(dist_escape_25m + 1), k = 5) +  
                    s(log(dist_to_fire + 1), k = 5), 
                  data = ssf_data_2023,
                  method = "REML",
                  family = cox.ph, 
                  weights = obs)
tictoc::toc() #1.5min

beepr::beep(3)


#.............................................
# add factor smooths, grouped by ID
tictoc::tic()
mod_id <- gam(cbind(times, stratum) ~ 
                step + log(step) + cos(angle) +
                s(elev_25m_scaled, k = 5) + 
                s(elev_25m_scaled, ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                s(log(dist_escape_25m + 1), k = 5) +  
                s(log(dist_escape_25m + 1), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                s(log(dist_to_fire + 1), k = 5) + 
                s(log(dist_to_fire + 1), ID, k = 5, bs = "fs"), # factor smooth, grouped by ID
              data = ssf_data_2023,
              method = "REML",
              family = cox.ph, 
              weights = obs)
tictoc::toc() # 28min
beepr::beep(4)




#.............................................
# add interaction terms
tictoc::tic()
# this is the full model
fit_smooths <- gam(cbind(times, stratum) ~ 
                     step + log(step) + cos(angle) +
                     s(scale(elev_25m), k = 5) + 
                     s(scale(elev_25m), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                     s(log(dist_escape_25m + 1), k = 5) +  
                     s(log(dist_escape_25m + 1), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                     s(log(dist_to_fire + 1), k = 5) + 
                     s(log(dist_to_fire + 1), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                     ti(scale(elev_25m), log(dist_to_fire + 1)) + # interaction between dist to fire & habitat covariate
                     ti(log(dist_escape_25m + 1), log(dist_to_fire + 1)), # interaction between dist to fire & habitat covariate
                   data = ssf_data_2023,
                   method = "REML",
                   family = cox.ph, 
                   weights = obs)
tictoc::toc() #1.24hr, scaled = 42.3min, id_year = 37.7 min, log = 45min
save(fit_smooths, file = "./data/ssf/fit_smooths.rda")

# Warning message:
#   In gam.side(sm, X, tol = .Machine$double.eps^0.5) :
#   model has repeated 1-d smooths of same variable.

# this means that there are multiple s() for same covariate (x)
# s(x) and s(x, f, bs = "fs") are both seen as 1-d smooths of x. Normally that would be a big problem, but the "fs" basis is fully penalized, meaning if necessary the entire set of smooths can be shrunk out of the model. https://stats.stackexchange.com/questions/652624/warning-interpretation-when-running-a-gam-using-factor-by-smooths-and-also-using
tictoc::tic() 
fit_smooths2 <- gam(cbind(times, stratum) ~ 
                      step + log(step) + cos(angle) +
                      s(scale(elev_25m), k = 5) + 
                      s(scale(elev_25m), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                      s(log(dist_escape_25m + 1), k = 5) +  
                      s(log(dist_escape_25m + 1), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                      s(log(dist_to_fire + 1), k = 5) + 
                      s(log(dist_to_fire + 1), ID, k = 5, bs = "fs") + # factor smooth, grouped by ID
                      ti(scale(elev_25m), log(dist_escape_25m + 1)) + # interaction between elev and dist esc
                      ti(scale(elev_25m), log(dist_to_fire + 1)) + # interaction between dist to fire & habitat covariate
                      ti(log(dist_escape_25m + 1), log(dist_to_fire + 1)), # interaction between dist to fire & habitat covariate
                    data = ssf_data_2023,
                    method = "REML",
                    family = cox.ph, 
                    weights = obs)
tictoc::toc() #1.24hr, scaled = 42.3min, id_year = 37.7 min, log = 49min
beepr::beep(3)

save(fit_smooths2, file = "./data/ssf/fit_smooths2.rda")





# check the model
summary(fit_smooths)

# check aic on all the models
AICc(base_model, mod_id, fit_smooths)





save(base_model, file = "./data/ssf/base_model.rda")
save(mod_id, file = "./data/ssf/mod_id.rda")
save(fit_smooths, file = "./data/ssf/fit_smooths_fire_log.rda")

load("./data/ssf/base_model.rda")
load("./data/ssf/mod_id.rda")
load("./data/ssf/fit_smooths_fire_log.rda")
load("./data/ssf/fit_smooths2.rda")









#....................................................................................
# model check


# We can use `gratia` to visualise the smooth terms. 
# `rug = FALSE` to omit plotting the observations on the x-axis -> this shows us that there are non-linear patterns of movement and habitat selection.

gratia::draw(fit_smooths, rug = FALSE)



### Checking K
# We can also check if the basis dimension K was set sufficiently high using the function `k.check()`. 
# If the EDF are close to K, k is too low and then this could indicate that K should be set higher. 
# EDF = effective degrees of freedom, measures the complexity of the smooth, 1 = straight line, greater than 1 = non-linear, higher values = more flexibility
# edf = how many parameters (effectively) were used to represent the smooth, i.e., how wiggly the smooth is

#k.check(): Takes a fitted gam object produced by gam() and runs diagnostic tests of whether the basis dimension choices are adequate for the smooth
# Note: gam.check() gets edf for each smooth and compares it against the basis dimension k
k.check(fit_smooths)

# k.eheck() shows that the smooths for step length and turning angle may need a larger K. 
# Refit model with a higher k value if needed
# Try 20 and recheck -> see how that affects the model outputs.


# check against random slope model for model selection, which model fit better
# AIC(fit_slopes, fit_smooths)
# AIC(fit_smooths) - AIC(fit_slopes)

#....................................................................................
# Model interpretation ----

# Klappstein et al:
# Both smooth and parametric (main) effects can be interpreted in terms of relative selection strength (RSS; Avgar et al., 2017; Fieberg et al., 2021)
# exp(beta) = is how many times more likely an animal is to take a step when the covariate increases by 1 (similarly to other regression coefficients).

#..........................
## beta hat ----
# beta hat = estimate of the coefficent of the covariate from the model, aka effect size of your covariate on the relative log-odds of a step being chosen (observed vs random), conditional on the stratum
# beta hat = tells you how strongly (and in what direction) that covariate influences the relative probability of choosing the observed step vs. the random alternatives
# positive beta hat = animal is more likely to select steps with higher values of that covariate.
# negative beta hat = animal is less likely to select such steps.
# these are only relative selection and not absolute probabilities
# beta hat = estimated slope/weight of the covariate

# SSF = mathematically equivalent to a conditional logistic regression .-. usually exponentiate the coefficient to look like exp(beta hat)

# RSS = calculated as the ratio between two points where the y-axis is exponential of the partial effect
# RSS = how much the odds of choosing a step change for a 1-unit increase in the covariate, holding others constant
# RSS is always relative hence the name, and not absolute

# if
# exp(beta hat) = 2.0 -> then 1 unit increase in the covariate doubles the relative odds of choosing that step.
# exp(beta hat) = 0.5 -> then 1 unit increase in the covariate halves the relative odds.
# exp(beta hat) = 1.0  -> no effect

#example:
# beta hat elevation = 0.7
# then exp(beta hat elevation) aka exp(0.7) approx = 2.01
# which means every 1 unit increase in elevation, relative odds of animal selection that step is doubled, compared to step with lower elevation, all else being equal

# non-linear smooths (for smooth terms in the model)
# s(x1), there isnt a single beta hat, you visualize the smooth function or cmpute pointwise RSS by looking at difference between predicted smooth values

# relative selection change between two covariates is: RSS(x1 -> x2) = exp(f(x2)-f(x1))

# .estimate = “how strong the model says the effect is at that x”
# i.e., negative value = less likely to select that location
# i.e., positive value = more likely to select that location

# spline = pull the data towards the line, i.e., penalizing the wiggliness, so its not over fitting









