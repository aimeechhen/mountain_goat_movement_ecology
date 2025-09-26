

load("./data/ssf/fit_smooths_fire_log.rda")

# extract fitted smooth effects from model

#.........................................................................
# plot data ----

# plot configuration
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # ordered by collar_id
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours
year_palette <- c("#b3b3b3", "#b3b3b3","#b3b3b3", "#b3b3b3", "#ae2012", "#b3b3b3") 
data_years <- as.factor(c('2019', '2020', '2021', '2022', '2023', '2024'))




#...........................................................
## random smooths effect ----
#...........................................................

# get predicted effects -> extract model's estimate effect on the response (y)

# create a plotting df of fitted smooth values from the model
# get smooth estimates and se from the model
smooth_est <- smooth_estimates(fit_smooths, dist = 0.1)
head(smooth_est)
unique(smooth_est$.type) #"TPRS"                "Factor smooth"       "Tensor product int."
# TPRS = thin plate regression splines aka the default smooth
names(smooth_est)
# .smooth = name of the smooth term
# .estimate = fitted smooth effect -> how the model says the response changes with that covariate i.e., the predicted smooth value at that covariate
# .se = standard error of the fitted effect

# .estimate = “how strong the model says the effect is at that x” i.e, the expected effect of the variable on the response (y)
# .estimate tells you how the model says the response changes as the covariate changes
# also its what the model determined based on the data
# so at some covariate value, the expected effect on the response is x

# when you add a random smooth -> you get the individual fitted effect

goats <- unique(ssf_data_2023$ID)

#..............................
# population-level

# extract the smooth estimates for each covariate (using first 100 points for each) for population level
# Note: the smooth term (does not contain ID)
# this is your grid of covariate values -> the 100 points, and getting the smooth effect value
# i.e., creating a grid of covariate values and filling it with the fitted effect (predicted selection strength, i.e. the .estimate) estimated by the model
pop_smooth_elev <- r_smooths[r_smooths$.smooth == "s(elev_25m_scaled)" & r_smooths$.type == "TPRS", ][c(1:100),]
# pop_smooth_dist_escape <- r_smooths[r_smooths$.smooth == "s(log(dist_escape_25m + 1))" & r_smooths$.type == "TPRS", ][c(1:100),]
# pop_smooth_dist_to_fire <- r_smooths[r_smooths$.smooth == "s(log(dist_to_fire + 1))" & r_smooths$.type == "TPRS", ][c(1:100),]
pop_smooth_dist_escape <- r_smooths[r_smooths$.smooth == "s(log_dist_escape_25m)" & r_smooths$.type == "TPRS", ][c(1:100),]
pop_smooth_dist_to_fire <- r_smooths[r_smooths$.smooth == "s(log_dist_to_fire)" & r_smooths$.type == "TPRS", ][c(1:100),]
# pop_smooth_ti_fire_elev <- r_smooths[r_smooths$.smooth == "ti(elev_25m_scaled,log(dist_to_fire + 1))" & r_smooths$.type == "Tensor product int.", ][c(1:100),]
# pop_smooth_ti_fire_dist_escape <- r_smooths[r_smooths$.smooth == "ti(log(dist_escape_25m + 1),log(dist_to_fire + 1))" & r_smooths$.type == "Tensor product int.", ][c(1:100),]
pop_smooth_ti_fire_elev <- r_smooths[r_smooths$.smooth == "ti(elev_25m_scaled,log_dist_to_fire)" & r_smooths$.type == "Tensor product int.", ][c(1:100),]
pop_smooth_ti_fire_dist_escape <- r_smooths[r_smooths$.smooth == "ti(log_dist_escape_25m, log_dist_to_fire)" & r_smooths$.type == "Tensor product int.", ][c(1:100),]

pop_smooth_data <- rbind(pop_smooth_elev, pop_smooth_dist_escape)
pop_smooth_data <- rbind(pop_smooth_data, pop_smooth_dist_to_fire)
pop_smooth_data <- rbind(pop_smooth_data, pop_smooth_ti_fire_elev)
pop_smooth_data <- rbind(pop_smooth_data, pop_smooth_ti_fire_dist_escape)

save(pop_smooth_data, file = "./data/ssf/pop_smooth_data.rda")
load("./data/ssf/pop_smooth_data.rda")

#..............................
# individual-level i.e. individual random smooths, the individual random smooth deviation

# combine the population smooth (population level trend) with individual random smooth deviation from the population level to get the fitted effect of each individual

# extract the smooth estimates for each covariate (using first 100 points for each) for individual level, to show individual-level deviation
r_smooth_ID_elev_data <- NULL
r_smooth_ID_dist_escape_data <- NULL
r_smooth_ID_dist_to_fire_data <- NULL

for (i in 1:length(goats)) {
  # extract the random smooth estimate value for an individual (i.e., the individual deviation from the population smooth)
  smooth_sub_elev <- subset(r_smooths, .smooth == "s(elev_25m_scaled,ID)" & ID == goats[i])
  smooth_sub_elev <- data.frame(ID =  goats[i], 
                                # individual covariate estimated value
                                elev = smooth_sub_elev$elev_25m_scaled,
                                # covariate total predicted smooth for this individual = population level smooth estimate + individual random smooth estimate (deviation, random effect), i.e., the model's prediction for the individual after accounting for both fixed and random effects 
                                # est (total fitted effect) = population smooth + individual deviation
                                elev_est = pop_smooth_elev$.estimate + smooth_sub_elev$.estimate)
  
  smooth_sub_dist_escape <- subset(r_smooths, .smooth == "s(log(dist_escape_25m + 1),ID)" & ID ==  goats[i])
  smooth_sub_dist_escape <- data.frame(ID =  goats[i], 
                                       dist_escape = smooth_sub_dist_escape$`log(dist_escape_25m + 1)`,
                                       dist_escape_est = pop_smooth_dist_escape$.estimate + smooth_sub_dist_escape$.estimate)
  
  smooth_sub_dist_to_fire <- subset(r_smooths, .smooth == "s(log(dist_to_fire + 1),ID)" & ID ==  goats[i])
  smooth_sub_dist_to_fire <- data.frame(ID =  goats[i], 
                                        dist_to_fire = smooth_sub_dist_to_fire$`log(dist_to_fire + 1)`,
                                        dist_to_fire_est = pop_smooth_dist_to_fire$.estimate + smooth_sub_dist_to_fire$.estimate)
  
  # add to the df
  r_smooth_ID_elev_data <- rbind(r_smooth_ID_elev_data, smooth_sub_elev)
  r_smooth_ID_dist_escape_data <- rbind(r_smooth_ID_dist_escape_data, smooth_sub_dist_escape)
  r_smooth_ID_dist_to_fire_data <- rbind(r_smooth_ID_dist_to_fire_data, smooth_sub_dist_to_fire)
}


# str(r_smooth_ID)
# head(r_smooth_ID)

head(r_smooth_ID_elev_data)



save(pop_smooth_elev, file = "./data/ssf/pop_smooth_fire_elev_data.rda")
save(pop_smooth_dist_escape, file = "./data/ssf/pop_smooth_fire_dist_escape_data.rda")
save(pop_smooth_dist_to_fire, file = "./data/ssf/pop_smooth_fire_dist_to_fire_data.rda")
load("./data/ssf/pop_smooth_fire_elev_data.rda")
load("./data/ssf/pop_smooth_fire_dist_escape_data.rda")
load("./data/ssf/pop_smooth_fire_dist_to_fire_data.rda")


save(r_smooth_ID_elev_data, file = "./data/ssf/r_smooth_ID_fire_elev_data.rda")
save(r_smooth_ID_dist_escape_data, file = "./data/ssf/r_smooth_ID_fire_dist_escape_data.rda")
save(r_smooth_ID_dist_to_fire_data, file = "./data/ssf/r_smooth_ID_fire_dist_to_fire_data.rda")
load("./data/ssf/r_smooth_ID_fire_elev_data.rda")
load("./data/ssf/r_smooth_ID_fire_dist_escape_data.rda")
load("./data/ssf/r_smooth_ID_fire_dist_to_fire_data.rda")






