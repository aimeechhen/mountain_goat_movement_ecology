
# 
# load("./data/ssf/pop_smooth_fire_elev_data.rda")
# load("./data/ssf/pop_smooth_fire_dist_escape_data.rda")
# load("./data/ssf/pop_smooth_fire_dist_to_fire_data.rda")
# 
# load("./data/ssf/r_smooth_ID_fire_elev_data.rda")
# load("./data/ssf/r_smooth_ID_fire_dist_escape_data.rda")
# load("./data/ssf/r_smooth_ID_fire_dist_to_fire_data.rda")

load("./data/ssf/fit_smooths.rda")
load("./data/ssf/pop_smooth_data.rda")
load("./data/ssf/ID_smooth_data.rda")
load("./data/ssf/ssf_data_w_fire_enlarged.rda")
ssf_data_2023 <- ssf_data[ssf_data$year == "2023",]

library(ggplot2)
library(cowplot)
library(mgcv)
library(gratia)
library(scico)


# plot h smooths

#format numbers in scientific notation with 10^n
scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

## elevation ----
p3 <-
  ggplot(data = pop_smooth_data[pop_smooth_data$.smooth == "s(scale(elev_25m))",], 
         aes(x = `scale(elev_25m)`, y = exp(.estimate))) + 
  geom_ribbon(aes(ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)), 
              fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.75) + 
  scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scientific_10) +
  # xlim(c(-2.5,2.5)) +
  # ylim(c(0,150)) +
  coord_cartesian()
ggtitle("") +
  ylab(expression(exp(f(scale(elev_25m))))) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")


p4 <-
  ggplot() + 
  geom_line(data = ID_smooth_data, color = "royalblue", alpha = 0.6,
            aes(x = scale_elev, y = exp(scale_elev_est), group = ID))+
  # xlim(c(-2.5,2.5)) +
  # ylim(c(0,400)) +
  ggtitle("") +
  ylab(expression(exp(f(scale(elev_25m))))) +
  # xlab("Elevation") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

plot_grid(p3, p4)




#.........................................................................
## plot dist_escape ----




# random smooths
p3b <-
  ggplot(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_escape_25m + 1))",], 
         aes(x = `log(dist_escape_25m + 1)`, y = exp(.estimate))) + 
  geom_ribbon(aes(ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)), 
              fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.75) + 
  # ylim(c(0,7)) +
  ggtitle("") +
  ylab(expression(exp(f(log(dist_escape_25m + 1))))) +
  # xlab("Distance to escape terrain") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

p4b <-
  ggplot() + 
  geom_line(data = ID_smooth_data, 
            aes(x = log_dist_escape, y = exp(log_dist_escape_est), group = ID),
            color = "royalblue", alpha = 0.6)+
  # xlim(c(-2,2)) +
  # ylim(c(0,150)) +
  ggtitle("") +
  ylab(expression(exp(f(log(dist_escape_25m + 1))))) +
  # xlab("Distance to escape terrain") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

plot_grid(p3b, p4b)




#.........................................................................
## plot dist_to_fire ----

# random smooths
p3c <-
  ggplot(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_to_fire + 1))",], 
         aes(x = `log(dist_to_fire + 1)`, y = exp(.estimate))) + 
  geom_ribbon(aes(ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)), 
              fill = "grey70", alpha = 0.5) +
  geom_line(color = "black", linewidth = 0.75) + 
  scale_y_continuous(labels = scales::label_comma()) +
  # ylim(c(0,150)) +
  ggtitle("") +
  ylab(expression(exp(f(log(dist_to_fire + 1))))) +
  # xlab("Distance to fire") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

p4c <-
  ggplot() + 
  geom_line(data = ID_smooth_data, 
            aes(x = log_dist_to_fire, y = exp(log_dist_to_fire_est), group = ID),
            color = "royalblue", alpha = 0.6)+
  # ylim(c(0,150)) +
  ggtitle("") +
  ylab(expression(exp(f(log(dist_to_fire + 1))))) +
  # xlab("Distance to fire") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

plot_grid(p3c, p4c)





#............................
# plot all
multi_plot_ssf <- plot_grid(p3,
                            p4,
                            p3b,
                            p4b,
                            p3c, 
                            p4c, 
                            nrow = 3, ncol = 2)
multi_plot_ssf


ggsave(multi_plot_ssf, file = "figures/ssf/fire_2023/ssf_random_smooths.png",
       bg = "transparent", width = 6.86, height = 6*1.5, units = "in", dpi = 600, create.dir = TRUE)








#////////////////////////////////////////////////////////

# as a single plot ----


collar_id <- c("30548", "30561", "30575", "30613", "30642", "30648")
goats <- c("goatzilla", "selena_goatmez", "the_goatmother", "goatileo", "toats_mcgoats", "vincent_van_goat") # in collar id order
ID_smooth_data$ID <- factor(ID_smooth_data$ID, levels = collar_id)
goat_labels <- c("Goatzilla", "Selena Goatmez", "The Goatmother", "Goatileo", "Toats Mcgoats", "Vincent Van Goat") # must be in the same order as above
legend_labels <- setNames(goat_labels, goats) # create display labels for the legend
goat_palette <- c("#4477AA", "#EE6677", "#228833", "#CCBB44", "#66CCEE", "#AA3377") # colours correspond with the goat order above
# add a goat colour column and assign them the proper colour
ID_smooth_data$goat_color <- goat_palette[match(ID_smooth_data$ID, collar_id)]


ID_smooth_data$exp_scale_elev_est <- NA
ID_smooth_data$exp_scale_elev_est <- exp(ID_smooth_data$scale_elev_est)

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

scientific_10 <- function(x) {
  sapply(x, function(val) {
    if (val == 0) {
      "0"
    } else {
      parse(text = gsub("e", " %*% 10^", scales::scientific_format()(val)))
    }
  })
}

# elevation
cp <-
  ggplot() +
  # pop level
  geom_ribbon(data = pop_smooth_data[pop_smooth_data$.smooth == "s(scale(elev_25m))", ],
              aes(x = `scale(elev_25m)`, ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)),
              fill = "grey70", alpha = 0.5) +
  # population line
  geom_line(data = pop_smooth_data[pop_smooth_data$.smooth == "s(scale(elev_25m))", ],
            aes(x = `scale(elev_25m)`, y = exp(.estimate)),
            color = "black",linewidth = 1.5) +
  # individual
  geom_line(data = ID_smooth_data,
            aes(x = scale_elev, y = exp(scale_elev_est), group = ID),
            color = "royalblue",
            alpha = 0.6) +
  # scale_y_continuous(labels = scales::label_comma()) +
  # scale_y_continuous(labels = scientific_10) +
  coord_cartesian(ylim = c(0, 150)) +
  # scale_y_log10() +
  # ylim(c(0,100)) +
  xlab("Elevation (scaled)") +
  ylab(expression(exp(f(scale(elevation))))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))





# dist_escape
cp2 <- 
  ggplot() +
  # pop level
  geom_ribbon(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_escape_25m + 1))", ],
              aes(x = `log(dist_escape_25m + 1)`, ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)),
              fill = "grey70", alpha = 0.5) +
  # population line
  geom_line(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_escape_25m + 1))", ],
            aes(x = `log(dist_escape_25m + 1)`, y = exp(.estimate)),
            color = "black",linewidth = 1.25) +
  # individual
  geom_line(data = ID_smooth_data,
            aes(x = log_dist_escape, y = exp(log_dist_escape_est), group = ID),
            color = "royalblue", alpha = 0.6) +
  scale_y_continuous(labels = scales::label_comma()) +
  xlab("Dist. to esc. terrain (log-transformed)") +
  ylab(expression(exp(f(log(`dist. to esc. terrain`))))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) 


# 
# ID_smooth_data$exp_log_dist_to_fire_est <- NA
# ID_smooth_data$exp_log_dist_to_fire_est <- exp(ID_smooth_data$log_dist_to_fire_est)
# pop_smooth_data$exp_estimate <- ifelse(pop_smooth_data$.smooth == "s(log(dist_to_fire + 1))",
#                                        exp(pop_smooth_data$.estimate), NA)

# dist_to_fire
cp3 <-
   ggplot() +
  # population level 95% CI
  geom_ribbon(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_to_fire + 1))",],
              aes(x = `log(dist_to_fire + 1)`,
                  ymin = exp(.estimate - 1.96 * .se), ymax = exp(.estimate + 1.96 * .se)),
              fill = "grey70", alpha = 0.5) +
  # population line
  geom_line(data = pop_smooth_data[pop_smooth_data$.smooth == "s(log(dist_to_fire + 1))",], 
            aes(x = `log(dist_to_fire + 1)`, y = exp(.estimate)),
            color = "black", linewidth = 0.75) + 
  # individual level
  geom_line(data = ID_smooth_data, 
            aes(x = log_dist_to_fire, y = exp(log_dist_to_fire_est), group = ID),
            color = "royalblue", alpha = 0.6) +
  scale_y_continuous(labels = scales::label_comma()) +
  coord_cartesian(ylim = c(0, 150)) +
  # ylim(c(0,150)) +
  # scale_y_log10() +
  xlab("Dist. to fire (log-transformed)") +
  ggtitle("") +
  ylab(expression(exp(f(log(`dist. to fire`))))) +
  # xlab("Distance to fire") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) 







# smooth plots
# gridExtra::grid.arrange(cp, cp2, cp3, nrow = 1, widths = c(1,1,1))
# 
# plot_ssf_s <- ggpubr::ggarrange(cp, cp2, cp3, 
#                         ncol = 3, nrow = 1, 
#                         align = "v", widths = c(1, 1, 1))








#///////////////////////////////////////////////////////////////////////////////////////
# interaction plots ----
load("./data/ssf/fit_smooths.rda")
load("./data/ssf/fit_smooths_fire_log.rda")
load("./data/ssf/fit_smooths2.rda")
load("./data/ssf/fit_smooths3.rda")

# r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))"
# is the combined interaction between two covariates that influences the predicted response (i.e., the relative probability of selection ())


#................................................................................
# interaction dist_to_fire with elev

library(RColorBrewer)
# Show only diverging palettes
display.brewer.all(type = "div")
display.brewer.all()

# get smooth estimates and se from the model


# subsetting works too
r_smooths <- smooth_estimates(fit_smooths)

# find the min and max across both ti smooths for same scale legend
range_est <- range(min = r_smooths$.estimate[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))"],
                   max = r_smooths$.estimate[r_smooths$.smooth == "ti(log(dist_escape_25m + 1),log(dist_to_fire + 1))"])

# subset the data and plot based on ti()
p5 <-
  ggplot(data = r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",], 
         aes(x = `log(dist_to_fire + 1)`, y = `scale(elev_25m)`, fill = .estimate)) +
  geom_tile() +
  xlab("Dist. to fire (log-transformed)") +
  ylab("Elevation (scaled)") +
  # scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))




# subset the data and plot based on ti()
p6 <-
  ggplot(data = r_smooths[r_smooths$.smooth == "ti(log(dist_escape_25m + 1),log(dist_to_fire + 1))",], 
         aes(x = `log(dist_to_fire + 1)`, y = `log(dist_escape_25m + 1)`, fill = .estimate)) + 
  geom_tile() +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  labs(fill = "Estimate") +
  xlab("Dist. to fire (log-transformed)") +
  ylab("Dist. to esc. terrain (log-transformed)") +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA))




plot_ssf_smooths <- (cp + cp2 + cp3 + patchwork::plot_layout(nrow = 1, widths = c(1,1,1)))

# plot_ssf_ti <- plot_grid(p5, p6) 
plot_ssf_ti <- p5 + p6 + patchwork::plot_layout(nrow = 1, widths = c(1,1))
# plot_ssf_ti

# having issues with plot being the same height, patchworks works to fix
# plot_ssf <- (cp + cp2 + cp3 + patchwork::plot_layout(nrow = 1, widths = c(1,1,1))) / (p5 + p6 + patchwork::plot_layout(nrow = 1, widths = c(1,1)))
# plot_ssf
# plot_ssf <- (cp + cp2 + cp3 + patchwork::plot_layout(nrow = 1, widths = c(1,1,1))) / plot_ssf_ti
# plot_ssf
plot_ssf <- plot_grid(plot_ssf_smooths, plot_ssf_ti, 
                      nrow = 2, labels = c("A", "B")) 
plot_ssf

ggsave(plot_ssf, file = "figures/tws/ssf_results.png", bg = "transparent",
       width = 6.86*2, height = 6, units = "in", dpi = 600)





# multi-panel
# plot_ssf_ti <- ggarrange(p5, p6,
#                          ncol = 2,
#                          common.legend = TRUE,
#                          legend = "right")

plot_ssf_ti <- plot_grid(p5, p6) # decrease the space around the legend
plot_ssf_ti

# smooth plots
plot_ssf_pop <- plot_grid(p3, p3b, p3c, ncol = 3)
plot_ssf_pop
plot_ssf_ID <- plot_grid(p4, p4b, p4c, ncol = 3)
plot_ssf_ID


# combine all
plot_ssf <- plot_grid(plot_ssf_pop, 
                      plot_ssf_ID, 
                      plot_ssf_ti, 
                      nrow = 3)
plot_ssf



ggsave(plot_ssf, file = "figures/ssf/fire_2023/ssf_results.png",
       width = 6.86*2, height = 6*2, units = "in", dpi = 600)

ggsave(plot_ssf_ti, file = "figures/ssf/fire_2023/ssf_random_smooths_ti.png",
       bg = "transparent", width = 6.86, height = 6*1.5, units = "in", dpi = 600, create.dir = TRUE)










#................................................................................
# convert to regular metric values ----

# avoid using scientific notation
options(scipen = 999)
# options(scipen = 0) # allow scientific notation

# figure out the tipping point
# unscale to get meter values for elevation
mean_elev <- mean(ssf_data_2023$elev_25m, na.rm = TRUE)
sd_elev   <- sd(ssf_data_2023$elev_25m, na.rm = TRUE)

ti_elev <- r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",]
# create the column for unscaled values to go
ti_elev$elev_m <- NA 
# unscale the values back to meters
ti_elev$elev_m <- ti_elev$`scale(elev_25m)`* sd_elev + mean_elev

# create the column for unscaled values to go
ti_elev$dist_to_fire_m <- NA
# convert to meters
ti_elev$dist_to_fire_m <- exp(ti_elev$`log(dist_to_fire + 1)`) - 1
# drop distances over 10km
ti_elev <- ti_elev[ti_elev$dist_to_fire_m <= 10000, ]


# estimate values around 0, i.e. the threshold or tipping point
threshold <- subset(ti_elev, abs(.estimate) < 0.1)

# not really accurate~~~~~~
mean(threshold$elev_m)   # rough threshold elevation
range(threshold$elev_m)  # range where effect ≈ 0
#~~~~~~~~~~~~~

# estimate above/below 0
est_above <- ti_elev[ti_elev$.estimate > 0, ]
est_below <- ti_elev[ti_elev$.estimate < 0, ]

# and what % of points are above/below
n_above <- nrow(ti_elev[ti_elev$.estimate > 0, ])
n_below <- nrow(ti_elev[ti_elev$.estimate < 0, ])
n_total <- nrow(ti_elev)

round((n_above/n_total)*100, 2)
round((n_below/n_total)*100, 2)














# plot elev vs dist escape
# used full model + additional interaction term
ti_smooths <- smooth_estimates(fit_smooths2, select = "ti(scale(elev_25m),log(dist_escape_25m + 1))") 

t1 <-
  ggplot(data = ti_smooths, 
         aes(x = `scale(elev_25m)`, y = `log(dist_escape_25m + 1)`, fill = .estimate)) +
  geom_tile() +
  # scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




#////////////////////////////////////////////////////////////////////////////////////
# different axes

load("./data/ssf/ssf_data_w_fire_enlarged.rda")
ssf_data_2023 <- ssf_data[ssf_data$year == "2023",]



# unscale to get meter values for elevation
mean_elev <- mean(ssf_data_2023$elev_25m, na.rm = TRUE)
sd_elev   <- sd(ssf_data_2023$elev_25m, na.rm = TRUE)

# create the column for unscaled values to go
r_smooths$elev_m <- NA 
# unscale the values back to meters
r_smooths$elev_m[r_smooths$.smooth == "s(scale(elev_25m))"] <- r_smooths$`scale(elev_25m)`[r_smooths$.smooth == "s(scale(elev_25m))"] * sd_elev + mean_elev

ggplot(data = r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",], 
       aes(x = `log(dist_to_fire + 1)`, y = `scale(elev_25m)`, fill = .estimate)) +
  geom_tile() +
  # scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# different axes
# original aka p5
t1 <-
  ggplot(data = r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",], 
         aes(x = `log(dist_to_fire + 1)`, y = `scale(elev_25m)`, fill = .estimate)) +
  geom_tile() +
  # scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# metric values on x axes
n_breaks <- 10

t2 <-
  ggplot(data = r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",], 
         aes(x = `log(dist_to_fire + 1)`, y = `scale(elev_25m)`, fill = .estimate)) +
  geom_tile() +
  scale_x_continuous(breaks = pretty(r_smooths$`log(dist_to_fire + 1)`[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))"], 
                                     n = n_breaks),
                     labels = function(x) round(exp(x) - 1)) + # back-transform labels
  scale_y_continuous(breaks = pretty(r_smooths$`scale(elev_25m)`[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))"], 
                                     n = n_breaks),
                     labels = function(y) round(y * sd_elev + mean_elev)) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# metric values below 15,000m (15km) on x axes
# set the limit
axes_limit <- log(5000 + 1)

# ti_elev <- r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))",]
# # create the column for unscaled values to go
# ti_elev$dist_to_fire_m <- NA 
# # convert to meters
# ti_elev$dist_to_fire_m <- exp(ti_elev$`log(dist_to_fire + 1)`) - 1
# # subset data under the limit
# ti_elev_sub <-  ti_elev[ti_elev$`log(dist_to_fire + 1)` < axes_limit,]
# ti_dist_escape <- r_smooths[r_smooths$.smooth == "ti(log(dist_escape_25m + 1),log(dist_to_fire + 1))",]
# ti_dist_escape$dist_escape_m <- NA 
# ti_dist_escape$dist_to_fire_m <- NA 
# ti_dist_escape$dist_escape_m <- exp(ti_dist_escape$`log(dist_escape_25m + 1)`) - 1
# ti_dist_escape$dist_to_fire_m <- exp(ti_dist_escape$`log(dist_to_fire + 1)`) - 1
# ti_dist_escape_sub <-  ti_dist_escape[ti_dist_escape$`log(dist_to_fire + 1)` < axes_limit,]


#note: trying to evenly space the axes breaks wont work because of the data
t3 <-
  ggplot(data = r_smooths[r_smooths$.smooth == "ti(scale(elev_25m),log(dist_to_fire + 1))" & r_smooths$`log(dist_to_fire + 1)` < axes_limit,], 
         aes(x = `log(dist_to_fire + 1)`, y = `scale(elev_25m)`, fill = .estimate)) +
  geom_tile() +
  scale_x_continuous(breaks = pretty(r_smooths$`log(dist_to_fire + 1)`[r_smooths$.smooth == "ti(`scale(elev_25m)`,log(dist_to_fire + 1))" & 
                                                                         r_smooths$`log(dist_to_fire + 1)` < axes_limit], 
                                     n = n_breaks),   # auto breaks on log scale
                     labels = function(x) round(exp(x) - 1)) + # back-transform labels
  scale_y_continuous(breaks = pretty( r_smooths$`scale(elev_25m)`[r_smooths$.smooth == "ti(`scale(elev_25m)`,log(dist_to_fire + 1))" &
                                                                    r_smooths$`log(dist_to_fire + 1)` < axes_limit ], 
                                      n = n_breaks),
                     labels = function(y) round(y * sd_elev + mean_elev)) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_grid(t1, t2, t3,ncol = 3)





#REVIEW MEETING RECORDING
# what does this mean biologically?

# d to fire & elevation

# close to fire not selecting for elevation, i.e. effect of elev not strong (very low - #)?
# far from fire, selecting for elev, i.e. effect is strong (high + #)?
# there is this region that they are selecting for based on the how close the fire is

# what does this mean biologically??
# when the fire is close, they tend to be selecting for higher elevation
# and up to a certain distance, then it drops off
# so biologically, they are going to be in higher elev in general til there is a certain distance between them and the fire
# potentially trapping themselves?
# its not metric, its that elevtion has a stronger effect right? based on the axis scale??

# and when the fire is far from them, then effect of elevation is weak




# d to fire & d to esc








library(ggplot2)
library(scico)



# full model + ti for elev/dist_escape


smooth_est <- smooth_estimates(fit_smooths3,
                               select = "ti(`scale(elev_25m)`,log(dist_escape_25m + 1))")
head(smooth_est)

ggplot(smooth_est, aes(x = `scale(elev_25m)` , y = `log(dist_escape_25m + 1)`, fill = .estimate)) +
  geom_tile() +  # works with unevenly spaced grids
  scale_fill_scico(palette = 'vik', midpoint = 0, direction = -1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())










#///////////////////////////////////////////////////
# dist escape NEED TO UPDATE

p6 <-
  ggplot(data = r_smooths_ti_dist_escape, aes(x = `log(dist_to_fire + 1)`, y = `log(dist_escape_25m + 1)`, fill = .estimate)) + 
  # ggplot(data = r_smooths_ti_dist_escape, aes(x = dist_to_fire_original, y = `log(dist_escape_25m + 1)`, fill = fill = exp(.estimate))) + 
  # geom_raster() 
  geom_tile() +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  # scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())






# axes labels backtransformed for meter metrics
p6 <-
  ggplot(r_smooths_ti_dist_escape, aes(x = `log(dist_to_fire + 1)`, y = `log(dist_escape_25m + 1)`, fill = .estimate)) +
  geom_tile() +
  scale_x_continuous(breaks = pretty(r_smooths_ti_dist_escape$`log(dist_to_fire + 1)`, 10),
                     labels = function(x) round(exp(x) - 1)  ) +
  scale_y_continuous(breaks = pretty(r_smooths_ti_dist_escape$`log(dist_escape_25m + 1)`, 10),
                     labels = function(y) round(exp(y) - 1)) +
  scale_fill_scico(palette = "vik", midpoint = 0, direction = -1, limits = range_est, breaks = seq(from = -10, to = 4, by = 2)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())














