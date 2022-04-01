# Create directory for results
if (!dir.exists(paste0(out_dir, "trajectories"))){ dir.create(paste0(out_dir, "trajectories")) }

# Split data by file for each parent
parents = tot[tot$eventualChildlessness == "Parent", ]
splits = split(parents, parents$ID)
splits = splits[unlist(lapply(splits, nrow)) > 0]

# Function to compute time before-after birth
compute_time_before_after_birth = function(x){
  x['sweep_to_childbirth'] = NA
  child_age = x$Age[which(x$childlessNatural == 0)[1]]
  x$sweep_to_childbirth = (x$Age - child_age) / 4
  return(x)
}

# Compute years to childbirth
par = rbind.fill(lapply(splits, compute_time_before_after_birth))
par['years_to_childbirth'] = par$sweep_to_childbirth * 4
par['Sex'] = ifelse(par$Sex == "Female", "Mothers", "Fathers")

# Plot: life satisfaction faceted by gender
g = ggplot() +
  facet_wrap(~ Sex) +
  # geom_vline(xintercept = -4, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_rect(data = data.frame(xmin = -4, xmax = 0, ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  labs(x = 'Years to First Sweep where Respondent Reported to be a Parent', y = 'Life Satisfaction') +
  theme_flo()
g1 = g + geom_smooth(data = par, aes(x = years_to_childbirth, y = lifeSatisfaction), color = 'red', fill = 'red', method = "lm", formula = y ~ poly(x, 3))
g2 = g + geom_smooth(data = par, aes(x = years_to_childbirth, y = lifeSatisfaction), color = 'red', fill = 'red', method = "loess")
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_lifesat_cube.png'), g1)  
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_lifesat_loess.png'), g2)  

# Plot: income faceted by gender
g = ggplot() +
  facet_wrap(~ Sex) +
  # geom_vline(xintercept = -4, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_rect(data = data.frame(xmin = -4, xmax = 0, ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  labs(x = 'Years to First Sweep where Respondent Reported to be a Parent', y = 'log(1 + Inflation-Corrected Monthly Gross Pay)') +
  theme_flo()
g1 = g + geom_smooth(data = par, aes(x = years_to_childbirth, y = logCorrectedMonthlyGrossPay), color = 'red', fill = 'red', method = "lm", formula = y ~ poly(x, 3)) 
g2 = g + geom_smooth(data = par, aes(x = years_to_childbirth, y = logCorrectedMonthlyGrossPay), color = 'red', fill = 'red', method = "loess") 
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_income_cube.png'), g1)  
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_income_loess.png'), g2)  

# Plot: trajectories faceted by variable and colored by gender
plottot = par[, c("years_to_childbirth", "Sex", "logCorrectedMonthlyGrossPay", "lifeSatisfaction")]
names(plottot) = c("years_to_childbirth", "Sex", "log(1 + Inflation-Corrected Monthly Gross Pay)", "Life Satisfaction")
plottot = reshape2::melt(plottot, id = c('years_to_childbirth', 'Sex'))  
g = ggplot() +
  facet_wrap(~ variable) +
  # geom_vline(xintercept = -4, linetype = "dashed", color = "grey") +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_rect(data = data.frame(xmin = -4, xmax = 0, ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey", alpha = 0.5) +
  labs(x = 'Years to First Sweep where Respondent Reported to be a Parent', y = NULL, color = NULL, fill = NULL) +
  theme_flo() +
  scale_color_manual(values = c("red", "green"))
g1 = g + geom_smooth(data = plottot, aes(x = years_to_childbirth, y = value, color = Sex, fill = Sex), method = "loess") 
g2 = g + geom_smooth(data = plottot, aes(x = years_to_childbirth, y = value, color = Sex, fill = Sex), method = "lm", formula = y ~ poly(x, 3)) 
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_lifesat_income_loess.png'), g1)  
ggsave(paste0(out_dir, 'trajectories/trajectory_before_after_birth_lifesat_income_cube.png'), g2)  

