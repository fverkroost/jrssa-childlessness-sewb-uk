swb_nam = "Life Satisfaction"
ewb_nam = "log(1 + Inflation-Corrected Monthly Gross Pay)"
childless_types = c("Parent", "Voluntarily childless", "Circumstantially childless", "Involuntarily childless",  "Unknown childless")

# Directory to output results to
if (!exists("out_dir")){
  out_dir = "model_outputs_testing/"
  if (!dir.exists("model_outputs_testing/")){
    dir.create("model_outputs_testing")
  }
}

# Source the functions
source('functions-results.R')

#-----------------------------------------------------------------------
# Result 1: Trajectories plots for each model
#-----------------------------------------------------------------------

# Define the models
models = c(list.files(path = "results/informative", pattern = "*.Rds", recursive = TRUE, full.names = TRUE),
           list.files(path = "results/uninformative", pattern = "*.Rds", recursive = TRUE, full.names = TRUE),
           list.files(path = "results/automatic", pattern = "*.Rds", recursive = TRUE, full.names = TRUE))
fits = unlist(lapply(str_split(models, "/"), tail, n = 1))
submodels = vector()
for (i in 1:length(models)){
  submodels[i] = gsub(paste0("/", fits[i]), "", models[i])
}
uni_models = unique(submodels)
uni_names = unique(unlist(lapply(str_split(submodels, "/"), tail, n = 1)))
no_conv = vector()
for (i in 1:length(uni_models)){
  
  # Load fit and check convergence
  fit_women_ewb = readRDS(paste0(uni_models[i], "/fit_women_ewb.Rds"))
  fit_men_ewb = readRDS(paste0(uni_models[i], "/fit_men_ewb.Rds"))
  if (uni_names[i] != "controls_no_econact"){
    fit_women_swb = readRDS(paste0(uni_models[i], "/fit_women_swb.Rds"))
    fit_men_swb = readRDS(paste0(uni_models[i], "/fit_men_swb.Rds"))
  } else {
    fit_women_swb = readRDS("results/informative/controls/fit_women_swb.Rds")
    fit_men_swb = readRDS("results/informative/controls/fit_men_swb.Rds")
  }
  fits_all = list(
    swb = list(women = fit_women_swb, men = fit_men_swb), 
    ewb = list(women = fit_women_ewb, men = fit_men_ewb))
  rm(fit_women_swb, fit_women_ewb, fit_men_swb, fit_men_ewb)
  
  # Obtain predictions
  preddf = data.frame()
  for (j in 1:length(fits_all)){
    
    for (z in 1:length(fits_all[[j]])){
      
      # Check convergence of fit
      my_fit = fits_all[[j]][[z]]
      if (!all(my_fit$rhats >= 0.99 & my_fit$rhats <= 1.1)){ 
        no_conv = c(no_conv, paste(c(i, j, z), collapse = " & "))
        # stop('Error: Model has not converged!') 
      }
      imp_data = read.csv(paste0("imputed_data/", names(fits_all[[j]])[z], "_imputed_data_1.csv"))
      
      # Compute predictions and combine
      pred = cbind(
        imp_data,
        Sex = ifelse(names(fits_all[[j]])[z] == "women", "Female", "Male"), 
        Dependent = ifelse(names(fits_all)[j] == "swb", swb_nam, ewb_nam), 
        predict(my_fit, my_fit$data)
      )
      
      preddf = rbind(preddf, pred)
      
    }
    
  }
  
  if (!dir.exists(paste0(out_dir, "trajectories/"))){ dir.create(paste0(out_dir, "trajectories/")) }
  
  # Make prediction plots 
  preddf$eventualTypeChildless = factor(
    preddf$eventualTypeChildless, 
    levels = childless_types
  )
  dep_vars = sort(unique(preddf$Dependent))
  for (u in 1:length(dep_vars)){
    g = ggplot() +
      geom_smooth(data = subset(preddf, Dependent == dep_vars[u]), method = "lm", formula = y ~ poly(x, 3),
                  aes(x = jitter(Age), y = Estimate, color = eventualTypeChildless, fill = eventualTypeChildless, linetype = eventualTypeChildless)) +
      labs(x = "Age", y = ifelse(dep_vars[u] == "Life Satisfaction", swb_nam, ewb_nam), color = NULL, fill = NULL, linetype = NULL) +
      facet_grid(~ Sex) +
      theme_flo() +
      guides(fill = FALSE, color = FALSE, linetype = FALSE) +
      scale_x_continuous(breaks = unique(preddf$Age))
    ggsave(paste0(out_dir, "trajectories/", uni_names[i], "_", ifelse(dep_vars[u] == "Life Satisfaction", "SWB", "EWB"), ".png"), 
           g, dpi = 600, height = 10, width = 15)
    g = g +
      facet_grid(Sex ~ eventualTypeChildless, labeller = labeller(eventualTypeChildless = label_wrap_gen(10)))
    ggsave(paste0(out_dir, "trajectories/", uni_names[i], "_", ifelse(dep_vars[u] == "Life Satisfaction", "SWB", "EWB"), "_facets.png"), 
           g, dpi = 600, height = 6, width = 10)
  }
  
}
print(no_conv)

#-----------------------------------------------------------------------
# Result 2: Regression coefficients tables for all models in one table
#-----------------------------------------------------------------------

model_types = c("uncontrols", "child", "otherdep", "marital", "education", "econact", "health", "controls")
folders = paste0("results/informative/", model_types)
women_models_swb = women_models_ewb = men_models_swb = men_models_ewb = list()
for (i in 1:length(folders)){
  women_models_swb[[i]] = readRDS(paste0(folders[i], "/fit_women_swb.Rds"))
  women_models_ewb[[i]] = readRDS(paste0(folders[i], "/fit_women_ewb.Rds"))
  men_models_swb[[i]] = readRDS(paste0(folders[i], "/fit_men_swb.Rds"))
  men_models_ewb[[i]] = readRDS(paste0(folders[i], "/fit_men_ewb.Rds"))
}
names(women_models_swb) = names(women_models_ewb) = names(men_models_swb) = names(men_models_ewb) = model_types
model_list = list(
  women = list(swb = women_models_swb, ewb = women_models_ewb),
  men = list(swb = men_models_swb, ewb = men_models_ewb)
)
for (i in 1:length(model_list)){
  
  mods_one_dep = list()
  for (j in 1:length(model_list[[i]])){
    
    this_col = list()
    for (z in 1:length(model_list[[i]][[j]])){
      
      # Obtain fixed effects
      fe = fixef(model_list[[i]][[j]][[z]])
      re = summary(model_list[[i]][[j]][[z]])$random$ID
      fe_coef = paste0(trimws(format(round(fe[, 1], 3), nsmall = 3), "l"), ifelse(fe[, 3] <= 0 & fe[, 4] >= 0, "", "$^{*}$"))
      fe_conf = paste0("(", trimws(format(round(fe[, 3], 3), nsmall = 3), "l"), "; ", format(round(fe[, 4], 3), nsmall = 3), ")")
      fe_tot = vector()
      for (u in 1:nrow(fe)){ fe_tot = c(fe_tot, fe_coef[u], fe_conf[u]) }
      
      # Obtain random effects
      re_coef = paste0(trimws(format(round(re[, 1], 3), nsmall = 3), "l"), ifelse(re[, 3] <= 0 & re[, 4] >= 0, "", "$^{*}$"))
      re_conf = paste0("(", trimws(format(round(re[, 3], 3), nsmall = 3), "l"), "; ", format(round(re[, 4], 3), nsmall = 3), ")")
      re_tot = vector()
      for (u in 1:nrow(re)){ re_tot = c(re_tot, re_coef[u], re_conf[u]) }
      
      # Obtain information criteria
      my_loo = round(LOO(model_list[[i]][[j]][[z]])$estimates["looic", 1], 3)
      my_waic = round(WAIC(model_list[[i]][[j]][[z]])$estimates["waic", 1], 3)
      
      # Combine into data column
      this_col[[z]] = data.frame(
        x1 = paste0(c(paste0(rep(rownames(fe), each = 2), c("_coef", "_conf")),
                      paste0(rep(rownames(re), each = 2), c("_coef", "_conf")), "LOOIC", "WAIC"), "_", names(model_list[[i]])[j]), 
        x2 = c(fe_tot, re_tot, my_loo, my_waic)
      )
      colnames(this_col[[z]]) = c("Coefficient", model_types[z])
      
    }
    
    mods_one_dep[[j]] = Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Coefficient", all = TRUE), this_col)
    
  }
  
  # Combine all results into one table and save to file
  final_table = rbind.fill(mods_one_dep)
  final_table$Coefficient[grepl("_conf", final_table$Coefficient)] = ""
  final_table$Coefficient = gsub("_coef", "", final_table$Coefficient)
  output = print(xtable(final_table, digits = rep(3, ncol(final_table) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
  if (!dir.exists(paste0(out_dir, "regression_tables/"))){ dir.create(paste0(out_dir, "regression_tables/")) }
  fileConn = file(paste0(out_dir, "regression_tables/coefficients_all_models_", names(model_list)[i], ".txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}

#-----------------------------------------------------------------------
# Result 3: Marginal effect of childlessness throughout models
#-----------------------------------------------------------------------

folder_names = c("No control variables", "Child timing and quantum", "otherdep", "Marital status and cohabitation", 
                 "Education", "Economic activity", "Health status", "Fully controlled")
marg_list = list()
for (i in 1:length(folders)){
  
  # Obtain marginal effects
  marg_list[[i]] = list()
  for (j in 1:length(model_list)){
    
    marg_swb = marginal_effects(model_list[[j]]$swb[[i]], "eventualTypeChildless", plot = FALSE)[[1]]
    marg_ewb = marginal_effects(model_list[[j]]$ewb[[i]], "eventualTypeChildless", plot = FALSE)[[1]]
    names(marg_swb)[names(marg_swb) == "lifeSatisfaction"] = "y"
    names(marg_swb)[names(marg_swb) == "logCorrectedMonthlyGrossPay"] = "x"
    names(marg_ewb)[names(marg_ewb) == "logCorrectedMonthlyGrossPay"] = "y"
    names(marg_ewb)[names(marg_ewb) == "lifeSatisfaction"] = "x"
    marg_swb["Model"] = ifelse(folder_names[i] == "otherdep", ewb_nam, folder_names[i])
    marg_ewb["Model"] = ifelse(folder_names[i] == "otherdep", swb_nam, folder_names[i])
    marg_list[[i]][[j]] = rbind(marg_swb, marg_ewb)
    marg_list[[i]][[j]]["Dependent"] = rep(c(swb_nam, ewb_nam), c(nrow(marg_swb), nrow(marg_ewb)))
    marg_list[[i]][[j]]["Sex"] = ifelse(names(model_list)[j] == "women", "Female", "Male")
    
  }
  
  rows = unlist(lapply(marg_list[[i]], nrow))
  marg_list[[i]] = rbind.fill(marg_list[[i]])
  
}

# Combine marginal effects into one data set
margdf = rbind.fill(marg_list)
margdf$Model = factor(margdf$Model, levels = c("No control variables", "Child timing and quantum", 
                                               swb_nam, ewb_nam, "Marital status and cohabitation", 
                                               "Education", "Economic activity", "Health status", "Fully controlled"))
margdf$eventualTypeChildless = factor(margdf$eventualTypeChildless, levels = childless_types)
dep_vars = sort(unique(margdf$Dependent))
margdf_plot = margdf
margdf_plot$Model = as.character(margdf_plot$Model)
margdf_plot$Model[margdf_plot$Model == ewb_nam] = "log(1 + IC Monthly Gross Pay)"
margdf_plot$Model[margdf_plot$Model == "Child timing and quantum"] = "Child timing & quantum"
margdf_plot$Model[margdf_plot$Model == "Marital status and cohabitation"] = "Marital status & cohabitation"
margdf_plot$Model = factor(margdf_plot$Model, levels = c("No control variables", "Child timing & quantum", swb_nam, "log(1 + IC Monthly Gross Pay)", 
                                                         "Marital status & cohabitation", "Education", "Economic activity", "Health status", "Fully controlled"))

# Plot marginal effects
if (!dir.exists(paste0(out_dir, "marginals/"))){ dir.create(paste0(out_dir, "marginals/")) }
for (i in 1:length(dep_vars)){
  g = ggplot() + 
    geom_point(data = subset(margdf_plot, Dependent == dep_vars[i]), size = 4,
               aes(x = eventualTypeChildless, color = eventualTypeChildless, shape = eventualTypeChildless, y = estimate__)) +
    geom_errorbar(aes(x = eventualTypeChildless, color = eventualTypeChildless, ymin = lower__, ymax = upper__), 
                  data = subset(margdf_plot, Dependent == dep_vars[i]), width = .75, size = 1) +
    facet_grid(Sex ~ Model, labeller = label_wrap_gen(11)) +
    labs(x = NULL, y = dep_vars[i], color = NULL, shape = NULL) + 
    theme_flo() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  ggsave(paste0(out_dir, "marginals/model_marginals_", ifelse(i == 1, "swb", "ewb"), ".png"), g, width = 10, height = 6.67)
}

#-----------------------------------------------------------------------
# Result 4: Predicted and observed distributions of SWB and EWB
#-----------------------------------------------------------------------

# Obtain distributions of outcome variables
control_mods = lapply(model_list, function(x){ lapply(x, function(y){ y$controls })})
pp = list()
y_labs = c(swb_nam, ewb_nam)
for (i in 1:length(control_mods)){
  
  bw = c(0.75, 1)
  pp[[i]] = list()
  for (j in 1:length(control_mods[[i]])){
    
    mypp = brms::pp_check(control_mods[[i]][[j]], plot = FALSE)
    real = mypp$data[mypp$data$is_y == TRUE, ]
    repl = mypp$data[mypp$data$is_y == FALSE, ]
    real["Type"] = rep("Actual", nrow(real))
    repl["Type"] = rep("Predicted", nrow(repl))
    pp[[i]][[j]] = rbind(real, repl)
    pp[[i]][[j]]["Dependent"] = ifelse(names(control_mods[[i]])[j] == "swb", swb_nam, ewb_nam)
    pp[[i]][[j]]["Sex"] = ifelse(names(control_mods)[i] == "women", "Female", "Male")
    
  }
  
  pp[[i]] = rbind.fill(pp[[i]])
  
}

# Combine data for both outcome variables and plot
pp = rbind.fill(pp)
dep_vars = sort(unique(pp$Dependent))
if (!dir.exists(paste0(out_dir, "pp_check/"))){ dir.create(paste0(out_dir, "pp_check/")) }
for (i in 1:length(dep_vars)){
  subpp = pp[pp$Dependent == dep_vars[i], ]
  g = ggplot() +
    facet_grid(~ Sex) +
    labs(x = dep_vars[i], y = "Proportion", fill = NULL, color = NULL) +
    theme_flo()
  if (dep_vars[i] == "Life Satisfaction"){
    g = g + geom_histogram(data = subpp, aes(x = value, y = ..density.., fill = Type), position = "identity", binwidth = 1, alpha = .5)
  } else {
    g = g + geom_density(data = subpp, aes(x = value, fill = Type, color = Type), alpha = .5)
  }
  ggsave(paste0(out_dir, "pp_check/pp_", ifelse(dep_vars[i] == swb_nam, "swb", "ewb"), ".png"), g)
}

#-----------------------------------------------------------------------
# Result 5: Multinomial model plotted predicted probabilities
#-----------------------------------------------------------------------

# Define model and name information
newdf = data.frame(
  Sex = rep(c("Female", "Male"), 5),
  eventualTypeChildless = rep(c("Parent", "Voluntarily childless", "Unknown childless", "Involuntarily childless", "Circumstantially childless"), each = 2)
)
multinom_vars = c("cohabitation", "maritalStatus_combined", "education_binary_bwimp", "economicActivity", "healthStatus_binary")
multinom_nams = c("Cohabitation", "Marital Status", "Education", "Economic Activity", "Health Status")
multinom_models = lapply(paste0("results/multinomial/", multinom_vars, ".Rds"), readRDS)
matchdf = data.frame(org = c("Cohabiting", "Noncohabiting", "Divorcedseparatedwidowed", "Remarried", "Singleandnevermarried",   
                             "NVQ3", "NVQ4","Fulltimepaidemployee", "Outofthelabourforce", "Parttimepaidemployee", "Bad", "Good"),
                     new = c("Cohabiting", "Non-cohabiting", "Divorced/widowed", "(Re)married", "Never married",   
                             "NVQ 3-", "NVQ 4+", "Full-time", "Out of labour force", "Part-time", "Fair", "Good"))

# Produce a plot for each control variable
plotlist = list()
for (i in 1:length(multinom_models)){
  
  pred = fitted(multinom_models[[i]], newdata = newdf, summary = TRUE)
  
  if (class(pred) == "array"){
    
    cats = substr(gsub(")", "", dimnames(pred)[[3]]), 7, nchar(dimnames(pred)[[3]]))
    predlist = list()
    for (j in 1:dim(pred)[3]){
      predlist[[j]] = cbind(newdf, as.data.frame(pred[,, j]), Category = matchdf$new[match(cats[j], matchdf$org)])
    }
    pred = plyr::rbind.fill(predlist)
    
  } else if (class(pred) == "matrix"){
    
    pred = cbind(newdf, pred)
    pred["Category"] = matchdf$new[match(levels(multinom_models[[i]]$data[, 1])[-1], matchdf$org)]
    pred_other = pred
    pred_other$Estimate = 1 - pred$Estimate
    pred_other$Q2.5 = pred_other$Estimate - (pred$Estimate - pred$Q2.5)
    pred_other$Q97.5 = pred_other$Estimate + (pred$Q97.5 - pred$Estimate)
    pred_other$Category = matchdf$new[match(levels(multinom_models[[i]]$data[, 1])[1], matchdf$org)]
    pred = rbind(pred, pred_other)
    
  } else {
    break
  }
  
  pred$eventualTypeChildless = factor(pred$eventualTypeChildless, levels = childless_types)
  my_ticks = c(round(min(pred$Q2.5), 1), round(min(pred$Q2.5) + (max(pred$Q97.5) - min(pred$Q2.5))/2, 1), round(max(pred$Q97.5), 1))
  plotlist[[i]] = ggplot() +
    geom_point(data = pred, aes(x = eventualTypeChildless, y = Estimate, color = Category, shape = Category), size = 4) +
    geom_errorbar(data = pred, aes_string(x = "eventualTypeChildless", ymin = "Q2.5", ymax = "Q97.5", color = "Category"), width = 0, size=1) +
    scale_y_continuous(breaks = my_ticks) + 
    coord_flip() +
    facet_wrap(~ Sex, ncol = 1) +
    theme_flo() +
    theme(legend.position = "bottom") + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size=.1, color="black"),
          panel.grid.minor.y = element_line(color="gray90"),
          plot.title = element_text(hjust = 0.5)) +
    labs(shape = NULL, color = NULL) +
    guides(color=guide_legend(nrow = 3, byrow = TRUE), shape=guide_legend(nrow = 2,byrow = TRUE)) +
    ggtitle(multinom_nams[i])
  if (i != 1){
    plotlist[[i]] = plotlist[[i]] + theme(axis.title.y=element_blank(),
                                          axis.text.y=element_blank(),
                                          axis.ticks.y=element_blank())
  }
  if (i == ceiling(sqrt(length(multinom_models)))){
    plotlist[[i]] = plotlist[[i]] + labs(y = "Predicted Probability", x = NULL)
  } else{
    plotlist[[i]] = plotlist[[i]] + labs(x = NULL, y = "")
  }
  
}

# Combine the plots into one larger plot
library(gridExtra)
if (!dir.exists(paste0(out_dir, 'multinomial/'))){ dir.create(paste0(out_dir, 'multinomial/')) }
g = ggarrange(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], 
              widths = c(2.015,1,1,1,1), align = 'h', ncol = 5)
ggsave(paste0(out_dir, "multinomial/predicted_probabilities.png"), g, height = 6, width = 13)

#-----------------------------------------------------------------------
# Result 6: Multinomial model table of predicted probabilities
#-----------------------------------------------------------------------

# Obtain coefficients and information criteria for multinomial models
for (i in 1:length(multinom_models)){
  
  my_loo = round(LOO(multinom_models[[i]])$estimates["looic", 1], 3)
  my_waic = round(WAIC(multinom_models[[i]])$estimates["waic", 1], 3)
  fe = fixef(multinom_models[[i]])
  
  if (any(grepl("mu", rownames(fe)))){
    
    cats = gsub("mu", "", unique(unlist(lapply(str_split(rownames(fe), "_"), function(x){ x[grepl("mu", x)] }))))
    for (j in 1:length(cats)){
      
      fe_sub = fe[grepl(cats[j], rownames(fe)), ]
      res = vector()
      for (u in 1:length(fe_sub[, 1])){
        sig = ifelse(fe_sub[u, 3] < 0 & fe_sub[u, 4] > 0, "", "$^{*}$")
        sub = c(paste0(round(fe_sub[u, 1], 3), sig), paste0("(", round(fe_sub[u, 3], 3), "; ", round(fe_sub[u, 4], 3), ")"))
        res = c(res, sub)
      }
      
      rn = unlist(lapply(str_split(rownames(fe_sub), "_"), function(x){ x[length(x)]}))
      df = data.frame(rownames = c(paste0(rep(rn, each = 2), rep(c("_coef", "_sd"), length(rn))), "LOOIC", "WAIC"), 
                      res = c(res, my_loo, my_waic))
      
      if (i == 1 & j == 1){
        final = df
      } else {
        final = merge(final, df, by = "rownames")
      }
      colnames(final)[ncol(final)] = cats[j]
      
    }
    
  } else {
    
    res = vector()
    for (u in 1:length(fe[, 1])){
      sig = ifelse(fe[u, 3] < 0 & fe[u, 4] > 0, "", "$^{*}$")
      sub = c(paste0(round(fe[u, 1], 3), sig), paste0("(", round(fe[u, 3], 3), "; ", round(fe[u, 4], 3), ")"))
      res = c(res, sub)
    }
    
    df = data.frame(rownames = c(paste0(rep(rownames(fe), each = 2), rep(c("_coef", "_sd"), nrow(fe))), "LOOIC", "WAIC"), 
                    res = c(res, my_loo, my_waic))
    
    if (i == 1){
      final = df
    } else {
      final = merge(final, df, by = "rownames")
    }
    colnames(final)[ncol(final)] = levels(multinom_models[[i]]$data[, 1])[-1] 
    
  }
  
}

# Save results to file
final$rownames[grepl("_sd", final$rownames)] = ""
final$rownames = gsub("_coef", "", final$rownames)
output = print(xtable(final, digits = rep(3, ncol(final) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
if (!dir.exists(paste0(out_dir, "regression_tables/"))){ dir.create(paste0(out_dir, "regression_tables/")) }
fileConn = file(paste0(out_dir, "regression_tables/coefficients_multinomial.txt"))
writeLines(output, fileConn)
close(fileConn)

#-----------------------------------------------------------------------
# Result 7: Marginal effect of SWB/EWB:childlessness on SWB/EWB
#-----------------------------------------------------------------------

# Obtain marginal effects
med_margs = list()
for (i in 1:length(control_mods)){
  marg_swb_on_ewb = brms::marginal_effects(control_mods[[i]]$ewb, "lifeSatisfaction:eventualTypeChildless", plot = FALSE)[[1]]
  marg_ewb_on_swb = brms::marginal_effects(control_mods[[i]]$swb, "logCorrectedMonthlyGrossPay:eventualTypeChildless", plot = FALSE)[[1]]
  marg_swb_on_ewb["Sex"] = marg_ewb_on_swb["Sex"] =  ifelse(names(control_mods)[i] == "women", "Female", "Male")
  med_margs[[i]] = list(marg_swb_on_ewb = marg_swb_on_ewb, marg_ewb_on_swb = marg_ewb_on_swb) # rbind(data.frame(lapply(marg_swb_on_ewb, as.character)), data.frame(lapply(marg_ewb_on_swb, as.character)))
}
names(med_margs) = names(control_mods)

# Plot marginal effects
swb_on_ewb = plyr::rbind.fill(lapply(med_margs, function(x){x$marg_swb_on_ewb}))
ewb_on_swb = plyr::rbind.fill(lapply(med_margs, function(x){x$marg_ewb_on_swb}))
marg_list = list(swb_on_ewb = swb_on_ewb, ewb_on_swb = ewb_on_swb)
for (i in 1:length(marg_list)){
  marg_list[[i]]$effect2__ = factor(marg_list[[i]]$effect2__, levels = childless_types)
  g = ggplot() + 
    geom_line(data = marg_list[[i]], aes(y = estimate__, x = effect1__, linetype = effect2__, color = effect2__, group = effect2__), size = 1) +
    geom_ribbon(data = marg_list[[i]], aes(ymin = lower__, ymax = upper__, x = effect1__, fill = effect2__, group = effect2__), alpha = 0.25) +
    theme_flo() +
    facet_grid(~Sex) +  
    labs(color = NULL, fill = NULL, linetype = NULL, y = rev(y_labs)[i], x = rev(y_labs)[-i]) + 
    scale_x_continuous(breaks = seq(0, round(max(marg_list[[i]]$effect1__)), 2)) + 
    scale_y_continuous(breaks = seq(0, round(max(marg_list[[i]]$estimate__)), 1))
  ggsave(paste0(out_dir, "marginals/", names(marg_list)[i], '.png'), g, width = 12, height = 10)
  g = g +
    facet_grid(Sex ~ effect2__, labeller = labeller(effect2__ = label_wrap_gen(10)))
  ggsave(paste0(out_dir, "marginals/", names(marg_list)[i], '_facets.png'), g, height = 6, width = 10)
  
}

#-----------------------------------------------------------------------
# Result 8: Trace and density plots for convergence
#-----------------------------------------------------------------------

for (i in 1:length(control_mods)){
  
  for (j in 1:length(control_mods[[i]])){
    
    draws = posterior_samples(control_mods[[i]][[j]], add_chain = TRUE)
    vars = names(draws)[gsub( "_.*$", "", names(draws)) %in% c("b", "sd", "cor", "sigma", "lp")]
    draws = draws[, c(vars, "chain", "iter")]
    splits = split(seq_along(vars), ceiling(seq_along(seq_along(vars)) / 10))
    nams = colnames(draws)
    
    for (l in 1:length(splits)){
      
      gx = list()
      for (v in 1:length(splits[[l]])){
        
        gx[[v]] = list()
        draws["title"] = nams[splits[[l]][v]]
        
        gx[[v]][[1]] = local({
          v <- v
          p1 = ggplot() +
            theme_flo() +
            labs(x = NULL, y = NULL) +
            facet_wrap(~ title) + 
            theme_flo() +
            theme(legend.position = "none") + 
            geom_density(data = draws, aes(x = draws[, splits[[l]][v]]), 
                         fill = "firebrick1", color = "firebrick1", alpha = 0.75)
          print(p1)
        })
        
        gx[[v]][[2]] = local({
          v <- v
          p2 = ggplot() +
            theme_flo() +
            labs(x = NULL, y = NULL) +
            facet_wrap(~ title) + 
            theme(legend.position = "none") + 
            geom_line(data = draws, aes_string(x = "iter", y = draws[, splits[[l]][v]], color = "chain"))
          print(p2)
        })
        
      }
      
      if (l == length(splits)){
        m = max((length(gx) - 2), 1)
        gx = gx[c(1:m)]
      }
      
      plot_list = unlist(gx, recursive = FALSE)
      gf = do.call("grid.arrange", c(plot_list, ncol = 2))
      if (!dir.exists(paste0(out_dir, "trace_density_plots/"))){ dir.create(paste0(out_dir, "trace_density_plots/")) }
      ggsave(paste0(out_dir, "trace_density_plots/trace_", names(control_mods)[i], "_", names(control_mods[[i]])[j], "_", l, ".png"), gf, dpi = 600, height = 20, width = 15)
      
    }
    
  }
  
}

#-----------------------------------------------------------------------
# Result 9: Reduced regression coefficient table for full control model
#-----------------------------------------------------------------------

fit_col = list()
for (i in 1:length(control_mods)){
  
  # Obtain fixed and random effects
  fit_col[[i]] = list()
  for (j in 1:length(control_mods[[i]])){
    
    fe_dep = fixef(control_mods[[i]][[j]])
    re_dep = summary(control_mods[[i]][[j]])$random$ID
    eff = list(fe_dep, re_dep)
    res_vec = list()
    for (z in 1:length(eff)){
      
      est = paste0(round(eff[[z]][, 1], 3), ifelse(eff[[z]][, 3] <= 0 & eff[[z]][, 4] >= 0, "", "$^{*}$"))
      conf = paste0("(", trimws(format(round(eff[[z]][, 3], 3), nsmall = 3), "l"), "; ", format(round(eff[[z]][, 4], 3), nsmall = 3), ")")
      res_vec[[z]] = vector()
      for (u in 1:length(est)){ res_vec[[z]] = c(res_vec[[z]], est[u], conf[u]) }
      
    }
    
    col = unlist(res_vec)
    nams_fe = paste0(rep(rownames(fe_dep), each = 2), rep(c("_coef", "_conf"), length(rownames(fe_dep))))
    nams_re = paste0(rep(rownames(re_dep), each = 2), rep(c("_coef", "_conf"), length(rownames(re_dep))))
    
    # Information criteria
    my_loo = round(LOO(control_mods[[i]][[j]])$estimates["looic", 1], 3)
    my_waic = round(WAIC(control_mods[[i]][[j]])$estimates["waic", 1], 3)
    
    # Combine information into one column
    my_col = data.frame(c(col, my_loo, my_waic))
    rownames(my_col) = c(nams_fe, nams_re, c("LOOIC", "WAIC"))
    colnames(my_col) = paste0(CapStr(names(control_mods)[i]), " - ", toupper(names(control_mods[[i]])[j]))
    fit_col[[i]][[j]] = my_col
    
  }
  
  fit_col[[i]] = merge(fit_col[[i]][[1]], fit_col[[i]][[2]], by = "row.names", all = TRUE)
  
}

# Combine data into table and save to file
fit_table = merge(fit_col[[1]], fit_col[[2]], by = "Row.names", all = TRUE)
fit_table$Row.names[grepl("_conf", fit_table$Row.names)] = ""
fit_table$Row.names = gsub("_coef", "", fit_table$Row.names)
output = print(xtable(fit_table, digits = rep(3, ncol(fit_table) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
if (!dir.exists(paste0(out_dir, "regression_tables/"))){ dir.create(paste0(out_dir, "regression_tables/")) }
fileConn = file(paste0(out_dir, "regression_tables/coefficients_full_control.txt"))
writeLines(output, fileConn)
close(fileConn)


