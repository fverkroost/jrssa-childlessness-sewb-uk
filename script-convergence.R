# Define all models and their locations
mods_inf_cont = paste0("results/informative/controls/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_uninf_cont = paste0("results/uninformative/controls_uninformative/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_auto_cont = paste0("results/automatic/controls_automatic/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_unc = paste0("results/informative/uncontrols/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_child = paste0("results/informative/child/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_otherdep = paste0("results/informative/otherdep/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_marital = paste0("results/informative/marital/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_edu = paste0("results/informative/education/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_econact = paste0("results/informative/econact/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_inf_health = paste0("results/informative/health/fit_", c("men_swb.Rds", "men_ewb.Rds", "women_swb.Rds", "women_ewb.Rds"))
mods_multinom = paste0("results/multinomial/", c("cohabitation.Rds", "maritalStatus_combined.Rds", "healthStatus_binary.Rds", 
                                                 "economicActivity.Rds", "education_binary_bwimp.Rds"))
mods_no_econact = paste0("results/informative/controls_no_econact/fit_", c("men_ewb.Rds", "women_ewb.Rds"))

# Combine all models into a vector respectively list
all_mods = c(mods_inf_unc, mods_inf_child, mods_inf_otherdep, mods_inf_marital, mods_inf_edu, mods_inf_econact, 
             mods_inf_health, mods_inf_cont, mods_uninf_cont, mods_auto_cont, mods_no_econact, mods_multinom)
all_mods_list = list(mods_inf_unc, mods_inf_child, mods_inf_otherdep, mods_inf_marital, mods_inf_edu, mods_inf_econact, 
                     mods_inf_health, mods_inf_cont, mods_uninf_cont, mods_auto_cont, mods_no_econact, mods_multinom)
names(all_mods_list) = c("No control variables", "Child quantum and timing", "Other dependent variable", "Marital status and cohabitation",
                         "Education", "Economic activity", "Health status", "Fully controlled", "Fully controlled - Uninformative", 
                         "Fully controlled - Highly informative", "Fully controlled (no economic activity)", "Multinomial")

# For each model, compute the rhat (averaged across imputations) and convergence diagnostics
npars = vals = perc_not_converged_rhat = pars_not_converged_rhat = list()
for (i in 1:length(all_mods)){
  
  # Load model and average Rhats across imputations
  fit = readRDS(all_mods[i])
  new = colMeans(fit$rhats) # colMeans
  nc = which(new > 1.1 | new < 0.99)
  perc = length(nc) / length(new) * 100
  
  # Print non-converged model results
  if (perc > 0){
    print(all_mods[i])
    print(perc)
    print(names(nc))
    print(new[nc])
  }
  
  # Save convergence diagnostics to list
  perc_not_converged_rhat[[i]] = length(nc) / length(new) * 100
  pars_not_converged_rhat[[i]]= names(nc)
  npars[[i]] = ncol(fit$rhats)
  vals[[i]] = new[nc]
  
  # Combine results into data frame
  if (i == 1){
    plotdf = data.frame(
      Index = 1:length(new), Rhat = new, Model = all_mods[i], 
      Class = names(all_mods_list)[which(unlist(lapply(all_mods_list, function(x){all_mods[i] %in% x})))]
    )
  } else {
    plotdf = rbind(
      plotdf, 
      data.frame(
        Index = 1:length(new), 
        Rhat = new, 
        Model = all_mods[i],
        Class = names(all_mods_list)[which(unlist(lapply(all_mods_list, function(x){all_mods[i] %in% x})))]
      )
    )
  }
  
}

# Directory for plots
if (!dir.exists(paste0(out_dir, "rhats/"))){ dir.create(paste0(out_dir, "rhats/")) }

# For each separate class of models, compute a grid of Rhat plots
names(plotdf)[names(plotdf) == "Index"] = "Index of estimated parameter"
unis = unique(plotdf$Class)
for (i in 1:length(unis)){
  
  # Rename the models for aesthetic purposes
  subdf = subset(plotdf, Class == unis[i])
  sub_model = unlist(lapply(str_split(subdf$Model, "/"), function(x){x[length(x)]}))
  sub_model = gsub(".Rds", "", sub_model)
  modnam = ifelse(sub_model == "fit_men_swb", "Male SWB", 
                  ifelse(sub_model == "fit_men_ewb", "Male EWB",
                         ifelse(sub_model == "fit_women_swb", "Female SWB",
                                ifelse(sub_model == "fit_women_ewb", "Female EWB",
                                       ifelse(sub_model == "cohabitation", "Cohabitation",
                                              ifelse(sub_model == "maritalStatus_combined", "Marital status",
                                                     ifelse(sub_model == "healthStatus_binary", "Health status",
                                                            ifelse(sub_model == "economicActivity", "Economic activity",
                                                                   ifelse(sub_model == "education_binary_bwimp", "Education", NA)))))))))
  subdf$Model = paste0(subdf$Class, " - ", modnam)
  
  # Plot a grid of rhat plots for this particular class and save to file
  g = ggplot(data = subdf) +
    geom_hline(yintercept = 1.1, color = "red") +
    geom_point(aes(x = `Index of estimated parameter`, y = Rhat), alpha = 0.25) +
    facet_wrap(~ Model, ncol = 2, scales = "free_x") +
    theme_flo() +
    scale_y_continuous(limits = c(0.99, max(1.15, plotdf$Rhat)))
  ggsave(paste0(out_dir, "rhat_", gsub(" ", "", unique(subdf$Class)), ".png"), g, dpi = 600, height = 10, width = 15)
  
}

# Compute grid with N plots for less figures
cols = 3
rows = 6
plots = cols*rows
all_mods_hierarchical = all_mods #[!grepl("multinomial", all_mods)]
plot_index = split(1:length(all_mods_hierarchical), ceiling(seq_along(1:length(all_mods_hierarchical))/plots))
for (i in 1:length(plot_index)){
  
  # Rename the models for aesthetic purposes
  subdf = plotdf[plotdf$Model %in% all_mods_hierarchical[plot_index[[i]]], ]
  sub_model = unlist(lapply(str_split(subdf$Model, "/"), function(x){x[length(x)]}))
  sub_model = gsub(".Rds", "", sub_model)
  modnam = ifelse(sub_model == "fit_men_swb", "Male SWB", 
                  ifelse(sub_model == "fit_men_ewb", "Male EWB",
                         ifelse(sub_model == "fit_women_swb", "Female SWB",
                                ifelse(sub_model == "fit_women_ewb", "Female EWB",
                                       ifelse(sub_model == "cohabitation", "Cohabitation",
                                              ifelse(sub_model == "maritalStatus_combined", "Marital status",
                                                     ifelse(sub_model == "healthStatus_binary", "Health status",
                                                            ifelse(sub_model == "economicActivity", "Economic activity",
                                                                   ifelse(sub_model == "education_binary_bwimp", "Education", NA)))))))))
  subdf$Model = paste0(subdf$Class, " - ", modnam)
  
  # Plot a grid of rhat plots for this particular class and save to file
  g = ggplot(data = subdf) +
    geom_hline(yintercept = 1.1, color = "red") +
    geom_point(aes(x = `Index of estimated parameter`, y = Rhat), alpha = 0.25) +
    facet_wrap(~ Model, ncol = cols, scales = "free_x", labeller = labeller(Model = label_wrap_gen(30))) +
    theme_flo() +
    scale_y_continuous(limits = c(0.95, max(1.15, plotdf$Rhat)))
  ggsave(paste0(out_dir, "rhats/rhat_", i, ".png"), g, dpi = 600, height = 15, width = 10)
  
}
