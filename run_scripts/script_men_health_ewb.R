run_my_model = function(
  head_folder,
  sub_folder,
  dep_vars,
  other_dep,
  number_imputations,
  gender,
  control_vars,
  priors = c('informative', 'uninformative', 'automatic'),
  poly_order_fe,
  poly_order_re,
  number_iterations = 1000,
  thinning_rate = 10,
  number_chains = 4,
  multivariate
){
  
  # Load libraries and set random seed
  library(brms)
  library(xtable)
  library(future)
  library(mice)
  set.seed(1234)
  
  # Folder naming and create folders in case they do not exist yet
  path = paste0(head_folder, "/results/", priors, "/", sub_folder, "/")
  if (!dir.exists(path)){ dir.create(path, recursive = TRUE) }
  
  # Define variables
  id_var = "ID"
  x_var = "eventualTypeChildless"
  
  # Load imputed data
  datasets = list.files(path = paste0(head_folder, "/imputed_data"))
  datasets = datasets[substr(datasets, 1, 3) == substr(gender, 1, 3)]
  datasets = datasets[1:number_imputations]
  datalist = lapply(paste0(head_folder, "/imputed_data/", datasets), read.csv)
  
  # Function to write results and diagnostics to files
  writeFiles = function(name, time, fit, path){
    if (!file.exists(paste0(path, name, "_duration.txt"))){
      file.create(paste0(path, name, "_duration.txt"))
      write(paste0(name, ": ", time), paste0(path, name, "_duration.txt"))
    } else {
      write(paste0(name, ": ", time), paste0(path, name, "_duration.txt"), append = TRUE)
    }
    
    capture.output(summary(fit), file = paste0(path, name, ".txt"))
    
    zz = file(paste0(path, name, ".Rout"), open = "wt")
    sink(zz, type = "message")
    summary(fit)
    sink(type = "message")
    close(zz)
    
  }
  
  # Time polynomials
  poly_time_vars = paste0("poly_time_", 1:poly_order_fe)
  re_vars = paste0("poly_time_", 1:poly_order_re)
  
  # Model formula with moderation
  if (any(c(dep_vars, other_dep) %in% control_vars)){
    indep_vars = control_vars[!control_vars %in% c(dep_vars, other_dep)]
    if (sub_folder == "controls_no_econact" & dep_vars == "logCorrectedMonthlyGrossPay"){ indep_vars = indep_vars[indep_vars != "economicActivity"] }
    fmla = bf(as.formula(paste0(dep_vars, ' ~ 1 + ', x_var, '*(', paste0(poly_time_vars, collapse = " + "), ' + ', 
                                other_dep, ') + (1 + ', paste0(re_vars, collapse = " + "), ' |', id_var, ')',
                                ifelse(length(indep_vars) == 0, '', paste0('+ ', paste(indep_vars, collapse = ' + '))))), family = gaussian())
  } else {
    indep_vars = control_vars
    if (sub_folder == "controls_no_econact" & dep_vars == "logCorrectedMonthlyGrossPay"){ indep_vars = indep_vars[indep_vars != "economicActivity"] }
    fmla = bf(as.formula(paste0(dep_vars, ' ~ 1 + ', x_var, '*(', paste0(poly_time_vars, collapse = " + "), 
                                ') + (1 + ', paste0(re_vars, collapse = " + "), ' |', id_var, ')',
                                ifelse(is.null(indep_vars), '', paste0('+ ', paste(indep_vars, collapse = ' + '))))), family = gaussian())
  }
  
  # Prior distributions
  # Define prior distributions
  if (priors == "automatic"){
    
    prior_nams = names(attr(stats::terms(fmla$formula), "factors")[1, ])
    prior_fixed = prior_nams[!grepl("\\|", prior_nams)]
    prior_random = prior_nams[grepl("\\|", prior_nams)]
    prior_y = fmla$resp
    
    basic_fmla_fixed = bf(as.formula(paste0(prior_y, ' ~ 1 + ', paste0(prior_fixed, collapse = " + "))), family = gaussian())
    basic_fmla_random = bf(as.formula(paste0(prior_y, ' ~ (', paste0(prior_random, collapse = " + "), ")")), family = gaussian())
    
    basic_fit_fixed = brm(basic_fmla_fixed, data = datalist[[1]], seed = 1234, iter = 250, chains = 1, 
                          control = list(max_treedepth = 15, adapt_delta = 0.99), save_all_pars = FALSE)
    fixed = fixef(basic_fit_fixed)
    rm(basic_fit_fixed, basic_fmla_fixed)
    
    simple_fmla = paste0(prior_y, " ~ (1 + ", paste0(poly_time_vars, collapse = " + "), " | ID)")
    basic_fit_random = brm(as.formula(simple_fmla), data = datalist[[1]], seed = 1234, iter = 250, chains = 1, save_all_pars = FALSE)
    random = summary(basic_fit_random)$random$ID
    rm(basic_fit_random, basic_fmla_random)
    
    my_prior = prior_string(paste0("normal(", fixed[rownames(fixed) == "Intercept", 1], ",", # column 1 = estimate, column 2 = error
                                   fixed[rownames(fixed) == "Intercept", 2], ")"), class = "Intercept")
    for (l in 2:nrow(fixed)){
      my_prior = my_prior + prior_string(paste0("normal(", fixed[l, 1], ", ", fixed[l, 2], ")"), # column 1 = estimate, column 2 = error
                                         class = "b", coef = rownames(fixed)[l])
    }
    for (l in 1:(nrow(random)-1)){
      my_prior = my_prior + prior_string(paste0("student_t(3, ", random[l, 1], ", ", random[l, 2], ")"), # column 1 = estimate, column 2 = error
                                         class = "sd", coef = gsub("[\\(\\)]", "", regmatches(rownames(random)[l], gregexpr("\\(.*?\\)", rownames(random)[l]))[[1]]))
    }
    default_prior = get_prior(fmla, datalist[[1]])
    my_prior = rbind(my_prior, default_prior[!default_prior$class %in% my_prior$class, ])
    my_prior$group[which(my_prior$class == "sd")] = 'ID'
    
  } else if (priors == "informative"){
    
    my_prior = c(prior_string("normal(0,10)", class = "b"),
                 prior(normal(7,2), class = Intercept),
                 prior_(~cauchy(0,2), class = ~sd, group = ~ID),
                 prior_(~cauchy(0,2), class = ~sigma)) 
    
  } else if (priors == "uninformative"){
    my_prior = get_prior(fmla, data = datalist[[1]])
  } else { stop('This option for priors is not feasible; please choose one of the specified options.') }
  
  my_prior = my_prior[!duplicated(my_prior), ]
  
  # Fit model
  start = Sys.time()
  plan(multiprocess)
  myfit = brm_multiple(fmla, data = datalist, prior = my_prior, future = TRUE, thin = thinning_rate, 
                       seed = 1234, iter = number_iterations, chains = number_chains, cores = number_chains,
                       control = list(max_treedepth = 15, adapt_delta = 0.99), save_all_pars = FALSE)
  end = Sys.time()
  tt = difftime(end, start, units = "secs")
  varname = ifelse(dep_vars[1] == "lifeSatisfaction", "swb",
                   ifelse(dep_vars[1] == "logCorrectedMonthlyGrossPay", "ewb", dep_vars[1]))
  saveRDS(myfit, file = paste0(path, paste0("fit_", gender), "_", varname, ".Rds"))
  writeFiles(paste0("fit_", gender, "_", varname), tt, myfit, path)
  rm(fmla, myfit, start, end, tt)
  
}

run_my_model(
  head_folder = main_file_folder, 
  sub_folder = "health",
  dep_vars = "logCorrectedMonthlyGrossPay",
  other_dep = "lifeSatisfaction",
  number_imputations = 4,
  gender = "men",
  control_vars = c("healthStatus_binary"),
  priors = "informative",
  poly_order_fe = 3,
  poly_order_re = 1,
  number_iterations = 2000,
  thinning_rate = 10,
  number_chains = 3,
  multivariate = FALSE
)

