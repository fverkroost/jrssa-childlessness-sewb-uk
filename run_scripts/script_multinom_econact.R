run_my_model = function(
  head_folder,
  sub_folder,
  indep_vars,
  number_imputations,
  number_iterations = 1000,
  thinning_rate = 10,
  number_chains = 4
){
  
  # Load libraries and set random seed
  library(brms)
  library(xtable)
  library(future)
  library(mice)
  library(stringr)
  library(parallel)
  set.seed(1234)
  
  # Load imputed data
  datasets = list.files(path = paste0(head_folder, "/imputed_data"))
  numbers = unique(as.numeric(gsub("\\D", "", datasets)))
  numbers = numbers[1:number_imputations]
  datalist = list()
  for (i in 1:length(numbers)){
    datalist[[i]] = rbind(cbind(read.csv(paste0(head_folder, "/imputed_data/women_imputed_data_", numbers[i], ".csv")), Sex = "Female"),
                          cbind(read.csv(paste0(head_folder, "/imputed_data/men_imputed_data_", numbers[i], ".csv")), Sex = "Male"))
    datalist[[i]][, indep_vars] = factor(gsub(" ", "", str_replace_all(gsub("\\+","", as.character(datalist[[i]][, indep_vars])), "[[:punct:]]", ""))) 
  }
  
  # Construct prior
  my_prior = c(prior_string("normal(0, 5)", class = "b"),
               prior(normal(0, 5), class = Intercept)) 
  
  # Folder naming and create folders in case they do not exist yet
  path = paste0(head_folder, "/results/", sub_folder, "/")
  if (!dir.exists(path)){ dir.create(path, recursive = TRUE) }
  
  # Family type
  number_categories = unique(unlist(lapply(datalist, function(x){ length(unique(x[, indep_vars])) })))
  family_type = ifelse(any(number_categories > 2), "categorical", "bernoulli")
  
  # Formula and model
  fmla = as.formula(paste0(indep_vars, "~ eventualTypeChildless*Sex"))
  plan(multiprocess)
  myfit = brm_multiple(fmla, data = datalist, family = family_type, seed = 1234, future = TRUE, prior = my_prior,
                       iter = number_iterations, chains = number_chains, thin = thinning_rate,
                       cores = min(number_chains*number_imputations, parallel::detectCores() - 1), 
                       control = list(max_treedepth = 15, adapt_delta = 0.99), save_all_pars = FALSE)
  saveRDS(myfit, file = paste0(path, indep_vars, ".Rds"))
  
}

run_my_model(
  head_folder = main_file_folder, 
  sub_folder = "multinomial",
  indep_vars = 'economicActivity', 
  number_iterations = 2000,
  number_imputations = 4,
  thinning_rate = 10,
  number_chains = 3
)
