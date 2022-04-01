library(mice)
set.seed(1234)

# Define variables
facs <- c("eventualTypeChildless", 'socialClass', 'economicActivity', 'eventualChildlessness','healthStatus_binary',
          'highestQualification', 'cohabitation', 'education_binary_bwimp', 'maritalStatus_combined', 
          'numberChildrenNaturalCategorical', 'childArrivedSinceLastWave')
nums <- c("logCorrectedMonthlyGrossPay", "correctedMonthlyGrossPay", "monthlyGrossPay", paste0("poly_time_", 1:3))
ints <- c('ID', "lifeSatisfaction", "finalMalaise", "Age")
all_vars = c(facs, nums, ints)

# Load the data (only those observed at least in their forties)
if (exists("women") & exists("men")){
  org_data = list(women, men)
} else {
  org_data = list(
    read.csv("womenBCS_obs_42_or_46.csv"),
    read.csv("menBCS_obs_42_or_46.csv")
  )
}
sexes = c("women", "men")

for (j in 1:length(org_data)){
  
  df = org_data[[j]][, all_vars]
  df[, ints] <- data.frame(sapply(df[, ints], as.integer))
  df[, nums] <- data.frame(sapply(df[, nums], as.numeric))
  df[, facs] <- data.frame(sapply(df[, facs], as.factor))
  if (!dir.exists("imputed_data/")){ dir.create("imputed_data/") }
  
  # In the predictor matrix, -2 denotes the class variable, a value 1 indicates a fixed effect and a value 2 indicates a random effect
  nas = apply(df, 2, function(x){ sum(is.na(x)) })
  complete_vars = names(nas)[nas == 0]
  ini = mice(df, maxit = 0)
  pred = ini$pred
  class_vars = c("ID")
  df[, class_vars] = as.integer(df[, class_vars])
  re_vars = c("eventualTypeChildless", "eventualChildlessness", "education_binary_bwimp")
  fe_vars = all_vars[!all_vars %in% c(re_vars, class_vars)]
  pred[, class_vars] = -2
  pred[, fe_vars] = 1
  pred[, re_vars] = 2
  pred[complete_vars, ] = 0
  diag(pred) = 0
  meth = ini$meth
  meth[meth %in% c("logreg", "polyreg") & names(meth) %in% re_vars] = "2lonly.pmm"
  meth[meth == "pmm" & names(meth) %in% nums] = "2l.norm"
  meth["logCorrectedMonthlyGrossPay"] <- "~log1p(squeeze(correctedMonthlyGrossPay, bounds = c(0, 170000)))"
  post = ini$post
  post["logCorrectedMonthlyGrossPay"] = "imp[[j]][,i] <- squeeze(imp[[j]][,i], c(0, 12))"
  
  # Perform multiple imputation
  # imp = mice(df, m = 5, maxit = 5, remove_collinear = TRUE, seed = 1234, pred = pred, meth = meth)
  imp = mice(df, m = 5, maxit = 5, meth = meth, pred = pred, post = post, remove_collinear = TRUE, seed = 1234)
  imp_data = mice::complete(imp, "all")
  res_nas = unlist(lapply(imp_data, function(x, vars){ sum(is.na(x[, vars])) }, vars = all_vars ))
  if (!all(res_nas == 0)){ stop("Error: There are still missing values in the imputed data sets!") }
  
  # Add orthogonal polynomials of time
  imp_list = imp_data
  for (i in 1:length(imp_data)){
    # imp_list[[i]][paste0('poly_time_', 1:3)] = poly((imp_data[[i]]$Age - min(imp_data[[i]]$Age)), 3)[, 1:3]
    imp_list[[i]]$monthlyGrossPay = squeeze(
      imp_list[[i]]$monthlyGrossPay, 
      bounds = c(min(na.omit(df$monthlyGrossPay)), max(na.omit(df$monthlyGrossPay)))
    )
    imp_list[[i]]$logCorrectedMonthlyGrossPay = squeeze(
      imp_list[[i]]$logCorrectedMonthlyGrossPay, 
      bounds = c(min(na.omit(df$logCorrectedMonthlyGrossPay)), max(na.omit(df$logCorrectedMonthlyGrossPay)))
    )
    imp_list[[i]]$lifeSatisfaction = squeeze(
      imp_list[[i]]$lifeSatisfaction, 
      bounds = c(min(na.omit(df$lifeSatisfaction)), max(na.omit(df$lifeSatisfaction)))
    )
    imp_list[[i]]$finalMalaise = squeeze(
      imp_list[[i]]$finalMalaise, 
      bounds = c(min(na.omit(df$finalMalaise)), max(na.omit(df$finalMalaise)))
    )
    write.csv(imp_list[[i]], file = paste0("imputed_data/", sexes[j], "_", "imputed_data_", i, ".csv"))
  }
  
  save(imp, file = paste0("imputed_data/imp_", sexes[j], ".Rdata"))
  
}

