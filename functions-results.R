# Function to prepare the data on the basis of requirements
dataPrep <- function(
  data, 
  age16 = c('delete', 'missing', 'zero'), 
  class_lifesat = c('ordinal', 'skew_normal', 'gaussian'),
  remove_parents_26 = c(TRUE, FALSE)
){
  
  # Decide what to do with age 16 data (for income)
  if (age16 == 'delete'){
    data <- data[data$Age != 16, ]
  } else if (age16 == 'zero'){
    data$logCorrectedMonthlyGrossPay[data$Age == 16] <- 0
  } else if (age16 == 'missing'){
    data$logCorrectedMonthlyGrossPay[data$Age == 16] <- NA
  }
  
  if (remove_parents_26 == TRUE){
    data = data[!(data$Age <= 26 & data$childlessHousehold == "0"), ]
  }
  
  # Change classes of variables into the correct types
  data$Age <- as.integer(data$Age)
  data$logCorrectedMonthlyGrossPay <- as.numeric(data$logCorrectedMonthlyGrossPay)
  data$childlessHH42 <- factor(data$childlessHH42)
  data$Time <- as.integer(data$Time)
  data$maritalStatus <- factor(data$maritalStatus)
  data$maritalStatus_binary <- factor(data$maritalStatus_binary)
  data$maritalStatus_combined <- factor(data$maritalStatus_combined)
  data$healthStatus <- factor(data$healthStatus)
  data$healthStatus_binary <- factor(data$healthStatus_binary)
  data$economicActivity <- factor(data$economicActivity)
  data$education_binary <- factor(data$education_binary)
  data$education_binary_bwimp <- factor(data$education_binary_bwimp)
  data$lifeSatisfaction <- factor(data$lifeSatisfaction, ordered = TRUE)
  
  if (class_lifesat != 'ordinal'){
    data$lifeSatisfaction <- as.integer(data$lifeSatisfaction) - 1
  } 
  
  # Clean up data
  data$economicActivity = as.character(data$economicActivity)
  data = data[!data$economicActivity %in% c("Other", "Other employment"), ]
  data = data[!grepl('self-employed', data$economicActivity), ]
  data$economicActivity[grepl("OLF", data$economicActivity)] = "Out of the labour force"
  ind = which(data$economicActivity == "Out of the labour force" & is.na(data$logCorrectedMonthlyGrossPay))
  data$logCorrectedMonthlyGrossPay[ind] <- 0
  data$economicActivity = factor(data$economicActivity)
  data$ID = as.integer(data$ID)
  
  # Check if NA values are correct type
  vars <- c('Age', 'childlessHH42', 'lifeSatisfaction', 'logCorrectedMonthlyGrossPay', 'education_binary_bwimp', 
            'maritalStatus_combined', 'healthStatus_binary', 'economicActivity')
  correctNAtype <- function(vars, data){
    for (i in 1:length(vars)){
      tab <- table(data[, vars[i]], useNA = "always")
      tab_na <- unname(tab[is.na(names(tab))])
      sum_na <- sum(is.na(data[, vars[i]]))
      print(paste0(vars[i], ' -- ', tab_na == sum_na))
    }
  }
  correctNAtype(vars, data)
  
  return(data)
  
}

# Parent at age 26?
parents26 = function(x){
  if (any(x$Age == 26)){
    if (!is.na(x$childlessHousehold[x$Age == 26])){
      if (x$childlessHousehold[x$Age == 26] == "0"){
        y = "parent at age 26"
      } else if (x$childlessHousehold[x$Age == 26] == "1"){
        if (any(na.omit(x$childlessHousehold) == "0")){
          y = "childless at age 26 but parent later"
        } else {
          y = "always childless"
        }
      }
    } else {
      y = "parental status at age 26 unknown"
    }
  } else {
    y = "not observed at 26"
  }
  return(y)
}

# Even and odd numbers
even <- function(x) x%%2 == 0 
odd <- function(x) x%%2 != 0 

# Table with model results
modelResults <- function(my_fit){
  summ <- summary(my_fit)
  tot <- rbind(summ$fixed, summ$random$ID, summ$spec_pars)
  tot <- data.frame(apply(tot, 2, function(x){mround(x, .001)}))
  res <- data.frame(matrix(NA, nrow = 2*nrow(tot), ncol = 2))
  n <- nrow(res)
  res[, 1] <- rep(rownames(tot), each = 2)
  res[c(1:n)[even(1:n)], 1] <- paste0(res[c(1:n)[even(1:n)], 1], "_conf")
  res[c(1:n)[even(1:n)], 2] <- paste0("(", tot$l.95..CI, "; ", tot$u.95..CI, ")")
  inds = c(1:n)[odd(1:n)]
  for (u in 1:length(inds)){
    conf = c(tot$l.95..CI[u], tot$u.95..CI[u])
    if (min(conf) < 0 & max(conf) > 0){
      estimate = tot$Estimate[u]
    } else {
      estimate = paste0(tot$Estimate[u], "$^{*}$")
    }
    res[inds[u], 2] <- paste0(estimate, " (", tot$Est.Error[u], ")")
  }
  fit <- add_criterion(my_fit, c("loo", "waic"))
  waic_df <- data.frame(fit$waic$estimates)
  loo_df <- data.frame(fit$loo$estimates)
  waic_dat <- data.frame(cbind(rownames(waic_df), paste0(mround(waic_df$Estimate, .001), " (", mround(waic_df$SE, .001), ")")))
  loo_dat <- data.frame(cbind(rownames(loo_df), paste0(mround(loo_df$Estimate, .001), " (", mround(loo_df$SE, .001), ")")))
  br2 <- data.frame(bayes_R2(fit))
  br_dat <- data.frame(cbind(rownames(br2), paste0(mround(br2$Estimate, .001), " (", mround(br2$Est.Error, .001), ")")))
  sample_size <- data.frame(X1 = "N", X2 = nrow(my_fit$data))
  res <- rbind(res, waic_dat, loo_dat, br_dat, sample_size)
  
  return(res)
}

# Round to the nearest decimal (base)
mround <- function(x, base){ 
  m <- base*round(x/base) 
  return(m)
} 

# Plot marginal effects
marginalEffects <- function(
  my_fit, 
  sexe
){
  
  # Define variables of interest
  summ = summary(my_fit)
  fixef = summ$fixed
  yy = c("logCorrectedMonthlyGrossPay", "lifeSatisfaction")
  xvars = c("childlessHH42", paste0("poly_time_", 1:3), paste0("childlessHH42:",  paste0("poly_time_", 1:3)))
  controlvars = c("maritalStatus_combined", "education_binary_bwimp", "economicActivity", "healthStatus_binary")
  for (p in 1:length(controlvars)){
    if (any(grepl(controlvars[p], rownames(fixef)))){
      xvars = c(xvars, controlvars[p])
    }
  }
  
  # Define nice names for variables for plots
  xnames = data.frame(
    old = c("childlessHH42",  paste0("poly_time_", 1:3), "maritalStatus_combined", 
            "education_binary_bwimp", "economicActivity", "healthStatus_binary", 
            "lifeSatisfaction", "logCorrectedMonthlyGrossPay"),
    new = c("Childless", "Time (1st OP)", "Time (2nd OP)",  "Time (3rd OP)", "Marital Status", "Education", 
            "Economic Activity", "Health Status", "Life Satisfaction", "Log(1 + Inflation-Corrected Monthly Gross Pay")
  )
  ynames = data.frame(
    old = c("logCorrectedMonthlyGrossPay", "lifeSatisfaction"),
    new = c("Log(1 + Inflation-Corrected Monthly Gross Pay)", "Life Satisfaction")
  )
  
  for (m in 1:length(yy)){
    xx = c(xvars, yy[-m], paste0("childlessHH42:", yy[-m]))
    for (v in 1:length(xx)){
      if (grepl(":", xx[v])){
        
        # Obtain marginal effects for interaction effect
        int_cond = list(setNames(seq(min(my_fit$data[, sub(".*\\:", "", xx[v])]), max(my_fit$data[, sub(".*\\:", "", xx[v])])), 
                                 as.character(seq(min(my_fit$data[, sub(".*\\:", "", xx[v])]), max(my_fit$data[, sub(".*\\:", "", xx[v])])))))
        names(int_cond) = sub(".*\\:", "", xx[v])
        margs = marginal_effects(x = my_fit, effects = xx[v], int_conditions = int_cond, resp = yy[m], plot = FALSE)
        margs[[1]]$effect2__ = factor(margs[[1]]$effect2__, levels = unique(margs[[1]]$effect2__))
        margs[[1]]$effect1__ = factor(ifelse(as.character(margs[[1]]$effect1__) == "0", "Parents", 
                                             ifelse(as.character(margs[[1]]$effect1__) == "1", "Childless", NA)))
        # Produce plot
        g = ggplot() + 
          theme_flo() + 
          geom_line(data = margs[[1]], aes(x = effect2__, y = estimate__, color = effect1__, group = effect1__, linetype = effect1__)) + 
          geom_ribbon(data = margs[[1]], alpha = 0.5,
                      aes(x = effect2__, ymin = lower__, ymax = upper__, fill = effect1__, group = effect1__)) +
          labs(y = ynames$new[match(yy[m], ynames$old)], x = xnames$new[match(sub(".*\\:", "", xx[v]), xnames$old)], 
               color = NULL, fill = NULL)
        
        # Define marginals
        if (grepl("childlessHH42:logCorrectedMonthlyGrossPay", xx[v])){
          my_res_lifesat = margs[[1]]
        }
        if (grepl("childlessHH42:lifeSatisfaction", xx[v])){
          my_res_income = margs[[1]]
        }
        
      } else {
        # Obtain marginal effects for direct effect and produce plot
        margs = marginal_effects(x = my_fit, effects = xx[v], resp = yy[m], plot = FALSE)
        g = ggplot() + 
          theme_flo() + 
          geom_point(data = margs[[1]], aes(x = effect1__, y = estimate__), 
                     size = 2, position = position_dodge(width = 0.5)) + 
          geom_errorbar(data = margs[[1]], aes(x = effect1__, ymin = lower__, ymax = upper__), 
                        width = 0.2, position = position_dodge(width = 0.5)) +
          labs(x = xnames$new[match(sub("\\:.*", "", xx[v]), xnames$old)], y = ynames$new[match(yy[m], ynames$old)])
        
        if (!all(grepl("\\d", unique(margs[[1]]$effect1__))) & length(unique(margs[[1]]$effect1__)) > 4){
          g = g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } 
        
      }
      
      if (!dir.exists(paste0(out_dir, folders[i], "/", tolower(sexe), "/marginal_effects"))){
        dir.create(paste0(out_dir, folders[i], "/", tolower(sexe), "/marginal_effects"), recursive = TRUE)
      }
      
      ggsave(paste0(out_dir, folders[i], "/", tolower(sexe), "/marginal_effects/", yy[m], "_", xx[v], ".png"), g)
      
    }
  }
  
  my_res = rbind(my_res_income, my_res_lifesat)
  my_res['depvar'] = rep(c("income", "lifesat"), c(nrow(my_res_income), nrow(my_res_lifesat)))
  return(my_res)
  
}

descriptiveStatistics <- function(
  desc_df, 
  vars, 
  my_folder
){
  
  # Keep only relevent variables
  cleandf <- desc_df[, c(vars, "Sex")]
  cleandf$childlessHH42 <- as.factor(as.character(cleandf$childlessHH42))
  facs <- names(which(sapply(cleandf, is.factor)))
  numvars <- vars[-which(vars %in% facs)]
  dumdf <- cbind(cleandf[, numvars], dummy.data.frame(cleandf[, facs]))
  for (i in 1:length(facs)){
    col = which(grepl("NA", colnames(dumdf)) & grepl(facs[i], colnames(dumdf)))
    inds = which(dumdf[, col] == 1)
    dumdf[inds, which(!grepl("NA", colnames(dumdf)) & grepl(facs[i], colnames(dumdf)))] = NA
  }
  dumdf <- dumdf[, !grepl("NA", names(dumdf))]
  wdumdf <- dumdf %>% filter(SexFemale == "1") %>% dplyr::select(-SexFemale, -SexMale, -childlessHH420)
  mdumdf <- dumdf %>% filter(SexMale == "1") %>% dplyr::select(-SexMale, -SexFemale, -childlessHH420)
  
  # Match variables to nicely looking names
  matchdf <- data.frame(
    original = c("lifeSatisfaction", "Year", "Age", "logCorrectedMonthlyGrossPay", "childlessHH421",                                
                 "healthStatus_binaryBad", "healthStatus_binaryGood", "economicActivityFull-time paid employee",  
                 "economicActivityFull-time self-employed","economicActivityOLF: Full-time education",
                 "economicActivityOLF: Full-time education/training",
                 "economicActivityOLF: Home/family care", "economicActivityOLF: Retired", "economicActivityOLF: Sick/disabled",            
                 "economicActivityOLF: Training scheme", "economicActivityOLF: Unemployed",               
                 "economicActivityOther", "economicActivityOther employment",              
                 "economicActivityPart-time paid employee", "economicActivityPart-time self-employed",       
                 "education_binary_bwimpNVQ3-", "education_binary_bwimpNVQ4+",
                 "maritalStatus_combined(Re)married/cohabiting", "maritalStatus_combinedDivorced/separated",      
                 "maritalStatus_combinedSingle and never married", "maritalStatus_combinedWidowed",
                 "economicActivityOut of the labour force"),
    new = c("SWB: Life satisfaction", "Year", "Age", "EWB: log(1 + inflation-corrected monthly gross pay)",
            "Childless", paste0("Health status - ", c("Bad", "Good")), paste0("Economic activity - ", 
                                                                              c("Full-time paid employee", "Full-time self-employed", "OLF: Full-time education",
                                                                                "OLF: Full-time education/training",
                                                                                "OLF: Home/family care", "OLF: Retired", "OLF: Sick/disabled", "OLF: Training scheme", 
                                                                                "OLF: Unemployed", "Other", "Other employment", "Part-time paid employee", "Part-time self-employed")),
            paste0("Education - ", c("Lower educated (NVQ3-)", "Higher educated (NVQ4+)")),
            paste0("Marital status - ", c("(Re)married/cohabiting", "Divorced/separated",      
                                          "Single and never married", "Widowed")), 
            "Economic activity - Out of the labour force")
  )
  
  # Produce descriptive statistics
  int <- union(names(wdumdf), names(mdumdf))
  descstat <- data.frame(matrix(NA, nrow = 2 + length(int), ncol = 9))
  descstat[1,] <- c(NA, 'Women', rep(NA, 3), 'Men', rep(NA, 3))
  descstat[2,] <- c('Variable', 'Mean', 'St. Dev', 'Min', 'Max', 'Mean', 'St. Dev', 'Min', 'Max')
  descstat[3:nrow(descstat), 1] <- int
  descstat[match(names(wdumdf), descstat[, 1]), 2] <- round(unlist(lapply(wdumdf, function(x){mean(na.omit(x))})), 3)
  descstat[match(names(wdumdf), descstat[, 1]), 3] <- round(unlist(lapply(wdumdf, function(x){sd(na.omit(x))})), 3)
  descstat[match(names(wdumdf), descstat[, 1]), 4] <- round(unlist(lapply(wdumdf, function(x){min(na.omit(x))})), 3)
  descstat[match(names(wdumdf), descstat[, 1]), 5] <- round(unlist(lapply(wdumdf, function(x){max(na.omit(x))})), 3)
  descstat[match(names(mdumdf), descstat[, 1]), 6] <- round(unlist(lapply(mdumdf, function(x){mean(na.omit(x))})), 3)
  descstat[match(names(mdumdf), descstat[, 1]), 7] <- round(unlist(lapply(mdumdf, function(x){sd(na.omit(x))})), 3)
  descstat[match(names(mdumdf), descstat[, 1]), 8] <- round(unlist(lapply(mdumdf, function(x){min(na.omit(x))})), 3)
  descstat[match(names(mdumdf), descstat[, 1]), 9] <- round(unlist(lapply(mdumdf, function(x){max(na.omit(x))})), 3)
  dumrows <- which((descstat[, 4] == 0 & descstat[, 5] == 1) | (descstat[, 8] == 0 & descstat[, 9] == 1))
  descstat[dumrows, c(3:5, 7:9)] <- NA
  descstat[3:nrow(descstat), 1] <- as.character(matchdf$new[match(int, matchdf$original)])
  
  # Add number of individuals and observations
  n_obs = c("Number of observations", nrow(subset(desc_df, Sex == "Female")), rep(NA, 3), nrow(subset(desc_df, Sex == "Male")), rep(NA, 3))
  n_ind = c("Number of individuals", length(unique(subset(desc_df, Sex == "Female")$ID)), rep(NA, 3), length(unique(subset(desc_df, Sex == "Male")$ID)), rep(NA, 3))
  
  # Combine information in a table
  descstat = rbind(descstat, n_obs, n_ind)
  
  # Save table to file
  output <- print(xtable(descstat, digits = rep(3, ncol(descstat) + 1)), include.rownames = FALSE, sanitize.text.function = function(x){x})
  fileConn <- file(paste0(my_folder, "descriptiveStatisticsBCS70.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}

# Capitalize string
CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

