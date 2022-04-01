# Unique characters in string
uniqchars <- function(x) unique(strsplit(x, "")[[1]]) 

# Install and load multiple packages simultaneously
install_load <- function (package1, ...){
  packages <- c(package1, ...)
  for(package in packages){
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}

# Round to number of decimals
mround <- function(x, base){ 
  m <- base*round(x/base) 
  return(m)
} 

# Recover the number of children from age 42 to earlier years
recoverNumberChildren <- function(x){
  if (!(all(is.na(x$numberChildren)))){
    ind <- which(!is.na(x$numberChildren))
    x$numberChildren <- rep(x$numberChildren[ind], nrow(x))
  }
  return(x)
}

# Calculate proportion of incomplete observations
propMiss <- function(x){
  pmiss <- length(which(!(complete.cases(x)))) / nrow(x) * 100
  return(pmiss)
}

# Source specific lines from a script
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}


# Calculate M for multiple imputation
calculateM <- function(data, vars){
  newdata <- data[, vars]
  sumna <- length(which(!complete.cases(newdata)))
  M <- round(sumna/nrow(newdata)*100)
  return(M)
}

# Specify custom plotting theme
ss = 14
theme_flo <- function () { 
  theme_bw() + theme(axis.text = element_text(size = ss),
                     axis.title = element_text(size = ss),
                     strip.text = element_text(size = ss),
                     legend.text = element_text(size = ss),
                     #strip.background = element_rect(fill = "white", colour = "black"),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     legend.position = "bottom")
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
      estimate = paste0(tot$Estimate[u], "^{*}")
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

# Combined legend for grid.arrange
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Keep only numbers in a string
numbers_only <- function(x) {!grepl("\\D", str_replace_all(x, "[^[:alnum:]]", ""))}

# Read STATA and/or SPSS data in R
readData <- function(file){
  if (grepl(".dta", file)){
    dataFile <- try(foreign::read.dta(file, convert.factors = TRUE), silent = TRUE)
    if (inherits(dataFile, "try-error")){
      dataFile <- try(data.frame(haven::read_dta(file)), silent = TRUE)
    }
  } else if (grepl(".sav", file)){
    dataFile <- foreign::read.spss(file, to.data.frame = TRUE) 
  }
  return(dataFile)
}

# Load multiple STATA files at once
load_data <- function(
  myPath = "./data-raw", 
  subfolder = c("US", "BCS", "NCDS"), 
  includeFiles = NULL
){
  newPath <- paste0(myPath, "/", subfolder)
  stataFiles <- list.files(path = newPath, pattern = "[.]dta", recursive = TRUE, full.names = TRUE)
  spssFiles <- list.files(path = newPath, pattern = "[.]sav", recursive = TRUE, full.names = TRUE)
  files <- c(stataFiles, spssFiles)
  dataFiles <- lapply(files, readData)
  names(dataFiles) <- sub('\\..*', '', sub('.*\\/', '', files))
  return(dataFiles)
}

# Load multiple STATA files at once
loadData <- function(
  myPath = "./data-raw", 
  subfolder = c("US", "BCS", "NCDS"), 
  includeFiles = NULL
){
  newPath <- paste0(myPath, "/", subfolder)
  stataFiles <- list.files(path = newPath, pattern = "[.]dta", recursive = TRUE, full.names = TRUE)
  spssFiles <- list.files(path = newPath, pattern = "[.]sav", recursive = TRUE, full.names = TRUE)
  files <- c(stataFiles, spssFiles)
  incl <- grep(paste(includeFiles, collapse = "|"), files, value = FALSE)
  files <- files[incl]
  dataFiles <- lapply(files, readData)
  names(dataFiles) <- sub('\\..*', '', sub('.*\\/', '', files))
  return(dataFiles)
}

# Give all ID variables the same name for merging
IDequal <- function(
  x, 
  survey = c('BCS', 'NCDS')
){
  nams <- tolower(names(x))
  id = ifelse(survey == "BCS", "BCSID", "NCDSID")
  ind <- which(nams == tolower(id))
  names(x)[ind] <- id
  return(x)
}

# Retain only relevant variables
relevantVariables <- function(
  x, 
  keepvars
){
  addVars <- keepvars[which(!(keepvars %in% names(x)))]
  x[addVars] <- NA
  x <- x[, keepvars]
  return(x)
}

# Compute total and grouped malaise scores
malaiseScore <- function(submat){
  b26mal <- apply(submat, 1, function(x){sum(na.omit(x))})
  b26miss <- apply(submat, 1, function(x){sum(is.na(x))})
  malmiss <- b26mal + b26miss
  for (i in 1:length(malmiss)){
    if (b26miss[i] > 0 & b26mal[i] <= 7 & malmiss[i] >= 8){
      b26mal[i] <- NA #-1
    } 
    if (b26miss[i] == 24){
      b26mal[i] <- NA #-2
    }
  }
  b26malg <- ifelse(b26mal >= 0 & b26mal <= 7, 1,
                    ifelse(b26mal > 7, 2, b26mal))
  submat["totalMalaise"] <- b26mal
  submat["groupedMalaise"] <- b26malg
  #x <- x[, -which(names(x) %in% malvars)]
  return(submat)
}

# Compute standardised life satisfaction variable
standLS <- function(x){
  if ('lifeSatisfaction' %in% names(x)){
    x['standLifesat'] <- NA
    x$standLifesat <- c(scale(x$lifeSatisfaction))
  }
  return(x)
}

# Create and save LaTeX table of descriptive statistics for men and women
descriptiveStatistics = function(
  data, 
  num_vars, 
  cat_vars, 
  output_folder, 
  decimals = 3
){
  wom = subset(data, Sex == "Female")
  men = subset(data, Sex == "Male")
  desc_wom = createDescriptives(wom, num_vars, cat_vars)
  desc_men = createDescriptives(men, num_vars, cat_vars)
  desc = merge(desc_wom, desc_men, by = 'Variable', suffixes = c("Women", "Men"))
  output = print(xtable(desc, digits = rep(decimals, ncol(desc) + 1)), include.rownames = FALSE)
  fileConn = file(paste0(output_folder, "descriptiveStatistics.txt"))
  writeLines(output, fileConn)
  close(fileConn)
  
}

# Create one table of descriptive statistics for one data set (either women or men)
createDescriptives = function(
  mydf, 
  num_vars, 
  cat_vars
){
  
  # Create data for numeric variables
  numdf = data.frame(
    Variable = num_vars , 
    Mean = apply(mydf[, num_vars], 2, function(x){mean(na.omit(x))}),
    StDev = apply(mydf[, num_vars], 2, function(x){sd(na.omit(x))}),
    Min = apply(mydf[, num_vars], 2, function(x){min(na.omit(x))}),
    Max = apply(mydf[, num_vars], 2, function(x){max(na.omit(x))}),
    propNonNA = apply(mydf[, num_vars], 2, function(x){length(which(!is.na(x))) / length(x) * 100})
  )
  
  # Create data for categorical variables
  catdf = data.frame(matrix(NA, nrow = 0, ncol = 6))
  names(catdf) = c("Variable", "Mean", "StDev", "Min", "Max", "propNonNA")
  for (i in 1:length(cat_vars)){
    x = factor(mydf[, cat_vars[i]])
    dumdf = dummy.data.frame(data.frame(x))
    dumdf[is.na(x), ] = NA
    dumdf = dumdf[, !grepl("NA", names(dumdf))]
    for (j in 1:ncol(dumdf)) {
      mn = mean(na.omit(dumdf[, j]))
      std = sd(na.omit(dumdf[, j]))
      minn = NA # min(na.omit(dumdf[, j]))
      maxx = NA # max(na.omit(dumdf[, j]))
      prop_comp = length(which(!is.na(dumdf[, j]))) / length(dumdf[, j]) * 100
      newdf = data.frame(Variable = gsub("x", cat_vars[i], names(dumdf)[j]), Mean = mn, StDev = std, Min = minn, Max = maxx, propNonNA = prop_comp)
      catdf = rbind(catdf, newdf)
    }
    
  }
  
  # Combine numerical and categorical data
  desc = rbind(numdf, catdf)
  return(desc)
  
}

# Reduce number of levels in malaise variable
compressMalaise <- function(x){
  y <- ifelse(x %in% c('Yes', 'YES', 'Most of the time', '1'), 1,
              ifelse(x %in% c('No', 'NO', 'Rarely or never', '2'), 0, 
                     ifelse(x == 'Some of the time', 0.5, NA)))
  return(as.numeric(y))
}

# Finish plot stanard coding
finishPlot <- function(g){
  g <- g + geom_point() + 
    geom_line() + 
    labs(x = 'Dimension', y = 'Increase in Total Fit') + 
    theme_flo() + 
    scale_x_continuous(breaks = seq(1, nrow(screevalues)))
  return(g)
}

# Return predicted values for one independent and the dependent variable
boostFit <- function (x, i.var = 1, n.trees = x$n.trees, continuous.resolution = 100, 
                      return.grid = FALSE, type = c("link", "response"), level.plot = TRUE, 
                      contour = FALSE, number = 4, overlap = 0.1, col.regions = viridis::viridis, 
                      ...) 
{
  type <- match.arg(type)
  if (all(is.character(i.var))) {
    i <- match(i.var, x$var.names)
    if (any(is.na(i))) {
      stop("Requested variables not found in ", deparse(substitute(x)), 
           ": ", i.var[is.na(i)])
    }
    else {
      i.var <- i
    }
  }
  if ((min(i.var) < 1) || (max(i.var) > length(x$var.names))) {
    warning("i.var must be between 1 and ", length(x$var.names))
  }
  if (n.trees > x$n.trees) {
    warning(paste("n.trees exceeds the number of tree(s) in the model: ", 
                  x$n.trees, ". Using ", x$n.trees, " tree(s) instead.", 
                  sep = ""))
    n.trees <- x$n.trees
  }
  if (length(i.var) > 3) {
    warning("plot.gbm() will only create up to (and including) 3-way ", 
            "interaction plots.\nBeyond that, plot.gbm() will only return ", 
            "the plotting data structure.")
    return.grid <- TRUE
  }
  grid.levels <- vector("list", length(i.var))
  for (i in 1:length(i.var)) {
    if (is.numeric(x$var.levels[[i.var[i]]])) {
      grid.levels[[i]] <- seq(from = min(x$var.levels[[i.var[i]]]), 
                              to = max(x$var.levels[[i.var[i]]]), length = continuous.resolution)
    }
    else {
      grid.levels[[i]] <- as.numeric(factor(x$var.levels[[i.var[i]]], 
                                            levels = x$var.levels[[i.var[i]]])) - 1
    }
  }
  X <- expand.grid(grid.levels)
  names(X) <- paste("X", 1:length(i.var), sep = "")
  if (is.null(x$num.classes)) {
    x$num.classes <- 1
  }
  y <- .Call("gbm_plot", X = as.double(data.matrix(X)), cRows = as.integer(nrow(X)), 
             cCols = as.integer(ncol(X)), n.class = as.integer(x$num.classes), 
             i.var = as.integer(i.var - 1), n.trees = as.integer(n.trees), 
             initF = as.double(x$initF), trees = x$trees, c.splits = x$c.splits, 
             var.type = as.integer(x$var.type), PACKAGE = "gbm")
  if (x$distribution$name == "multinomial") {
    X$y <- matrix(y, ncol = x$num.classes)
    colnames(X$y) <- x$classes
    if (type == "response") {
      X$y <- exp(X$y)
      X$y <- X$y/matrix(rowSums(X$y), ncol = ncol(X$y), 
                        nrow = nrow(X$y))
    }
  }
  else if (is.element(x$distribution$name, c("bernoulli", "pairwise")) && 
           type == "response") {
    X$y <- 1/(1 + exp(-y))
  }
  else if ((x$distribution$name == "poisson") && (type == "response")) {
    X$y <- exp(y)
  }
  else if (type == "response") {
    warning("`type = \"response\"` only implemented for \"bernoulli\", ", 
            "\"poisson\", \"multinomial\", and \"pairwise\" distributions. ", 
            "Ignoring.")
  }
  else {
    X$y <- y
  }
  f.factor <- rep(FALSE, length(i.var))
  for (i in 1:length(i.var)) {
    if (!is.numeric(x$var.levels[[i.var[i]]])) {
      X[, i] <- factor(x$var.levels[[i.var[i]]][X[, i] + 
                                                  1], levels = x$var.levels[[i.var[i]]])
      f.factor[i] <- TRUE
    }
  }
  names(X)[1:length(i.var)] <- x$var.names[i.var]
  if (return.grid) {
    return(X)
  }
}

# Make perfectly balanced sample for factor
balancedSample <- function(x, depvar){
  min_per_class <- table(x[, depvar])[which.min(table(x[, depvar]))]
  u <- unique(x[, depvar])
  train <- test <- data.frame(matrix(NA, nrow = 0, ncol = ncol(x)))
  for (i in 1:length(u)){
    inds <- which(x[, depvar] == u[i])
    samp <- sample(inds, min_per_class)
    train <- rbind(train, x[samp, ])
    test <- rbind(test, x[-samp, ])
  }
  return(list(train = train, test = test))
}

# Bayesian modelling using BRMS
bayesianModel <- function(depvar, indepvars, groupingvar, timevar, modeldata, objectName, its, timeInteraction, higherPolynomials, crs){
  
  
  if (timeInteraction == TRUE & higherPolynomials == TRUE){
    fmla <- as.formula(paste0(depvar, ' ~ ', paste(paste0(indepvars, ' * ', c(timevar, paste0(timevar, '2'), paste0(timevar, '3'))), collapse = " + "), ' + ', 
                              # paste(indepvars, collapse = " + "), ' + ', 
                              # paste0('I(', timevar, '^2)'), '+', paste0('I(', timevar, '^3)'), '+',
                              '(', timevar, '|', groupingvar, ')'))
  } else if (timeInteraction == FALSE & higherPolynomials == TRUE){
    fmla <- as.formula(paste0(depvar, ' ~ ', paste(indepvars, collapse = " + "), ' + ', timevar, ' + ',
                              #paste(indepvars, collapse = " + "), ' + ', 
                              paste0('I(', timevar, '^2)'), '+', paste0('I(', timevar, '^3)'), '+',
                              '(', timevar, '|', groupingvar, ')'))
  } else if (timeInteraction == TRUE & higherPolynomials == FALSE){
    fmla <- as.formula(paste0(depvar, ' ~ ', paste(paste0(indepvars, ' * ', timevar), collapse = " + "), ' + ', 
                              #paste(indepvars, collapse = " + "), ' + ', 
                              '(', timevar, '|', groupingvar, ')'))
    
  } else if (timeInteraction == FALSE & higherPolynomials == FALSE){
    fmla <- as.formula(paste0(depvar, ' ~ ', paste(indepvars, collapse = " + "), ' + ', timevar, ' + ',
                              #paste(indepvars, collapse = " + "), ' + ', 
                              '(', timevar, '|', groupingvar, ')'))
  }
  
  
  
  if (class(modeldata[, depvar]) == 'factor'){
    fam <- 'categorical'
  } else if (class(modeldata[, depvar]) %in% c('numeric', 'integer')){
    fam <- 'gaussian'
  }
  
  (prior <- get_prior(fmla, data = modeldata, family = fam))
  
  fit <- brm(fmla, data = modeldata, family = fam, inits = 'random', # prior = prior, 
             thin = 10, warmup = .1*its, iter = its, chains = 2, seed = 1234, cores = crs)
  
  # Whenever you see the warning "There were x divergent transitions after warmup." 
  # you should really think about increasing adapt_delta. To do this, write 
  # control = list(adapt_delta = <x>), where <x> should usually be value between 0.8 
  # (current default) and 1. Increasing adapt_delta will slow down the sampler but 
  # will decrease the number of divergent transitions threatening the validity of your posterior samples.
  
  #system("killall R")
  #gc()
  #closeAllConnections()
  
  assign(objectName, fit)
  
  save(list = objectName, file = paste0(objectName, '.Rdata'))
  
  return(fit)
}

bayesianDiagnostics <- function(fit){
  summ <- summary(fit)
  #plott <- plot(fit, ask = FALSE)
  #predd <- predict(fit)
  #margs <- plot(marginal_effects(fit), ask = FALSE)
  looo <- loo_R2(fit)
  #ppcheck <- pp_check(fit)
  #fit <- add_ic(fit, ic = "R2")
  R2 <- bayes_R2(fit)
  #res <- list(fit = fit, summ = summ, plott = plott, predd = predd,
  #            margs = margs, looo = looo, ppcheck = ppcheck, R2 = R2)
  res <- list(summ, looo, R2)
  return(res)
}

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

sumNA <- function(data){
  nas <- apply(data, 2, function(x){sum(is.na(x))})
  return(nas)
}

cleanChains <- function(){
  system("killall R")
  gc()
}

recoverEventualChildlessness <- function(x){
  x <- x[order(x$Age), ]
  
  # Household childlessness
  if (all(is.na(x$childlessHousehold))){
    x$childlessHH42 <- NA
  } else if (length(which(x$childlessHousehold == 0)) > 0){
    if (any(x$Age[which(x$childlessHousehold == 0)] <= 42)){
      x$childlessHH42 <- 0
    } else {
      x$childlessHH42 <- 1
    }
  } else if (all(na.omit(x$childlessHousehold) == 1)){ # else if (!(any(is.na(x$childlessHousehold))) & all(x$childlessHousehold == 1)){
    x$childlessHH42 <- 1
  }
  
  # Natural childlessness
  if (!(all(is.na(x$childlessNatural)))){
    if (!(is.na(x$childlessNatural[nrow(x)])) & x$childlessNatural[nrow(x)] == 1){
      x$childlessNatural <- rep(1, nrow(x))
    }
    if (length(which(x$childlessNatural == 0)) > 0){
      minn <- min(which(x$childlessNatural == 0))
      x$childlessNatural[minn:nrow(x)] <- 0
    }
    if (!(all(is.na(x$childlessNatural[which(x$Age %in% c(42:46))]))) & length(which(x$childlessNatural[which(x$Age %in% c(42:46))] == 1)) > 0){
      x$childlessNat42 <- rep(1, nrow(x))
    }
    if (!(all(is.na(x$childlessNatural[which(x$Age %in% c(42:46))]))) & length(which(x$childlessNatural[which(x$Age %in% c(42:46))] == 0)) > 0){
      x$childlessNat42 <- rep(0, nrow(x))
    }
  }
  return(x)
}

# Multiple methods for imputation
bigImputer <- function(imputationMethod, data, imp.vars, pred.vars, imp.rows, maxt, pmiss, seed){
  
  if (is.null(imp.rows)){
    imp.rows <- 1:nrow(data)
  }
  
  if (is.null(maxt)){
    maxt <- 5
  }
  
  if (is.null(pmiss)){
    pmiss <- 5
  }
  
  set.seed(seed)
  
  # Option 1: Multiple imputation
  if (imputationMethod == 'multipleImputation'){
    
    ini <- mice(data[imp.rows, ], maxit = 0, print = F)
    pred <- ini$pred
    pred[which(!(names(pred[1,]) %in% imp.vars)),] <- 0
    fit.imp <- mice(data[imp.rows, ], pred = pred, maxit = maxt, m = pmiss)
    data.imp <- list()
    for (i in seq_len(pmiss)){
      data.imp[[i]] <- mice::complete(fit.imp, i)
    }
    
    # Option 2: kNN imputation  
  } else if (imputationMethod == 'knnImputation'){
    data.imp <- VIM::kNN(data[imp.rows, ], imp_var = FALSE, dist_var = pred.vars, variable = imp.vars)
    
    # Option 3: Neural network imputation 
  } else if (imputationMethod == 'neuralNetwork'){
    
    print('Warning: This option is deprecated.')
    
    data.imp <- data.frame(matrix(NA, nrow = length(imp.rows), ncol = length(imp.vars)))
    for (i in 1:length(imp.vars)){
      fit.imp <- nnet(as.formula(paste0(imp.vars[i], ' ~ ', paste(imp.vars[-c(i)], collapse = ' + '))), 
                      data = data[imp.rows, ], size  = 2, maxit = 1000)
      data.imp[, i] <- predict(fit.imp, data[imp.rows, ])
    }
    
    # data.imp <- monmlp.fit(y = data.matrix(response_matrix), 
    #                        x = data.matrix(dummy_cols(data[imp.rows, indepvars])), hidden1 = 2)
    
    # Option 4: Random forest imputation  
  } else if (imputationMethod == 'randomForest'){
    fit.imp <- missForest(data[imp.rows, imp.vars], ntree = 10, mtry = 2)
    data.imp <- fit.imp$ximp
    data.imp <- cbind(fit.imp$ximp, data[imp.rows, which(!(names(data) %in% imp.vars))])
  }
  
  # Give an error when there are still NA values in the imputed data
  # if (class(data.imp) == 'list'){
  #   NAS <- unlist(lapply(data.imp, function(x){sum(is.na(x[, imp.vars]))}))
  #   if (any(NAS > 0)){
  #     print("Error: The imputed data set still has NA values.")
  #     break 
  #   }
  # } else {
  #   if (sum(is.na(data.imp[, imp.vars])) > 0){
  #     print("Error: The imputed data set still has NA values.")
  #     break 
  #   }
  # }
  
  # Return the result
  return(data.imp)
  
}

# Compute eventual childlessness measures from continuous childlessness
recoverChildlessness <- function(x){
  
  x <- x[order(x$Age), ]
  
  # Household childlessness
  if (all(is.na(x$childlessHousehold))){
    x$childlessHH42 <- NA
  } else {
    if (all(is.na(x$childlessHousehold[which(x$Age <= 42)]))){
      x$childlessHH42 <- NA
    } else {
      if (any(na.omit(x$childlessHousehold[which(x$Age <= 42)]) == 0)){
        x$childlessHH42 <- 0
      } else {
        x$childlessHH42 <- 1
      }
    }
  }
  
  # Natural childlessness
  ind <- which(x$childlessNatural == 1)
  if (length(ind) > 0){
    mmax <- max(ind)
    x$childlessNatural[1:mmax] <- 1
  }
  ind <- which(x$childlessNatural == 0)
  if (length(ind) > 0){
    mmin <- min(ind)
    x$childlessNatural[mmin:nrow(x)] <- 0
  }
  
  if (all(is.na(x$childlessNatural))){
    x$childlessNat42 <- NA
  } else {
    if (all(is.na(x$childlessNatural[which(x$Age <= 42)]))){
      if (any(na.omit(x$childlessNatural[which(x$Age > 42)]) == 1)){
        x$childlessNat42 <- 1
      } else {
        x$childlessNat42 <- 0
      }
    } else {
      if (any(na.omit(x$childlessNatural[which(x$Age <= 42)]) == 0)){
        x$childlessNat42 <- 0
      } else {
        x$childlessNat42 <- 1
      }
      
    }
  }
  
  return(x)
  
}

# Recover computed variables after imputation
imputationRecovery <- function(originalData, imputedData, inflationData){
  
  # ID
  imputedData['ID'] <- originalData[, 'ID']
  
  # Monthly gross pay and transformations
  imputedData['logMonthlyGrossPay'] <- log1p(originalData[, 'monthlyGrossPay'])
  imputedData['correctedMonthlyGrossPay'] <- originalData[, 'monthlyGrossPay'] * inflationData$Cumulative[match(originalData$Year, inflationData$Year)]
  imputedData['logCorrectedMonthlyGrossPay'] <- log1p(imputedData$correctedMonthlyGrossPay)
  
  # Eventual childlessness
  imputedData['childlessHH42'] <- imputedData['childlessNat42'] <- NA
  IDsplit <- split(imputedData, imputedData$ID)
  IDsplit <- IDsplit[which(unlist(lapply(IDsplit, nrow)) > 0)]
  imputedData <- rbind.fill(lapply(IDsplit, recoverChildlessness))
  
  # Binary health
  lavs <- c("Excellent" = "Good", "Very good" = "Good", "Good" = "Good", 
            "Fair" = "Bad", "Poor" = "Bad", "Very poor" = "Bad")
  imputedData['healthStatus_binary'] <- revalue(as.factor(as.character(originalData$healthStatus)), lavs)
  
  # Childlessness type
  IDsplit <- split(imputedData, imputedData$ID)
  IDsplit <- IDsplit[which(unlist(lapply(IDsplit, nrow)) > 0)]
  imputedData <- rbind.fill(lapply(IDsplit, typeChildlessness))
  
  return(imputedData)
}

# Add time variables
addTime <- function(x){
  x['Time'] <- (x$Age - min(x$Age))
  x['Time2'] <- x$Time^2 / 10
  x['Time3'] <- x$Time^3 / 1000
  y <- poly(x$Time, 3)
  x['polyTime1'] <- y[, 1]
  x['polyTime2'] <- y[, 2]
  x['polyTime3'] <- y[, 3]
  return(x)
}

# Recover type of childlessness
typeChildlessness <- function(x){
  x['typeChildless'] <- NA
  volVars <- c('Sterilised', 'No desire', 'Career focus', 'Partner relationship')
  circVars <- c('Financial situation', 'Health situation', 'Housing situation', 'No partner', 'Partner has children',
                'Wanted but not got round to it')
  if (any(na.omit(x$childlessHH42) == 0)){
    x$typeChildless <- rep('Children', nrow(x))
  } else {
    if (any(na.omit(x$reasonChildless) == 'Infertility')){
      x$typeChildless <- rep('Involuntary', nrow(x))
    } else {
      if (any(na.omit(x$childIntentions) == 'No more children')){
        x$typeChildless <- rep('Voluntary', nrow(x))
      } else {
        if (all(na.omit(x$maritalStatus) == 'Single and never married')){
          x$typeChildless <- rep('Circumstantial', nrow(x))
        } else {
          if (!(all(na.omit(x$maritalStatus) %in% c('Cohabiting', 'Single and never married', 'Married')))){
            x$typeChildless <- rep('Circumstantial', nrow(x))
          } else {
            if (any(na.omit(x$childIntentions) == 'More children')){
              x$typeChildless <- rep('Involuntary', nrow(x))
            }
          }
        }
      }
    }
  }
  return(x)
}

backwardsImputeEducation <- function(x){
  uni <- unique(na.omit(x$education_binary))
  x['education_binary_bwimp'] <- NA
  if (length(uni) == 1){
    x$education_binary_bwimp <- rep(uni, nrow(x))
  } else if (length(uni) > 1){
    x$education_binary_bwimp <- rep("NVQ4+", nrow(x))
  }
  return(x)
}

averageObservations <- function(data){
  splits <- split(data, data[, 'ID'])
  splits <- splits[which(unlist(lapply(splits, nrow)) > 0)]
  rows <- unlist(lapply(splits, nrow))
  res <- c(mean(rows), sd(rows), min(rows), max(rows))
  return(res)
}

# Function to prepare the data on the basis of requirements
dataPrep <- function(data, age16 = c('delete', 'missing', 'zero'), 
                     class_lifesat = c('ordinal', 'skew_normal', 'gaussian'),
                     remove_parents_26 = c(TRUE, FALSE),
                     censoring_removal = c("none", "at_least_one_forties", "all_ages")){
  
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
  
  splits = split(data, data$ID)
  splits = splits[unlist(lapply(splits, nrow)) > 0]
  
  if (censoring_removal == "at_least_one_forties"){
    obs = unlist(lapply(splits, function(x){any(c(42, 46) %in% x$Age)}))
    keep_ids = names(obs)[obs == TRUE]
    data = data[data$ID %in% keep_ids, ]
  } else if (censoring_removal == "all_ages"){
    obs = unlist(lapply(splits, function(x){all(seq(26, 46, 4) %in% x$Age)}))
    keep_ids = names(obs)[obs == TRUE]
    data = data[data$ID %in% keep_ids, ]
  }
  
  # Change classes of variables into the correct types
  data$Age <- as.integer(data$Age)
  data$logCorrectedMonthlyGrossPay <- as.numeric(data$logCorrectedMonthlyGrossPay)
  # data$childlessHH42 <- factor(data$childlessHH42)
  data$Time <- as.integer(data$Time)
  data$maritalStatus <- factor(data$maritalStatus)
  data$maritalStatusCohabitation <- factor(data$maritalStatusCohabitation)
  data$maritalStatus_combined <- factor(data$maritalStatus_combined)
  data$healthStatus <- factor(data$healthStatus)
  data$healthStatus_binary <- factor(data$healthStatus_binary)
  data$economicActivity <- factor(data$economicActivity)
  data$education_binary <- factor(data$education_binary)
  data$education_binary_bwimp <- factor(data$education_binary_bwimp)
  data$lifeSatisfaction <- factor(data$lifeSatisfaction, ordered = TRUE)
  data$numberChildrenNatural = as.integer(data$numberChildrenNatural)
  data$numberChildrenHousehold = as.integer(data$numberChildrenHousehold)
  data$numberChildrenNaturalCategorical = factor(data$numberChildrenNaturalCategorical)
  data$numberChildrenHouseholdCategorical = factor(data$numberChildrenHouseholdCategorical)
  data$eventualChildlessness = factor(data$eventualChildlessness)         
  data$eventualTypeChildless = factor(data$eventualTypeChildless)
  data$ID = as.integer(data$ID)
  
  if (class_lifesat != 'ordinal'){
    data$lifeSatisfaction <- as.integer(data$lifeSatisfaction) - 1
  } 
  
  # Check if NA values are correct type
  vars <- c('Age', 'eventualTypeChildless', 'eventualChildlessness', 'lifeSatisfaction', 'logCorrectedMonthlyGrossPay', 'education_binary_bwimp', 
            'maritalStatus_combined', 'maritalStatusCohabitation', 'healthStatus_binary', 'economicActivity')
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

# -------------------------------------------------
# Analysis of parents by age 26
# -------------------------------------------------

childBeforeAge = function(x, age){
  
  if (any(x$Age == 26)){
    if (!is.na(x$childlessHousehold[x$Age == 26])){
      if (x$childlessHousehold[x$Age == 26] == 1){
        var = "respondent childless at age"
      } else {
        var = "respondent parent at age"
      }
    } else {
      var = "don't know childlessness status at age"
    }
  } else {
    var = "respondent not observed at age"
  }
  
  y = data.frame(ID = unique(x$ID),
                 targetAge = age,
                 childBefore = var)
  
  return(y)
  
}

# -------------------------------------------------
# Analysis of parents by age 26
# -------------------------------------------------

childBefore26 = function(x) { x$numberChildrenNatural[x$Age == 26] > 0 }

