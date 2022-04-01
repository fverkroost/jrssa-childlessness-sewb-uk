# Unique characters in string
uniqchars <- function(x) unique(strsplit(x, "")[[1]]) 

# Give all ID variables the same name for merging
IDequal <- function(x){
  nams <- tolower(names(x))
  ind <- which(nams == "bcsid")
  names(x)[ind] <- "BCSID"
  return(x)
}

# Indicator for whether at least one child was born since last sweep
indicatorChildBorn = function(x){
  x = x[order(x$Age), ]
  d = diff(as.integer(x$numberChildrenNatural))
  x['childArrivedSinceLastWave'] = NA
  x$childArrivedSinceLastWave[1] = ifelse(x$numberChildrenNatural[1] == 0, 0, NA)
  if (length(d) > 0){
    x$childArrivedSinceLastWave[2:nrow(x)] = ifelse(d > 0, 1, ifelse(is.na(d), NA, 0))
  }
  return(x)
}

# Load in SPSS data
loadData = function(x){
  dataFile <- try(foreign::read.dta(x, convert.factors = TRUE), silent = TRUE)
  if (inherits(dataFile, "try-error")){
    dataFile <- try(data.frame(haven::read_dta(x)), silent = TRUE)
  }
  return(dataFile)
}

# Check when respondents are observed
checkOberservedAges = function(x){
  
  ages = paste0(sort(x$Age), collapse = ' & ')
  return(data.frame(ID = unique(x$ID), sex = unique(x$Sex), ages = ages))
  
}

# Determine eventual childlessness from age 42 or 46
determineEventualChildlessness = function(x){
  
  if (46 %in% x$Age){
    
    res = ifelse(x$numberChildrenNatural[x$Age == 46] == 0, 'Childless',
                 ifelse(x$numberChildrenNatural[x$Age == 46] > 0, 'Parent', NA))
    
  } else if (42 %in% x$Age) {
    
    res = ifelse(x$numberChildrenNatural[x$Age == 42] == 0, 'Childless',
                 ifelse(x$numberChildrenNatural[x$Age == 42] > 0, 'Parent', NA))
    
  } else {
    
    res = "Not observed in forties"
    
  }
  
  x['eventualChildlessness'] = rep(res, nrow(x))
  
  return(x)
  
}

# Recover the type of childlessness from age 42
recoverTypeChildlessness = function(x){
  
  if (all(is.na(x$reasonChildless_combined))){
    
    if (unique(x$eventualChildlessness) == "Childless"){
      
      res = "Unknown childless"
      
    } else if (unique(x$eventualChildlessness) == "Parent") {
      
      res = "Parent"
      
    } else {
      
      res = "Not observed in forties"
      
    }
    
  } else {
    
    res = unique(na.omit(x$reasonChildless_combined))
    
  }
  
  x['eventualTypeChildless'] = rep(res, nrow(x))
  
  return(x)
  
}

# Recover the number of children from age 42 to earlier years
recoverNumberChildren <- function(x, variable){
  if (!(all(is.na(x[, variable])))){
    ind <- max(which(!is.na(x[, variable])))
    x[, variable] <- rep(x[ind, variable], nrow(x))
  }
  return(x)
}

# Retain only relevant variables
relevantVariables <- function(x, keepvars){
  addVars <- keepvars[which(!(keepvars %in% names(x)))]
  x[addVars] <- NA
  x <- x[, keepvars]
  return(x)
}

# Compute eventual childlessness measures from continuous childlessness
recoverChildlessness <- function(x, age){
  
  x <- x[order(x$Age), ]
  
  # Household childlessness
  if (all(is.na(x$childlessHousehold))){
    x$childlessHH42 <- NA
  } else {
    if (all(is.na(x$childlessHousehold[which(x$Age <= age)]))){
      x$childlessHH42 <- NA
    } else {
      if (any(na.omit(x$childlessHousehold[which(x$Age <= age)]) == 0)){
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
    if (all(is.na(x$childlessNatural[which(x$Age <= age)]))){
      if (any(na.omit(x$childlessNatural[which(x$Age > age)]) == 1)){
        x$childlessNat42 <- 1
      } else {
        x$childlessNat42 <- 0
      }
    } else {
      if (any(na.omit(x$childlessNatural[which(x$Age <= age)]) == 0)){
        x$childlessNat42 <- 0
      } else {
        x$childlessNat42 <- 1
      }
      
    }
  }
  
  return(x)
  
}

# Reduce number of levels in malaise variable
compressMalaise <- function(x){
  
  y <- ifelse(x %in% c('Yes', 'YES', 'Most of the time', '1'), 1,
              ifelse(x %in% c('No', 'NO', 'Rarely or never', '2'), 0, 
                     ifelse(x == 'Some of the time', 0.5, NA)))
  
  return(as.numeric(y))
}

backwardsImputeEducation <- function(x){
  uni <- unique(na.omit(x$education_binary))
  x['education_binary_bwimp'] <- NA
  if (length(uni) == 1){
    x$education_binary_bwimp <- rep(uni, nrow(x))
  } else if (length(uni) > 1){
    edus = as.numeric(gsub("\\D", "", uni))
    edus = edus[which(!grepl("one", edus))]
    sign = ifelse(max(edus) == 3, "-", "+")
    x$education_binary_bwimp <- rep(paste0("NVQ", max(edus), sign), nrow(x))
  }
  return(x)
}
