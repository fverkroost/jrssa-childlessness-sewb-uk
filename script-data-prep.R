# ----------------------------------------------------------------------
# Load the raw data 
# ----------------------------------------------------------------------

# Source functions
source('functions-data-prep.R')

# Specify path and file names
path = "data-raw/BCS/"
files = c("bcs5derived", "bcs96x", "bcs6derived", "bcs2000", "bcs7derived", "bcs_2004_followup", "bcs8derived", 
          "bcs_2008_followup", "bcs70_2012_derived", "bcs70_2012_flatfile", "bcs_age46_main")

# Load the data into R and make ID variable same across surveys (BCSID)
BCSfiles = lapply(paste0(path, files, ".dta"), loadData)
names(BCSfiles) = files
BCSfiles = lapply(BCSfiles, IDequal)

# Merge flat and derived data for each year into single file and save in list element
BCS = list(
  merge(BCSfiles$bcs5derived, BCSfiles$bcs96x, by = 'BCSID', all = TRUE),
  merge(BCSfiles$bcs6derived, BCSfiles$bcs2000, by = 'BCSID', all = TRUE),
  merge(BCSfiles$bcs7derived, BCSfiles$bcs_2004_followup, by = 'BCSID', all = TRUE),
  merge(BCSfiles$bcs8derived, BCSfiles$bcs_2008_followup, by = 'BCSID', all = TRUE),
  merge(BCSfiles$bcs70_2012_derived, BCSfiles$bcs70_2012_flatfile, by = 'BCSID', all = TRUE),
  BCSfiles$bcs_age46_main
)

# Check whether merging was done correctly
unlist(lapply(BCSfiles, nrow))
unlist(lapply(BCS, nrow))

# Keep only the relevant variables per data set
BCSvarlist <- list(
  c('BCSID', 'sex', 'BD5GOR', 'BD5CNTRY', 'b960666', 'b960667', 'BD5MAL', 'BD5MALG', paste0('b9606', 37:60), 'empstat', 
    'rgsc91', 'wklypayc', 'hqual26d', 'depchild', 'haschild', 'b960432', 'b960322', 'eldest', 'eldest2', 'b960321', 'numkids'),
  c('BCSID', 'cmsex', 'BD6GOR', 'BD6CNTRY', 'lifesat1', 'BD6MAL', 'BD6MALG', paste0('mal', sprintf("%02d", 1:24)),
    'econact', 'sc', 'cgroprd', 'cgropay', 'HINVQ00', 'anychd', 'ownchild', 'infertlc', 'hlthgen', 'marstat2', 'ms', 
    'hhsize', 'dmsppart', paste0('reltoke', c('y', 2:9)), "prega",   "prega2",  "prega3",  "prega4",  "prega5",
    "prega6",  "prega7",  "prega8" , "prega11" ,"prega12", "prega16", "prega17" ,"prega21", "prega22",
    "prega23" ,"prega26", "prega27", "prega31", "prega36"),
  c('BCSID', 'bd7sex', 'BD7GOR', 'BD7CNTRY', 'b7lifet1', 'BD7MAL', 'BD7MALG', paste0('b7mal', sprintf("%02d", c(2, 3, 5, 9, 12, 14, 16, 20, 21))), 
    'b7zotha0', 'b7sc', 'b7cgropd', 'b7cgropy', 'BD7HNVQ', 'bd7nchhh', 'bd7ochhh', 'b7chchk', 'b7khlstt', 'b7marst2', 'bd7ms',
    "bd7nchhh", "b7chchk",  "b7chchk2", "b7chchk3", "b7chchk4", "b7chchk5"),
  c('BCSID', 'bd8sex', 'BD8GOR', 'BD8CNTRY', 'BD8ECACT', 'b8cgropd', 'b8cgropy', 'BD8HNVQ', 'b8anychd',
    'b8ownchd', 'b8abint101', 'b8hlthgn', 'bd8ms', 'b8sc', 'b8cohab', 'b8numch', "bd8nchhh", 
    "b8abre02" ,"b8abre03", "b8abre04", "b8abre05", "b8abre06", "b8abre07", "b8abre08", "b8abre09",
    "b8abre16", "b8abre17" ,"b8abre18", "b8abre19", "b8abre20","b8abre21"),
  c('BCSID', 'B9CMSEX', 'BD9GOR', 'BD9CNTRY', 'B9LIFST1', 'BD9MAL', 'BD9MALG', paste0('B9SCQ41', toupper(paste(letters[1:9]))),
    'BD9ECACT', 'B9CSC', 'B9GROP', 'B9GROA', 'BD9HNVQ', 'BD9NUMCH', 'BD9TOTCE', 'B9WHNC2', 'B9HLTHGN', 'BD9MS', 'BD9COHAB', 'BD9TOTOC',
    paste0("B9WHNC0", 1:9), paste0("B9WHNC", 10:18)),
  c('BCSID', 'B10CMSEX', 'BD10GOR', 'BD10CNTRY', 'B10LIFST1', 'BD10MAL', 'BD10MALG', paste0('B10Q28', toupper(paste(letters[1:9]))),
    'BD10ECACT', 'B10NSSECAN', 'B10GROP', 'B10GROA', 'BD10HNVQ', 'BD10NUMCH', 'BD10TOTCE', 'B10HLTHGN', 'BD10MS', 'BD10COHAB', 'BD10TOTOC')
)
for (i in 1:length(BCSvarlist)){
  BCS[[i]] <- BCS[[i]][, match(BCSvarlist[[i]], names(BCS[[i]]))]
}

# Convert all variables in all data sets to character
BCS <- lapply(BCS, function(x){x %>% mutate_all(as.character)})

# Rename list elements of BCS
names(BCS) = c("1996age26", "2000age30", "2004age34", "2008age38", "2012age42", "2016age46")

# Recover variables
source('recover-variables.R')

# Rename variables accordingly across sweeps
BCSnamelist <- list(
  c('ID', 'Sex', 'Region', 'Country', 'b960666', 'b960667', 'totalMalaise', 'groupedMalaise', paste0('Malaise', 1:24), 
    'economicActivity', 'socialClass', 'wklypayc', 'highestQualification', 'depchild', 'haschild', 'healthStatus', 'maritalStatus', 
    'eldest', 'eldest2', 'b960321', "numkids",'Year', 'Age', 'monthlyGrossPay', 'childlessNatural', 'childlessHousehold', 
    "numberChildrenHousehold", "numberChildrenNatural", 'cohabitation', 'lifeSatisfaction'),
  c('ID', 'Sex', 'Region', 'Country', 'lifeSatisfaction', 'totalMalaise', 'groupedMalaise', paste0('Malaise', 1:24),
    'economicActivity', 'socialClass', 'payPeriod', 'payAmount', 'highestQualification', 'anychd', 'ownchild', 
    'childIntentions', 'healthStatus', 'maritalStatus', 'ms', "hhsize", "dmsppart", 
    "prega", paste0("prega", c(2:8, 11:12, 16:17, 21:23, 26:27, 31, 36)),
    'Year', 'Age', 'childlessNatural', 'childlessHousehold', 'cohabitation', "numberChildrenHousehold", "numberChildrenNatural"),
  c('ID', 'Sex', 'Region', 'Country', 'lifeSatisfaction', 'totalMalaise', 'groupedMalaise', paste0('Malaise', c(2, 3, 5, 9, 12, 14, 16, 20, 21)), 
    'economicActivity', 'socialClass', 'payPeriod', 'payAmount', 'highestQualification', 'bd7nchhh', 'bd7ochhh', 'b7chchk', 
    'healthStatus', 'maritalStatus', 'bd7ms', "bd7nchhh.1" ,  "b7chchk.1", "b7chchk2",               
    "b7chchk3","b7chchk4", "b7chchk5", 'Year', 'Age', 'childlessNatural', 'childlessHousehold', 'cohabitation', 
    "numberChildrenHousehold", "numberChildrenNatural"),
  c('ID', 'Sex', 'Region', 'Country', 'economicActivity', 'payPeriod', 'payAmount', 'highestQualification', 'b8anychd',
    'b8ownchd', 'b8abint101', 'healthStatus','maritalStatus', 'socialClass', 'b8cohab',"b8numch", "bd8nchhh",
    paste0("b8abre0", 2:9), paste0("b8abre", 16:21),
    'Year', 'Age', 'childlessNatural', 'childlessHousehold', "numberChildrenHousehold", "numberChildrenNatural", 'cohabitation'),
  c('ID', 'Sex', 'Region', 'Country', 'lifeSatisfaction', 'totalMalaise', 'groupedMalaise', paste0('Malaise', c(2, 3, 5, 9, 12, 14, 16, 20, 21)),
    'economicActivity', 'socialClass', 'payPeriod', 'payAmount', 'highestQualification', 'BD9NUMCH', 'BD9TOTCE', 'B9WHNC2', 
    'healthStatus', 'maritalStatus', 'BD9COHAB',"BD9TOTOC", sprintf("B9WHNC%02d", 1:18), 'Year', 'Age', 
    'childlessNatural', 'childlessHousehold', 'cohabitation', 'eventualNumberChildren42', "numberChildrenHousehold", 
    "numberChildrenNatural", "reasonChildless"),
  c('ID', 'Sex', 'Region', 'Country', 'lifeSatisfaction', 'totalMalaise', 'groupedMalaise', paste0('Malaise', c(2, 3, 5, 9, 12, 14, 16, 20, 21)),
    'economicActivity', 'socialClass', 'payPeriod', 'payAmount', 'highestQualification', 'BD10NUMCH', 'BD10TOTCE', 
    'healthStatus', 'maritalStatus', 'BD10COHAB',  "BD10TOTOC", 'Year', 'Age', 'childlessNatural', 'childlessHousehold', 'cohabitation', 
    'eventualNumberChildren46', "numberChildrenHousehold", "numberChildrenNatural")
)

# Change names of data sets
for (i in 1:length(BCS)){
  names(BCS[[i]]) <- BCSnamelist[[i]]
}

# Retain only necessary variables and fill up in case it's empty
keepvars <- c('ID', 'Sex', 'Region', 'Country', 'Year', 'Age', 'lifeSatisfaction',
              'totalMalaise', 'groupedMalaise', paste0('Malaise', c(1:24)), 
              'economicActivity', 'socialClass', 'payPeriod', 'payAmount', 'monthlyGrossPay',
              'highestQualification', 'childlessHousehold', 'childlessNatural', 'cohabitation',
              'reasonChildless', 'childIntentions', 'healthStatus', 'maritalStatus',
              'eventualNumberChildren42', 'eventualNumberChildren46', 'numberChildrenHousehold', 'numberChildrenNatural')
BCS <- lapply(BCS, relevantVariables, keepvars)

# Convert all variables in all data sets to character
BCS <- lapply(BCS, function(x){x %>% mutate_all(as.character)})

# ----------------------------------------------------------------------
# Merge sweeps and clean up the variables
# ----------------------------------------------------------------------

# Merge all data sets into one big data set
df <- rbind(rbind.fill(BCS))

# Impute number of children at maximum age (usually 46) to earlier years
IDsplit <- split(df, df$ID)
IDsplit = IDsplit[unlist(lapply(IDsplit, nrow)) > 0]
df <- rbind.fill(lapply(IDsplit, recoverNumberChildren, 'eventualNumberChildren46'))

# Make all variables correct types and classes
ints <- c('Age', 'Year', 'totalMalaise')
nums <- c('payAmount')
df[which(names(df) %in% nums)] <- lapply(df[which(names(df) %in% nums)], as.numeric)
df[which(names(df) %in% ints)] <- lapply(df[which(names(df) %in% ints)], as.integer)
df$numberChildrenHousehold[df$numberChildrenHousehold < 0] = NA 
df$numberChildrenNatural[df$numberChildrenNatural < 0] = NA 

# To match different categories to the one correct one, use manually constructed matching file
# Sex
match_file = read.xlsx('matchingControls.xlsx', "Sex")
df$Sex <- as.factor(match_file$Target[match(df$Sex, match_file$Original)])

# Life satisfaction
match_file = read.xlsx('matchingControls.xlsx', "lifeSatisfaction")
df$lifeSatisfaction <- as.numeric(as.character(match_file$Target[match(df$lifeSatisfaction, match_file$Original)])) 

# Economic activity
table(df$socialClass, df$economicActivity)
table(df$economicActivity, df$Age)
match_file = read.xlsx('matchingControls.xlsx', "economicActivity")
df$economicActivity <- as.factor(match_file$Target[match(df$economicActivity, match_file$Original)])

# Social class
match_file = read.xlsx('matchingControls.xlsx', "socialClass")
df$socialClass <- as.factor(match_file$Target[match(df$socialClass, match_file$Original)])

# Recode unemployed individuals'social class
df$socialClass <- as.character(df$socialClass)
df$socialClass[which(grepl('Unemployed', df$economicActivity))] <- 'Unemployed'
df$socialClass <- as.factor(df$socialClass)

# Pay amount and period for monthly wage
olfvars <- as.character(unique(df$economicActivity[which(grepl("OLF", df$economicActivity))]))
hist(df$payAmount[which(df$economicActivity %in% olfvars)])
match_file = read.xlsx('matchingControls.xlsx', "payPeriod")
df$payPeriod <- as.factor(match_file$Target[match(df$payPeriod, match_file$Original)])

df$payAmount[which(df$payAmount < 0 & df$economicActivity %in% olfvars)] <- 0
df$payAmount[which(df$payAmount < 0 & !(df$economicActivity %in% olfvars))] <- NA
df$payAmount[which(df$payAmount > 1e6 | all(uniqchars(as.character(df$payAmount)) %in% c("8", "9")))] <- NA # Only missings (9999999/9999998) or very large numbers (3 cases)

age26income <- df$monthlyGrossPay[which(df$Age == 26)]
df['monthlyGrossPay'] <- ifelse(df$payPeriod == 'Week', df$payAmount * 4.34524, 
                                ifelse(df$payPeriod == 'Fortnight', df$payAmount * 2.17262, 
                                       ifelse(df$payPeriod == '4 Weeks', df$payAmount * 4.34524 / 4, 
                                              ifelse(df$payPeriod == 'Month', df$payAmount, 
                                                     ifelse(df$payPeriod == '3 Months', df$payAmount / 3,
                                                            ifelse(df$payPeriod == '6 Months', df$payAmount / 6,
                                                                   ifelse(df$payPeriod == 'Day', df$payAmount * 5 * 4.34524,
                                                                          ifelse(df$payPeriod == 'Year', df$payAmount / 12, 
                                                                                 ifelse(df$payPeriod == '2 Months', df$payAmount / 2, 
                                                                                        ifelse(df$payPeriod == '2 Weeks', df$payAmount * 4.34524 / 2, 
                                                                                               ifelse(df$payPeriod == '3 Weeks', df$payAmount * 4.34524 / 3,  NA)))))))))))
df$monthlyGrossPay[which(df$Age == 26)] <- age26income
df$monthlyGrossPay[which(df$monthlyGrossPay < 0)] <- NA
df$monthlyGrossPay <- as.numeric(df$monthlyGrossPay)
df$monthlyGrossPay[df$Age == 16] <- 0

# Health status
match_file = read.xlsx('matchingControls.xlsx', "healthStatus")
df$healthStatus <- as.factor(match_file$Target[match(df$healthStatus, match_file$Original)])

# Cohabitation
match_file = read.xlsx('matchingControls.xlsx', "cohabitation")
df$cohabitation <- as.factor(match_file$Target[match(df$cohabitation, match_file$Original)])

# Marital status (remove civil partner responses to keep only heterosexuals)
# df <- df[which(!(grepl('ivil', df$maritalStatus))),]
match_file = read.xlsx('matchingControls.xlsx', "maritalStatus")
df$maritalStatus <- as.factor(match_file$Target[match(df$maritalStatus, match_file$Original)])

# Survey
df$Survey <- ifelse(df$Age == 16 & df$Year == 1986, 'BCS16',
                    ifelse(df$Age == 21, 'BCS21',
                           ifelse(df$Age == 26, 'BCS26',
                                  ifelse(df$Age == 30, 'BCS30',
                                         ifelse(df$Age == 34, 'BCS34',
                                                ifelse(df$Age == 38, 'BCS38',
                                                       ifelse(df$Age == 42 & df$Year == 2012, 'BCS42',
                                                              ifelse(df$Age == 46 & df$Year == 2016, 'BCS46', NA))))))))

# ----------------------------------------------------------------------
# Recover variables for previous sweeps and/or merge categories
# ----------------------------------------------------------------------

# Recover and match highest qualification throughout surveys
table(df$highestQualification, df$Survey)
match_file = read.xlsx('matchingControls.xlsx', "highestQualification")
df$highestQualification <- as.factor(match_file$Target[match(df$highestQualification, match_file$Original)])

#### Change levels of malaise variables
# Convert to character and check distribution
df[, paste0('Malaise', 1:24)] <- apply(df[, paste0('Malaise', 1:24)], 2, as.character)
table(unlist(lapply(df[, paste0('Malaise', 1:24)], function(x){x})))

# Change levels and remove incomplete responses etc.
df[, paste0('Malaise', 1:24)] <- as.data.frame(lapply(df[, paste0('Malaise', 1:24)], compressMalaise))
table(unlist(lapply(df[, paste0('Malaise', 1:24)], function(x){x})))

# Remove irrelevant/too small categories
# df$healthStatus[which(df$healthStatus == 'Unknown')] <- NA
lavs <- c("Excellent" = "Good", "Very good" = "Good", "Good" = "Good", 
          "Fair" = "Bad", "Poor" = "Bad", "Very poor" = "Bad")
df['healthStatus_binary'] <- plyr::revalue(as.factor(as.character(df$healthStatus)), lavs)

# Make sure categories for working variables correspond
df$highestQualification <- as.character(df$highestQualification)
df$socialClass <- as.character(df$socialClass)
df$economicActivity <- as.character(df$economicActivity)
df$socialClass[which(grepl('Unemployed', df$economicActivity))] <- 'Unemployed'
hist(df$monthlyGrossPay[which(df$socialClass == 'Unemployed')])
df$highestQualification <- factor(df$highestQualification)
df$socialClass <- factor(df$socialClass)
df$economicActivity <- factor(df$economicActivity)

# Remove irrelevant categories and variables
df <- df[, which(!(names(df) %in% c('payAmount', 'payPeriod')))]
df$monthlyGrossPay[which(grepl('Unemployed', df$economicActivity))] <- 0
hist(df$monthlyGrossPay[which(grepl('Unemployed', df$economicActivity))])
df <- df[which(!(is.na(df$Sex))),]
df$monthlyGrossPay[which(df$monthlyGrossPay > 1e5)] <- 1e5

# ----------------------------------------------------------------------
# Correct income for inflation
# ----------------------------------------------------------------------

# Income inflation all proportional to wages in 2019 and log(1 + x)
splits <- split(df, df$Survey)
empties <- which(unlist(lapply(splits, function(x){all(unique(x$monthlyGrossPay) %in% c(0,NA))})))
df$monthlyGrossPay[which(df$Survey %in% empties)] <- NA
inflation_data = read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/cdko/mm23&series=&fromYear=1947&toYear=2019&frequency=years")
names(inflation_data) = c("year", "cpi")
cpi_2019 = as.numeric(as.character(inflation_data$cpi[inflation_data$year == "2019"]))
df['correctedMonthlyGrossPay'] = NA
for (i in 1:nrow(df)){
  cpi_year = as.numeric(as.character(inflation_data$cpi[match(df$Year[i], inflation_data$year)]))
  df$correctedMonthlyGrossPay[i] = df$monthlyGrossPay[i] * (cpi_2019 / cpi_year)
}
df['logCorrectedMonthlyGrossPay'] <- log1p(df$correctedMonthlyGrossPay)
df['logMonthlyGrossPay'] <- log1p(df$monthlyGrossPay)

# Compute total malaise score from nine variables in all data
maldf <- df[, paste0('Malaise', c(2, 3, 5, 9, 12, 14, 16, 20, 21))]
maldf <- data.frame(sapply(maldf, as.numeric))
b34mal <- rowSums(maldf)
b34miss <- apply(maldf, 1, function(x){sum(is.na(x))})
malmiss <- b34mal + b34miss
b34malaise <- ifelse((b34miss > 0 & b34mal <=3 & malmiss >= 4) | b34miss == 9, NA, b34mal)
df['finalMalaise'] <- b34malaise

# Make indicators for whether someone was childless at age 42 or later
# df['childlessHH42'] <- df['childlessNat42'] <- NA

# ----------------------------------------------------------------------
# Add information about eventual childlessness and type
# ----------------------------------------------------------------------

# Check when respondents are observed
IDsplit <- split(df, df$ID)
IDsplit <- IDsplit[which(unlist(lapply(IDsplit, nrow)) > 0)]
obsages = rbind.fill(lapply(IDsplit, checkOberservedAges))

# Recover eventual childlessness (decided at age 46 or 42 if not observed at 46)
# df <- rbind.fill(lapply(IDsplit, recoverChildlessness, age = 46))
df = rbind.fill(lapply(IDsplit, determineEventualChildlessness))

# Recover reason for childlessness and group into fewer types
unknown = c("Don't know", "Don't want to answer", "Not applicable", "No particular reason", "Other reason")
vol = c("I have not wanted to have children", "Partner and I have not wanted children", 
        "Won't compromise relationship with part", "I have been focused on my career")
invol = c('Infertility problem (partner)', "Infertility problem (personal)", "Other health reason",
          "Part. sterilised/vasectomy/hysterectomy")
circ = c("My financial sit. made it difficult", "My housing situation made it difficult",
         "My spouse/partner doesn't want children", "Partner has children doesn't want more", 
         "Not met right person to have child with", "Wanted children but not got round to it",
         "Homosexual relationship")
df['reasonChildless_combined'] = ifelse(df$reasonChildless %in% unknown & df$eventualChildlessness == "Childless", "Unknown childless",
                                        ifelse(df$reasonChildless %in% vol & df$eventualChildlessness == "Childless", 'Voluntarily childless',
                                               ifelse(df$reasonChildless %in% invol & df$eventualChildlessness == "Childless", 'Involuntarily childless',
                                                      ifelse(df$reasonChildless %in% circ & df$eventualChildlessness == "Childless", 'Circumstantially childless', 
                                                             ifelse(df$eventualChildlessness == "Parent", "Parent", NA)))))
IDsplit <- split(df, df$ID)
IDsplit <- IDsplit[which(unlist(lapply(IDsplit, nrow)) > 0)]
df = rbind.fill(lapply(IDsplit, recoverTypeChildlessness))

# ----------------------------------------------------------------------
# Add information about time and cohabitation
# ----------------------------------------------------------------------

# Add new time variable
df['Time'] <- as.integer(df$Age) - min(as.integer(df$Age))

# Merge categories in marital status
df$maritalStatus <- as.character(df$maritalStatus)
df$cohabitation <- as.character(df$cohabitation)
df$cohabitation[df$cohabitation == "Not applicable" & df$maritalStatus %in% c("Single and never married", "Separated", "Divorced")] = "Non-cohabiting"
df$cohabitation[df$cohabitation == "Not applicable" & df$maritalStatus %in% c('Remarried', "Married")] = "Cohabiting"
df$cohabitation[which(df$cohabitation == "Not applicable")] = NA
df['maritalStatus_combined'] <- as.character(ifelse(df$maritalStatus %in% c('Remarried', "Married"), '(Re)married', 
                                                    ifelse(df$maritalStatus %in% c("Divorced", "Separated", "Widowed"), "Divorced/separated/widowed", df$maritalStatus)))
match_file = read.xlsx('matchingControls.xlsx', "maritalCohab")
df['maritalStatusCohabitation'] = NA
for (i in 1:nrow(match_file)){
  ind = which(df$maritalStatus_combined == match_file$maritalStatus_combined[i] & df$cohabitation == match_file$cohabitation[i])
  df$maritalStatusCohabitation[ind] = rep(match_file$new[i], length(ind))
}

# Impute education backwards
df['education_binary'] <- ifelse(df$highestQualification %in% c("NVQ4", "NVQ5,6"), 'NVQ4+',
                                 ifelse(df$highestQualification %in% c(paste0("NVQ", 1:3), "None"), "NVQ3-", NA))
splits <- split(df, df$ID)
splits <- splits[which(unlist(lapply(splits, nrow)) > 0)]
df <- rbind.fill(lapply(splits, backwardsImputeEducation))

# Combine economic activity values
df$economicActivity = as.character(df$economicActivity)
df = df[!df$economicActivity %in% c("Other", "Other employment"), ]
df = df[!grepl('self-employed', df$economicActivity), ]
df$economicActivity[grepl("OLF", df$economicActivity)] = "Out of the labour force"
ind = which(df$economicActivity == "Out of the labour force" & is.na(df$logCorrectedMonthlyGrossPay))
df$logCorrectedMonthlyGrossPay[ind] <- 0
df$economicActivity = factor(df$economicActivity)

# ----------------------------------------------------------------------
# Add information about unsuccessful pregnancies and deceased children
# ----------------------------------------------------------------------

# Load data about pregnancies and deceased cildren
child_died = read.dta13('data-raw/BCS/bcs_age46_child_died.dta')
preg_hist = read.dta13('data-raw/BCS/bcs_age46_unsuccessful_pregnancies.dta')
preg30 = read.dta('data-raw/BCS/bcs2000.dta')

# Add unsuccessful pregnancy (assume those not asked have zero)
df['totalNumberTerminations46'] = df['totalNumberStillbirths46'] = df['totalNumberMiscarriages46'] = 0
for (i in 1:nrow(preg_hist)){
  
  ind = which(df$ID == preg_hist$BCSID[i])
  if (length(ind) > 0){
    
    sub = preg_hist[i, grepl("B10SBA", names(preg_hist))]
    df[ind, "totalNumberMiscarriages46"] = rep(length(which(sub == "Miscarriage")), length(ind))
    df[ind, "totalNumberStillbirths46"] = rep(length(which(sub == "Still birth")), length(ind))
    df[ind, "totalNumberTerminations46"] = rep(length(which(sub == "Termination")), length(ind))
    
  }
  
}

# Add deceased children (assume those not asked have zero)
df['totalNumberNaturalChildrenDied46'] = 0
for (i in 1:nrow(child_died)){
  
  ind = which(df$ID == child_died$BCSID[i])
  if (length(ind) > 0){
    
    sub = child_died[child_died$BCSID == child_died$BCSID[i] & child_died$B10DCHRTK == "Own child", ]
    df[ind, 'totalNumberNaturalChildrenDied46'] = rep(nrow(sub), length(ind))
    
  }
  
}

# Add unsuccessful pregnancies at age 30 (assume those not asked have zero)
df['totalNumberTerminations30'] = df['totalNumberStillbirths30'] = df['totalNumberMiscarriages30'] = 0
for (i in 1:nrow(preg30)){
  
  ind = which(df$ID == preg30$bcsid[i])
  if (length(ind) > 0){
    
    sub = preg30[i, grepl("prega", names(preg30)) & !grepl("cgprega", names(preg30))]
    df[ind, "totalNumberMiscarriages30"] = rep(length(which(sub == "Miscarriage")), length(ind))
    df[ind, "totalNumberStillbirths30"] = rep(length(which(sub == "Still birth")), length(ind))
    df[ind, "totalNumberTerminations30"] = rep(length(which(sub == "Abortion")), length(ind))
    
  }
  
}

# Compute total number of unsuccessful pregnancies from numbers at age 30 and 46
df['totalNumberMiscarriages'] = apply(df[, paste0("totalNumberMiscarriages", c(30, 46))], 1, max)
df['totalNumberTerminations'] = apply(df[, paste0("totalNumberTerminations", c(30, 46))], 1, max)
df['totalNumberStillbirths'] = apply(df[, paste0("totalNumberStillbirths", c(30, 46))], 1, max)
unsuc_vars = c(paste0("totalNumberMiscarriages", c(30, 46)), 
               paste0("totalNumberTerminations", c(30, 46)), 
               paste0("totalNumberStillbirths", c(30, 46)))
df = df[, !names(df) %in% unsuc_vars]

# Compute indicator variable for whether at least one child was born since last sweep
splits = split(df, df$ID)
splits = splits[unlist(lapply(splits, nrow)) > 0]
df = rbind.fill(lapply(splits, indicatorChildBorn))

# ----------------------------------------------------------------------
# Correct variable types and save the final data set
# ----------------------------------------------------------------------

# Make sure childless individuals have zero children everywhere
# There seem to be some (36) reporting mistakes where people indicate having a natural child at some point,
# but having none (and also no deceased child) in all of the later sweeps
# As these people are most often never married and with bad health, we assume this is a reporting mistake
df$numberChildrenNatural[df$eventualTypeChildless != "Parent"] = 0
df$childlessNatural[is.na(df$childlessNatural)] = ifelse(df$numberChildrenNatural[is.na(df$childlessNatural)] == 0, 1,
                                                         ifelse(df$numberChildrenNatural[is.na(df$childlessNatural)] > 0, 0, NA))

# Make sure variables are correct levels
facs <- c('Sex', 'Region', 'Country', 'economicActivity', 'highestQualification', 'healthStatus', 'maritalStatus', 'healthStatus_binary', #'childlessHH42', 
          'cohabitation', 'socialClass', 'groupedMalaise', paste0("Malaise", 1:24), 'childlessHousehold', 'childlessNatural', 'reasonChildless',
          "childIntentions", "Survey", "eventualChildlessness", "reasonChildless_combined", "eventualTypeChildless",
          "maritalStatus_combined", "maritalStatusCohabitation" , "education_binary", "education_binary_bwimp", "childArrivedSinceLastWave")
nums <- c('monthlyGrossPay', 'correctedMonthlyGrossPay', 'logCorrectedMonthlyGrossPay', 'logMonthlyGrossPay')
ints <- c('lifeSatisfaction', 'finalMalaise', 'Year', 'Age', 'totalMalaise', "Time",
          "eventualNumberChildren42", "eventualNumberChildren46", "numberChildrenHousehold", "numberChildrenNatural",
          "totalNumberNaturalChildrenDied46", "totalNumberMiscarriages", "totalNumberStillbirths", "totalNumberTerminations")
chars <- c('ID')
df[, ints] <- data.frame(sapply(df[, ints], as.integer))
df[, nums] <- data.frame(sapply(df[, nums], as.numeric))
df[, facs] <- data.frame(sapply(df[, facs], as.factor))
df[, chars] <- data.frame(sapply(df[, chars], as.character))
df["numberChildrenHouseholdCategorical"] = factor(ifelse(df$numberChildrenHousehold == 0, "no children",
                                                         ifelse(df$numberChildrenHousehold == 1, "one child",
                                                                ifelse(df$numberChildrenHousehold == 2, "two children",
                                                                       ifelse(df$numberChildrenHousehold >= 3, "three or more children", NA)))))
df['numberChildrenNaturalCategorical'] = factor(ifelse(df$numberChildrenNatural == 0, "no children",
                                                       ifelse(df$numberChildrenNatural == 1, "one child",
                                                              ifelse(df$numberChildrenNatural == 2, "two children",
                                                                     ifelse(df$numberChildrenNatural >= 3, "three or more children", NA)))))

# Make sure all NA's are coded as NA and not <NA> and convert back to right factor level
df[] <- lapply(df, function(x) {
  is.na(levels(x)) <- levels(x) == NA
  x
})

# Check missingness in the data
missing <- apply(df, 2, function(x){sum(is.na(x))}) / nrow(df) * 100
emptyVars <- names(which(missing > 70))
length(which(!(complete.cases(df[, -which(names(df) %in% emptyVars)])))) / nrow(df) * 100

# Save cleaned data to file
write.csv(df, file = 'dfAfterDataPrep_latest.csv')

# Split data by gender and survey
womenBCS <- subset(df, Sex == 'Female')
menBCS <- subset(df, Sex == 'Male')

# Add time polynomials
womenBCS[paste0('poly_time_', 1:3)] = poly((womenBCS$Age - min(womenBCS$Age)), 3)[, 1:3]
menBCS[paste0('poly_time_', 1:3)] = poly((menBCS$Age - min(menBCS$Age)), 3)[, 1:3]

# Save data
write.csv(womenBCS, file = 'womenBCS_46_latest.csv')
write.csv(menBCS, file = 'menBCS_46_latest.csv')

