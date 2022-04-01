# Add variables for year and age to every BCS survey
ages <- c(26, 30, 34, 38, 42, 46)
years <- 1970 + ages
for (i in 1:length(BCS)){ 
  BCS[[i]]['Year'] <- rep(years[i], nrow(BCS[[i]]))
  BCS[[i]]['Age'] <- rep(ages[i], nrow(BCS[[i]]))
}

# Compute monthly pay variable for age 26 cohort
BCS$`1996age26`['monthlyGrossPay'] <- as.numeric(BCS$`1996age26`$wklypayc) * 4.34524

# Compute highest qualification for age 16 cohort
BCS$`1986age16`['hqual21'] <- BCS$`1991age21`$hqual16[match(BCS$`1986age16`$BCSID, BCS$`1991age21`$BCSID)]

# Compute household and natural childlessness variables for age 26 cohort
BCS$`1996age26`['childlessNatural'] <- ifelse(BCS$`1996age26`$haschild == '1', 0, 
                                              ifelse(BCS$`1996age26`$haschild == '2', 1, NA))
BCS$`1996age26`['childlessHousehold'] <- ifelse(BCS$`1996age26`$depchild == 0, 1, 
                                                ifelse(BCS$`1996age26`$haschild > 0, 0, NA))
BCS$`1996age26`['numberChildrenHousehold'] <- as.integer(as.character(BCS$`1996age26`$depchild))
BCS$`1996age26`['numberChildrenNatural'] = ifelse(BCS$`1996age26`$numkids < 0, NA, as.integer(as.character(BCS$`1996age26`$numkids)))
BCS$`1996age26`['cohabitation'] = ifelse(BCS$`1996age26`$b960321 %in% c("Living as a couple", "Living with spouse"), "Cohabiting",
                                         ifelse(BCS$`1996age26`$b960321 == "Living alone/other", "Non-cohabiting", NA))
  
# Compute household and natural childlessness variables for age 30 cohort
# Warning: natural children is about natural children in the household
BCS$`2000age30`['childlessNatural'] <- ifelse(BCS$`2000age30`$ownchild == 'Yes', 0,
                                              ifelse(BCS$`2000age30`$ownchild == 'No', 1, NA))
BCS$`2000age30`['childlessHousehold'] <- ifelse(BCS$`2000age30`$anychd == 'Yes', 0, 
                                                ifelse(BCS$`2000age30`$anychd == 'No', 1, NA))
BCS$`2000age30`['cohabitation'] = ifelse(BCS$`2000age30`$ms %in% c("Cohabiting", "Married"), 'Cohabiting',
                                         ifelse(BCS$`2000age30`$ms %in% c("Divorced", "Separated", "Single never married", "Widowed"), "Non-cohabiting", NA))
BCS$`2000age30`['numberChildrenHousehold'] <- NA
BCS$`2000age30`['numberChildrenNatural'] = apply(BCS$`2000age30`[, grepl("prega", names(BCS$`2000age30`))], 1, function(x){length(which(x == "Live birth"))})

# Compute household and natural childlessness variables for age 34 cohort
BCS$`2004age34`['childlessNatural'] <- ifelse(BCS$`2004age34`$bd7nchhh > 0 & BCS$`2004age34`$b7chchk == 'Yes', 0,
                                              ifelse(BCS$`2004age34`$bd7nchhh == 0 & BCS$`2004age34`$b7chchk == 'No', 1, NA))
BCS$`2004age34`['childlessHousehold'] <- ifelse(BCS$`2004age34`$bd7nchhh > 0 | BCS$`2004age34`$bd7ochhh > 0, 0,
                                                ifelse(BCS$`2004age34`$bd7nchhh == 0 & BCS$`2004age34`$bd7ochhh == 0, 1, NA))
BCS$`2004age34`['cohabitation'] = ifelse(BCS$`2004age34`$bd7ms %in% c("Cohabiting (living as a couple)", "Married"), "Cohabiting",
                                         ifelse(BCS$`2004age34`$bd7ms %in% c("Divorced", "Separated", "Single (and never married)", "Widowed"), "Non-cohabiting", NA))
BCS$`2004age34`['numberChildrenHousehold'] = as.integer(as.character(BCS$`2004age34`$bd7nchhh)) + as.integer(as.character(BCS$`2004age34`$bd7ochhh))
age34naturalhh = ifelse(BCS$`2004age34`$bd7nchhh < 0, NA, as.integer(BCS$`2004age34`$bd7nchhh))
age34naturalab = apply(BCS$`2004age34`[, grepl("chchk", names(BCS$`2004age34`))], 1, function(x){length(which(x == "Yes"))})
BCS$`2004age34`['numberChildrenNatural'] = age34naturalhh + age34naturalab 

# Compute household and natural childlessness variables for age 38 cohort
# Warning: natural children is about natural children in the household
BCS$`2008age38`['childlessNatural'] <- ifelse(BCS$`2008age38`$b8ownchd == 'No', 1, 
                                              ifelse(BCS$`2008age38`$b8ownchd == 'Yes', 0, NA))
BCS$`2008age38`['childlessHousehold'] <- ifelse(BCS$`2008age38`$b8anychd == 'Yes', 0, 
                                                ifelse(BCS$`2008age38`$b8anychd == 'No', 1, NA))
BCS$`2008age38`['numberChildrenHousehold'] <- BCS$`2008age38`$b8numch
age38naturalhh = as.integer(BCS$`2008age38`$bd8nchhh)
age38naturalab = apply(BCS$`2008age38`[, grepl("abre", names(BCS$`2008age38`))], 1, function(x){length(which(x == "Own child"))})
BCS$`2008age38`['numberChildrenNatural'] = age38naturalhh + age38naturalab 
BCS$`2008age38`['cohabitation'] = ifelse(BCS$`2008age38`$b8cohab == 'Yes', "Cohabiting", 
                                         ifelse(BCS$`2008age38`$b8cohab == 'No', "Non-cohabiting", 
                                                ifelse(BCS$`2008age38`$b8cohab == "Item not applicable", "Not applicable", NA)))

# Compute household and natural childlessness variables for age 42 cohort
BCS$`2012age42`['childlessNatural'] <- ifelse(BCS$`2012age42`$BD9TOTCE == 0, 1, 
                                              ifelse(BCS$`2012age42`$BD9TOTCE > 0, 0, NA))
BCS$`2012age42`['childlessHousehold'] <- ifelse(BCS$`2012age42`$BD9NUMCH == 0, 1,
                                                ifelse(BCS$`2012age42`$BD9NUMCH > 0, 0, NA))
BCS$`2012age42`['cohabitation'] = ifelse(BCS$`2012age42`$BD9COHAB == "Yes", "Cohabiting", 
                                         ifelse(BCS$`2012age42`$BD9COHAB %in% c("No", "No partner in hhgrid but partner job data"), "Non-cohabiting", NA))
BCS$`2012age42`['eventualNumberChildren42'] = ifelse(BCS$`2012age42`$BD9NUMCH < 0, NA, BCS$`2012age42`$BD9NUMCH)
BCS$`2012age42`['numberChildrenHousehold'] = BCS$`2012age42`$BD9NUMCH
BCS$`2012age42`['numberChildrenNatural'] = BCS$`2012age42`$BD9TOTOC

question_reasons = data.frame(question = sprintf("B9WHNC%02d", 1:18),
                              reason = c("Infertility problem (personal)", "Infertility problem (partner)", "Part. sterilised/vasectomy/hysterectomy",
                                         "Other health reason", "I have not wanted to have children", "Wanted children but not got round to it",
                                         "My spouse/partner doesn't want children", "Partner has children doesn't want more",
                                         "Not met right person to have child with", "My financial sit. made it difficult",
                                         "My housing situation made it difficult", "Won't compromise relationship with part",
                                         "I have been focused on my career", "Homosexual relationship", "No particular reason",
                                         "Other reason", "Don't know", "Don't want to answer"))
question_reasons$question = as.character(question_reasons$question)
question_reasons$reason = as.character(question_reasons$reason)

# use main reason if given, otherwise one of the other answers
BCS$`2012age42`['reasonChildless'] = NA
for (i in 1:nrow(BCS$`2012age42`)){
  
  if (BCS$`2012age42`$B9WHNC2[i] == 'Not applicable'){
    
    myrow = BCS$`2012age42`[i, grepl("B9WHNC", names(BCS$`2012age42`))]
    if (all(myrow == "Not applicable")){
      res = "Not applicable"
    } else {
      ind = which(myrow == 'Yes')
      if (length(ind) == 1){
        res = question_reasons$reason[question_reasons$question == names(myrow)[ind]]
      } else if (length(ind) > 1) {
        if (all(names(myrow)[ind] == c("B9WHNC05", "B9WHNC07"))){
          res = "Partner and I have not wanted children"
        } else if (any(paste0("B9WHNC", 15:18) %in% names(myrow)[ind])) {
          myreason = names(myrow)[ind][!names(myrow)[ind] %in% paste0("B9WHNC", 15:18)]
          res = question_reasons$reason[question_reasons$question == myreason]
        } else {
          res = "Not applicable"
        }
      } else {
        res = "Not applicable"
      }
    }
    
  } else {
    
    res = as.character(BCS$`2012age42`$B9WHNC2[i])
    
  }
  
  BCS$`2012age42`$reasonChildless[i] = res
  
}

# Compute household and natural childlessness variables for age 42 cohort
BCS$`2016age46`['childlessNatural'] <- ifelse(BCS$`2016age46`$BD10TOTCE == 0, 1, 
                                              ifelse(BCS$`2016age46`$BD10TOTCE > 0, 0, NA))
BCS$`2016age46`['childlessHousehold'] <- ifelse(BCS$`2016age46`$BD10NUMCH == 0, 1,
                                                ifelse(BCS$`2016age46`$BD10NUMCH > 0, 0, NA))
BCS$`2016age46`['cohabitation'] =  ifelse(BCS$`2016age46`$BD10COHAB == "0", "Non-cohabiting", ifelse(BCS$`2016age46`$BD10COHAB == "1", "Cohabiting", NA))
BCS$`2016age46`['eventualNumberChildren46'] = ifelse(BCS$`2016age46`$BD10NUMCH < 0, NA, BCS$`2016age46`$BD10NUMCH)
BCS$`2016age46`$B10LIFST1 = ifelse(BCS$`2016age46`$B10LIFST1 > 0, as.numeric(as.character(BCS$`2016age46`$B10LIFST1)) - 1, BCS$`2016age46`$B10LIFST1)
BCS$`2016age46`['numberChildrenHousehold'] = BCS$`2016age46`$BD10NUMCH
BCS$`2016age46`['numberChildrenNatural'] = BCS$`2016age46`$BD10TOTOC

# Recover life satisfaction for BSC age 26 from b960666 (1-9) and b960667 (10)
unique(BCS$`1996age26`[, c('b960666', 'b960667')])
BCS$`1996age26`['lifeSatisfaction'] <- ifelse(BCS$`1996age26`$b960666 %in% c(0:9), BCS$`1996age26`$b960666,
                                              ifelse(BCS$`1996age26`$b960667 == '1', 10, NA))

# Change integers into characters for economic activity at age 26
vals <- c("1" = "Full-time paid employee", "2" = "Part-time paid employee",
          "3" = "Full-time self-employed", "4" = "Part-time self-employed",
          "5" = "OLF: Unemployed", "6" = "OLF: Full-time education",
          "7" = "OLF: Sick/disabled", "8" = "OLF: Sick/disabled",
          "9" = "OLF: Home/family care", "10" = "OLF: Training scheme",
          "11" = "OLF: Other", "0" = NA)
BCS$`1996age26`$empstat <- revalue(as.factor(BCS$`1996age26`$empstat), vals)

# Change integers into characters for highest qualification at age 26
vals <- c("0" = "No Qualification", "1" = "CSE 2-5/nvq1", "2" = "O Level/nvq2",
          "3" = "A Level/nvq3", "4" = "Higher Qual/nvq4", "5" = "Degree/nvq5 +", "-3" = "Quals but d/k level")
BCS$`1996age26`$hqual26d <- revalue(as.factor(BCS$`1996age26`$hqual26d), vals)

# Change integers into characters for economic activity at age 46
vals <- c("1" = "Full-time paid employee", "2" = "Part-time paid employee",
          "3" = "Full-time self-employed", "4" = "Part-time self-employed",
          "5" = "OLF: Unemployed", "6" = "OLF: Full-time education",
          "8" = "OLF: Sick/disabled", "9" = "OLF: Sick/disabled",
          "10" = "OLF: Home/family care", "7" = "OLF: Training scheme",
          "12" = "OLF: Other", "11" = "OLF: Retired", "-9" = "Not enough information",
          "-8" = "Not applicable", "-1" = "Refused")
BCS$`2016age46`$BD10ECACT <- revalue(as.factor(BCS$`2016age46`$BD10ECACT), vals)

# Change integers into characters for pay period at age 46
vals <- c("1" = "One week", "2" = "Two weeks",
          "3" = "Three weeks", "4" = "Four weeks",
          "5" = "Calendar month", "6" = "Two Calendar months",
          "8" = "Nine times a year", "9" = "Ten times a year",
          "10" = "Three months/13 weeks", "11" = "Six months/26 weeks",
          "12" = "One Year/12 months/52 weeks", "13" = "Less than one week",
          "14" = "One off/lump sum", "15" = "Other period",
          "-1" = "Not applicable", "-8" = "Not known",
          "-9" = "Refused")
BCS$`2016age46`$B10GROP <- revalue(as.factor(BCS$`2016age46`$B10GROP), vals)

# Change integers into characters for health at age 46
vals <- c("1" = "Excellent", "2" = "Very good",
          "3" = "Good", "4" = "Fair",
          "5" = "Poor", "-1" = "Not applicable", "-8" = "Not known",
          "-9" = "Refused")
BCS$`2016age46`$B10HLTHGN <- revalue(as.factor(BCS$`2016age46`$B10HLTHGN), vals)


# Change integers into characters for marital status at age 46
vals <- c("1" = "Legally separated", "2" = "Married",
          "3" = "Divorced", "4" = "Widowed",
          "5" = "A Civil Partner", "6" = "A former Civil Partner", 
          "7" = 'A surviving Civil Partner', "8" = "Never married or in a Civil Partnership",
          "-8" = "No information")
BCS$`2016age46`$BD10MS <- revalue(as.factor(BCS$`2016age46`$BD10MS), vals)


# Change integers into characters for highest qualification at age 46
vals <- c("0" = "none", "1" = "nvq1 level", "2" = "nvq2 level",
          "3" = "nvq3 level", "4" = "nvq4 level",
          "5" = "nvq5 level", "-1" = "not applicable")
BCS$`2016age46`$BD10HNVQ <- revalue(as.factor(BCS$`2016age46`$BD10HNVQ), vals)

# Change integers into characters for social class at age 46
vals <- c("-1" = "Not applicable", "1" = "1 Higher managerial and administrative", "2" = "Lower managerial and administrative",
          "3" = "Intermediate occupations", "4" = "Small employers and own account workers",
          "5" = "Lower supervisory and technical", "6" = "Semi-routine occupations",
          "7" = "Routine occupations", "8" = "Never worked and long-term unemployed",
          "9" = "Occs not stated or inadequate desc", "1.1" = "Large employers and higher managerial",
          "1.2" = "1.2 Higher professional occupations", "-8" = "Not known",
          "-9" = "Refused")
BCS$`2016age46`$B10NSSECAN <- revalue(as.factor(BCS$`2016age46`$B10NSSECAN), vals)

# Recover number of children in household at age 30 (no variable in the data)
childoptions = c("Own child", "Adopted child", "Child of current spouse/partner", "Fostered child", 
                 "Child of previous spouse/partner", "Child of nonrelative adult in hhld")
sub_reltoke = BCS$`2000age30`[, grepl("reltoke", names(BCS$`2000age30`))]
BCS$`2000age30`$numberChildrenHousehold = apply(sub_reltoke, 1, function(x){ length(which(x %in% childoptions)) })
BCS$`2000age30`$numberChildrenHousehold[BCS$`2000age30`$anychd == "No"] = 0
BCS$`2000age30` = BCS$`2000age30`[, !grepl("reltoke", names(BCS$`2000age30`))]
