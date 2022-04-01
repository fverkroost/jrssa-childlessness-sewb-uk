if (!dir.exists(paste0(out_dir, "descriptives"))){ dir.create(paste0(out_dir, "descriptives")) }
if (!dir.exists(paste0(out_dir, "trajectories"))){ dir.create(paste0(out_dir, "trajectories")) }

# -------------------------------------------------
# Descriptives on observations and waves
# -------------------------------------------------

# Descriptive statistics on observations for women
prop_par_women = women %>% 
  group_by(ID) %>% 
  mutate(childless = unique(eventualTypeChildless)) %>% 
  distinct(ID, childless) %>%
  ungroup() %>% 
  group_by(childless) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))
prop_par_men = men %>% 
  group_by(ID) %>% 
  mutate(childless = unique(eventualTypeChildless)) %>% 
  distinct(ID, childless) %>%
  ungroup() %>% 
  group_by(childless) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# Descriptive data
desc_data = data.frame(
  indicators = c("Number of observations", "Average number of sweeps per person", "Number of unique persons",
                 "Proportion of eventually circumstantially childless individuals",
                 "Proportion of eventually voluntarily childless individuals",
                 "Proportion of eventually involuntarily childless individuals",
                 "Proportion of eventually unknowingly childless individuals",
                 "Proportion of eventual parents",
                 "Proportion of people who had at least one miscarriage",
                 "Proportion of people who had at least one terminated pregnancy",
                 "Proportion of people who had at least one stillbirth",
                 "Proportion of eventually childless women who had at least one miscarriage",
                 "Proportion of eventually childless women who had at least one terminated pregnancy",
                 "Proportion of eventually childless women who had at least one stillbirth"),
  women = unlist(c(nrow(women), women %>% group_by(ID) %>% mutate(n = n()) %>% ungroup() %>% distinct(ID, n) %>% summarise(mean(n)),
                   women %>% group_by(ID) %>% summarise(n = n()) %>% n_distinct(),
                   round(prop_par_women$freq[prop_par_women$childless == "Circumstantially childless"], 3),
                   round(prop_par_women$freq[prop_par_women$childless == "Voluntarily childless"], 3),
                   round(prop_par_women$freq[prop_par_women$childless == "Involuntarily childless"], 3),
                   round(prop_par_women$freq[prop_par_women$childless == "Unknown childless"], 3),
                   round(prop_par_women$freq[prop_par_women$childless == "Parent"], 3),
                   women %>% group_by(ID) %>% mutate(childless = unique(totalNumberMiscarriages > 0)) %>% distinct(ID, childless) %>%
                     ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                   women %>% group_by(ID) %>% mutate(childless = unique(totalNumberTerminations > 0)) %>% distinct(ID, childless) %>%
                     ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                   women %>% group_by(ID) %>% mutate(childless = unique(totalNumberStillbirths > 0)) %>% distinct(ID, childless) %>%
                     ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                   women %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberMiscarriages > 0)) %>% distinct(ID, childless) %>%
                     ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                   women %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberTerminations > 0)) %>% distinct(ID, childless) %>%
                     ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                   0 # women %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberStillbirths > 0)) %>% distinct(ID, childless) %>%
                   # ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq)
  )),
  men = unlist(c(nrow(men), men %>% group_by(ID) %>% mutate(n = n()) %>% ungroup() %>% distinct(ID, n) %>% summarise(mean(n)),
                 men %>% group_by(ID) %>% summarise(n = n()) %>% n_distinct(),
                 round(prop_par_men$freq[prop_par_men$childless == "Circumstantially childless"], 3),
                 round(prop_par_men$freq[prop_par_men$childless == "Voluntarily childless"], 3),
                 round(prop_par_men$freq[prop_par_men$childless == "Involuntarily childless"], 3),
                 round(prop_par_men$freq[prop_par_men$childless == "Unknown childless"], 3),
                 round(prop_par_men$freq[prop_par_men$childless == "Parent"], 3),
                 men %>% group_by(ID) %>% mutate(childless = unique(totalNumberMiscarriages > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                 men %>% group_by(ID) %>% mutate(childless = unique(totalNumberTerminations > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                 men %>% group_by(ID) %>% mutate(childless = unique(totalNumberStillbirths > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                 men %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberMiscarriages > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                 men %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberTerminations > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq),
                 men %>% filter(eventualTypeChildless != "Parent") %>% group_by(ID) %>% mutate(childless = unique(totalNumberStillbirths > 0)) %>% distinct(ID, childless) %>%
                   ungroup() %>% group_by(childless) %>% summarise(n = n()) %>% mutate(freq = n / sum(n)) %>% filter(childless == TRUE) %>% select(freq))))

# Save table to file
output = print(xtable(desc_data, digits = rep(3, ncol(desc_data) + 1)), include.rownames = FALSE)
fileConn = file(paste0(out_dir, "descriptives/metaData.txt"))
writeLines(output, fileConn)
close(fileConn)

# ---------------------------------------------------------------------------
# Compute and plot proportions of (non-)biological children at ages 42 and 46
# ---------------------------------------------------------------------------

# Obtain data and statistics
dflist = list(
  read.dta('data-raw/BCS/bcs70_2012_derived.dta'), 
  read.dta13('data-raw/BCS/bcs_age46_main.dta')
)
ages = c(42, 46)
for (i in 1:length(dflist)){
  x = dflist[[i]]
  
  # Produce statistics about children
  x['totalNumberChildrenBio'] = x[, paste0("BD", 8 + i, "TOTOC")]
  x['totalNumberChildrenAll'] = x[, paste0("BD", 8 + i, "TOTAC")]
  x['totalNumberChildrenNonbio'] = x$totalNumberChildrenAll - x$totalNumberChildrenBio
  x['hasAtLeastOneBioChild'] = ifelse(x$totalNumberChildrenBio > 0, 1, ifelse(x$totalNumberChildrenBio < 0, NA, 0))
  x['hasAtLeastOneNonbioChild'] = ifelse(x$totalNumberChildrenNonbio > 0, 1, ifelse(x$totalNumberChildrenNonbio < 0, NA, 0))
  
  # Compute relative proportions
  tab = table(x$hasAtLeastOneBioChild, x$hasAtLeastOneNonbioChild)
  bio_and_non_bio = tab["1", "1"] / sum(tab) * 100
  no_bio_nor_non_bio = tab["0", "0"] / sum(tab) * 100
  bio_but_no_non_bio = tab["1", "0"] / sum(tab) * 100
  no_bio_but_non_bio = tab["0", "1"] / sum(tab) * 100
  
  # Print results to console
  print(paste0("Percentage of individuals with both bio and non-bio children at age ", ages[i], ": ", bio_and_non_bio))
  print(paste0("Percentage of individuals with neiter bio nor non-bio children at age ", ages[i], ": ", no_bio_nor_non_bio))
  print(paste0("Percentage of individuals with some bio but no non-bio children at age ", ages[i], ": ", bio_but_no_non_bio))
  print(paste0("Percentage of individuals with no bio but some non-bio children at age ", ages[i], ": ", no_bio_but_non_bio))
  
  # Produce data for pie chart
  piedata <- data.frame(
    group = c("Both biological and non-biological children", "Neither biological nor non-biological children",
              "Any/some biological but none non-biological children", "None biological but any/some non-biological children"),
    value = c(bio_and_non_bio, no_bio_nor_non_bio, bio_but_no_non_bio, no_bio_but_non_bio)
  )
  piedata = piedata %>%
    arrange(desc(group)) %>%
    mutate(ypos = cumsum(value) - 0.5*value)
  
  # Produce pie chart and save to file
  g = ggplot(piedata, aes(x = "", y = value, fill = group)) +
    geom_bar(stat = "identity", width = 1, color="white") +
    coord_polar("y", start = 0) + 
    theme_void() +
    labs(fill = NULL) +
    theme(legend.position = "bottom",
          legend.direction = "vertical") +
    geom_text(aes(y = ypos, label = paste0(round(value, 1), "%")), color = "white")
  ggsave(paste0(out_dir, "descriptives/pie_chart_bio_nonbio_age", ages[i], ".png"), g)
  
}

# -------------------------------------------------
# Descriptives statistics
# -------------------------------------------------

# Descriptive statistics
numvars = c('lifeSatisfaction', 'Year', 'Age', 'logCorrectedMonthlyGrossPay',
            'totalNumberMiscarriages', 'totalNumberStillbirths', 'totalNumberTerminations')
catvars = c('healthStatus_binary', 'economicActivity', 'education_binary_bwimp', 'maritalStatus_combined', 'cohabitation',  
            'eventualChildlessness', 'eventualTypeChildless', "childArrivedSinceLastWave", "numberChildrenNaturalCategorical")
descriptiveStatistics(
  data = tot, 
  num_vars = numvars, 
  cat_vars = catvars, 
  output_folder = paste0(out_dir, "descriptives/"), 
  decimals = 3
)

# -------------------------------------------------
# Missingness plots
# -------------------------------------------------

# Missingness plots
vars = c("eventualChildlessness", "logCorrectedMonthlyGrossPay", "lifeSatisfaction", "education_binary_bwimp",
         "maritalStatus_combined", "cohabitation", "healthStatus_binary", "economicActivity", "Age",
         "childArrivedSinceLastWave", "numberChildrenNaturalCategorical")
nams = c("Childlessness", "Monthly Gross Pay", "Life Satisfaction", "Education", "Marital Status", "Cohabitation", "Health Status",
         "Economic Activity", "Age", "Child Arrival", "Number of children")
datalist = list(women, men)
for (i in 1:length(datalist)){
  my_plot = aggr(datalist[[i]][, vars], delimiter = NULL, plot = TRUE, col = c("skyblue", "red", "orange"), bars = TRUE,
                 numbers = FALSE, prop = TRUE, combined = FALSE, varheight = FALSE,
                 only.miss = FALSE, border = par("fg"), sortVars = FALSE,
                 sortCombs = TRUE, ylabs = NULL, axes = TRUE, labels = nams, cex.lab = 0.6, 
                 cex.axis = 0.55, cex.numbers = par("cex"), gap = 2, las = 2, ylab = 'Proportion of Missings')
  dev.copy(pdf, paste0(out_dir, 'descriptives/aggr_', unique(datalist[[i]]$Sex), '.pdf'))
  dev.off()
}

# -------------------------------------------------
# Density plots for dependent variables
# -------------------------------------------------

# Density plot and histograms for EWB and SWB
g = ggplot() + 
  theme_flo() + 
  facet_wrap(~ Sex) 
g1d = g + geom_density(data = tot, aes(logCorrectedMonthlyGrossPay), fill = "firebrick1", color = "firebrick1", binwidth = 1) +
  labs(x = "log(1 + Inflation-Corrected Monthly Gross Pay)", y = "Density")
g2d = g + geom_density(data = tot, aes(lifeSatisfaction), fill = "firebrick1", color = "firebrick1", binwidth = 1) +
  labs(x = "Life Satisfaction", y = "Density")
g1h = g + geom_histogram(data = tot, aes(logCorrectedMonthlyGrossPay), fill = "firebrick1", color = "firebrick1", binwidth = 1) +
  labs(x = "log(1 + Inflation-Corrected Monthly Gross Pay)", y = "Frequency")
g2h = g + geom_histogram(data = tot, aes(lifeSatisfaction), fill = "firebrick1", color = "firebrick1", binwidth = 1) +
  labs(x = "Life Satisfaction", y = "Frequency")
ggsave(paste0(out_dir, "descriptives/density_income.png"), g1d)
ggsave(paste0(out_dir, "descriptives/density_lifesat.png"), g2d)
ggsave(paste0(out_dir, "descriptives/histogram_income.png"), g1h)
ggsave(paste0(out_dir, "descriptives/histogram_lifesat.png"), g2h)

# Descriptive plot of EWB
plotdf = tot[complete.cases(tot$eventualChildlessness), ]
colors <- c("Corrected for inflation" = "red", "Not corrected for inflation" = "blue")
g = ggplot() + 
  stat_summary(data = plotdf, aes(x = Age, y = correctedMonthlyGrossPay, color = "Corrected for inflation",
                                  linetype = "Corrected for inflation"), fun.data = "mean_se", geom = "line") +
  stat_summary(data = plotdf, aes(x = Age, y = correctedMonthlyGrossPay, color = "Corrected for inflation",
                                  shape = "Corrected for inflation"), fun.data = "mean_se", geom = "point") +
  stat_summary(data = plotdf, aes(x = Age, y = correctedMonthlyGrossPay, color = "Corrected for inflation"), 
               fun.data = "mean_se", geom = "errorbar", width = 1) +
  stat_summary(data = plotdf, aes(x = Age, y = monthlyGrossPay, color = "Not corrected for inflation",
                                  linetype = "Not corrected for inflation"), fun.data = "mean_se", geom = "line") +
  stat_summary(data = plotdf, aes(x = Age, y = monthlyGrossPay, color = "Not corrected for inflation",
                                  shape = "Not corrected for inflation"), fun.data = "mean_se", geom = "point") +
  stat_summary(data = plotdf, aes(x = Age, y = monthlyGrossPay, color = "Not corrected for inflation"), 
               fun.data = "mean_se", geom = "errorbar", width = 1)  +
  facet_grid(eventualChildlessness ~ Sex) +
  scale_color_manual(values = colors) + 
  labs(color = NULL, shape = NULL, linetype = NULL, y = "Monthly Gross Pay") + 
  theme_flo() +
  scale_x_continuous(breaks = unique(plotdf$Age))
ggsave(paste0(out_dir, "descriptives/descriptive_EWB.png"), g)

# Descriptive plot of SWB
plotdf$lifeSatisfaction = as.numeric(as.character(plotdf$lifeSatisfaction))
g = ggplot() + 
  stat_summary(data = plotdf, aes(x = Age, y = lifeSatisfaction), fun.data = "mean_se", geom = "line") +
  stat_summary(data = plotdf, aes(x = Age, y = lifeSatisfaction), fun.data = "mean_se", geom = "point") +
  stat_summary(data = plotdf, aes(x = Age, y = lifeSatisfaction), fun.data = "mean_se", geom = "errorbar", width = 1) +
  facet_grid(eventualChildlessness ~ Sex) +
  labs(y = "Life Satisfaction") + 
  theme_flo() +
  scale_x_continuous(breaks = unique(plotdf$Age))
ggsave(paste0(out_dir, "descriptives/descriptive_SWB.png"), g)

# -------------------------------------------------
# Normal vs. inflation-corrected income
# -------------------------------------------------

# Monthly gross pay vs inflation-corrected monthly gross pay
inflation_rates = read.csv("https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/cdko/mm23&series=&fromYear=1947&toYear=2019&frequency=years")
inflation_rates = inflation_rates[-c(1:5), ]
names(inflation_rates) = c('year', 'cpi')
inflation_rates$year = as.numeric(as.character(inflation_rates$year))
inflation_rates$cpi = as.numeric(as.character(inflation_rates$cpi))
g = ggplot() + geom_line(data = inflation_rates, aes(x = year, y = cpi)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(inflation_rates$year), max(inflation_rates$year), 5)) +
  theme_flo() +
  labs(x = NULL, y = "Consumer Price Index (CPI)")
ggsave(paste0(out_dir, "descriptives/descriptive_CPI.png"), g)

# -------------------------------------------------
# Descriptive EWB and SWB trajectories by controls
# -------------------------------------------------

# Descriptive by explanatory variable and EWB/SWB trajectories
vars = c("maritalStatus_combined", "economicActivity", "education_binary_bwimp", "healthStatus_binary")
for (i in 1:length(vars)){
  newmen = men
  newwomen = women
  names(newmen)[names(newmen) == vars[i]] = "xvar"
  names(newwomen)[names(newwomen) == vars[i]] = "xvar"
  hmen = newmen[complete.cases(newmen[, c("Age", "eventualChildlessness", "xvar")]), ] %>% 
    group_by(Age, eventualChildlessness, xvar) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  hwomen = newwomen[complete.cases(newwomen[, c("Age", "eventualChildlessness", "xvar")]), ] %>% 
    group_by(Age, eventualChildlessness, xvar) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  dat = rbind(hmen, hwomen)
  dat["gender"] = c(rep("Men", nrow(hmen)), rep("Women", nrow(hwomen)))
  g = ggplot() + geom_bar(data = dat, aes(x = Age, y = freq, fill = xvar),
                          position="stack", stat="identity") +
    facet_wrap(gender ~ eventualChildlessness) + labs(fill = vars[i])
  print(g)
}
mydf = tot[complete.cases(tot$eventualChildlessness), ]
ggplot() + geom_smooth(data=mydf, aes(x=jitter(Age), y=logCorrectedMonthlyGrossPay, color = eventualChildlessness, fill= eventualChildlessness), 
                       method = "lm", formula = y~poly(x,3)) + facet_wrap(~ Sex)
ggplot() + geom_smooth(data=mydf, aes(x=jitter(Age), y=lifeSatisfaction, color = eventualChildlessness, fill= eventualChildlessness), 
                       method = "lm", formula = y~poly(x,3)) + facet_wrap(~ Sex)

# -------------------------------------------------
# Mean income at ages 42 and 46
# -------------------------------------------------

# Load the data
age42 = read.dta13('data-raw/BCS/bcs70_2012_flatfile.dta')
age46 = read.dta13('data-raw/BCS/bcs_age46_main.dta')

# Define nice variable names for age 42 data
match42 = data.frame(
  org = c("One week", "Two weeks", "Three weeks", "Four weeks", "Calendar month",
          "Two calendar months", "Eight times a year", "Nine times a year", 
          "Ten times a year", "Three months/13 weeks", "Six months/26 weeks",
          "Less than one week", "Lump sum/one off", "None of these", "na",
          "One year/12 months/52 weeks", "Refused", "Don't know", "Not applicable"),
  mult = c(4.34524, 4.34524/2, 4.34524/3, 4.34524/4,
           1, 1/2, rep(NA, 3), 1/3, 1/6,rep(NA, 4), 1/12, rep(NA, 3))
)

# Define nice variable names for age 46 data
match46 = data.frame(
  org = c("One week", "Two weeks", "Three weeks", "Four weeks", "Calendar month",
          "Two Calendar months", "Eight times a year", "Nine times a year", 
          "Ten times a year", "Three months/13 weeks", "Six months/26 weeks",
          "One Year/12 months/52 weeks", "Less than one week", "One off/lump sum", "Other period",
          "Refused", "Not known", "Not applicable"),
  mult = c(4.34524, 4.34524/2, 4.34524/3, 4.34524/4,
           1, 1/2, rep(NA, 3), 1/3, 1/6, 1/12, rep(NA, 6))
)

# Add variables to age 42 and 46 data
age42['multiplier_income'] = match42$mult[match(age42$B9GROP, match42$org)]
age46['multiplier_income'] = match46$mult[match(age46$B10GROP, match46$org)]
age42['monthly_income'] = age42$B9GROA * age42$multiplier_income
age46['monthly_income'] = age46$B10GROA * age46$multiplier_income
age42$monthly_income[age42$monthly_income < 0] = NA
age46$monthly_income[age46$monthly_income < 0] = NA
age42derived = read.dta('data-raw/BCS/bcs70_2012_derived.dta')
age42['childless'] = ifelse(age42derived$BD9TOTCE == 0, 1, ifelse(age42derived$BD9TOTCE > 0, 0, NA))
age46['childless'] = ifelse(age46$BD10TOTCE == 0, 1, ifelse(age46$BD10TOTCE > 0, 0, NA))
age42['sex'] = age42$B9CMSEX
age46['sex'] = age46$B10CMSEX

# Restructure the data to obtiain results
df42 = age42 %>% 
  group_by(sex, childless) %>% 
  mutate(mean_income = mean(na.omit(monthly_income))) %>% 
  distinct(sex, childless, mean_income)
df46 = age46 %>% 
  group_by(sex, childless) %>% 
  mutate(mean_income = mean(na.omit(monthly_income))) %>% 
  distinct(sex, childless, mean_income)

# Combine data and plot
mydf = rbind(df42, df46)
mydf['age'] = c(rep(42, nrow(df42)), rep(46, nrow(df46)))
mydf = mydf[complete.cases(mydf), ]
mydf$childless = factor(mydf$childless)
ggplot() + 
  geom_point(data = mydf, aes(x = age, y = mean_income, color = childless)) + 
  facet_wrap(~ sex)

# -------------------------------------------------
# Trajectories by type of childlessnes
# -------------------------------------------------

# Life satisfaction by type of childlessness
g = ggplot() +
  geom_smooth(data = tot, aes(x = jitter(Age), y = lifeSatisfaction, color = eventualTypeChildless, fill = eventualTypeChildless), 
              method = "lm", formula = y ~ poly(x, 3)) +
  facet_grid(Sex ~ eventualTypeChildless)
ggsave(paste0(out_dir, 'trajectories/trajectory_swb_type_childless.png'), g)

# Income by type of childlessness
g = ggplot() +
  geom_smooth(data = tot, aes(x = jitter(Age), y = logCorrectedMonthlyGrossPay, color = eventualTypeChildless, fill = eventualTypeChildless), 
              method = "lm", formula = y ~ poly(x, 3)) +
  facet_grid(Sex ~ eventualTypeChildless)
ggsave(paste0(out_dir, 'trajectories/trajectory_ewb_type_childless.png'), g)

# Life satisfaction and income by eventual parental status
tot_melt = reshape2::melt(tot, measure.vars = c('lifeSatisfaction', 'logCorrectedMonthlyGrossPay'))
g = ggplot() +
  geom_smooth(data = tot_melt, #[complete.cases(tot_melt[, c("Age", "eventualChildlessness", "value")]), ], 
              aes(x = jitter(Age), y = value, color = factor(eventualChildlessness), fill = factor(eventualChildlessness)), 
              method = "lm", formula = y ~ poly(x, 3)) +
  facet_grid(variable ~ Sex, scales = "free_y") 
ggsave(paste0(out_dir, 'trajectories/trajectory_swb_ewb_childless.png'), g)

#---------------------------------------------------------
# Statistics on unsuccessful pregnancies
#---------------------------------------------------------

# Statistics on unsuccessful pregnancies: miscarriages
tot %>% 
  group_by(ID, Sex) %>% 
  mutate(val = any(totalNumberMiscarriages) > 0) %>% 
  distinct(ID, val, Sex) %>%
  ungroup() %>% 
  group_by(val, Sex) %>% 
  mutate(n = n()) %>% 
  distinct(val, n, Sex) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(freq = n / sum(n))

# Statistics on unsuccessful pregnancies: terminations
tot %>% 
  group_by(ID, Sex) %>% 
  mutate(val = any(totalNumberTerminations) > 0) %>% 
  distinct(ID, val, Sex) %>%
  ungroup() %>% 
  group_by(val, Sex) %>% 
  mutate(n = n()) %>% 
  distinct(val, n, Sex) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(freq = n / sum(n))

# Statistics on unsuccessful pregnancies: still births
tot %>% 
  group_by(ID, Sex) %>% 
  mutate(val = any(totalNumberStillbirths) > 0) %>% 
  distinct(ID, val, Sex) %>%
  ungroup() %>% 
  group_by(val, Sex) %>% 
  mutate(n = n()) %>% 
  distinct(val, n, Sex) %>%
  ungroup() %>%
  group_by(Sex) %>%
  mutate(freq = n / sum(n))

