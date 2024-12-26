# load data from Excel file
setwd("~/Tj/Credit Risk Scoring")

germancredit <- read_excel("GermanCreditData2.xlsx", 
                           sheet = "Sheet1")
# View(germancredit)

library(scorecard)

library(dplyr)

germancredit <- germancredit %>%
  mutate_at(vars('Checking', 'History', 'Savings', 'Emploed', 'marital', 'Coapp', 'Property', 'Other', 'housing', 'Job', 'Telephone', 'Foreign', 'Purpose'), as.character)

# filter variable via missing rate, iv, identical value rate
dt_f = var_filter(germancredit, y="Good", lims = list(missing_rate = 0.95, identical_rate
                                                      = 0.95, info_value = 0.16),)




library(DescTools)
# Select only numeric columns for winsorization
germancredit_numeric <- select_if(germancredit, is.numeric)

summary(germancredit_numeric)

# Apply winsorization
germancredit_winsorized <- apply(germancredit_numeric, 2, Winsorize, probs = c(0.01, 0.99))

# Replace original numeric columns with winsorized versions
germancredit[ , names(germancredit_numeric)] <- germancredit_winsorized

# Check the summary statistics of the winsorized data in the original dataset
summary(select_if(germancredit, is.numeric))






# breaking dt into train and test
dt_list = split_df(dt_f, y="Good", ratios = c(0.7, 0.3), seed = 30)
label_list = lapply(dt_list, function(x) x$Good)

# woe binning ------
bins = woebin(dt_f, y="Good")

# print(bins)

# woebin_plot(bins)
woebin_plot(bins)

# converting train and test into woe values
dt_woe_list = lapply(dt_list, function(x) woebin_ply(x, bins))

print(dt_woe_list$train)

# glm / selecting variables ------
m1 = glm( Good ~ ., family = binomial(), data = dt_woe_list$train)
# vif(m1, merge_coef = TRUE) # summary(m1)
# Select a formula-based model by AIC (or by LASSO for large dataset)
m_step = step(m1, direction="both", trace = FALSE)
m2 = eval(m_step$call)
# vif(m2, merge_coef = TRUE) # summary(m2)

# performance ks & roc ------
## predicted proability
pred_list = lapply(dt_woe_list, function(x) predict(m2, x, type='response'))
## Adjusting for oversampling (support.sas.com/kb/22/601.html)
# card_prob_adj = scorecard2(bins_adj, dt=dt_list$train, y='creditability', 
#                x=sub('_woe$','',names(coef(m2))[-1]), badprob_pop=0.03, return_prob=TRUE)

## performance
perf = perf_eva(pred = pred_list, label = label_list)
# perf_adj = perf_eva(pred = card_prob_adj$prob, label = label_list$train)

# score ------
## scorecard
card = scorecard(bins, m2)
## credit score
score_list = lapply(dt_list, function(x) scorecard_ply(x, card))
## psi
perf_psi(score = score_list, label = label_list)

# make cutoff decisions -----
## gains table
gtbl = gains_table(score = unlist(score_list), label = unlist(label_list))

# Example II, multiple datsets
## predicted p1
perf_eva(pred = pred_list, label=label_list,
         show_plot = c('ks', 'gain', 'roc','f1'))
## predicted score
perf_eva(score_list, label_list)
