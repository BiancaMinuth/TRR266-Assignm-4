#++++ Research on Corporate Transparency ++++
#++++++++++++++++++++++++++++++++++++++++++++
#++++ Bianca Minuth ++++
#++++ Assignment 4 ++++
#++++ 07/15/2021 +++++

# Which firms are covered by financial analysts? Current evidence from non-U.S. firms


## PREPARATION

# select libraries
library(tidyverse)
library(zoo)
library(ExPanDaR)
library(plm)
library(ggplot2)
library(lmtest)
library(dplyr)
library(lfe)
library(kableExtra)


#########################################################################################################

## EXCHANGE RATES
# Exchange rates were retrieved from WRDS using SAS: 
# Go to WRDS --> SASStudio --> open: "comp_exchange rates" ('code/SAS/comp_exchange rates.sas') 
# --> run the code.
# Saved as 'exch_rates.csv' ('data/generated/exch_rates.csv').


#########################################################################################################

## OPEN AND CHECKS

#open data
comp_raw <- read_csv(
  'data/external/ass4_compustat_global.csv',
  col_types = cols()
)

ibes_raw <- read_csv(
  "data/external/ass4_ibes_int.csv",
  col_types = cols(),
) 
#%>%
#  select(-ibes_year)


exch_rates <- read_csv(
  "data/generated/exch_rates.csv",
  col_types = cols(),
)

ff48 <- read_csv(
  "data/external/fama_french_48_industries.csv",
  col_types = cols(),
)


# check dublicates and N/A
check_dups <- function(df, ...) {
  dups <- df %>%
    group_by(...) %>%
    filter(n() > 1)
  if (nrow(dups) > 0) warning(sprintf(
    "%d duplicates detected", nrow(dups)
  ))
  dups
}

dups_1 <- check_dups(comp_raw, gvkey, datadate)
dups_2 <- check_dups(ibes_raw, gvkey, ibes_year)
dups_3 <- check_dups(exch_rates, datadate, tocurd)


#########################################################################################################

##  PREPARE DATA

# merge comp and ibes data 
comp_ibes <- comp_raw %>%
  left_join(ibes_raw, by = "gvkey") 

# merge comp_ibes and exchange rates data 
comp_ibes_exch <- comp_ibes %>%
  mutate(
  tocurd = as.character(comp_raw$curcd),
  ) %>%
  left_join(exch_rates, by = c( "datadate", "tocurd"))

# prepare data and create ratios
df_ratios  <-  comp_ibes_exch  %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  dplyr::mutate(
    analy_folw = number_analyst_following, 
    analy_folw_ln = log(number_analyst_following), 
    ta = at*exchange_rate,
    size_ta = (log(at * exchange_rate)),
    size_employ = emp,
    sales_ln = log(sale * exchange_rate),
    cap_expend = capx / at,
    rnd_intens = xrd / at,
    liquidity = act/lct,
    leverage = (dltt-dlc)/ seq,
    gross_margin = (revt-cogs)/ revt,
    roa = nicon / at,
    eps = nicon / cshoi
  )  %>%
  arrange(gvkey, fyear
  ) %>%
  filter(!is.na(sales_ln)  & !is.na(gross_margin) & !is.na(roa))

# growth rate revenue
df_growth <- df_ratios %>%
  group_by(gvkey) %>%
  dplyr::mutate(
    rev_growth = (revt / dplyr::lag(revt)) - 1
  )

# merge comp_ibes_exch and Fama French 48 Industry Identifier
df_merged <- df_growth %>%
  ungroup()  %>%
  mutate(
    sic = as.character(df_growth$sich),
  ) %>%
  left_join(ff48, by = "sic")

# select variables
df_sample <- df_merged %>%
  select(
    gvkey, conm, fyear, loc, sic, ff48_ind, analy_folw, analy_folw_ln,
    ta, size_ta, size_employ, sales_ln, cap_expend, rnd_intens, liquidity,  
    gross_margin, roa, eps, rev_growth, leverage
  )


#########################################################################################################

## ANALYSE DATA

# treat ouliers 
df_sample_adj <- treat_outliers(
  df_sample %>%
    filter(
      !is.na(analy_folw), !is.na(analy_folw_ln), !is.na(ta), !is.na(size_ta), !is.na(size_employ), 
      !is.na(cap_expend), !is.na(rnd_intens), !is.na(liquidity), !is.na(gross_margin), !is.na(eps), 
      !is.na(roa), !is.na(leverage),
      !is.na(ff48_ind)
    ) %>%
    select(
      gvkey, conm, fyear, loc, sic, ff48_ind, analy_folw, analy_folw_ln,
      ta, size_ta, size_employ, sales_ln, cap_expend, rnd_intens, liquidity,  
      gross_margin, roa, eps, rev_growth, leverage
    )
)


# To check descriptive statistics and regression analysis
# ! used for presentation !
ExPanD_config <- readRDS('code/R/ExPanD.RDS')
ExPanD(df = df_sample_adj, cs_id = c("gvkey", "cname"), ts_id = "fyear", config_list = ExPanD_config)
#ExPanD(df_sample_adj, cs_id = c("gvkey", "cname"))


# but are also shown by ...

# OLS regression analysis
tab_reg_simple <- lm(analy_folw_ln ~  size_ta + factor(ff48_ind), data = df_sample_adj)
summary(tab_reg_simple)


# Linear regression model controlling for industry wide effects summary
tab_reg_1 <- felm(analy_folw_ln ~ size_ta + size_employ + cap_expend + rnd_intens
                      + liquidity + roa +  leverage 
                      | ff48_ind, data = df_sample_adj)
summary(tab_reg_1)

# Linear regression model controlling for industry wide effects summary including rev_growth
tab_reg_2 <- felm(analy_folw_ln ~ size_ta + size_employ + cap_expend + rnd_intens
                    + liquidity + roa +  leverage + rev_growth
                    | ff48_ind, data = df_sample_adj)
summary(tab_reg_2)


#########################################################################################################

# DISPLAY DATA

fig_1 <- ggplot(df_sample_adj, aes(x = analy_folw, y = size_ta)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_1

fig_2 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = size_employ)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_2

fig_3 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = cap_expend)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_3

fig_4 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = rnd_intens)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_4

fig_5 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = liquidity)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_5

fig_6 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = roa)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_6

fig_7 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = eps)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_7

fig_8 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = rev_growth)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_8 

fig_9 <- ggplot(df_sample_adj, aes(x = analy_folw_ln, y = leverage)) + 
  geom_point(color = "lightblue", alpha = 0.2) +
  geom_smooth() + theme_classic()
fig_9


fig_ind_var <- ggplot(df_sample_adj, aes(x=analy_folw_ln, y=ta, color=ff48_ind, shape=ff48_ind)) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
fig_ind_var




