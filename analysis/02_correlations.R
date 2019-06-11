#? This script ...

# Set working directory
setwd('/home/jrl/doct/events/19_uppsala/')

# Load packages
library('magrittr'); library('Hmisc'); library('knitr'); library('stargazer')

# Load objects
payDf <- readRDS('analysis/table_nuts2.Rds')


# Change ----------

## Scatterplot of change
#plot(payDf$eafrd.cap, payDf$agrarea_ha_total, type = 'n')
#for (i in unique(payDf$geo)[20:30]) {
#  payDf[payDf$geo == i, c('eafrd.cap', 'agrarea_ha_total')] %>% .[complete.cases(.), ] %>% lines
#}


# Correlations with variables ----------

## Select variables and times
vars <- c('eafrd.cap', 'gdp_eur_hab_thous', 'unemp_1574', 
          'hold.pc.lt2', 'area.pc.lt2', 'emp.pc.a', 'gfcf.pc.a', 
          'hold.pc.organic', 'area.pc.organic')
names <- c('EAFRD payments (€/person)', 'GDP (1000 €/person)', 'Unemployment, ages 15-74 (%)', 
           'Share, holdings <2 ha (%)', 'Area, holdings <2 ha (%)', 
           'Employment, NACE2 A (%)', 'Investments, NACE2 A (%)', 
           'Share, organic holdings (%)', 'Area, organic holdings (%)')

## Subset data
payDfCc <- payDf[complete.cases(payDf[, vars]), c('geo', 'time', vars)]
#table(as.character(payDfCc$geo), payDfCc$time) # Visualize missing
#length(unique(payDfCc$geo)); length(unique(substr(payDfCc$geo, 1, 2))) # Count included
times <- sort(unique(payDfCc$time)) %>% tail(3)

## Calculate correlations
corLs <- lapply(times, function(t) {
  payDfCc[payDfCc$time == t, vars] %>% as.matrix %>% rcorr(type = 'pearson')
})
#names(corLs) <- times

## Create and save a table of correlations
getCor <- function(stat) {
  corLs %>% lapply(`[`, stat) %>% unlist(recursive = F) %>% lapply(subset, select = 'eafrd.cap')
}
corTab <- getCor(c('r', 'P')) %>% do.call('cbind', .)
dimnames(corTab)[[2]] <- rep(c('r', 'p'), length.out = length(times) * 2)
dimnames(corTab)[[1]] <- names
stargazer(corTab, type = 'text', out = 'results/cor_vars.html', title = paste(times, collapse = ', '), 
          digit.separate = 0, digits = 3, digits.extra = 0)

## Save summary tables of the correlations
sink('results/cor_vars_sum.html')
for (i in times) {
  stargazer(payDfCc[payDfCc$time == i, vars], type = 'html', summary = T, title = paste(i), 
            digit.separate = 0, digits = 3, digits.extra = 0)
}
sink()


# Correlations with other funds ----------

## Select variables and times
vars <- c('eafrd', 'cf', 'erdf', 'esf')
names <- c('EAFRD', 'Cohesion Fund', 'European Regional Development Fund', 'European Social Fund')
#times <- 2007:2014
#times <- c('1993-1999', '2000-2006', '2007-2013')

## Aggregate data
#periods <- data.frame(period = rep(times, each = 7), year = 1993:2013, stringsAsFactors = F)
#payDf$period <- periods$period[match(payDf$time, periods$year)]
#payDfCor <- aggregate(cbind(cf, eafrd, erdf, esf) ~ geo + period, 
#                      payDf[!is.na(payDf$period), c('geo', 'period', vars)], sum, na.rm = T, 
#                      na.action = 'na.pass')

## Calculate correlations
corLs <- lapply(times, function(t) {
  payDf[payDf$time == t, vars] %>% as.matrix %>% rcorr(type = 'pearson')
})
#names(corLs) <- times

## Create and save a table of correlations
getCor <- function(stat) {
  corLs %>% lapply(`[`, stat) %>% unlist(recursive = F) %>% lapply(subset, select = 'eafrd')
}
corTab <- getCor(c('r', 'P')) %>% do.call('cbind', .)
dimnames(corTab)[[2]] <- rep(c('r', 'p'), length.out = length(times) * 2)
dimnames(corTab)[[1]] <- names
stargazer(corTab, type = 'text', out = 'results/cor_funds.html', title = paste(times, collapse = ', '), 
          digit.separate = 0, digits = 3, digits.extra = 0)

## Save summary tables of the correlations
#stargazer(payDf[, vars], type = 'text', summary = T, digits = 0, 
#          title = paste(times, collapse = ', '), out = 'cor_funds_sum.html')
sink('results/cor_funds_sum.html')
for (i in times) {
  stargazer(payDf[payDf$time == i, vars] / 1e6, type = 'html', summary = T, 
            digits = 3, digits.extra = 3, digit.separate = 0, 
            title = paste(i))
}
sink()
