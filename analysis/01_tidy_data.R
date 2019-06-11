#? This script ...

# Set working directory
setwd('/home/jrl/doct')

# Load packages
library('magrittr'); library('dplyr'); library('imputeTS')


# Load data and impute ----------

## Load
payDf <- readRDS('data/payments_nuts2.Rds')
esDf <- readRDS('data/eurostat_nuts2.Rds')

## Impute
vars <- c('agrarea_ha_total', 'population', 'gdp_mio_eur', 'emp_a_ths')
esDfImp <- split(esDf[, vars], esDf$geo) %>% 
  lapply(function(x) lapply(x, function(y) {
    if (is.numeric(y) & sum(!is.na(y)) > 2) na.interpolation(y, option = 'linear') else y
    })) %>% 
  bind_rows
esDfImp <- cbind(esDf, setNames(esDfImp, paste0('imp.', vars)))

## Visual test
testImp <- function(geo, var) {
  plot(esDf[esDf$geo == geo, c('time', var)], main = paste(geo))
  lines(esDfImp[esDfImp$geo == geo, c('time', var)])
}
testImp(sample(esDf$geo, 1), 'agrarea_ha_total')
  

# Merge and calculate variables ----------

Oie <- merge(payDf, esDfImp, all.x = T)
Oie %<>% mutate(eafrd.uaa = eafrd / imp.agrarea_ha_total, 
                eafrd.cap = eafrd / imp.population, 
                eafrd.gdp = eafrd / imp.gdp_mio_eur, 
                eafrd.emp = eafrd / imp.emp_a_ths, 
                gdp_eur_hab_thous = gdp_eur_hab / 1000, 
                hold.pc.lt2 = hold_hold_ha_lt2 / hold_hold_total * 100, 
                area.pc.lt2 = agrarea_ha_ha_lt2 / agrarea_ha_total * 100, 
                gfcf.pc.a = gfcf_a_mio_eur / gfcf_total_mio_eur * 100, # Gross fixed capital formation
                emp.pc.a = emp_a_ths / emp_total_ths * 100, 
                area.pc.organic = organic_agrarea_ha / agrarea_ha_total * 100, 
                hold.pc.organic = organic_hold_hold / hold_hold_total * 100)


# Save ----------

saveRDS(Oie, 'events/19_uppsala/analysis/table_nuts2.Rds')
