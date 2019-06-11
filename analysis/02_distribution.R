#? This script ...

# Set working directory
setwd('/home/jrl/doct/events/19_uppsala/')
setwd('C:/Users/lillemets/Dropbox/doct/events/19_uppsala')

# Load packages
library('magrittr'); library('dplyr')
library('ggplot2'); library('RColorBrewer')
library('spdep'); library('sf')

# Load objects
payDf <- readRDS('analysis/table_nuts2.Rds')
mapSf <- readRDS('analysis/maps.Rds')
mapSf <- mapSf[!(mapSf$geo %in%c('ES70', 'PT20', 'PT30')), ] # Tidy


# Inequality ----------

# Load package
library('ineq')

## Density
ggplot(payDf[payDf$time %in% 2007:2014, ]) + 
  aes(x = eafrd.cap, group = time, color = time) + 
  geom_density()

## Lorenz curve
### Calculate
payDf$eu15 <- substr(payDf$geo, 1, 2) %in% 
  c('BE', 'DK', 'DE', 'IE', 'GR', 'ES', 'FR', 'IT', 'LU', 'NL', 'AT', 'PT', 'FI', 'SE', 'GB')
Lcs <- list()
for (i in unique(payDf$eu15)) {
  for (j in 2007:2014) {
    Lcs[[paste(i)]][[paste(j)]] <- Lc(payDf[payDf$eu15 == i & payDf$time == j, 'eafrd'])
  }
}
### Convert to data.frame
lcsDf <- unlist(Lcs, recursive = F)
lcsDf <- lapply(lcsDf, function(x) do.call('cbind', x))
lcsDf <- cbind(eu15 = rep(strsplit(names(lcsDf), '\\.') %>% sapply(`[`, 1), 
                          times = sapply(lcsDf, nrow)), 
               time = rep(strsplit(names(lcsDf), '\\.') %>% sapply(`[`, 2), 
                          times = sapply(lcsDf, nrow)), 
               do.call('rbind', lcsDf)) %>% as.data.frame(stringsAsFactors = F)
lcsDf[, 2:ncol(lcsDf)] <- lapply(lcsDf[, 2:ncol(lcsDf)], as.numeric) %>% data.frame
### Plot
ggplot(lcsDf) + aes(x = p, y = L, group = eu15, color = eu15) + geom_line() + facet_wrap(~time)
ggplot(lcsDf) + aes(x = p, y = L, group = time, color = time) + geom_line() + facet_wrap(~eu15)

## Gini index
Ginis <- sapply(2007:2014, function(x) Gini(payDf[payDf$time == x, 'eafrd']))
barplot(Ginis, names.arg = 2007:2014)


# Spatial correlation ----------

paySf <- merge(mapSf, 
               aggregate(eafrd.cap ~ geo, payDf[payDf$time %in% 2007:2014, c('geo', 'eafrd.cap')], 
                         mean, na.rm = T))
paySf <- paySf[!is.na(paySf$eafrd.cap) & is.finite(paySf$eafrd.cap), ]
paySf$eafrd.cap <- scale(paySf$eafrd.cap) %>% c

#centroids <- paySf %>% st_centroid %>% st_coordinates
#nutsNb <- dnearneigh(centroids, d1 = 0, d2 = 2e5, longlat = T)
#hasNbs <- sapply(nutsNb, function(x) any(x != 0)) # Has neighbours?

nutsNb <- poly2nb(paySf)
spatWts <- nb2listw(nutsNb, style = 'W', zero.policy = T)
sink('results/moransi.txt');moran.test(paySf$eafrd.cap, spatWts, zero.policy = T);sink()
moran.plot(paySf$eafrd.cap, spatWts, zero.policy = T)
#abline(0,1, lty = 'dotted')

## Find quadrants
lags <- data.frame(var = paySf$eafrd.cap, lag = lag.listw(spatWts, paySf$eafrd.cap))
nonMiss <- !is.na(lags$lag)
lags <- lags[nonMiss, ]
quad <- apply(lags, 1, function(x) {
  meanVar <- mean(lags$var)
  meanLag <- mean(lags$lag)
  if (x[1] >= meanVar & x[2] >= meanLag) return('HH')
  if (x[1] < meanVar & x[2] >= meanLag) return('LH')
  if (x[1] < meanVar & x[2] < meanLag) return('LL')
  if (x[1] >= meanVar & x[2] < meanLag) return('HL')
})

## Moran's scatterplot
plot(lags, xlab = "Standardized EAFRD payments (€/person), 2007-2014 averages", 
     ylab = "Spatial lag")
abline(h = mean(lags$lag), lty = 'dotted'); abline(v = mean(lags$var),  lty = 'dotted')
abline(0, moran.test(paySf$eafrd.cap, spatWts, zero.policy = T)[[3]]) # Moran's I
abline(0, 1, lty = 'dashed')
corners <- list(c('HH', 'HL', 'LL', 'LH'), c('topright', 'bottomright', 'bottomleft', 'topleft'))
for (i in 1:4) legend(corners[[2]][i], corners[[1]][i], bty = 'n')
dev.print(png, 'results/map_scatter.png', height = 400, width = 600)

## Plot function
plotMap <- function(data, ...) {
  plot(data, xlim = c(2.7e6, 6.4e6), ylim = c(1.5e6, 5e6), 
       key.pos = 2, key.width = lcm(3), key.length = .3, cex.main = 1, 
       ...)
}

## Quadrant on Moran's scatterplot
moranMap <- merge(mapSf, data.frame(geo = paySf$geo[nonMiss], moran = quad), all.x = T)
plotMap(moranMap['moran'], pal = brewer.pal(n = 4, name = "RdBu"), 
        main = NULL)
dev.print(png, 'results/map_quad_cap.png', height = 500, width = 600)

## Local spatial association
lisa <- localmoran(paySf$eafrd.cap, spatWts, zero.policy = T)
### Histograms
#par(mfrow = 1:2)
#hist(lisa[lisa[, 'Pr(z > 0)'] < .05, 'Ii'], 20, xlim = c(-3,5))
#hist(lisa[lisa[, 'Pr(z > 0)'] >= .05, 'Ii'], 20, xlim = c(-3,5))
### Map 
lisaMap <- merge(mapSf, data.frame(geo = paySf$geo, lisa), all.x = T)
plotMap(lisaMap['Ii'], breaks = seq(-3, 4, 1), at = seq(-3, 4, 1), 
        pal = brewer.pal(n = 7, name = "RdBu"), 
        main = NULL)
dev.print(png, 'results/map_local.png', height = 500, width = 600)
par(mar = rep(1, 4))
plotMap(lisaMap['Ii'] %>% st_geometry, 
        main = "Pseudo p-value for local Moran's I value < 0.05")
plot(lisaMap %>% st_geometry %>% `[`(which(lisaMap$Pr.z...0. < .05)), col = 'red', add = T)
dev.print(png, 'results/map_local_sig.png', height = 500, width = 600)
dev.off()

#plot(lisaMap %>% st_geometry, xlim = c(2.7e6, 6.4e6), ylim = c(1.5e6, 5e6), 
#     main = "EAFRD payments (€/person), 2007-2014 averages")
#plot(lisaMap[which(lisaMap$Pr.z...0. < .05), 'Ii'], 
#     breaks = seq(-3, 4, 1), at = seq(-3, 4, 1), 
#     pal = brewer.pal(n = 7, name = "RdBu"),
#     add = T)

## Raw values
payMap <- merge(mapSf, payDf, all.x = T)
plotMap(payMap['eafrd.cap'], breaks = seq(0, 140, 20), at = seq(0, 140, 20), 
        pal = brewer.pal(n = 7, name = "Blues"), 
        main = "EAFRD payments (€/person), 2007-2014 averages")
dev.print(png, 'results/map_raw.png', height = 500, width = 600)

## Associations
par(mar = rep(1, 4))
plot(st_geometry(moranMap), xlim = c(2.7e6, 6.4e6), ylim = c(1.5e6, 5e6), 
     main = "Illustration of the spatial weights matrix")
plot(nutsNb, paySf %>% st_centroid %>% st_coordinates, 
     points = F, lwd = .5, col = 'red', add = T)
dev.print(png, 'results/map_weights.png', height = 500, width = 600)
dev.off()

## Moran tests
times <- 1992:2014
testsLs <- lapply(times, function(t) {
  paySf <- merge(mapSf, 
               aggregate(eafrd.cap ~ geo, payDf[payDf$time %in% t, c('geo', 'eafrd.cap')], 
                         mean, na.rm = T))
  paySf <- paySf[!is.na(paySf$eafrd.cap) & is.finite(paySf$eafrd.cap), ]
  nutsNb <- poly2nb(paySf)
  spatWts <- nb2listw(nutsNb, style = 'W', zero.policy = T)
  test <- moran.test(paySf$eafrd.cap, spatWts, zero.policy = T)
  return(c(test$estimate, 'P-value' = test$p.value))
})
testsMat <- do.call('rbind', testsLs)
plot(times, testsMat[, 'Moran I statistic'], type = 'l', ylim = 0:1, 
     main = "Spatial autocorrelation of EAGGF/EAFRD payments", 
     xlab = NA, ylab = "Moran's I")
for (i in c(2000, 2007, 2014)) abline(v = i, lty = 'dotted')
#for (i in c(1995, 2004, 2007)) abline(v = i, lty = 'dashed')
dev.print(png, 'results/moransi.png', height = 400, width = 600)
