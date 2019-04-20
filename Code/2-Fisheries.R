# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  LIBRARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sf)
library(magrittr)
library(tidyverse)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                    DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
load('./data/rawData/zif.RData')
load('./Data/Grids/Data/egslGrid.RData')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                            GEAR CLASS & MOBILITY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Divide fisheries data by gear class
zif$gearClass <- ""
zif$gearClass[zif$Engin == "Casier"]                     <- 'DNH'
zif$gearClass[zif$Engin == "Chalut de fond"]             <- 'DD'
zif$gearClass[zif$Engin == "Drague"]                     <- 'DD'
zif$gearClass[zif$Engin == "Filet maillant"]             <- 'PHB'
zif$gearClass[zif$Engin == "Peche a la ligne"]           <- 'PLB'
zif$gearClass[zif$Engin == "Palangre"]                   <- 'PHB'
zif$gearClass[zif$Engin == "Plongee"]                    <- 'DNL'
zif$gearClass[zif$Engin == "Senne bourse"]               <- 'PLB'
zif$gearClass[zif$Engin == "Senne Danoise ou Ecossaise"] <- 'DNH'
zif$gearClass[zif$Engin == "Senne de rivage"]            <- 'DNH'
zif$gearClass[zif$Engin == "Trappe"]                     <- 'DNH'
zif$gearClass[zif$Engin == "Turlutte"]                   <- 'PLB'

# Divide geats between fixed or mobile
zif$gearMob <- ""
zif$gearMob[zif$Engin == "Casier"]                      <- 'Fixed'
zif$gearMob[zif$Engin == "Chalut de fond"]              <- 'Mobile'
zif$gearMob[zif$Engin == "Drague"]                      <- 'Mobile'
zif$gearMob[zif$Engin == "Filet maillant"]              <- 'Fixed'
zif$gearMob[zif$Engin == "Peche a la ligne"]            <- 'Fixed'
zif$gearMob[zif$Engin == "Palangre"]                    <- 'Fixed'
zif$gearMob[zif$Engin == "Plongee"]                     <- 'Fixed'
zif$gearMob[zif$Engin == "Senne bourse"]                <- 'Fixed'
zif$gearMob[zif$Engin == "Senne Danoise ou Ecossaise"]  <- 'Fixed'
zif$gearMob[zif$Engin == "Senne de rivage"]             <- 'Fixed'
zif$gearMob[zif$Engin == "Trappe"]                      <- 'Fixed'
zif$gearMob[zif$Engin == "Turlutte"]                    <- 'Fixed'

# Add buffer around fishing activities based on their mobility
# Fixed and mobile gear ID
fix <- zif$gearMob == 'Fixed'
mob <- zif$gearMob == 'Mobile'

# Buffers
fix <- st_buffer(zif[fix, ], 200)
mob <- st_buffer(zif[mob, ], 2000)

# Polygons dataset
zif <- rbind(fix, mob)
zif <- zif[order(as.numeric(gsub('zif', '', zif$ID))), ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                               FISHING INTENSITY
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source fishing metrics functions
source('./Code/fishingMetrics.R')

# Calculate total biomass captured during each fishing event
zif$catch <- rowSums(zif[, 5:57, drop = T]) # !! Change the numbered index for species names !!

# Get combinations of years and gear type as vector to divide the analysis
years <- unique(zif$year)
type <- unique(zif$gearClass)
comb <- expand.grid(type, years, stringsAsFactors = F)
colnames(comb) <- c('type','years')

# Empty list to store fishing intensity by gear type and by year
intensity <- vector('list', nrow(comb))
names(intensity) <- apply(comb, 1, paste, collapse = "-")

# Fishing intensity evaluation
for(i in 1:length(intensity)) {
  id <- zif$year == comb[i, 'years'] & zif$gearClass == comb[i, 'type']
  intensity[[i]] <- fishingMetrics(zif[id, ], egslGrid)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  SUMMARISE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Divide between gear types
dnl <- intensity[comb$type == 'DNL']
dnh <- intensity[comb$type == 'DNH']
dd  <- intensity[comb$type == 'DD']
phb <- intensity[comb$type == 'PHB']
plb <- intensity[comb$type == 'PLB']

# Transform as dataframe
dnl <- dplyr::bind_cols(dnl)
dnh <- dplyr::bind_cols(dnh)
dd <- dplyr::bind_cols(dd)
phb <- dplyr::bind_cols(phb)
plb <- dplyr::bind_cols(plb)

# Remove extra ID columns
# Names of columns to remove
id <- paste0('ID', seq(1:((ncol(dnl)-2)/2)))

# Remove columns
dnl <- dnl[, !colnames(dnl) %in% id]
dnh <- dnh[, !colnames(dnh) %in% id]
dd  <- dd[, !colnames(dd) %in% id]
phb <- phb[, !colnames(phb) %in% id]
plb <- plb[, !colnames(plb) %in% id]

# Annual mean value per grid cell
dnl$mean <- rowMeans(dnl[, -1])
dnh$mean <- rowMeans(dnh[, -1])
dd$mean  <- rowMeans(dd[, -1])
phb$mean <- rowMeans(phb[, -1])
plb$mean <- rowMeans(plb[, -1])

# Add to grid and select only the annual mean (remove this if you wish to consider years separately)
fishMean <- egslGrid %>%
            dplyr::left_join(., dnl[, c('ID','mean')], by = 'ID') %>%
            dplyr::left_join(., dnh[, c('ID','mean')], by = 'ID') %>%
            dplyr::left_join(., dd[, c('ID','mean')], by = 'ID') %>%
            dplyr::left_join(., phb[, c('ID','mean')], by = 'ID') %>%
            dplyr::left_join(., plb[, c('ID','mean')], by = 'ID') %>%
            setNames(c('ID','FisheriesDNL','FisheriesDNH','FisheriesDD','FisheriesPHB','FisheriesPLB','geometry'))


# Single object per fisheries type
fisheriesDNL <- fishMean[, c('ID','FisheriesDNL')]
fisheriesDNH <- fishMean[, c('ID','FisheriesDNH')]
fisheriesDD  <- fishMean[, c('ID','FisheriesDD')]
fisheriesPHB <- fishMean[, c('ID','FisheriesPHB')]
fisheriesPLB <- fishMean[, c('ID','FisheriesPLB')]

# Remove empty cells
fisheriesDNL <- fisheriesDNL[fisheriesDNL$FisheriesDNL > 0, ]
fisheriesDNH <- fisheriesDNH[fisheriesDNH$FisheriesDNH > 0, ]
fisheriesDD <- fisheriesDD[fisheriesDD$FisheriesDD > 0, ]
fisheriesPHB <- fisheriesPHB[fisheriesPHB$FisheriesPHB > 0, ]
fisheriesPLB <- fisheriesPLB[fisheriesPLB$FisheriesPLB > 0, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                  EXPORT DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export object as .RData
save(fisheriesDNL, file = './Data/Driver/FisheriesDNL.RData')
save(fisheriesDNH, file = './Data/Driver/FisheriesDNH.RData')
save(fisheriesDD, file = './Data/Driver/FisheriesDD.RData')
save(fisheriesPHB, file = './Data/Driver/FisheriesPHB.RData')
save(fisheriesPLB, file = './Data/Driver/FisheriesPLB.RData')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                                 VISUALIZE DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png('./Figures/FisheriesDNL.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(fisheriesDNL[, 'FisheriesDNL'], border = 'transparent')
dev.off()

png('./Figures/FisheriesDNH.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(fisheriesDNH[, 'FisheriesDNH'], border = 'transparent')
dev.off()

png('./Figures/FisheriesDD.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(fisheriesDD[, 'FisheriesDD'], border = 'transparent')
dev.off()

png('./Figures/FisheriesPHB.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(fisheriesPHB[, 'FisheriesPHB'], border = 'transparent')
dev.off()

png('./Figures/FisheriesPLB.png', width = 1280, height = 1000, res = 200, pointsize = 6)
plot(fisheriesPLB[, 'FisheriesPLB'], border = 'transparent')
dev.off()
