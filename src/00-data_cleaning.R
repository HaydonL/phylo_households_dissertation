# This file cleans the pairs_tsi.csv data set to remove strange observations and
# then saves it as pairs_tsi_clean.csv in the data folder.
library(data.table)

pairs_tsi <- read.csv(here::here("data", "pairs_tsi.csv"))
setDT(pairs_tsi)

# Check whether same household pairs are the same community
same_hh_pairs <- pairs_tsi[same_hh == 1]
source_comm <- same_hh_pairs[,.(COMM.SOURCE)]
recip_comm <- same_hh_pairs[,.(COMM.RECIPIENT)]

source_comm == recip_comm
same_hh_pairs[81,] # Observation 226 to be removed from original data

pairs_tsi <- pairs_tsi[-226,]

# Check minimum and maximum ages are >15 and <50 respectively
min(pairs_tsi[,.(AGE_TRANSMISSION.SOURCE)])
max(pairs_tsi[,.(AGE_TRANSMISSION.SOURCE)])

min(pairs_tsi[,.(AGE_INFECTION.RECIPIENT)])
max(pairs_tsi[,.(AGE_INFECTION.RECIPIENT)])

# Save cleaned data
write.csv(pairs_tsi, file = here::here("data", "pairs_tsi_clean.csv"))
