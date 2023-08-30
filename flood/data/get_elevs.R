library(elevatr)

# Read in
load("slosh_dat_nj.rda")
prj_dd <- "EPSG:4326"
coords_df <- as.data.frame(coords)

# Set up subsets
nGroups <- 50
groupSize <- 1000
n <- nrow(coords_df)
groupStarts <- seq(1, (nGroups - 1) * groupSize, by = groupSize)
indices <- lapply(groupStarts, function(start) {
  end <- start + groupSize - 1
  return(c(start:end))
})
indices[[nGroups]] <- ((nGroups - 1) * groupSize + 1):n

# Read and save elevation data, one subset at a time
for (i in 1:nGroups) {
  index <- indices[[i]]
  subset <- coords_df[index, ]
  elevs <- get_elev_point(subset, prj = prj_dd, src = "epqs")
  new_df <- data.frame(subset, elev_meters = elevs$elevation)
  saveRDS(new_df, paste0("elevs/subset", i, ".RDS"))
  cat(paste0("Saved subset ", i, ".\n"))
}


# Combine elevation data from all subsets
subsets <- lapply(1:nGroups, function(i) {
  readRDS(paste0("elevs/subset", i, ".RDS"))
})
full_df <- do.call(rbind.data.frame, subsets)
saveRDS(full_df, "elev_data.RDS")

