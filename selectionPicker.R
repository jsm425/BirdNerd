### packages
library(dplyr)
library(rebird)

###
# key
apiKey = "5bs9bui5p642"
# get most recent sightings per species in US in last ten days
ebirdregion(loc = "US", key = apiKey, back = 10) %>%
  arrange(speciesCode) -> birdSightings
# set params
level1Check <- 0; level2Check <- 0; level3Check <- 0; level4Check <- 0; level5Check <- 0
# try random birds that've been sighted until you have levels 1-5
while (sum(level1Check, level2Check, level3Check, level4Check, level5Check) < 5) {
  # random bird species from sightings
  randomRow <- sample(1:nrow(birdSightings), size = 1)
  randomBird <- birdSightings[randomRow, ]$speciesCode
  # query that species
  rbirdSightings <- ebirdregion(loc = "US", key = apiKey, back = 10, species = randomBird)
  # assign it to question based on number of sightings returned
  numSightings <- nrow(rbirdSightings)
  # level 5
  if (numSightings <= 10) {
    if (level5Check == 0) {
      # if not already set, set it and flip the check
      level5Sightings <- rbirdSightings
      level5Check <- 1
      print("level5")
    }
  # level 4
  } else if ( (numSightings > 25) & (numSightings <= 100) ) {
    if (level4Check == 0) {
      # if not already set, set it and flip the check
      level4Sightings <- rbirdSightings
      level4Check <- 1
      print("level4")
    }
  # level 3
  } else if ( (numSightings > 200) & (numSightings <= 400) ) {
    if (level3Check == 0) {
      # if not already set, set it and flip the check
      level3Sightings <- rbirdSightings
      level3Check <- 1
      print("level3")
    }
  # level 2
  } else if ( (numSightings > 500) & (numSightings <= 1000) ) {
    if (level2Check == 0) {
      # if not already set, set it and flip the check
      level2Sightings <- rbirdSightings
      level2Check <- 1
      print("level2")
    }
  # level 2
  } else if ( (numSightings > 1000) & (numSightings <= 5000) ) {
    if (level1Check == 0) {
      # if not already set, set it and flip the check
      level1Sightings <- rbirdSightings
      level1Check <- 1
      print("level1")
    }
  } else { print("level0") }
}


### write these to csvs in folder
#$ setwd(r"{D:\~Programming\Rthings\BirdNerd}")
write.csv(level1Sightings, file = "BirdSightingLists\\level1Sightings.csv")
write.csv(level2Sightings, file = "BirdSightingLists\\level2Sightings.csv")
write.csv(level3Sightings, file = "BirdSightingLists\\level3Sightings.csv")
write.csv(level4Sightings, file = "BirdSightingLists\\level4Sightings.csv")
write.csv(level5Sightings, file = "BirdSightingLists\\level5Sightings.csv")
