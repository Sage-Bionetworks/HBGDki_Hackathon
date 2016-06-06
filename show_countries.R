#-----------------------------------------------------------------------------------------------------
#
#  Code to create a map of the world that shows countries that are included
#  in the dataset of country-level indicators
#
#-----------------------------------------------------------------------------------------------------

library(maps)

# Source in the country mapping function
source('ctry_map_function.R')


# Read dataset
dat0 <- read.csv("dhsipums_macro.csv")


# Get unique countries into a dataframe and assign country colors
ctryf <- unique(dat0[,c('RMAPS','CTRYCD','Region')])
ctryf$COL <- 'blue'


# Create a country map
ctry_map(fname='show_countries'  # Name of output file (without .png extension)
  ,countries=ctryf$RMAPS         # Vector of country names 
  ,c.colors=ctryf$COL            # Vector of country colors (same length as countries)
)



# Closeout
dev.off()
warning()



