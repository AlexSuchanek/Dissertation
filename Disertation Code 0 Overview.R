################################
## Disertation Code 0 Run All ##
################################

#### Notes ####

# These notes will move to the markdown file on Github soon - just putting down thoughts as
# I think of them

# There is a lot of data that needs to be loaded. Make sure that the system has
# at least 4GB of RAM

# If you want to look at all of the workspace items just remove the rm() commands

# As well as the required packages one requires a working version of MPI for your OS
# the npRMPI package is from the archive on CRAN

# The data.table package is the development branch, downloaded from github which requires 
# the devtools() package. 

# The npRMPI package is quite unstable. The models have been run on a Linux system, using the 14.04 LTS
# version of Ubuntu

## Required Packages ##

require(reshape2)
require(np)
require(xtable)
require(readxl)
require(data.table)

