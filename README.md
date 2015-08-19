# Dissertation
This is the .R and .Rnw files used to write my dissertation on parametric and nonparametric methods for discrete choice models. This will include the rest of the abstract eventually.

## Basic Requirements

In order to run the code one first needs a few initial things:

1. Access to the UK data service to download the data. This requires the signing of the EUL, which is on the website.
2. A working installation of R and Latex
3. At least 4GB of system memory - a lot is used when reading in files. 

## Downloading & Filing the Data

The dataset is the National Transport Survey, (NTS), dataset. Only the first level of the dataset is required. The filetype to download is the .tab format. Extract the .tab files and the .xls files to a directory that will be the working directory for the R code.

## Running the Code

There is a list of required packages that one needs to have to run the code:

1. np
2. DataTable - this is the 1.9.5 version on github
3. Devtools - to install the dev version of DataTable from github
4. Rmpi
5. npRmpi
6. readxls
7. knitr

The Rmpi and npRmpi packages allow the code to be executed in parallel which greatly speeds up the estimation procedure. This is not essential. Simply remove the mpi code and run the basic np functions instead. This code was run on Ubuntu 14.04 LTS using OpenMPI. 





