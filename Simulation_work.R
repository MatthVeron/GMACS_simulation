# Author: Matthieu VERON
# Date: 12/23/2022

# OBJECT: Script to work on simulation project
# ------------------------------------------------
rm(list = ls())
library(gmr)

# 1- Working directory and needed ADMB directories ----
# Working directory
WD <- file.path(getwd(), "Aug_12_LatestGmacsNews/")

# Directories for ADMB
ADMBpaths <- file.path(getwd(), "ADpaths_Windows.txt")
# ------------------------------------------------

# 2- Build the GMACS executable
# ===================================== # ----
# Clean the initial files if needed
clean_root(path = WD)

# Create GMACS executable
createGmacsExe(vv = 1, Dir = WD, ADMBpaths = ADMBpaths)
# ------------------------------------------------

# 3- Check existence of needed folders
# ===================================== # ----

# Spc <- c("AIGKC/EAG", "AIGKC/WAG", "BBRKC", "SMBKC", "SNOW_crab")
Spc <- c("BBRKC")
# Create the sub directory if applicable
SetGMACSfold(
  vv = 1,
  Dir = WD,
  Spc = Spc,
  Sp = "all",
  verbose = TRUE
)
# ------------------------------------------------

# 4- Check if the assessment can be run
# ===================================== # ----
# Names of the GMACS version to consider
GMACS_version <- c("Dvpt_Version_Simulation")

# Define directories
VERSIONDIR <- WD

# Use Last Assessment for comparison?
# If yes, you must provide the names of the model for each species in the variable .ASSMOD_NAMES
# Those model folder must have to be hold in the folder Assessments
ASS <- FALSE

# Need to conpile the model?
# vector of length(.GMACS_version)
# 0: GMACS is not compiled. This assumes that an executable exists in the directory of the concerned version.
# 1: GMACS is compiled
COMPILE <- 0       # You already compile and build the executable

# Run GMACS
RUN_GMACS <- TRUE

# Use latest available data for the assessment?
LastAssDat <- FALSE

# Show Rterminal
VERBOSE <- TRUE

# Do comparison?
MAKE_Comp <- FALSE

res <- GMACS(
  Spc = Spc,
  GMACS_version = GMACS_version,
  Dir = VERSIONDIR,
  ASS = ASS,
  compile = COMPILE,
  run = RUN_GMACS,
  LastAssDat = LastAssDat,
  ADMBpaths = ADMBpaths,
  make.comp = MAKE_Comp,
  verbose = VERBOSE
)
# ------------------------------------------------

# 5- Read files
# ===================================== # ----

# Dir <- file.path(WD, "build/SNOW_crab")
# SNOWfiles <- readGMACSfiles(Dir = Dir, verbose = TRUE)
# GmacsFile <- SNOWfiles$GmacsFile
# DatFile <- SNOWfiles$DatFile
# CtlFile <- SNOWfiles$CtlFile
# PrjFile <- SNOWfiles$PrjFile
# ParFile <- readGMACSpar(Dir = Dir,
#                         verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)
# SimData <- readGMACSsimdat(Dir = Dir, verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)

# 
# Dir <- file.path(WD, "build/AIGKC/EAG")
# EAGfiles <- readGMACSfiles(Dir = Dir, verbose = TRUE)
# GmacsFile <- EAGfiles$GmacsFile
# DatFile <- EAGfiles$DatFile
# CtlFile <- EAGfiles$CtlFile
# PrjFile <- EAGfiles$PrjFile
# 
# ParFile <- readGMACSpar(Dir = Dir,
#                         verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)
# SimData <- readGMACSsimdat(Dir = Dir, verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)


# Dir <- file.path(WD, "build/AIGKC/WAG")
# WAGfiles <- readGMACSfiles(Dir = Dir, verbose = TRUE)
# GmacsFile <- WAGfiles$GmacsFile
# DatFile <- WAGfiles$DatFile
# CtlFile <- WAGfiles$CtlFile
# PrjFile <- WAGfiles$PrjFile
# ParFile <- readGMACSpar(Dir = Dir,
#                         verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)
# SimData <- readGMACSsimdat(Dir = Dir, verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)


# Dir <- file.path(WD, "build/SMBKC")
# SMBKCfiles <- readGMACSfiles(Dir = Dir, verbose = TRUE)
# GmacsFile <- SMBKCfiles$GmacsFile
# DatFile <- SMBKCfiles$DatFile
# CtlFile <- SMBKCfiles$CtlFile
# PrjFile <- SMBKCfiles$PrjFile
# ParFile <- readGMACSpar(Dir = Dir,
#                         verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)
# SimData <- readGMACSsimdat(Dir = Dir, verbose = TRUE, DatFile = DatFile, CtlFile = CtlFile)
# ------------------------------------------------


# 6- Simulation approach with GMACS ----
# =====================================

Nsim <- 1


# 6.1 Generate the simulated data
Gen_GmacsSim(path = WD,
  Stock = "BBRKC",
  RandomRec = TRUE,
  Ndev_Rec = 46,
  SigmaR = 0.6,
  RandSim = TRUE,
  UseParam = TRUE,
  No_Est = FALSE,
  Nsim = Nsim,
  seed_val = 781,
  verbose = FALSE
)

# 6.2 Run the simulations
RunGmacsSim(path = WD,
            Stock = "BBRKC",
            Nsim = Nsim,
            verbose = TRUE)




