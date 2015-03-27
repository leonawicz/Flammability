


##
##
## AlfrescoCalibration.R

The `AlfrescoCalibration.R` script carries out the first and most basic round of post-processing of ALFRESCO simulation outputs.
The key features of the script include the creation of basic ALFRESCO model calibration plots:

*    Cumulative burn area vs. fire size
*    Cumulative burn area vs. time
*    Burn area vs. time
*    Distribution of maximum fire size

The script also saves key **R** workspace files.

Most files of interest are attached to an email which is sent from the Atlas cluster to intended recipients as part of the broader SLURM process.
This script is called by the SLURM script, `CompileData.slurm`, which is in turn first called by `RunAlfresco.slurm`.
First calling `RunAlfresco.slurm` to launch ALFRESCO is done via web GUI interface, using an **R** Shiny web application, `run_alfresco`, where various parameters of an ALFRESCO run are set by the user.

## R code


```r
comArgs <- commandArgs(TRUE)
print(comArgs)
if (length(comArgs > 0)) {
    arg.mat <- do.call("rbind", strsplit(comArgs, "="))
    options(warn = -1)
    arg.char <- which(is.na(as.numeric(arg.mat[, 2])))
    options(warn = 0)
    if (length(arg.char > 0)) 
        arg.mat[arg.char, 2] <- paste("'", arg.mat[arg.char, 2], "'", sep = "")
    eval(parse(text = apply(arg.mat, 1, paste, collapse = "=")))
}

if (exists("main")) dir.create(mainDir <- main, showWarnings = F) else stop("must provide 'main' directory")
if (exists("input")) dir.create(mainDir <- input, showWarnings = F) else stop("must provide 'input' directory")
if (exists("out")) dir.create(outDir <- out, showWarnings = F) else stop("must provide 'out' directory")
if (exists("yr.start") & exists("yr.end")) yrs <- yr.start:yr.end else yrs <- 1950:2013
if (!exists("baseline.year")) stop("baseline.year not found") else baseline.year <- as.numeric(baseline.year)
if (substr(tolower(alf.domain), 1, 6) == "statew") alf.domain <- "Statewide" else if (substr(tolower(alf.domain), 
    1, 6) == "noatak") alf.domain <- substr(alf.domain, 1, 6)

# setwd('/workspace/UA/mfleonawicz/leonawicz/projects/Flammability/code')
# load(paste0('../workspaces/alfresco/Alf_Calib_Setup_112912_', alf.domain,
# '.RData')) lapply(c('CABvsTimePlot.R', 'AByearPlot.R', 'fireSizePlot.R',
# 'CABvsFSPlot.R'), source)

# load(paste0('/big_scratch/mfleonawicz/Alf_Calib_Setup_112912_',
# alf.domain, '.RData'))
```
