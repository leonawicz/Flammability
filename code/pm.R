# @knitr create_project
source("C:/github/ProjectManagement/code/rpm.R") # eventually load a package instead of source script
proj.name <- "Flammability" # Project name
proj.location <- matt.proj.path # Use default file location

docDir <- c("Rmd/include", "md", "html", "Rnw", "pdf", "timeline")
newProject(proj.name, proj.location, docs.dirs=docDir, overwrite=T) # create a new project

rfile.path <- file.path(proj.location, proj.name, "code") # path to R scripts
docs.path <- file.path(proj.location, proj.name, "docs")
rmd.path <- file.path(docs.path, "Rmd")

# generate Rmd files from existing R scripts using default yaml front-matter
genRmd(path=rfile.path) # specify header.args list argument if necessary

# @knitr update_project
# update yaml front-matter only
genRmd(path=rfile.path, update.header=TRUE)

# obtain knitr code chunk names in existing R scripts
chunkNames(path=file.path(proj.location, proj.name, "code"))

# append new knitr code chunk names found in existing R scripts to any Rmd files which are outdated
chunkNames(path=file.path(proj.location, proj.name, "code"), append.new=TRUE)

# @knitr website
# Setup for generating a project website
user <- "leonawicz"
proj.github <- file.path("https://github.com", user, proj.name)
index.url <- "index.html"
#file.copy(index.url, "index.html")

proj.title <- "Flammability"
proj.menu <- c("Overview", "Data Prep", "GBM Flammability", "ALFRESCO", "Statistics", "Apps", "All Projects")

proj.submenu <- list(
	c("empty"),
	c("clim_resample_2km_1km.R", "clim_1km_clip2ak.R", "meanTPbyVegClass_CRU31.R", "meanTPbyVegClass_CMIP5.R"),
	c("GBM Modeling", "gbm.R", "divider", "Flammability maps", "gbm_flam_prep.R", "gbm_flam_maps.R", "divider", "ALFRESCO prep", "duplicate_flam_maps.R", "FlammabilityMapMultipliers.R"),
	c("Main scripts", "AlfrescoCalibration.R", "AlfrescoFRP.R", "fseByVeg.R", "divider", "Supporting scripts", "obs_fire_setup.R", "divider", "Functions", "histPrep.R", "fireSizePlot.R", "AByearPlot.R", "CABvsFSPlot.R", "CABvsTimePlot.R"),
	c("Fire size EDA and MLE", "fseMLE.R"),
	c("ALFRESCO launcher", "divider", "Results app"), # Insert code files from both apps
	c("empty")
)

proj.files <- list(
	c("index.html"),
	c("clim_resample_2km_1km.html", "clim_1km_clip2ak.html", "meanTPbyVegClass_CRU31.html", "meanTPbyVegClass_CMIP5.html"),
	c("header", "gbm.html", "divider", "header", "gbm_flam_prep.html", "gbm_flam_maps.html", "divider", "header", "duplicate_flam_maps.html", "FlammabilityMapMultipliers.html"),
	c("header", "AlfrescoCalibration.html", "AlfrescoFRP.html", "fseByVeg.html", "divider", "header", "obs_fire_setup.html", "divider", "header", "histPrep.html", "fireSizePlot.html", "AByearPlot.html", "CABvsFSPlot.html", "CABvsTimePlot.html"),
	c("header", "fseMLE.html"),
	c("header", "divider", "header"), # Insert html files for code from both apps
	c("http://leonawicz.github.io")
)

# generate navigation bar html file common to all pages
genNavbar(htmlfile=file.path(rmd.path, "include/navbar.html"), title=proj.title, menu=proj.menu, submenus=proj.submenu, files=proj.files, site.url=proj.github, include.home=FALSE)

# generate _output.yaml file
# Note that external libraries are expected, stored in the "libs" below
yaml.out <- file.path(proj.location, proj.name, "docs/Rmd/_output.yaml")
libs <- "libs"
common.header <- "include/in_header.html"
genOutyaml(file=yaml.out, lib=libs, header=common.header, before_body="include/navbar.html", after_body="include/after_body.html")

# @knitr knit_setup
library(rmarkdown)
library(knitr)
setwd(rmd.path)

# Rmd files
files.Rmd <- list.files(pattern=".Rmd$", full=T)

# @knitr save
# write all yaml front-matter-specified outputs to Rmd directory for all Rmd files
lapply(files.Rmd, render, output_format="all")
insert_gatc(list.files(pattern=".html$"))
moveDocs(path.docs=docs.path)

# if also making PDFs for a project, speed up the Rmd to Rnw file conversion/duplication
rnw.path <- file.path(docs.path, "Rnw")
setwd(rnw.path)
#themes <- knit_theme$get()
highlight <- "solarized-dark"
convertDocs(path=rmd.path, emphasis="replace", overwrite=TRUE, highlight=highlight) # Be careful
lapply(list.files(pattern=".Rnw$"), knit2pdf)
moveDocs(path.docs=docs.path, type="pdf", remove.latex=FALSE)
