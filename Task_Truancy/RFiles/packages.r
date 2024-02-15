#if (!require("haven")) install.packages("haven")
library("ggbreak")
library("reshape2")
library("splines")
# https://rdrr.io/r/splines/bs.html
# https://wwenjie.org/splines2/ equal results for bs()
library("stringr")
#library("dvmisc")
library("haven")
library("foreign")
library("ggplot2")
library("bayesrules")
library("ggeffects")
library("rstanarm")
library("broom")
library("bayesplot")
library("labelled")
library("HuraultMisc")
library("loo")
# Windows 10 users: loo may be very slow if 'mc.cores' is set in your .Rprofile file (see https://github.com/stan-dev/loo/issues/94).
library("LaplacesDemon")
library("xtable")
library("fabricatr")
library("scoring")
library("mice")
library("dplyr")
library("broom.mixed")
library("ggpubr")
library("patchwork")
library("kableExtra")
library("xtable")
library("DescTools")
library("gridExtra")
library("sjPlot")
library("see")
library("bayestestR")
#library("plyr")

#library(tidyverse)        # ggplot, dplyr, %>%, and friends
library(brms)             # Bayesian modeling through Stan
library(tidybayes)        # Manipulate Stan objects in a tidy way
library(broom)            # Convert model objects to data frames
library(broom.mixed)      # Convert brms model objects to data frames
#library(vdemdata)         # Use data from the Varieties of Democracy (V-Dem) project
#library(betareg)          # Run beta regression models
#library(extraDistr)       # Use extra distributions like dprop()
library(ggdist)           # Special geoms for posterior distributions
#library(gghalves)         # Special half geoms
#library(ggbeeswarm)       # Special distribution-shaped point jittering
#library(ggrepel)          # Automatically position labels
#library(patchwork)        # Combine ggplot objects
#library(scales)           # Format numbers in nice ways
library(marginaleffects)  # Calculate marginal effects for regression models
#library(modelsummary)     # Create side-by-side regression tables