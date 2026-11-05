# Setup ----
here::i_am("init.R")
library(here)

# data
load(here::here( "sysdata.rda"))

# broadly useful tidyverse packages loaded here.
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)

# Colors & Misc ----
# Paul Tol's 10-color polychrome palette (colorblind-friendly)
# https://cran.r-project.org/web/packages/khroma/vignettes/tol.html
# https://sronpersonalpages.nl/~pault/data/colourschemes.pdf
tol_colors <- c(
  "#332288",  # indigo
  "#88CCEE",  # cyan
  "#44AA99",  # teal
  "#117733",  # green
  "#999933",  # olive
  "#DDCC77",  # sand
  "#CC6677",  # rose
  "#882255",  # wine
  "#AA4499",  # purple
  "#DDDDDD"   # pale grey
)

tol_seq_colors <- c(
  "#364B9A",
  "#4A7BB7",
  "#6EA6CD",
  "#98CAE1",
  "#C2E4EF",
  "#EAECCC",
  "#FEDA8B",
  "#FDB366",
  "#F67E4B",
  "#DD3D2D",
  "#A50026"
)

# https://colorany.com/color-palettes/color-blind-friendly-color-palettes/
# selections from Vibrant Nature Colors for All Eyes
rating_colors <- c(
  "#4CAF50",
  "#8BC34A",
  "#FFEB3B",
  "#FFC107",
  "#FF5722",
  "#795548"
)

# Scenario Names
snm <- c("Eco Machine",
         "New Green Watershed",
         "Tunnel",
         "Bolster & Fortify",
         "Calling on Reserves",
         "30% Increased Tiered Outflow")



# TODO
# gather this from implementation codebook (edit implementation codebook, load in deltagov, export to transgov)
icb <- data.frame(
  level = c(LETTERS[1:5], "X"),
  desc = c(
    "Directly Performed in the Just Transitions Scenario (JTS)",
    "Partly Performed in the JTS",
    "A probable precondition of the JTS (even if the target differs)",
    "Supportive of the JTS with alterations",
    "Broadly beneificial to Delta salinity management without impeding the JTS, and lacking a specific tie-in to the JTS.",
    "Largely irrelevant to the JTS"
  ),
  score = c(1, 0.75, 0.5, 0.25, 0.2, 0)
)

source(here::here("viz.R"))
source(here::here("data_wrangling.R"))

viz_node_sizes <- c(10, 45)
