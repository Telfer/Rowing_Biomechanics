# Rowing biomechanics animations
## Creates animations (.gifs) for paper "The effect of foot-stretcher position 
## and stroke rate on ergonmeter rowing kinematics"
## Author: Scott Telfer  scott.telfer@gmail.com


# =============================================================================

# libraries required
library(tidyverse)
library(fs)
library(ggplot2)
library(gganimate)
library(gifski)
library(transformr)


# =============================================================================

# helper functions
get_marker_mean <- function(df_markers, marker, fp, rate) {
  # marker
  m_trials <- which(str_detect(df_markers[2, ], marker))
  df_ <- df_markers[, m_trials]
  
  # fp
  fp_trials <- which(str_detect(df_[1, ], fp))
  df_ <- df_[, fp_trials]
  
  # rate
  r_trials <- which(str_detect(df_[1, ], rate))
  df_ <- df_[, r_trials]
  
  # averages
  ## Y
  df_Y <- df_[4:nrow(df_), ]
  df_Y <- mutate_all(df_Y, function(x) as.numeric(as.character(x)))
  df_Y <- df_Y[, seq(1, ncol(df_Y), 2)]
  rmy_22 <- rowMeans(df_Y, na.rm = TRUE)
  
  ## Z
  df_Z <- df_[4:nrow(df_), ]
  df_Z <- mutate_all(df_Z, function(x) as.numeric(as.character(x)))
  df_Z <- df_Z[, seq(2, ncol(df_Z), 2)]
  rmz_22 <- rowMeans(df_Z, na.rm = TRUE)
  
  # output df
  df_results <- data.frame(time = 1:101,
                           Rate = rep(rate, 101),
                           fp = rep(fp, 101),
                           marker = rep(marker, 101),
                           y = rmy_22,
                           z = rmz_22)
  
  # return
  return(df_results)
}


# =============================================================================

# format df
## load marker data (excluding header)
df_markers <- read.table("testtarget.txt", sep = "\t", skip = 5) 

## remove frame number column
df_markers <- df_markers[, 2:ncol(df_markers)]

## load header section
df_marker_names <- read.table("testtarget.txt", sep = "\t", nrows = 5, 
                              stringsAsFactors = FALSE)
## remove unneeded rows
df_marker_names <- df_marker_names[c(1, 2, 5), 2:ncol(df_marker_names)]

## remove file path from first row
df_marker_names[1, ] <- path_file(df_marker_names[1, ])

## join header and marker data
df_markers <- rbind(df_marker_names, df_markers)

## remove unneeded X data columns
x_cols <- which(df_markers[3, ] == "X")
df_markers <- df_markers[, -x_cols]


# =============================================================================

# create data frames for plotting
## create empty data frames to be filled with marker positions
df_rate <- data.frame(time = factor(), 
                      rate = factor(), 
                      fp = factor(), 
                      y = double(), 
                      z = double())
df_fp <- df_rate

## marker list
markers <- c("RTOE", "RANK", "RKNE", "RCIS", "T10", "RSHO", "C7", "RELB", "RWRB")

## stroke rate data frame
### variables
rates <- c("22", "26", "32")
fp <- "Neutral"
### process marker data
for (marker in seq_along(markers)) {
  for (rate in seq_along(rates)) {
    df_m <- get_marker_mean(df_markers, markers[marker], fp, rates[rate])
    df_rate <- rbind(df_rate, df_m)
  }
}

### Foot position data frame
#### variables
#rates <- "32"
#fp <- c("Minus 40", "Minus 20", "Neutral", "Plus 20", "Plus 40", "Toe down", "Toe up")
#### process marker data
#for (marker in seq_along(markers)) {
#  for (rate in seq_along(rates)) {
#    df_m <- get_marker_mean(df_markers, markers[marker], fp, rates[rate])
#    df_fp <- rbind(df_fp, df_m)
#  }
#}


# =============================================================================

# plot
## plot settings
theme_set(theme_void())

p <- ggplot(df_rate, aes(x = y, y = z, color = Rate))
p <- p + geom_point(show.legend = FALSE, alpha = 0.7)
p <- p + geom_path()
p <- p + coord_fixed()
p <- p + transition_time(time)
p
anim_save("rate.gif")

#p <- ggplot(df_fp, aes(x = z, y = y, color = fp))
#p <- p + geom_point(show.legend = FALSE, alpha = 0.7)
#p <- p + geom_path()
#p <- p + coord_fixed()
#p <- p + transition_time(time)
#p
#anim_save("footPosition.gif")
