#--------------------#
#renv::activate()

# Packages -----

## Shiny -----
# Install and load required packages
if (!require("shiny")) {
  install.packages("shiny")
}
library(shiny)

## Tidyverse -----
# Install and load required packages
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
library(tidyverse)

## Shiny Dashboard -----
# Install and load required packages
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
}
library(shinydashboard)

## Shiny Widgets -----
# Install and load required packages
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
}
library(shinyWidgets)

## Shiny Dashboard Plus -----
# Install and load required packages
if (!require("shinydashboardPlus")) {
  install.packages("shinydashboardPlus")
}
library(shinydashboardPlus)

## Shiny Custom Loader -----
# Install and load required packages
if (!require("shinycustomloader")) {
  install.packages("shinycustomloader")
}
library(shinycustomloader)

## DT -----
# Install and load required packages
if (!require("DT")) {
  install.packages("DT")
}
library(DT)

## eCharts4R -----
# Install and load required packages
if (!require("echarts4r")) {
  install.packages("echarts4r")
}
library(echarts4r)

## Rmarkdown -----
# Install and load required packages
if (!require("markdown")) {
  install.packages("markdown")
}
library(markdown)

#--------------------#

# Functions -----

## Est 1RM Value -----
E1RMcalc <- function(df, exercise) {
  e <- exercise
  
  # Set MVT
  mvt <- LVmvt(e)
  
  # Clean data
  data <- df 
  
  # linear model
  lvFit <- lm(data = data, Velocity ~ Load)
  
  # y-intercept
  yInt <- as.double(lvFit$coefficients[1])
  
  # slope
  slope <- as.double(lvFit$coefficients[2])
  
  # estimate 1RM
  e1rm <- round((mvt - yInt)/slope,0)
  
  return(e1rm)
}

## Minimum Velocity Threshold (MVT) -----
LVmvt <- function(exercise) {
  ## Selected exercise for mvt ##
  e <- exercise
  
  ## set mvt ##
  mvt <- ifelse(
    e == "Bench Press", 0.17,
    ifelse(
      e == "Prone Bench Pull", 0.5,
      ifelse(
        e == "Prone Pull Up", 0.23,
        ifelse(
          e == "Seated Military Press", 0.19,
          ifelse(
            e == "Lat Pulldown", 0.47,
            ifelse(
              e == "Seated Cable Row", 0.4,
              ifelse(
                grepl("Squat",e), 0.30,
                ifelse(
                  grepl("Deadlift",e), 0.15,
                  ifelse(
                    e == "Hip-Thrust", 0.25,
                    0.2
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  
  return(mvt)
}

## LV Coefficients -----
## takes df with exercise, loads, velocity and returns coefficients from linear
## model to calculate profile variables and return them as a new table
LVcoef <- function(data) {

  #- Inputs -#
  ## Pre-filtered data ##
  df <- data
  
  ## Selected exercise for mvt ##
  e <- df$Exercise[1]
  
  ## set mvt ##
  mvt <- LVmvt(e)
  
  #- linear regression model -#
  ## lm model
  dflm <- lm(Velocity ~ Load, data = df)
  
  #- lm coefficients -#
  # y-intercept
  yInt <- as.double(dflm$coefficients[1])
  
  # slope
  slope <- as.double(dflm$coefficients[2])
  
  # estimate L0
  L0 <- round((0 - yInt)/slope,0)
  
  # estimate 1RM
  e1rm <- round((mvt - yInt)/slope,0)
  
  #- output dataframe -#
  dframe <- data.frame(e1rm, L0, slope, yInt, mvt)
  
  return(dframe)
  
}

## LV Data Frame -----
## takes LVcoef data to find theoretical max load. Then creates vector of 
## increments of 1 from 0 to max load. After using LVcoef data to create
## training zones with tzones function, a new table is made with the vector of
## loads, calculated velocities, and assigned training zones.
LVdf <- function(LVcoef) {
  
  # LV Coefficients df
  df <- LVcoef
  
  # Y-Intercept
  b <- df$yInt[1]
  
  # Slope
  m <- df$slope[1]
  
  # Theoretical Peak Load
  L0 <- df$L0[1]
  
  # Load Sequence
  Loads <- as.vector(seq(from = 0, to = L0, by = 1))
  
  #- Training Zones -#
  tzones <- trnZones(df)
  
  # Max Strength
  MaxStr1 <- tzones$MaxStr[1]
  MaxStr2 <- tzones$MaxStr[2]
  
  # Strength-Speed
  StrSpd1 <- tzones$StrSpd[1]
  StrSpd2 <- tzones$StrSpd[2]
  
  # Peak Power
  PeakP1 <- tzones$PeakP[1]
  PeakP2 <- tzones$PeakP[2]
  
  # Speed-Strength
  SpdStr1 <- tzones$SpdStr[1]
  SpdStr2 <- tzones$SpdStr[2]
  
  # Max Speed
  MaxSpd1 <- tzones$MaxSpd[1]
  MaxSpd2 <- tzones$MaxSpd[2]
  
  # Complete LV data frame
  LVdf <- data.frame(Loads) %>%
    mutate(
      "Velocity" = round((m*Loads)+b,3),
      "Zone" = ifelse(
        (Velocity<=MaxStr1) & (Velocity>=MaxStr2), "Max Strength",
        ifelse(
          (Velocity<=StrSpd1) & (Velocity>=StrSpd2), "Strength-Speed",
          ifelse(
            (Velocity<=PeakP1) & (Velocity>=PeakP2), "Power",
            ifelse(
              (Velocity<=SpdStr1) & (Velocity>=SpdStr2), "Speed-Strength",
              ifelse(
                (Velocity<=MaxSpd1) & (Velocity>=MaxSpd2), "Max Speed",
                ifelse(
                  Velocity<MaxStr2, "Super-Max", "Below Threshold"
                )
              )
            )
          )
        )
      )
    ) 
  
  return(LVdf)
  
}

## Velocity @ %1RM -----
## returns a velocity from given variables and desired %1RM
VeloAtPerc <- function(yint, slope, e1rm, perc) {
  b <- yint
  m <- slope
  x <- e1rm*(perc/100)
  
  y <- (m*x)+b
  
  return(round(y,3))
}

##  Load @ Velocity -----
## returns a load from given variables
LoadAtVelo <- function(yint, slope, velo) {
  
  b <- yint
  m <- slope
  y <- velo
  
  x <- (y-b)/m
  
  return(round(x,0))
}

## Power/Velocity Curve Data -----
PowCurve <- function(df, exrcs) {
  
  e <- exrcs
  
  #-- Data Frame -- #
  # filtered data
  dataf <- df %>%
    group_by(Load) %>%
    summarise("AvgP" = mean(aPower), "AvgV" = max(aVelocity))
  
  #-- Power --#
  ## GG plot for build
  plotGGP <- ggplot(dataf, aes(x = Load, y = AvgP)) +
    geom_smooth(span = 0.8) 
  
  ## GG Build
  powBuild <- ggplot_build(plotGGP)
  
  # Power curve x and y data frame
  dataPCurve <- as.data.frame(powBuild$data[[1]]) %>% 
    select (x,y) %>%
    transmute("x" = round(x,0), "y2" = round(y,0))
  
  #-- Velocity --#
  # Set MVT
  mvt <- LVmvt(e)
  
  e1rm <- E1RMcalc(df, e)
  
  dataV <- dataf %>%
    select(Load, AvgV)
  
  nr <- c(Load = e1rm, AvgV = mvt)
  
  dataV <- rbind(dataV, nr)
  ## GG plot for build
  plotGGV <- ggplot(dataV, aes(x = Load, y = AvgV)) +
    geom_smooth(method = lm) 
  
  ## GG Build
  veloBuild <- ggplot_build(plotGGV)
  
  # Power curve x and y data frame
  dataVCurve <- as.data.frame(veloBuild$data[[1]]) %>% 
    select (x,y) %>%
    transmute("x" = round(x,0), "y1" = round(y,3))
  
  #-- Combine Table --#
  
  df <- full_join(x=dataPCurve, y=dataVCurve, by= "x")
  
  colnames(df) <- c("Load", "Power", "Velocity")
  
  return(df)
}

## LV Training Zones -----
trnZones <- function(LVcoef) {
  # LV Coefficients df
  df <- LVcoef
  
  # Y-Intercept
  b <- df$yInt[1]
  
  # Slope
  m <- df$slope[1]
  
  # MVT
  mvt <- df$mvt[1]
  
  # estimate 1RM
  e1rm <- round((mvt - b)/m,0)
  
  # Velocity Bands
  ## Velocity @ 90%
  v90 <- VeloAtPerc(b, m, e1rm,90)
  ## Velocity @ 80%
  v80 <- VeloAtPerc(b, m, e1rm,80)
  ## Velocity @ 70%
  v70 <- VeloAtPerc(b, m, e1rm,70)
  ## Velocity @ 60%
  v60 <- VeloAtPerc(b, m, e1rm,60)
  ## Velocity @ 50%
  v50 <- VeloAtPerc(b, m, e1rm,50)
  
  # Max Strength Training Zone
  MSzone <- as.vector(c(v90, mvt))
  
  # Strength-Speed Zone
  STSPzone <- as.vector(c(v80, v90))
  
  # Peak Power Zone
  PPzone <- as.vector(c(v70, v80))
  
  # Speed-Strength Zone
  SPSTzone <- as.vector(c(v60, v70))
  
  # Max Speed Zone <-
  SPZone <- as.vector(c(v50, v60))
  
  # Create Zone table
  df <- bind_cols(MSzone, STSPzone, PPzone, SPSTzone, SPZone)
  
  colnames(df) <- c("MaxStr", "StrSpd", "PeakP", "SpdStr", "MaxSpd")
  
  return(df)
}

## Training Intensity Table -----
IntnstyVelo <- function(LVcoef) {

  # LV Coefficients df
  df <- LVcoef
  
  # Y-Intercept
  b <- df$yInt[1]
  
  # Slope
  m <- df$slope[1]
  
  # MVT
  mvt <- df$mvt[1]
  
  # estimate 1RM
  e1rm <- round((mvt - b)/m,0)
  
  # Intensity categories
  Intensity = c("Maximum", "Very Heavy", "Heavy", "Moderately Heavy", "Moderate", "Moedrately Light", "Light")
  Intensity <- as.data.frame(Intensity)
  
  # Create Rep Columns
  r1 <- as.vector(c(mvt, 
                    VeloAtPerc(b, m, e1rm,97),
                    VeloAtPerc(b, m, e1rm,95),
                    VeloAtPerc(b, m, e1rm,93),
                    VeloAtPerc(b, m, e1rm,91),
                    VeloAtPerc(b, m, e1rm,89),
                    VeloAtPerc(b, m, e1rm,87)
                    ))
  r2 <- as.vector(c(VeloAtPerc(b, m, e1rm,95),
                    VeloAtPerc(b, m, e1rm,93),
                    VeloAtPerc(b, m, e1rm,91),
                    VeloAtPerc(b, m, e1rm,89),
                    VeloAtPerc(b, m, e1rm,87),
                    VeloAtPerc(b, m, e1rm,86),
                    VeloAtPerc(b, m, e1rm,85)
                    ))
  r3 <- as.vector(c(VeloAtPerc(b, m, e1rm,91),
                    VeloAtPerc(b, m, e1rm,89),
                    VeloAtPerc(b, m, e1rm,87),
                    VeloAtPerc(b, m, e1rm,86),
                    VeloAtPerc(b, m, e1rm,85),
                    VeloAtPerc(b, m, e1rm,84),
                    VeloAtPerc(b, m, e1rm,83)
                    ))
  r4 <- as.vector(c(VeloAtPerc(b, m, e1rm,87),
                    VeloAtPerc(b, m, e1rm,86),
                    VeloAtPerc(b, m, e1rm,85),
                    VeloAtPerc(b, m, e1rm,84),
                    VeloAtPerc(b, m, e1rm,83),
                    VeloAtPerc(b, m, e1rm,82),
                    VeloAtPerc(b, m, e1rm,81)
                    ))
  r5 <- as.vector(c(VeloAtPerc(b, m, e1rm,85),
                    VeloAtPerc(b, m, e1rm,84),
                    VeloAtPerc(b, m, e1rm,83),
                    VeloAtPerc(b, m, e1rm,82),
                    VeloAtPerc(b, m, e1rm,81),
                    VeloAtPerc(b, m, e1rm,80),
                    VeloAtPerc(b, m, e1rm,79)
                     ))
  r6 <- as.vector(c(VeloAtPerc(b, m, e1rm,83),
                    VeloAtPerc(b, m, e1rm,82),
                    VeloAtPerc(b, m, e1rm,81),
                    VeloAtPerc(b, m, e1rm,80),
                    VeloAtPerc(b, m, e1rm,79),
                    VeloAtPerc(b, m, e1rm,77.5),
                    VeloAtPerc(b, m, e1rm,76)
                     ))
  r7 <- as.vector(c(VeloAtPerc(b, m, e1rm,81),
                    VeloAtPerc(b, m, e1rm,80),
                    VeloAtPerc(b, m, e1rm,79),
                    VeloAtPerc(b, m, e1rm,77.5),
                    VeloAtPerc(b, m, e1rm,76),
                    VeloAtPerc(b, m, e1rm,74.5),
                    VeloAtPerc(b, m, e1rm,73)
                    ))
  r8 <- as.vector(c(VeloAtPerc(b, m, e1rm,79),
                    VeloAtPerc(b, m, e1rm,77.5),
                    VeloAtPerc(b, m, e1rm,76),
                    VeloAtPerc(b, m, e1rm,74.5),
                    VeloAtPerc(b, m, e1rm,73),
                    VeloAtPerc(b, m, e1rm,71.5),
                    VeloAtPerc(b, m, e1rm,70)
                    ))
  
  # Create df from combined columns
  
  df <- bind_cols(Intensity, r1,r2,r3,r4,r5,r6,r7,r8)
  
  colnames(df) <- c("Intensity", "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8")
  
  return(df)
}

## Team %1RM Velocity Table -----
## returns a df of velocities at % of E1RM in 10% increments
dfTeamVelo <- function(df) {
  
  # df as variable
  x <- df
  
  # count of athletes
  c <- n_distinct(x$User)
  
  # create empty table for return
  ath <- c()
  p50 <- c()
  p55 <- c()
  p60 <- c()
  p65 <- c()
  p70 <- c()
  p75 <- c()
  p80 <- c()
  p85 <- c()
  p90 <- c()
  p95 <- c()
  p100 <- c()
  dframe1 <- data.frame(ath, p50, p55, p60, p65, p70, p75, p80, p85, p90, p95, p100)
  
  
  # for loop to create entries for return table
  
  for (i in 1:c) {
    
    u <- unique(x$User)
    
    n <- u[i]
    
    d <- x %>% filter(User==n)
    
    f <- LVcoef(d)
    
    # Y-Intercept
    b <- f$yInt[1]
    
    # Slope
    m <- f$slope[1]
    
    # Theoretical Peak Load
    L0 <- f$L0[1]
    
    # estimated 1RM
    e1rm <- f$e1rm[1]
    
    newRow <- data.frame(
      ath = n,
      p50 = VeloAtPerc(b, m, e1rm, 50),
      p55 = VeloAtPerc(b, m, e1rm, 55),
      p60 = VeloAtPerc(b, m, e1rm, 60),
      p65 = VeloAtPerc(b, m, e1rm, 65),
      p70 = VeloAtPerc(b, m, e1rm, 70),
      p75 = VeloAtPerc(b, m, e1rm, 75),
      p80 = VeloAtPerc(b, m, e1rm, 80),
      p85 = VeloAtPerc(b, m, e1rm, 85),
      p90 = VeloAtPerc(b, m, e1rm, 90),
      p95 = VeloAtPerc(b, m, e1rm, 95),
      p100 = VeloAtPerc(b, m, e1rm, 100)
    )

    dframe1 <- rbind(dframe1, newRow)
  }
  
  colnames(dframe1) <- c("Athelte", "50%", "55%", "60%", "65%", "70%", "75%",
                         "80%", "85%", "90%", "95%", "100%")
  
  return(dframe1)
  
}

## Team %1RM Velocity Table -----
## returns a df of velocities at % of E1RM in 10% increments
dfTeamLoad <- function(df) {
  
  # df as variable
  x <- df
  
  # count of athletes
  c <- n_distinct(x$User)
  
  # create empty table for return
  ath <- c()
  p50 <- c()
  p55 <- c()
  p60 <- c()
  p65 <- c()
  p70 <- c()
  p75 <- c()
  p80 <- c()
  p85 <- c()
  p90 <- c()
  p95 <- c()
  p100 <- c()
  dframe1 <- data.frame(ath, p50, p55, p60, p65, p70, p75, p80, p85, p90, p95, p100)
  
  
  # for loop to create entries for return table
  
  for (i in 1:c) {
    
    u <- unique(x$User)
    
    n <- u[i]
    
    d <- x %>% filter(User==n)
    
    f <- LVcoef(d)
    
    # estimated 1RM
    e1rm <- f$e1rm[1]
    
    newRow <- data.frame(
      ath = n,
      p50 = round(e1rm*.50, 0),
      p55 = round(e1rm*.55, 0),
      p60 = round(e1rm*.60, 0),
      p65 = round(e1rm*.65, 0),
      p70 = round(e1rm*.70, 0),
      p75 = round(e1rm*.75, 0),
      p80 = round(e1rm*.80, 0),
      p85 = round(e1rm*.85, 0),
      p90 = round(e1rm*.90, 0),
      p95 = round(e1rm*.95, 0),
      p100 = e1rm
    )
    
    dframe1 <- rbind(dframe1, newRow)
  }
  
  colnames(dframe1) <- c("Athelte", "50%", "55%", "60%", "65%", "70%", "75%",
                         "80%", "85%", "90%", "95%", "100%")
  
  return(dframe1)
  
}

#--------------------#