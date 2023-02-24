## function to estimate the maximum length of the femur from an estimated stature
## datatable = dataframe with statures from which femur lengths will be estimated, 
## the funtion expects this table to have the following columns: 
## col_name           col_data
## "stature"          estimated stature data
## "method"           method used to estimate the stature 
## "femur_measured"   length of the femur for any individuals for which they have 
##                    been directly measured
## The function can estimate femur lengths based on statures estimated using the 
## methods of "Manouvrier1892", "Breitinger1937", "Bach1965", "Trotter1952", 
## "Sjøvold1990", "Ruff2012", "Pearson1899", and "Olivier1978". Note that the name
## of the estimation method in the "method" column of the dataframe must be the same
## as what is listed here. 

getFemur <- function(datatable){
  df <- datatable
  df$femur_est <- NA
  df$femur_method <- NA
  for(i in 1:nrow(datatable)){
    femur <- df$femur_measured[i]
    method <- df$method[i]
    stature <- df$stature[i]
    sex <- df$Sex[i]
    if(!is.na(femur)){
      method <- "Measured"
    } else if(!is.na(method)){
      if(method == "Manouvrier1892"){
        femur <- Manouvrier1892(datatable, stature, sex)
      } else if(method == "Breitinger1937"){
        femur <- Breitinger1938(datatable, stature, sex)
      } else if(method == "Bach1965"){
        femur <- Bach1965(datatable, stature, sex)
      } else if(method == "Trotter1952" ){
        femur <- TrotterGleser(datatable, stature, sex)
      } else if(method == "Sjøvold1990"){
        femur <- Sjovold1990(datatable, stature)
      } else if(method == "Ruff2012"){
        femur <- Ruff2012(datatable, stature, sex)
      } else if(method == "Pearson1899"){
        femur <- Pearson1899(datatable, stature, sex)
      } else if(method == "Olivier1978"){
        femur <- Olivier1978(datatable, stature, sex)
      }
    }
    df$femur_est[i] <- femur
    df$femur_method[i] <- toString(method)
  }
  df$femur_method <- as.factor(df$femur_method)
  return(df)
}

## functions in getFemur()
Manouvrier1892 <- function(datatable, stature, sex){
  ## These eqns are derived from the table provided in the original manuscript
  if(sex == "F"){
    if(stature < 155.5){
      femur <- stature/3.8
    } else if(stature > 155.5 && stature < 161.0){
      femur <- stature/3.7
    } else if(stature > 161.1){
      femur <- stature/3.6
    }
  } else if(sex == "M"){
    if(stature < 163.0){
      femur <- stature/3.8
    } else if(stature > 163.1 && stature < 173.0){
      femur <- stature/3.7
    } else if(stature > 161.1){
      femur <- stature/3.6
    }
  } else {
    stop("Manouvrier sex unknown")
  }
  return(femur)
}

Breitinger1938 <- function(datatable, stature, sex){
  if(sex == "F"){
    stop("Breitinger1938 is for males only")
  } else if(sex == "M"){
    femur <- (stature-94.31)/1.64
  } else {
    stop("Breitinger sex unknown")
  }
  return(femur)
}

Bach1965 <- function(datatable, stature, sex){
  if(sex == "F"){
    femur <- (stature-106.69)/1.313
  } else if(sex == "M"){
    stop("Bach1965 is for females only")
  } else {
    stop("Bach sex unknown")
  }
  return(femur)
}

TrotterGleser <- function(datatable, stature, sex){
  if(sex == "F"){
    # from the 1952 paper
    femur <- (stature-54.10)/2.47
  } else if(sex == "M"){
    # from 1958 paper
    femur <- (stature-65.53)/2.32
  } else {
    stop("TrotterGleser sex unknown")
  }
  return(femur)
}

Sjovold1990 <- function(datatable, stature){
  femur <- (stature-49.96)/2.63
  return(femur)
}

Ruff2012 <- function(datatable, stature, sex){
  if(sex == "F"){
    femur <- (stature-43.56)/2.69
  } else if(sex == "M"){
    femur <- (stature-42.85)/2.72
  } else {
    stop("Ruff sex unknown")
  }
  return(femur)
}

Pearson1899 <- function(datatable, stature, sex){
  if(sex == "F"){
    femur <- (stature-72.844)/1.945
  } else if(sex == "M"){
    femur <- (stature-81.306)/1.880
  } else {
    stop("Pearson sex unknown")
  }
  return(femur)
}

Olivier1978 <- function(datatable, stature, sex){
  # for estimates using single long bones, Olivier divides his equations by left vs right sides. 
  # I used the left side eqns here because only the left was give for females.
  if(sex == "F"){
    femur <- (stature-70.2)/2.096
  } else if(sex == "M"){
    femur <- (stature-58.33)/2.4202
  } else {
    stop("Olivier sex unknown")
  }
  return(femur)
}

## function to estimate statures using the method of Ruff et al 2012
## datatable = data frame with femur measurements to use in the estimation; 
## femur = the name of the column with the femur data;
## sex = name of the column with the sex data
statCalc <- function(datatable, femur, sex){
  df <- datatable
  df$stature_est <- NA
  for(i in 1:nrow(df)){
    if(is.na(df[i, sex])){
      next
    } else if(df[i , sex] == "F"){
      df$stature_est[i] <- (df[i,femur] * 2.69) + 43.56
    } else if(df[i , sex] == "M"){
      df$stature_est[i] <- (df[i,femur] * 2.72) + 42.85
    } else {
      next
    }
  }
  return(df)
}