#' Get age
#' 
#' Attempts to calculate the child's age at recording from .cha file 
#' (CHAT formatted transcript, as from the CHILDES database).
#' It begins by looking for the child's age in the ID headers at the top of the file.
#' If unsuccessful, it goes on to check for date of recording and date of birth in the headers and calculates age from those.
#' If date of recording and date of birth are not both provided, it checks for possible age information encoded in the file names
#' (as is the case for some corpora).
#' 
#' 
#' @export
get_age <- function(this.transcript){
  # begin with NA's for age info
  age <- NA
  y <- NA
  m <- NA
  d <- NA
  age.mos <- NA
  
  # get headers
  ids <- grep(this.transcript, pattern="^@ID:", value=TRUE)
  chi.id <- ids[grep(x=ids, pattern="[|]Target_Child[|]|[|]Child[|]")]
  
  # no @ID headers in file?
  if(length(ids) == 0){
    # set chi.id to NA
    chi.id <- NA 
    message("No ID headers. Could not identify Target Child in ", this.file)
  }
  if(length(chi.id) == 0){
    # if there are ID headers, but nothing about a child, make chi.id NA
    chi.id <- NA
  }
  
  # if more than one child comes up and one is labeled Target_Child, use that
  if(length(chi.id) > 1){
    target.id <- chi.id[grep(x=chi.id, pattern = "[|]Target_Child[|]")]
    child.id <- chi.id[grep(x=chi.id, pattern = "[|]Child[|]")]
    if(length(target.id) == 1){
      chi.id <- target.id
    } else if(length(child.id) == 1){
      chi.id <- child.id
    } else {
      # set chi.id to NA
      chi.id <- NA
      message("Could not identify Target Child in ", this.file)
    }
  }
  
  # no @ID headers in file?
  if(length(ids) == 0){
    # set chi.id to NA
    chi.id <- NA 
    message("No ID headers. Could not identify Target Child in ", this.file)
  }
  if(length(chi.id) == 0){
    # if there are ID headers, but nothing about a child, make chi.id NA
    chi.id <- NA
  }
  
  # if there is chi.id info from the @ID headers
  if(!is.na(chi.id)){
    # fill in age info from chi.id
    age <- gsub(x=chi.id, pattern=".*[[:upper:][:digit:]]{3}[|]([[:digit:][:punct:]]+)[|].*", replacement="\\1", perl=T)
    
    # if age is available, use that to get age.mos
    if(grepl(x=age, pattern = "[[:digit:]]")){
      # If there are digits (numbers) in the age position in the header, calculate age
      # separate age into year, month, day
      age.components <- stringr::str_split_fixed(age, pattern="[^[:digit:]]", n=3)
      y <- as.numeric(age.components[1])
      m <- as.numeric(age.components[2])
      d <- as.numeric(age.components[3])
      age.mos <- sum(cbind(y*12,  m, d/(365.25/12)), na.rm = TRUE) # calculate one numerical value for age in months
    } else {
      # If there are no digits (numbers) in the age position in header, make NA
      age <- NA
      y <- NA
      m <- NA
      d <- NA
    }
  }
    
  # if it was not possilbe to extract age from @ID headers...
  if(is.na(age.mos)){
    
    # look for possible age information elsewhere
    age.dates <- grep(this.transcript, pattern="^@Birth of CHI|^@Date|^@Age of CHI", value=TRUE)
    
    # if there is age.dates available (not in the @ID header), try to calc age
    if(length(age.dates) > 0){
      dob <- grep(age.dates, pattern="^@Birth", value=TRUE)
      dob <- gsub(x=dob, pattern = "^@Birth.* ([[:digit:]].*)", replacement = "\\1")
      dor <- grep(age.dates, pattern="^@Date", value=TRUE)
      dor <- gsub(x=dor, pattern = "^@Date.* ([[:digit:]].*)", replacement = "\\1")
      age <- grep(age.dates, pattern="^@Age", value=TRUE)
      age <- gsub(x=age, pattern = "^@Age.* ([[:digit:]].*)", replacement = "\\1")
      
      # both date of birth (dob) and date of recording (dor) available
      if(length(dob)*length(dor) > 0){
        age <- NA
        y <- NA
        m <- NA
        d <- NA
        age.calc <- difftime(lubridate::parse_date_time(dor, orders="dmy"), 
                             lubridate::parse_date_time(dob, orders="dmy"), 
                             units="days")
        age.mos <- as.double(age.calc)/(365.25/12) # convert to months
      } 
    }
  }

  # If it has still not been possible to get age...
  if(is.na(age.mos)){
  # extract the filepath from the attributes
  this.file <- attributes(this.transcript)$file 
  # (child age is encoded in the file name for the MPI-EVA-Manchester corpus)
  
    if(grepl(x=this.file, pattern = "[[:digit:]]")){
      age <- gsub(x=this.file, pattern = ".*[/][^[:digit:]]*([[:digit:][:punct:]]+)[^[:digit:]]*[.]cha", replacement = "\\1")
      age.components <- stringr::str_split_fixed(age, pattern="[^[:digit:]]", n=3)
      
      # numbers are also sometimes used in file names to indicate something other than age, 
      # so only keep if there are three numbers separated by punct, as for years-months-days
      if(!any(is.na(age.components))){
        y <- as.numeric(age.components[1])
        m <- as.numeric(age.components[2])
        d <- as.numeric(age.components[3])
        age.mos <- sum(cbind(y*12,  m, d/(365.25/12)), na.rm = TRUE) # calculate one numerical value for age in months
      }
    }
  }
  
  age.info <- list(age=age, y=y, m=m, d=d, age.mos = age.mos)
  return(age.info)
}

