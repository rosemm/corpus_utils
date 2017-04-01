restore_xxx <- function(utts){
  if( any(grepl(x = utts$utterance, pattern = "xxx")) ){
    stopifnot( length(utts$utterance) == 1 )
    
    utterance <- strsplit(x = utts$utterance, split = " ")[[1]]
    xxx.where <- grep(x = utterance, pattern = "xxx")
    
    mor <- strsplit(x = utts$mor, split = " ")[[1]]
    mor.count <- 1
    
    new.mor <- vector(length = (length(mor) + length(xxx.where)) ) 
    
    for( i in 1:length(new.mor) ){
      if(i %in% xxx.where) { 
        new.mor[i] <- "xxx|xxx"
      } else {
        new.mor[i] <- mor[mor.count]
        mor.count <- mor.count + 1
      }
    } # end of for loop
    utts$mor <- paste(new.mor[!is.na(new.mor)], collapse = " ")
  } # end of xxx if statement
  
  return(utts)
}
