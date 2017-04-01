
read_transcripts <- function(file.names, use.mor=FALSE, debug=FALSE, dir = "corpora"){
  
  if( length(unique(file.names$file)) > 1 ) message("More than one transcription file being analyzed simultaneously! Code won't work.") # only analyze one file at a time
  this.file <- as.character(file.names$file[1])
  
  if(debug) message("processing... ", this.file) # for troubleshooting :)
  
  this.transcript <- readLines(file.path(dir, this.file))
  
  # for some corpora (including Thomas) long utterances are wrapped onto the next line, messing up our ability to select lines by theie starting strings
  # paste it all together, correct the wrapping, then separate it out again into a vector
  this.transcript <- paste(this.transcript, collapse = "\n")
  this.transcript <- gsub(x=this.transcript, pattern = "\n\t", replacement = " ") # replace new line + tab with a single space
  this.transcript <- strsplit(this.transcript, split = "\n")[[1]]
  attributes(this.transcript)$file <- this.file # save the filepath as an attribute
  
  # this includes all speaker tiers and %mor tiers (and %trn, which takes the place of %mor on the Japanese corpora)
  # important for retaining correct utterance numbering
  tiers <- data.frame(content = grep(this.transcript, pattern="^[*][[:upper:][:digit:]]{3}|^[%]mor|^[%]trn", value=TRUE) ) %>% 
    as.tbl() 
  if(nrow(tiers) > 0){
    tiers <- tiers %>% 
      tidyr::separate(col = content, into = c("label", "content"), sep = ":\t") %>% 
      dplyr::mutate(unique.id = 1:nrow(.))
    spe.tier <-  filter(tiers, label != "%mor" & label != "%trn") %>% 
      dplyr::mutate(utt.num = 1:nrow(.)) %>% 
      tidyr::extract(col = label, into = "speaker", regex = "[*]([[:alnum:]]{3})", remove = FALSE)
    labled.tiers <- left_join(tiers, spe.tier, by = c("label", "content", "unique.id")) 
    # see http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
    labled.tiers$utt.num <- na.locf(labled.tiers$utt.num)
    labled.tiers$speaker <- na.locf(labled.tiers$speaker)
    labled.tiers$unique.id <- NULL # drop this column, no longer needed now that correct utt.num is available
    
    mor.tier <- filter(labled.tiers, label == "%mor" | label == "%trn") %>% 
      select(label, content, utt.num, speaker)
    
    # note that some files that have BOTH a mor tier and a trn tier (identical information, but sometimes it's still in there)
    if(any(grepl(x=mor.tier$label, pattern = "%mor")) & any(grepl(x=mor.tier$label, pattern = "%trn"))){
      mor.n <- length(grep(x=mor.tier$label, pattern = "%mor"))
      trn.n <- length(grep(x=mor.tier$label, pattern = "%trn"))
      # if there are more utterances avialable for one type of code, use that
      if(mor.n > trn.n){
        mor.tier <- dplyr::filter(mor.tier, label == "%mor")
      } else if(trn.n > mor.n){
        mor.tier <- dplyr::filter(mor.tier, label == "%trn")
      } else {
        # if there are the same number of mor and trn codes, use mor
        mor.tier <- dplyr::filter(mor.tier, label == "%mor")
      }
    }
    
    # clean up the mor tier a little
    # remove punctuation markers: bq, eq, end, beg, and cm (do not correspond to words)
    mor.tier$content <- gsub(x=mor.tier$content, pattern="bq[|]bq", replacement="")
    mor.tier$content <- gsub(x=mor.tier$content, pattern="eq[|]eq", replacement="")
    mor.tier$content <- gsub(x=mor.tier$content, pattern="cm[|]cm", replacement="")
    mor.tier$content <- gsub(x=mor.tier$content, pattern="end[|]end", replacement="")
    mor.tier$content <- gsub(x=mor.tier$content, pattern="beg[|]beg", replacement="")
    mor.tier$content <- gsub(x=mor.tier$content, pattern=" +", replacement=" ") # replace multiple spaces with a single space
    
    # clean up speaker tier a little
    spe.tier <-  filter(labled.tiers, label != "%mor" & label != "%trn")
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[[].*[]]", replacement="") # remove everything inside square brackets
    spe.tier$content <- gsub(x=spe.tier$content, pattern="www|yyy", replacement="xxx") # just use xxx for all untranscribable speech
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[(]laughing[)]", replacement="") # remove notes for laughing
    spe.tier$content <- gsub(x=spe.tier$content, pattern="(.)", replacement="", fixed=TRUE) # remove pauses
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[(](.*)[)]", replacement="\\1") # remove all other parentheses
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[(]", replacement="") # remove all other parentheses
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[)]", replacement="") # remove all other parentheses
    spe.tier$content <- gsub(x=spe.tier$content, pattern="[<](.*)[>]", replacement="\\1") # remove all < >
    spe.tier$content[!grepl(x=spe.tier$content, pattern="[[:alpha:]]")] <- NA  # erase utterances that don't have at least one letter in them (just punctuation, symbols, and numbers)
    
    # remove those gosh darn curly quotes
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x98", perl=TRUE, replacement="") 
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x99", perl=TRUE, replacement="")
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9A", perl=TRUE, replacement="") 
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9B", perl=TRUE, replacement="")
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9C", perl=TRUE, replacement="") 
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9D", perl=TRUE, replacement="")
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9E", perl=TRUE, replacement="") 
    spe.tier$content <- gsub(x=spe.tier$content, pattern="\xE2\x80\x9F", perl=TRUE, replacement="")
    
    # drop non-alphas (white space, punct and digits) at the end of each utt, such as CLAN timing bullets
    spe.tier <- spe.tier %>% 
      tidyr::extract(col = content, 
                     into = "content", 
                     regex="[[:space:]]*(.*[[:alpha:]])[^[:alpha:]]*")
    
    clean.tiers <- rbind(spe.tier, mor.tier) %>% 
      dplyr::arrange(utt.num)
    
    clean.tiers$tier <- ifelse(clean.tiers$label=="%mor" | clean.tiers$label == "%trn", "mor", "utterance")
    
    # tidy data frame, where each row is one utterance, and there are different columns for speaker utt and mor translation
    utts <- clean.tiers %>% 
      dplyr::group_by(utt.num) %>%
      dplyr::select(content, tier, speaker, utt.num) %>%
      tidyr::extract(col=content, 
                     into="content", 
                     regex="[[:space:]]*(.*[[:alpha:]])[^[:alpha:]]*") %>% # drop spaces at the beginning and any non-alpha characters at the end (e.g. period or question mark) 
      # reformat to wide
      tidyr::spread(key=tier, content)  %>%
      dplyr::arrange(utt.num)
    
    if( use.mor & "mor" %in% colnames(utts) ){
      # put missing untranscribable words back into mor tier, to preserve position in utterance count
      utts <- utts %>% 
        group_by(utt.num) %>% 
        do({
          restore_xxx(.)
        })
    }
    
    if( !use.mor & "mor" %in% colnames(utts) ){ 
      # see http://childes.psy.cmu.edu/manuals/clan.pdf for more info on the %mor tier
      utts <- utts %>% 
        dplyr::select(-mor) %>% 
        na.omit() # drop utterances that are incomplete
    }
    
    # clean up the utterances a little
    # remove the @ tags at the end of some words, used to indicate, for example wordplay (@wp), babbling (@b), etc
    # also remove the $ tags at the end of some words, used to force a MOR pos for words that aren't in the MOR lexicon
    utts$utterance <- gsub(x=utts$utterance, pattern="[@$][[:alpha:]]+", replacement="") 
    utts$utterance <- gsub(x=utts$utterance, pattern=",", replacement="") # remove commas
    utts$utterance <- gsub(x=utts$utterance, pattern=" +", replacement=" ") # replace strings of spaces with a single space
    
    # add some meta data to the utts dataframe
    utts$file <- this.file
    
    # speaker ids
    participants <- grep(this.transcript, pattern="^@Participants", value=TRUE)
    participants <- gsub(x=participants, pattern="@Participants:[[:space:]]+", replacement="")
    speaker.ids <- data.frame(speaker=strsplit(participants, ", ", fixed = TRUE)[[1]]) %>%
      separate(col=speaker, into=c("speaker", "name", "role"), sep=" ", remove=TRUE , extra="drop")
    # add speaker id info to utterances data frame
    utts <- left_join(utts, speaker.ids, by="speaker")
    
    
    # extract age information and save it to utts
    age.info <- get_age(this.transcript)
    utts$age <- age.info$age
    utts$y <- age.info$y
    utts$m <- age.info$m
    utts$d <- age.info$d
    utts$age.mos <- age.info$age.mos
    
    # fix missing values in role (fill them in with the speaker code)
    utts$role <- ifelse(is.na(utts$role), utts$speaker, 
                        ifelse(utts$role=="", utts$speaker,
                               utts$role))
    
    # if there are no utterances for "Target_Child" see if it's being labeled another way and fill in
    if(!"Target_Child" %in% utts$role){
      utts$role[utts$role=="Child"] <- "Target_Child"
      if(!"Target_Child" %in% utts$role){
        utts$role[utts$speaker=="CHI"] <- "Target_Child"
      } 
    }
    
    # if needed, add an empty mor column (to be able to rbind() with files that do have mor)
    if(use.mor & !"mor" %in% colnames(utts)) {
      utts$mor <- NA
      # make sure columns are in the right order
      utts <- dplyr::select(utts, speaker, utt.num, utterance, mor, file, name, role, age, y, m, d, age.mos)
    }
  } else {
    message("No tiers found for ", this.file)
    utts <- data.frame(speaker = NA, utt.num = NA, utterance = NA, mor = NA, file=this.file, name = NA, role = NA, age = NA, y = NA, m = NA, d = NA, age.mos = NA)
  }
  return(utts)
}
