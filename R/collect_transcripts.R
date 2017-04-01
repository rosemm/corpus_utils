#' @export
collect_transcripts <- function(use.corpus.as.name=c("always", "as.needed", "never"), dir="corpora", test.sample=NULL){
  
  stopifnot(any(c("always", "as.needed", "never")==use.corpus.as.name))
  
  cha.files <- list.files(dir, pattern = ".cha", recursive = TRUE)
  
  files <- data.frame(file=cha.files) %>% 
    tidyr::separate(file, into=c("language", "corpus", "child", "cha"), sep = "/", remove = FALSE) %>% 
    dplyr::mutate(cha = ifelse(grepl(x=child, pattern = ".cha"), child, cha),
                  child = ifelse(grepl(x=child, pattern = ".cha"), NA, child))
  
  if(use.corpus.as.name == "always"){
    files$child <- files$corpus
  } else {
    # if child is blank, see if there are child names in file names
    candidate.names <- files %>% 
      # pull out candidate names
      dplyr::mutate(candidate.name = gsub(x=cha, pattern = "([[:alpha:]]*).*[.]cha", replacement = "\\1")) 
    names <- candidate.names  %>% 
      dplyr::filter(candidate.name != "") %>% 
      dplyr::count(language, corpus, child, candidate.name) %>% 
      dplyr::filter(n > 1) %>%  # only keep candidate names that are in more than one file 
      dplyr::select(-n) %>% 
      unique()
    candidate.names <- candidate.names %>% dplyr::filter(candidate.name %in% names$candidate.name)
    
    files <- left_join(files, candidate.names, by=c("language", "corpus", "child", "file", "cha"))
    
    files <- files %>% 
      dplyr::mutate(child = ifelse(is.na(child), candidate.name, child)) %>% 
      dplyr::select(-candidate.name) 
  }
  
  if(use.corpus.as.name == "as.needed"){
    files$child <- ifelse(is.na(files$child), files$corpus, files$child)
  }
  # if there are still unknown child names at this point, fill in UNKNOWN
  files$child <- ifelse(is.na(files$child), "UNKNOWN", files$child)
  
  if( !is.null(test.sample) ) {
    # in testing mode, only keep a random selection of the files, to speed up processing
    if(test.sample > 1) files <- sample_n(files, size = test.sample)
    if(test.sample < 1) files <- sample_frac(files, size = test.sample)
  }
  
  return(files)
}

