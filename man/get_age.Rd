% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_age.R
\name{get_age}
\alias{get_age}
\title{Get age}
\usage{
get_age(this.transcript)
}
\description{
Attempts to calculate the child's age at recording from .cha file 
(CHAT formatted transcript, as from the CHILDES database).
It begins by looking for the child's age in the ID headers at the top of the file.
If unsuccessful, it goes on to check for date of recording and date of birth in the headers and calculates age from those.
If date of recording and date of birth are not both provided, it checks for possible age information encoded in the file names
(as is the case for some corpora).
}

