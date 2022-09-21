library(data.table)
library(dplyr)
library(rlang)
library(tidyr)
library(stringr)
library(purrr)
library(DT)

#' ReadFile
#'
#' @param file File name. Locates file by partial matching. 
#' #' @param bank Either "NH" or "KH", defaults to "KH"
#' @param year Either a 4-digit number corresponding to production year, or "DATERT". Defaults to DATERT
#'
#' @return
#' @export
#'
#' @examples
ReadFile <- function(file = NULL, 
                      bank = "KH", 
                      year = "DATERT"){
  
  if(!(bank %in% c("KH", "NH"))) {
    stop("`bank` must be either 'KH' or 'NH'")
  }
  
  if(!(str_detect(year, "[:digit:]{4}") & str_length(year) == 4| 
       year == "DATERT")) {
    stop("`year` must be either 4 digits or 'DATERT'")
  }
  
  if(is.null(file)) {
    stop("file not selected")
  }
  
  BANK <- case_when(bank == "KH" ~ "KOMMUNEHELSA",
                    bank == "NH" ~ "NORGESHELSA")
  
  YEAR <- case_when(year == "DATERT" ~ paste0(year, "/csv"),
                    TRUE ~ paste0(bank, year, "NESSTAR"))
  
  basepath <- file.path("F:", 
                        "Forskningsprosjekter", 
                        "PDB 2455 - Helseprofiler og til_",
                        "PRODUKSJON", 
                        "PRODUKTER", 
                        "KUBER", 
                        BANK, 
                        YEAR)
  
  filename <- list.files(basepath, pattern = file)
  
  if(length(filename) == 0){
    stop("File not found, check spelling")
  } else if(length(filename) > 1){
    message("More than 1 file found:")
    stop("Please specify file name to only select one file", 
         cat(filename, sep = "\n"))
  } else {
    filepath <- file.path(basepath, filename)
  }
  
  message(paste0("Loads file: ", BANK, "/", YEAR, "/", basename(filepath)))
  fread(filepath)
}

