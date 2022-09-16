#' CompareDim
#' 
#' Detects all existing levels of selected dimension in the new KUBE, and compares towards a previous KUBE. The total number of levels, all new or expired levels, and a list of all existing levels in the new KUBE are listed in the output. 
#'
#' @param data1 new KUBE, defaults to dfnew
#' @param data2 old KUBE, defaults do dfold
#' @param dim Dimension you want to check
#'
#' @return a list. If no new or expired columns are detected, these elements will return "none". 
#' @export
#'
#' @examples
#' CompareDim(dim = GEO)  
#' CompareDim(dim = AAR)
#' CompareDim(dim = KJONN)
#' CompareDim(dim = ALDER)
#' CompareDim(dim = YTELSE)
CompareDim <- function(data1 = dfnew, 
                       data2 = dfold, 
                       dim = NULL){
  
  .levelsnew <- data1 %>% 
    pull(dim) %>% 
    unique()
  
  .levelsold <- data2 %>% 
    pull(dim) %>% 
    unique()
  
  .length <- length(.levelsnew)
  
  .newdims <- .levelsnew[!(.levelsnew %in% .levelsold)]
  
  .expdims <- .levelsold[!(.levelsold %in% .levelsnew)]
  
  all <- str_c(.levelsnew, collapse = ", ")
  newlevels <- ifelse(length(.newdims) > 0,
                      str_c(.newdims, collapse = ", "),
                      "none")
  explevels <- ifelse(length(.expdims) > 0,
                      str_c(.expdims, collapse = ", "),
                      "none")
  
  tibble("Dimension" = dim,
         "N levels" = .length,
         "New levels" = newlevels,
         "Expired levels" = explevels)
}

#' CompareDims
#' 
#' Wrapper around `CompareDim`, to print results for several dims simultaneously
#'
#' @param dims Character vector of dimensions to compare
#'
#' @return table with 4 columns: Dimension, N levels, New levels, Expired levels, with one row per input dimension
#' @export
#'
#' @examples
CompareDims <- function(dims = c(STANDARDdims, EXTRAdims)){
  map_df(dims, ~CompareDim(dim = .x))
}


#' ComparePrikk
#' 
#' Calculate the number of censored observations in the new and old KUBE, and calculate the absolute and relative difference. Results can be further grouped by an additional dimension. 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param data2 old KUBE, defaults to dfold set in INPUT
#' @param groupdim dimension to group output by
#'
#' @return a table containing the number of flagged rows in the new and old KUBE, and the absolute and relative difference, grouped by type of SPVFLAGG and an additional dimension (optional)
#' @export
#'
#' @examples
#' ComparePrikk(groupdim = ALDER)
ComparePrikk <- function(data1 = dfnew, 
                         data2 = dfold, 
                         groupdim = EXTRAdims){
  
  full_join(
    data1 %>% 
      group_by(across(c(SPVFLAGG, all_of(groupdim)))) %>% 
      summarise(n_new = n(), .groups = "drop"),
    data2 %>% 
      group_by(across(c(SPVFLAGG, all_of(groupdim)))) %>% 
      summarise(n_old = n(), .groups = "drop"),
    by = c("SPVFLAGG", all_of(groupdim))) %>% 
    replace_na(list(n_new = 0,
                    n_old = 0)) %>% 
    mutate(across(SPVFLAGG, as.character),
           SPVFLAGG = case_when(SPVFLAGG == "0" ~ "No flag", 
                                TRUE ~ SPVFLAGG),
           absolute = n_new - n_old,
           absolute = case_when(absolute == 0 ~ "Identical",
                                absolute > 0 ~ paste0("+ ", absolute),
                                absolute < 0 ~ paste0("- ", abs(absolute))),
           relative = round((n_new/n_old - 1)*100,1),
           relative = case_when(relative == Inf ~ "Cannot be estimated",
                                relative == 0 ~ "Identical", 
                                relative > 0 ~ paste0("+ ", relative, " %"),
                                relative < 0 ~ paste0("- ", abs(relative), " %"))) 
    
}

#' CheckPrikk
#' 
#' Check if all values below the censoring limit has been removed. If ok, the function returns a confirmation. If any number below the limit is detected, all rows containing unacceptable values are returned for inspection. 
#'
#' @param data1 New KUBE, defaults to dfnew 
#' @param dim Dimension you want to check, defaults to sumTELLER
#' @param limit Censor limit, the highest unacceptable value of dim. Defaults to `PRIKKlimit`, defined in input section of the Rmarkdown file. 
#'
#' @return
#' @export
#'
#' @examples
CheckPrikk <- function(data1 = dfnew,
                       dim = PRIKKval, 
                       limit = PRIKKlimit){
  
  filtered <- data1[data1[[dim]] <= limit]
  
  ifelse(nrow(filtered) == 0,
         result <- "No values < limit",
         result <- as_tibble(filtered))
  
  result
}

#' CompareLandFylke
#' 
#' 
#'
#' @param data1 new KUBE, defaults to dfnew set in INPUT
#' @param ... dimension(s) to group output by
#'
#' @return
#' @export
#'
#' @examples
CompareLandFylke <- function(data1 = dfnew, groupdim = EXTRAdims, compare = COMPAREval){
  
  
  
  data1 %>% 
    mutate(geolevel = case_when(GEO == 0 ~ "Land",
                                GEO < 100 ~ "Fylke",
                                GEO < 10000 ~ "Kommune",
                                TRUE ~ "Bydel")) %>% 
    filter(geolevel %in% c("Land", "Fylke")) %>% 
    group_by(across(c(geolevel, all_of(groupdim)))) %>% 
    summarise(sum = sum(.data[[compare]], na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = geolevel, 
                values_from = sum) %>% 
    mutate(absolute = Land-Fylke,
           relative = Land/Fylke) %>% 
    arrange(desc(relative))
}

#' CompareBydelKommune
#'
#' @param data1 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
CompareBydelKommune <- function(data1 = dfnew, groupdim = EXTRAdims, compare = COMPAREval) {
  data1 %>% 
    mutate(geolevel = case_when(GEO == 0 ~ "Land",
                                GEO < 100 ~ "Fylke",
                                GEO < 10000 ~ "Kommune",
                                TRUE ~ "Bydel")) %>%  
    filter(geolevel %in% c("Bydel", "Kommune"),
           str_detect(GEO, "^301|^1103|^4601|^5001")) %>% 
    mutate(KOMMUNE = case_when(str_detect(GEO, "^301") ~ "Oslo",
                               str_detect(GEO, "^1103") ~ "Stavanger",
                               str_detect(GEO, "^4601") ~ "Bergen",
                               str_detect(GEO, "^5001") ~ "Trondheim")) %>% 
    group_by(across(c(KOMMUNE, geolevel, all_of(groupdim)))) %>% 
    summarise(sum = sum(.data[[compare]], na.rm = T), .groups = "drop") %>% 
    pivot_wider(names_from = geolevel, 
                values_from = sum) %>% 
    mutate(absolute = Kommune-Bydel,
           relative = Kommune/Bydel)
}
