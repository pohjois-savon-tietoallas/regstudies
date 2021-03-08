#' Filter register based data and add dichotomous variables for each disease.
#'
#' Can be used for filtering, e.g., for hospital visits and to add dichotomous variables for each disease.
#' The returned data set is in wide format.
#' 
#' @param cohort_data cohort data set containing information about the persons: ID, gender, and index day.
#' @param reg_data long format data with variables containing id-numbers, additional date variables, variable holding the ICD, SII or ATC codes, and a variable describing the version of the codes (ICD-9, ICD-10 etc.)
#' @param adm_date the variable name of the date of admission
#' @param disc_date the name of the variable holding the date of discharge
#' @param index_date the name of the variable which is the reference point around which to search
#' @param time_before the time before the index date what defines the start of filtering interval. Numeric format, days, e.g, 1 year = 365.25
#' @param time_after the time after the index date what defines the start of filtering interval. Numeric format, days, e.g, 1 week = 7
#' @param idnum name of the variable holding identification codes (ID) of subjects
#' @param codes name of the variable holding ICD, SII, ATC etc. -codes
#' @param diag_tbl name of the `regstudies` classification one wants to apply. It can be either `charlson` or `elixhauser`. Alternatively one can use a tibble which holds the classification details
#' @param add_zero_class a logical value indicating whether diseases with zero frequencies should be added to the data. Default is TRUE
#' 
#' @return returns a tibble object with filtered rows based on a date interval and dichotomous variables for each disease.
#' 
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr contains
#' @importFrom dplyr any_of
#' @importFrom rlang as_label
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom dplyr select
#' @importFrom tidyr %>%
#' @importFrom tidyr drop_na
#' @importFrom tidyr pivot_wider
#' 
#' @examples
#' \dontrun{
#' d <- make_indicators(cohort_data = regstudies::sample_cohort, 
#'                      reg_data = regstudies::sample_regdata,
#'                      adm_date = adm_date, disc_date = disc_date, index_date = postingdate,
#'                      time_before = 2*365.25, time_after = 0,
#'                      idnum = personid, 
#'                      codes = CODE1, diag_tbl = charlson)
#'
#' }
#' 
#' @rdname make_indicators
#'
#' @export
#'

make_indicators = function(cohort_data, reg_data, 
                           adm_date = NULL, disc_date = NULL, index_date = NULL, time_before = 0, time_after = 0,
                           idnum, codes, diag_tbl, add_zero_class = TRUE){
  
  if(!is.null(rlang::enquo(adm_date))|!is.null(rlang::enquo(disc_date))|!is.null(rlang::enquo(index_date))){
    
    tmp <- reg_data %>%
      dplyr::inner_join(cohort_data) %>%
      dplyr::filter({{adm_date}} - time_after <= {{index_date}} & {{disc_date}} + time_before >= {{index_date}}) 
    
  }else{
    
    tmp <- reg_data %>%
      dplyr::inner_join(cohort_data)
    
  }
  
  diag_tbl_check <- rlang::quo_name(rlang::enquo(diag_tbl))
  
  if(diag_tbl_check != "charlson" & diag_tbl_check != "elixhauser"){
    
    classify_d <- tmp %>% 
      regstudies::classify_codes(codes = {{codes}}, diag_tbl = read_classes(diag_tbl))
    
    classify_wide <- classify_d %>%
      dplyr::distinct() %>%
      filter(!is.na(select(., dplyr::contains("score")))) %>%
      tidyr::pivot_wider(names_from = dplyr::contains("class"), values_from = score, values_fill = 0) 
    
    classes <- classify_d %>% 
      select(dplyr::contains("class")) %>% 
      tidyr::drop_na() %>%
      dplyr::distinct()
    
    classes <- unlist(classes, use.names = F)
    
    all.classes <- diag_tbl %>% 
      dplyr::select(dplyr::contains("class")) %>% 
      unlist(use.names = F)
    
    zero.classes <- dplyr::setdiff(c(all.classes), c(classes))
    
    zero.classes <- zero.classes[!is.na(zero.classes)]
    
  }
  
  if(diag_tbl_check == "charlson"){
    
    classify_d <- tmp %>%
      regstudies::classify_charlson(icd_codes = {{codes}})
    
    classify_wide <- classify_d %>%
      dplyr::distinct() %>% 
      filter(!is.na(select(., dplyr::contains("score")))) %>%
      dplyr::mutate(score_charlson = as.integer(score_charlson > 0)) %>% 
      tidyr::spread(class_charlson, score_charlson, fill = 0) 
    
    classes <- classify_d %>% 
      dplyr::select(class_charlson) %>% 
      dplyr::filter(!is.na(class_charlson)) %>%
      dplyr::distinct(class_charlson)
    
    classes <- unlist(classes, use.names = F)
    
    all.classes <- regstudies::charlson_classes %>% 
      dplyr::select(dplyr::contains("class")) %>% 
      unlist(use.names = F) %>% 
      as.character()
    
    zero.classes <- dplyr::setdiff(c(all.classes), c(classes))
    
    zero.classes <- zero.classes[!is.na(zero.classes)]
    
  }
  
  if(diag_tbl_check == "elixhauser"){
    
    classify_d <- tmp %>%
      regstudies::classify_elixhauser(icd_codes = {{codes}})
    
    classify_wide <- classify_d %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(select(., dplyr::contains("score")))) %>%
      tidyr::spread(class_elixhauser, score_AHRQ, fill = 0)
    
    classes <- classify_d %>% 
      dplyr::select(class_elixhauser) %>% 
      dplyr::filter(!is.na(class_elixhauser)) %>%
      dplyr::distinct(class_elixhauser)
    
    classes <- unlist(classes, use.names = F)
    
    all.classes <- regstudies::elixhauser_classes %>% 
      dplyr::select(dplyr::contains("class")) %>% 
      unlist(use.names = F) %>% 
      as.character()
    
    zero.classes <- dplyr::setdiff(c(all.classes), c(classes))
    
    zero.classes <- zero.classes[!is.na(zero.classes)]
    
  }
  
  classified_classes <- classify_wide %>%
    dplyr::select({{idnum}}, dplyr::any_of(classes)) %>%
    dplyr::group_by({{idnum}}) %>%
    dplyr::summarise(
      across(any_of(classes), ~max(.x))
    )
  
  if(add_zero_class == T) classified_classes[ , zero.classes] <- 0
  
  if(!is.null(cohort_data)){
    persons_classes <- cohort_data %>%
      dplyr::left_join(classified_classes, by = rlang::quo_name(rlang::enquo(idnum))) %>%
      dplyr::mutate(
        across(any_of(classes), ~ ifelse(is.na(.), 0, .))
      )
    if(add_zero_class == T) persons_classes[ , zero.classes] <- 0
  }
  
  if(!is.null(cohort_data)) return(persons_classes)
  
  if(is.null(cohort_data)) return(classified_classes)
  
}