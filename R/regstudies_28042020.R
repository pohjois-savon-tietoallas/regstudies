#' # regstudies.R
#' require(dplyr)
#' require(tidyr)
#' require(stringr)
#' require(purrr)
#' 
#' # filter-functions: ----------------------------
#' #
#' #' Get first value based on datetime variable
#' #'
#' #' @param .date data which contains datetime or other comparable variable
#' #' @param var datetime or other comparable variable
#' #' @return The row which is the first (by which.min) in the .date
#' #' @examples
#' #' # searching for the first event of the each class (for each individual):
#' #' dat %>%
#' #'   filter(!is.na(label)) %>%
#' #'   group_by(lomno1,label) %>%
#' #'   getfirst(tulopvm)
#' #'
#' getfirst<-function(.date,var) { # fill=NA
#'   var_quo<-enquo(var)
#'   .date %>%
#'     slice(which.min(!!var_quo)) #TODO: Jos ei ole yhtään tapahtumaa, niin ei tule mitään tulosta!
#' }
#' #' Get last value based on datetime variable
#' #'
#' #' @param .date data which contains datetime or other comparable variable
#' #' @param var datetime or other comparable variable
#' #' @return The row which is the first (by which.max) in the .date
#' #' @examples
#' #' # searching for the first event of the each class (for each individual):
#' #' dat %>%
#' #'   filter(!is.na(label)) %>%
#' #'   group_by(lomno1,label) %>%
#' #'   getlast(tulopvm)
#' #'
#' getlast<-function(.date,var) {
#'   var_quo<-enquo(var)
#'   .date %>%
#'     slice(which.max(!!var_quo))
#' }
#' #' Get nearest value based on datetime variable
#' #'
#' #' @param .date data which contains datetime or other comparable variable
#' #' @param var datetime or other comparable variable
#' #' @param centroid datetime or other comparable variable as reference point around which to search
#' #' @param only_previous optional parameter (default=FALSE): should we search only previously observed data prior centroid?
#' #' @return The row which is the nearest .date
#' #' @examples
#' #' # searching for the first event of the each class (for each individual):
#' #' dat %>%
#' #'   getnearest(tulopvm,centroid=Postituspvm)
#' #'
#' getnearest<-function(.date,var,centroid,only_previous=FALSE) {
#'   var_quo<-enquo(var)
#'   cen_quo<-enquo(centroid)
#'   if (only_previous) {
#'     .date %>%
#'       filter((!!var_quo) <= (!!cen_quo)) %>%
#'       slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
#'       return()
#'   } else {
#'     .date %>%
#'       slice(which.min(abs((!!var_quo)-(!!cen_quo)))) %>%
#'       return()
#'   }
#' }
#' 
#' 
#' 
#' #' Filtering datetime data within interval.
#' #'
#' #'
#' #'
#' #' @param indexdate index date (which date variable is to be compared with register data)
#' #' @param range the distance from index date (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#' #' @param ... datetime variables which need to be within the range. May be any number (>1) of variables.  If one variable is within interval, then that row is included in the data (Applies OR operator).
#' #'
#' #' @return
#' #' @examples
#' #' 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #'
#' #' dat<-ostprekoh %>%
#' #' left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm,icd),by="lomno1") %>%
#' #'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#' #'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm)
#' #'
#' datefilter<-function(.data,indexdate,range=years(2),...) { #,datevars=c("tulopvm","lahtopvm")
#'   # .data: data to be used
#'   # indexdate: index date (which date variable is to be compared with register data)
#'   # range: the distance from index date (lubridate format, e.g years(1), weeks(10), days(20) etc.)
#'   # ...: variables which user wants to be used for filtering. May be any number (>1) of variables. If one variable is within interval, then that row is included in the data (Applies OR operator).
#'   indexdate<-enquo(indexdate)
#'   datevars<-enquos(...)
#'   ndatevars<-length(datevars)
#' 
#'   require(purrr)
#'   fwithin<-function(var,ival) {
#'     expr((!! var) %within% ival)
#'   }
#'   vars_within_ival <- purrr::map(datevars, fwithin, ival=ival)
#' 
#'   .data %>% # join the datas
#'     mutate(ival= ((!!indexdate)-range) %--% ((!!indexdate)+range)) %>% # 2. create an interval out of 'indexdate'
#'     filter(!!!vars_within_ival) %>%# this works with arbirary number of date variables entered
#'     select(-ival)
#'   # TODO: Make tests for this function!
#' }
#' 
#' # ----------------------------------------------
#' 
#' 
#' 
#' #' Summaryscore
#' #'
#' #' Sum scores for each individual. Allows arbitrary number of scores to be calculated using single function call.
#' #'
#' #'
#' #' @param .data the data which contains the variables to be summed over.
#' #' @param ... the names of the variables to be summed over.
#' #'
#' #' @return returns scores summed over the given variables.
#' #' @examples
#' #'
#' #' # Lets calculate two different Elixhauser scores for a data set 'filtereddata'
#' #' elixhauser_classes <- classes_to_wide(vroom(file = "datas/classification_codes/elixhauser_classes_wide.csv"))
#' #' elixscore <- filtereddata %>%
#' #'   classifydata_long(icdcodes=CODE1,diag_tbl=elixhauser_classes) %>%
#' #'   summaryscore(score_AHRQ,score_van_Walraven)
#' #'
#' summaryscore<-function(.data,...) {
#'   vars<-enquos(...)
#'   # A bad code to extract names given by "..." (I could not figure out the better way):
#'   nimet <- .data %>% head(0) %>% select(!!! vars) %>% names()
#' 
#'   output <- .data %>%
#'     select(c("personid","classification","class",nimet)) %>%
#'     filter(!is.na(classification)) %>%
#'     distinct() %>%
#'     group_by(personid,classification) %>%
#'     summarise_at(nimet,sum,na.rm=T)
#'   names(output)[3:(3+length(nimet)-1)]<-paste("summary(",nimet,")",sep="")
#'   output
#' }
#' 
#' 
#' 
#' #' Internal function for providing long classification definitions.
#' #'
#' #' Operates classification definitions from wide format to long format.
#' #'
#' #' @param sel_classes This variable contains the data in the wide format. See the classifications defined in /datas/classification_codes/
#' #' @return returns classification definintion in long format (tidy data) as that is what the package needs in operation.
#' #' @examples
#' #' sel_classes <- vroom(file = "datas/classification_codes/elixhauser_classes_wide.csv")
#' #' sel_classes2 <- classes_to_wide(sel_classes = sel_classes)
#' #' head(sel_classes2)
#' classes_to_wide <- function(sel_classes) {
#'   require(tidyselect)
#'   main        <- sel_classes %>% select(classification, label, contains("score")) %>% distinct()
#'   dat_to_long <- sel_classes %>% select(-contains("score"), -classification)
#'   nimet       <- setdiff(names(dat_to_long), c("class","label"))
#'   sel         <- grep(".rm", x=nimet) # names of exceptions!
#'   nexcep      <- length(sel) # how many exceptions
#'   vnim<-nimet
#'   pnim<-c()
#'   if(nexcep>0) {
#'     vnim        <- nimet[-sel] # varsinaiset nimet
#'     pnim        <- nimet[sel]  # poikkeusnimet
#'   }
#' 
#'   sel_classes2 <- pivot_longer(dat_to_long %>% select(-all_of(pnim)), -c("class","label"), names_to="icd", values_to="regex")
#'   if (nexcep>0) {
#'     sel_classes2rm <- pivot_longer(dat_to_long %>% select(-all_of(vnim)), -c("class","label"), names_to="icd", values_to="regex.rm") %>%
#'       mutate(icd=sub(pattern=".rm", "", x=icd))
#'     sel_classes2 <- left_join(sel_classes2, sel_classes2rm)
#'   }
#'   sel_classes2 <- right_join(main, sel_classes2)
#'   nimet2       <- c("classification","icd","class","label","regex")
#'   if(nexcep>0) {
#'     nimet2<-c(nimet2,"regex.rm")
#'   }
#'   sel_classes2 %>%
#'     select(contains(c("classification","icd","class","label","regex","score")))
#' }
#' 
#' 
#' 
#' #' Classify diagnosis codes to long format with exemptions
#' #'
#' #' Computes classification table which can be attached to original data using left_join().
#' #'
#' #' @param .data tibble of register data which we want to study
#' #' @param icdcodes name of the variable holding ICD-codes
#' #' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#' #'   'regex' must hold a string with a regular expression defining classes.
#' #'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#' #'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' #' @return
#' #' @examples
#' #' # we calculate the table which can be used for classification.
#' #' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #' ostprekoh %>%
#' #'   left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm),by="lomno1") %>%
#' #'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#' #'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>% # filtering the diagnosis codes which are in the interval for each individual!
#' #'   classify_long(icdcodes=KOODI1,diag_tbl=sel_classes)
#' #'
#' classify_long <- function(.data,icdcodes,diag_tbl,return_binary=TRUE) {
#'   # .data: tibble from which we want to study
#'   # icdcodes: name of the variable holding ICD-codes (you can use any type of string codes but change your classification definitions according to that)
#'   # diag_tbl: tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   # 'regex' must hold a string with a regular expression defining classes.
#'   # 'regex.rm' is optional, defines exceptions to 'regex'
#'   # 'label' defines the names of the variables of classes (comorbidity indicators)
#' 
#'   icdcodes <- enquo(icdcodes)
#'   if (!dplyr::setequal(intersect(c("regex","label"),names(diag_tbl)),c("regex","label"))) {
#'     print("Names of the diag_tbl are wrong. Need to have 'regex' and 'label'.")
#'     lt<-diag_tbl # TODO: Throw an error or return some sensible object!
#'   } else {
#'     #diag_tbl<-diag_tbl %>% select(regex,regex.rm,label) # regex.rm ei ole implementoitu! (viel?)
#'     codes<-.data %>% select(!! icdcodes) %>% distinct()
#'     cr<-crossing(codes,diag_tbl %>% select(regex))
#'     cr <- cr %>%
#'       left_join(diag_tbl,by="regex") %>%
#'       mutate(match.yes=str_detect(!! icdcodes,regex))
#'     if(is.element("regex.rm",names(diag_tbl))) {
#'       print("Element 'regex.rm' in use. Taking exceptions in use.")
#'       cr <- cr %>%
#'         mutate(match.rm=str_detect(!! icdcodes,regex.rm),
#'                match.rm=ifelse(is.na(match.rm),FALSE,match.rm)
#'         )
#'     } else {
#'       print("Element 'regex.rm' NOT in use. Exceptions omitted.")
#'       cr <- cr %>%
#'         mutate(match.rm = FALSE,regex.rm=NA)
#'     }
#'     cr <- cr %>%
#'       mutate(match = match.yes & !match.rm) %>%
#'       select(-match.yes,-match.rm)
#'     #print(cr)# %>% filter(!is.na(match.rm) & match.rm))
#'     if(return_binary) {
#'       cr<-cr %>%
#'         mutate(match=as.integer(match))
#'     }
#'     classification_name<-diag_tbl %>%
#'       select(classification) %>%
#'       distinct() %>% as.vector()
#' #    print(paste0("Using classification '",classification_name,"'")) # TODO: Ei ole tarkastettu, ett? on annettu vain yksi luokittelu
#'     lt<-cr %>%
#'       select(-contains("regex")) #%>%
#'       #mutate(label=paste0(substr(classification_name,1,5),"_",label))
#' #    lt <- pivot_wider(lt,
#' #                      names_from=label,
#' #                      values_from=match)
#'   }
#'   lt
#'   # TODO: Kehit? s.e. muuttujien nimen_eteen saadaan teksti 'elixhauser', jos se on luokittelun nimi!
#' }
#' 
#' 
#' 
#' #' Extends the original data with the classification table and keeps data in long format.
#' #'
#' #' Computes classification table and attaches it to original data. Data stays in long format.
#' #'
#' #' @param .data tibble of register data which we want to study
#' #' @param icdcodes name of the variable holding ICD-codes
#' #' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#' #'   'regex' must hold a string with a regular expression defining classes.
#' #'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#' #'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' #' @return
#' #' @examples
#' #' # we calculate the table which can be used for classification.
#' #' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #'
#' #' dat<-ostprekoh %>%
#' #' left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm,icd),by="lomno1") %>%
#' #'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#' #'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>%
#' #'   classifydata_long(icdcodes=KOODI1,diag_tbl=sel_classes) %>%
#' #'   filter(match>0)
#' classifydata_long<-function(.data,id,icdcodes,diag_tbl,fill=0) {
#'   icdcodes_quo <- enquo(icdcodes)
#'   id_quo <- enquo(id)
#'   ctobj<-classify_long(.data=.data,icdcodes=!!icdcodes_quo,diag_tbl=diag_tbl,return_binary=FALSE) #classification table object'
#' 
#'   classification_name<-ctobj %>%
#'     select(classification) %>%
#'     distinct() %>% as.vector()
#'   nimet<-names(ctobj)
#'   nimet
#'   print("ctobj:")
#'   print(names(ctobj))
#'   print(".data:")
#'   print(names(.data))
#' 
#'   # icdcodes = KOODI1
#'   # id = lomno1 # user needs to enter this currently!
#'   text<-c(as_label(icdcodes_quo),"icd")
#'   print(text)
#'   outdat<-.data %>%
#'     #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
#'     left_join(ctobj%>%filter(match)%>%select(-match),by=text)
#'   outdat
#' }
#' 
#' 
#' 
#' #' Make regural expression classifications from LIKE\% classifications
#' #'
#' #' @param .data holds the data which has variable names given as 'classname' and 'diagnosis'. Must be convertible to a tibble.
#' #' @param classname name of the variable containing names/labels for the classes
#' #' @param diagnosis name of the variable containing strings in LIKE\% -format
#' #' @param diagnosis.rm name of the variable defining exceptions of ICD-codes not to be included in the class
#' #' @param sep separator of different diagnoses in 'diagnosis'. String.
#' #'
#' #' @return returns a tibble with regular expression definitions
#' #'
#' make_regex <- function(.data,classname,diagnosis,diagnosis.rm,sep=", ") {
#'   .data=as_tibble(.data)
#'   diagnosis = enquo(diagnosis)
#'   diagnosis.rm = enquo(diagnosis.rm)
#'   nr<-dim(.data)[1]
#'   # lets transform diag.list and diag.list.rm to 'regex' format!
#'   diag.vec<-vector("character",nr)
#'   diag.vec.rm<-diag.vec
#'   for(l in 1:nr) {
#'     vec1 <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis)),", "))
#'     vec.rm <- unlist(strsplit(as.character(.data %>% slice(l) %>% select(!! diagnosis.rm)),", "))
#'     diag.vec[l]    = paste(paste0("^",gsub(pattern="%",replacement="",x=vec1)),collapse="|")
#'     diag.vec.rm[l] = paste(paste0("^",gsub(pattern="%",replacement="",x=vec.rm)),collapse="|")
#'   }
#'   diag.names<-.data$Lyhenne
#'   # lets create a table 'diag_tbl' which holds diagnosis classification!
#'   diag_tbl<-tibble(regex=diag.vec,regex.rm=diag.vec.rm,label=diag.names) %>%
#'     mutate(regex.rm=ifelse(regex.rm=="^NA",NA,regex.rm))
#'   diag_tbl
#' }
#' 
#' 
#' # ----------------------------------------------
#' # deprecated functions
#' 
#' 
#' 
#' #' Deprecated: Extends the original data with the classification table and makes wide data.
#' #'
#' #' Computes classification table and attaches it to original data. Attached data is wide format (many new variables).
#' #'
#' #' @param .data tibble of register data which we want to study
#' #' @param id personal identification number
#' #' @param icdcodes name of the variable holding ICD-codes
#' #' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#' #'   'regex' must hold a string with a regular expression defining classes.
#' #'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#' #'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' #' @param return_binary returns 0 or 1 if set as TRUE, TRUE/FALSE if set up as FALSE
#' #' @return
#' #' @examples
#' #' # we calculate the table which can be used for classification.
#' #' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #'
#' classifydata<-function(.data,id,icdcodes,diag_tbl,return_binary=TRUE) {
#'   icdcodes_quo <- enquo(icdcodes)
#'   id_quo <- enquo(id)
#'   ctobj<-classify(.data=.data,icdcodes=!!icdcodes_quo,diag_tbl=diag_tbl,return_binary=return_binary) #classification table object'
#' 
#'   classification_name<-ctobj %>%
#'     select(classification) %>%
#'     distinct() %>% as.vector()
#'   nimet<-names(ctobj)
#'   nimet
#'   print(names(ctobj))
#'   # icdcodes = KOODI1
#'   # id = lomno1 # user needs to enter this currently!
#'   text<-c(as_label(icdcodes_quo),"icd")
#'   outdat<-.data %>%
#'     #select(!!id_quo,!!icdcodes_quo) %>% # removed unnecessary variables
#'     left_join(ctobj,by=text)
#'   outdat
#' }
#' 
#' 
#' 
#' #' Deprecated: Classify diagnosis codes (no exceptions)
#' #'
#' #' Computes classification table which can be attached to original data using left_join(). Does not utilise exceptions to class definitions via 'regex.rm'
#' #'
#' #' @param .data tibble of register data which we want to study
#' #' @param icdcodes name of the variable holding ICD-codes
#' #' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#' #'   'regex' must hold a string with a regular expression defining classes.
#' #'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#' #'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' #' @param return_binary returns 0 or 1 if set as TRUE, TRUE/FALSE if set up as FALSE
#' #' @return
#' #' @examples
#' #' # we calculate the table which can be used for classification.
#' #' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #' ostprekoh %>%
#' #'   left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm),by="lomno1") %>%
#' #'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#' #'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>% # filtering the diagnosis codes which are in the interval for each individual!
#' #'   classify_long(icdcodes=KOODI1,diag_tbl=sel_classes)
#' #'
#' classify_no_rm <- function(.data,icdcodes,diag_tbl,return_binary=TRUE) {
#'   # .data: tibble from which we want to study
#'   # icdcodes: name of the variable holding ICD-codes
#'   # diag_tbl: tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   # 'regex' must hold a string with a regular expression defining classes.
#'   # 'label' defines the names of the variables of classes (comorbidity indicators)
#' 
#'   icdcodes <- enquo(icdcodes)
#'   if (!dplyr::setequal(intersect(c("regex","label"),names(diag_tbl)),c("regex","label"))) {
#'     print("Names of the diag_tbl are wrong. Needs to have 'regex' and 'label'.")
#'     lt<-diag_tbl # TODO: Throw an error or return some sensible object!
#'   } else {
#'     diag_tbl<-diag_tbl %>% select(regex,label) # regex.rm ei ole implementoitu! (viel?)
#'     codes<-.data %>% select(!! icdcodes) %>% distinct()
#'     cr<-crossing(codes,diag_tbl %>% select(regex))
#'     cr <- cr %>%
#'       left_join(diag_tbl,by="regex") %>%
#'       mutate(match=str_detect(!! icdcodes,regex))
#'     if(return_binary) {
#'       cr<-cr %>%
#'         mutate(match=as.integer(match))
#'     }
#'     lt <- pivot_wider(cr %>%
#'                         select(-regex),
#'                       names_from=label,
#'                       values_from=match)
#'   }
#'   lt
#' }
#' 
#' #' Classify diagnosis codes (old one)
#' #'
#' #' Computes classification table which can be attached to original data using left_join(). Utilises exceptions to class definitions via 'regex.rm'
#' #'
#' #' @param .data tibble of register data which we want to study
#' #' @param icdcodes name of the variable holding ICD-codes
#' #' @param diag_tbl tibble which holds the classification details: needs to have variables 'regex' and 'label'
#' #'   'regex' must hold a string with a regular expression defining classes.
#' #'   'regex.rm' is optional, defines exceptions to 'regex' (these are not in the group they are named in)
#' #'   'label' defines the names of the variables of classes (e.g. comorbidity indicators)
#' #' @param return_binary returns 0 or 1 if set as TRUE, TRUE/FALSE if set up as FALSE
#' #' @return
#' #' @examples
#' #' # we calculate the table which can be used for classification.
#' #' # 'ostprekoh' is the register data containing 'lomno1' individual id number and data 'dg' contains the register data of disease diagnoses.
#' #' ostprekoh %>%
#' #'   left_join(dg %>% select(lomno1,KOODI1,tulopvm,lahtopvm),by="lomno1") %>%
#' #'   mutate(Postituspvm=ymd(Postituspvm)) %>%
#' #'   datefilter(indexdate=Postituspvm,range=years(2),tulopvm,lahtopvm) %>% # filtering the diagnosis codes which are in the interval for each individual!
#' #'   classify_long(icdcodes=KOODI1,diag_tbl=sel_classes)
#' #'
#' classify <- function(.data,icdcodes,diag_tbl,return_binary=TRUE) {
#'   # classify2: with 'regex.rm' -option
#'   # .data: tibble from which we want to study
#'   # icdcodes: name of the variable holding ICD-codes
#'   # diag_tbl: tibble which holds the classification details: needs to have variables 'regex' and 'label'
#'   # 'regex' must hold a string with a regular expression defining classes.
#'   # 'regex.rm' is optional, defines exceptions to 'regex'
#'   # 'label' defines the names of the variables of classes (comorbidity indicators)
#' 
#'   icdcodes <- enquo(icdcodes)
#'   if (!dplyr::setequal(intersect(c("regex","label"),names(diag_tbl)),c("regex","label"))) {
#'     print("Names of the diag_tbl are wrong. Needs to have 'regex' and 'label'.")
#'     lt<-diag_tbl # TODO: Throw an error or return some sensible object!
#'   } else {
#'     #diag_tbl<-diag_tbl %>% select(regex,regex.rm,label) # regex.rm ei ole implementoitu! (viel?)
#'     codes<-.data %>% select(!! icdcodes) %>% distinct()
#'     cr<-crossing(codes,diag_tbl %>% select(regex))
#'     cr <- cr %>%
#'       left_join(diag_tbl,by="regex") %>%
#'       mutate(match.yes=str_detect(!! icdcodes,regex))
#'     if(is.element("regex.rm",names(diag_tbl))) {
#'       print("Element 'regex.rm' in use. Taking exceptions in use.")
#'       cr <- cr %>%
#'         mutate(match.rm=str_detect(!! icdcodes,regex.rm),
#'                match.rm=ifelse(is.na(match.rm),FALSE,match.rm)
#'                )
#'     } else {
#'       print("Element 'regex.rm' NOT in use. Exceptions omitted.")
#'       cr <- cr %>%
#'         mutate(match.rm = FALSE)
#'     }
#'     cr <- cr %>%
#'       mutate(match = match.yes & !match.rm) %>%
#'       select(-match.yes,-match.rm)
#'     #print(cr)# %>% filter(!is.na(match.rm) & match.rm))
#'     if(return_binary) {
#'       cr<-cr %>%
#'         mutate(match=as.integer(match))
#'     }
#'     classification_name<-diag_tbl %>%
#'       select(classification) %>%
#'       distinct() %>% as.vector()
#' #    print(paste0("Using classification '",classification_name,"'")) # TODO: Ei ole tarkastettu, ett? on annettu vain yksi luokittelu
#'     lt <- pivot_wider(cr %>% select(-contains("regex")),# %>%
#'                         #mutate(label=paste0(substr(classification_name,1,5),"_",label)),
#'                       names_from=label,
#'                       values_from=match)
#'   }
#'   lt
#'   # TODO: Kehit? s.e. muuttujien nimen_eteen saadaan teksti 'elixhauser', jos se on luokittelun nimi!
#' }
#' 
