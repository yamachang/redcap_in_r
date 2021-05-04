bsrc.ProtectDeathPull<-function(df = NULL, file_path = NULL, 
                                options = c("death info", "last visit")){
  require(dplyr);require(readxl);require(tibble)
  
  # get the list of participants if df is not specified 
  if (is.null(df)){df <- readxl::read_xlsx(file_path,sheet = 1) }else{
    message("The file_path is not used.")}
  
  #Setup
  masterdemo_environment<-bsrc.checkdatabase2(ptcs$masterdemo,online = T)
    idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
    names(idmap)<-c("masterdemoid","wpicid","soloffid")
  Protect<-bsrc.checkdatabase2(ptcs$protect,online = T) 
  
  # get all Protect pts
  MD_PT<-masterdemo_environment$data %>% mutate_all(~replace(.,.=="",NA)) %>% as_tibble() %>%
    select(registration_redcapid,registration_initials,registration_lastfour,registration_dob,
           cod_dead,cod_dc_wait,cod_source,!!quo(paste0("registration_ptcstat___",c("suicide","suicid2","protect","protect2","protect3")))) %>%
    filter_at(vars(starts_with("registration_ptcstat___")),any_vars(.==1)) %>% 
    select(-starts_with("registration_ptcstat")) # these cols are unnecessary
  
  # Below steps assumes Initial+DOB can find an unique match. test if this assumption is true
  if(any(count(MD_PT,registration_initials,registration_dob)$n!=1)) {
    stop("This function's assumption that there are no two people with the same initials and date of birth
          is no longer valid. You will need to correct the codes.")}
  
  ## match initials
  # mark problematic rows (col: Errors), add column number for identifying rows from each other 
  df0<-df %>% 
    add_column(rownumber = 1:nrow(.), .before = 1) %>% 
    mutate(#Init = toupper(paste(substr(`First Name`,1,1),substr(`Last Name`,1,3))), # the initial is given in the original sheet so this row is no longer needed
      LastFour = stringr::str_sub(SSN,-4,-1), # get the last four digits from the full SSN
      DOB = as.character(as.Date(DOB))) %>%  # standardize the format of date-type data
    mutate_all(~replace(.,as.character(.)=="",NA)) %>% 
    mutate(Errors = NA, # create a col to indicate whose data have errors
           Errors = replace(Errors, is.na(Initials), "The initial is missing."))
  if (any(!is.na(df0$Errors) & df0$Errors=="The initial is missing.")){warning("Some subjects' initials are missing. Please refer to the column 'Errors'.")}
  
  # match pts to redcap IDs based on Initial + DOB. Here I assume this combination can find an unique match. If the assumption no longer works, consider using Initials+DOB+SSN Last Four
  df<-df0 %>% filter(is.na(Errors)) %>% 
    left_join(MD_PT,by = c("Initials" = "registration_initials","DOB"="registration_dob")) %>% # FIND THE MATCH BY INITIALS+DOB
    mutate(Errors = replace(Errors, (LastFour!=registration_lastfour), "SSN is not matched."), # check if SSN is matched. Report if not.
           Errors = replace(Errors, is.na(registration_redcapid), "No match in Redcap")) %>% # report if can find a match by INITIALS+DOB
    # add back rows in which intitials are missing 
    bind_rows(subset(df0,!rownumber %in% .$rownumber)) %>% arrange(rownumber)
  
  # PENDING for those rows that don't match, try matching based on initials only
  
  
  ## get death information
  if (options == "death info"){
    df %>% mutate(IfDead = NA, IfDead = replace(IfDead, cod_dead == 1, "Dead"),
                  # create a col to indicate the status of death certificate 
                  DeathCert = NA, 
                  DeathCert = replace(DeathCert, cod_dead ==1 & (is.na(cod_source)|cod_source!=1), "Death certificate pending"), # dead but do not have certificate
                  DeathCert = replace(DeathCert, cod_dc_wait == 1, "Waiting for death certificate")) %>% 
      select(Errors,`First Name`:SSN,IfDead,DeathCert) %>% 
      return()
    
    ## Latest visit 
  } else if (options == "last visit"){
    # load the Protect project from redcap 
    
    # get the last visit dates of everyone 
    df_LastVisit<-bsrc.getform(formname = "followup_questions",curdb = Protect) %>% 
      transmute(ID = registration_redcapid, VisitDate = fug_date, event = redcap_event_name) %>% 
      bind_rows( bsrc.getform(formname = "baseline_questions",curdb = Protect)%>%
                   transmute(ID = registration_redcapid, VisitDate = bq_date, event = redcap_event_name)) %>%
      mutate_all(~replace(.,.=="",NA)) %>%
      bsrc.findid(idmap) %>% filter(ifexist) %>% as_tibble() %>% 
      group_by(ID) %>% slice(which.max(as.Date(VisitDate))) %>% ungroup() %>% 
      transmute(masterdemoID = masterdemoid, LastSeenDate = VisitDate, Event = event)
    
    # last seen date + event for the pts of interest 
    df %>% left_join(df_LastVisit, by = c("registration_redcapid"= "masterdemoID")) %>% 
      select(Errors, `First Name`:SSN, LastSeenDate,Event) %>%
      return()
  } else {
    stop("Please specify the option. Note that options are case sensitive.")
  }
  
}
