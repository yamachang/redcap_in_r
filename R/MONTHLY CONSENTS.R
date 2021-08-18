p3.monthly.consent<-function(enddate= Sys.Date(), grabnew=T){
  #Libraries
  library(tidyr);library(dplyr);library(lubridate);library(eeptools); library(reshape2)
  
  #Setup
  pt<-bsrc.checkdatabase2(ptcs$protect, online=grabnew)
  md<-bsrc.checkdatabase2(ptcs$masterdemo, online=grabnew)
  idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
  names(idmap)<-c("masterdemoid","wpicid","soloffid")
  map <- bsrc.getIDDateMap(db=pt)
  map<-bsrc.findid(map, idmap=idmap, "registration_redcapid")
  map<-map %>% select(masterdemoid,redcap_event_name,date)
  
  
  #Get variables from the master demo
  MD<-md$data %>% select(masterdemoid=registration_redcapid, registration_wpicid,registration_initials, 
                         reg_condate_protect2,reg_condate_protect3,registration_dob,reg_status_protect2, 
                         reg_termdate_protect2, reg_term_reason_protect2,reg_status_protect3, 
                         reg_termdate_protect3, reg_term_reason_protect3,reg_condate_snake,
                         reg_condate_pi, reg_condate_explore, reg_condate_idecide, registration_group,
                         registration_futurenp, registration_ptcstat___protect2, registration_ptcstat___protect3,
                         reg_term_yesno_protect3, reg_term_yesno_protect2, registration_gender,
                         reg_p3catchup)

  P3 <- MD %>% 
    filter(registration_ptcstat___protect3==1) %>% # filter pts in p3
    mutate(con_month = month(ymd(reg_condate_protect3)), 
           con_year = year(ymd(reg_condate_protect3))) %>%  # extract consent month and year
    mutate(cur_month = month(ymd(Sys.Date())),
           last_month = cur_month - 1,
           cur_year = year(ymd(Sys.Date()))) 
  
  P3_this_month <- P3 %>% 
    filter(con_month == cur_month & con_year==cur_year) # filter current month's pts
  
  P3_last_month <- P3 %>% 
    filter(con_month == last_month & con_year==cur_year) # filter current month's pts
  
  cat("Today's date: ", Sys.Date())
  cat("This month consents: ", paste0(P3_this_month$registration_initials, collapse = ", "))
  cat("Last month consents: ", paste0(P3_last_month$registration_initials, collapse = ", "))
}
