p3.weekly.report<-function(enddate= Sys.Date(), grabnew=T){
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

# #Check p2 people not in p3 yet (whenever there are 0 people, can remove p2)
#     md$data %>% filter(registration_ptcstat___protect2==1)->p2
#     p2 %>% filter(registration_ptcstat___protect3==0 & 
#                     (is.na(reg_term_yesno_protect2) | 
#                     reg_term_yesno_protect2==0))->x
#     x %>% select(registration_redcapid,registration_initials,
#                  registration_ptcstat___protect3,
#                  registration_ptcstat___protect2,reg_term_yesno_protect2)->x

#Get variables from the master demo
    MD<-md$data %>% select(masterdemoid=registration_redcapid, registration_wpicid,registration_initials, 
                    reg_condate_protect2,reg_condate_protect3,registration_dob,reg_status_protect2, 
                    reg_termdate_protect2, reg_term_reason_protect2,reg_status_protect3, 
                    reg_termdate_protect3, reg_term_reason_protect3,reg_condate_snake,
                    reg_condate_pi, reg_condate_explore, reg_condate_idecide, registration_group,
                    registration_futurenp, registration_ptcstat___protect2, registration_ptcstat___protect3,
                    reg_term_yesno_protect3, reg_term_yesno_protect2, registration_gender,
                    reg_p3catchup)
  
#Add monthly consent
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
  
#Remove unnecessary people
  #Only consented for Protect2 or Protect 3
  MD[which(rowSums(MD %>% select(registration_ptcstat___protect2,
                                 registration_ptcstat___protect3))>0),]->MD
  #Remove HCs (no followups) and ineligible (group=89)
  MD[which(MD$registration_group!="HC" & MD$registration_group!=89),]->MD
  
  
#Get additional variables
  #Calculate age at data pull
    #If anyone is missing date of birth, can't calculate age, give error
    if(any(is.na(MD$registration_dob))){
      stop("Someone is missing date of birth, please enter date of birth for ID before proceeding")
      print(MD[which(is.na(MD$registration_dob)),"registration_redcapid"])}
  as.Date(MD$registration_dob)->MD$registration_dob
  as.Date(enddate)->enddate
  MD %>% mutate(age=ifelse(!is.na(MD$registration_dob), age_calc(na.omit(MD$registration_dob), 
                               enddate = enddate, units="years", precise=F), NA))->MD
#Fix group issue ([IDE and DEP]->DNA)
  MD[which(MD$registration_group %in% c("IDE","DEP")),"registration_group"]<-"DNA"
  
#Get Attempters' last attempt date
  att<-md$data %>% select(masterdemoid=registration_redcapid, contains("sahx_sadate"))
  att<-reshape(att, direction='long', 
                         varying=names(att)[-c(1)], 
                         timevar='att',
                         times=1:30,
                         v.names="date",
                         idvar="masterdemoid")
  att$date<-ymd(att$date)
  att<-att[which(!is.na(att$date) & att$date!="9999-09-09"),]
  att<-att %>% group_by(masterdemoid) %>% filter(date==max(date)) %>% ungroup() %>%
    mutate(attin_last5years=ifelse(enddate-date>5*365,F, T))
  
  att$attin_last5years[match(MD$masterdemoid, att$masterdemoid)]->MD$attempt

  
#3 groups of people (that we care about- M FIR 440156 is weird): 
  #1) Consented for P2, not terminated
  MD %>% filter(registration_ptcstat___protect2==1 & (is.na(reg_term_yesno_protect2) | 
    reg_term_yesno_protect2!=1) & registration_ptcstat___protect3!=1)->p2
  if(nrow(p2)==0){print("You can remove p2 group now, everyone consented to p3 or termed")}
  #2) Consented for P2 & P3, not terminated from P3
  MD %>% filter(registration_ptcstat___protect2==1 & registration_ptcstat___protect3==1 &
                  (is.na(reg_term_yesno_protect3) | reg_term_yesno_protect3!=1) & 
                  masterdemoid!="440156")->p2p3
  #3) Consented for P3, not consented for P2 or terminated from P3
  MD %>% filter(registration_ptcstat___protect3==1 & registration_ptcstat___protect2!=1 &
                  (is.na(reg_term_yesno_protect3) | reg_term_yesno_protect3!=1) |
                  masterdemoid=="440156")->p3
  
#Grab Protect data and make a dataframe
  #Get variables from the master demo
    pro<-pt$data %>% select(registration_redcapid,redcap_event_name,
                            bq_assessor, fuq_assessor, sgcomplete,snakeg_date,
                            st_complete, starts_with("st_whichtask"),
                    starts_with("mri_") | starts_with("ssi"))
  #Idmap to get masterdemoid
  bsrc.findid(pro,idmap=idmap, "registration_redcapid")->pro
    #If any ids don't exist and aren't names "test" or "TEST", stop function
    if(any(!pro$ifexist & !grepl("est",tolower(pro$registration_redcapid)))){
      print(pro[which(!pro$ifexist & !grepl("est",tolower(pro$registration_redcapid))),
                      "registration_redcapid"])
      stop("This ID doesn't exist:")}
  
  #Get data frame for SSI (used to determine risk, >=5 is high risk, <5 is low risk)
    pro[c("masterdemoid","redcap_event_name",
                            paste("ssi_",c(1:19),"_worst", sep=""),
                             paste("ssi_",c(1:19),"_curr", sep=""))]->ssi
    #Get dates of SSI
    merge(map, ssi, all.y=T, by=c("masterdemoid","redcap_event_name"))->ssi
    
    #Remove rows with all NAs/ blanks
    ssi[which(rowSums(is.na(ssi[grepl("ssi",names(ssi))]) | 
                            ssi[grepl("ssi",names(ssi))]=="")<38),]->ssi
    
    #Check people missing visit dates
    as.Date(ssi$date)->ssi$date
    ssi[which(ssi$masterdemoid %in% MD$masterdemoid),]->ssi
    ssi[is.na(ssi["date"]),]->check
    if(nrow(check)>0){
      print(check[c("masterdemoid","redcap_event_name","date")])
      stop("Some people have SSIs but don't have visit dates")
    }
    #Change "na" to be scored as 0 and "dk/refuse" to be scored with NA
    ssi<-apply(ssi, 2, function(x){ifelse(x %in% c("na",NA,""), 0, x)})
    ssi<-apply(ssi, 2, function(x){ifelse(x %in% c("dk","refuse"), NA, x)})
    as.data.frame(ssi)->ssi
    #Change everything from factor to numeric
    as.character(ssi$redcap_event_name)->ssi$redcap_event_name
    as.Date(ssi$date)->ssi$date
    for(i in 1:ncol(ssi)){
      if(is.factor(ssi[,i])| is.character(ssi[,i]) & !names(ssi)[i] %in% c("redcap_event_name","date")){
        as.numeric(as.character(ssi[,i]))->ssi[,i]}}
    #Score worst and current w/ NA rules (if >10% NAs, score=NA, else score=score)
    ssi %>% mutate(worst.score=round(ifelse(rowSums(is.na(ssi[grepl("worst",names(ssi))]))>1, 
        NA,ifelse(rowSums(is.na(ssi[grepl("worst",names(ssi))]))==1,
        rowSums(ssi[grepl("worst",names(ssi))],na.rm=T)*19/18, ifelse(rowSums(is.na(ssi[grepl("worst",names(ssi))]))==0,
        rowSums(ssi[grepl("worst",names(ssi))]),NA)))))->ssi
    ssi %>% mutate(curr.score=round(ifelse(rowSums(is.na(ssi[grepl("curr",names(ssi))]))>1, 
        NA,ifelse(rowSums(is.na(ssi[grepl("curr",names(ssi))]))==1,
        rowSums(ssi[grepl("curr",names(ssi))],na.rm=T)*19/18, ifelse(rowSums(is.na(ssi[grepl("curr",names(ssi))]))==0,
        rowSums(ssi[grepl("curr",names(ssi))]),NA)))))->ssi
    #To determine high risk vs. low risk, we want highest score in past 2 years 
      #(if baseline), then highest lifetime doesn't count [worst]
      ssi[which(ssi$date>(enddate-2*365)),]->ssi2yr
      ssi2yr[which(grepl("^baseline",ssi2yr$redcap_event_name) & !grepl("catchup", ssi2yr$redcap_event_name)),"worst.score"]<-NA
      ssi2yr %>% mutate(ssi.max2yr=pmax(worst.score,curr.score,na.rm = T))->ssi2yr
      ssi2yr %>% group_by(masterdemoid) %>% mutate(max2yr.score=max(ssi.max2yr,na.rm=T))->ssi2yr
      ssi2yr %>% select(masterdemoid, max2yr.score) %>% 
        filter(row_number()==1)->SSI
    

#Get data frame of tasks
  pro %>% select(masterdemoid,redcap_event_name,sgcomplete,
          snakeg_date,st_complete,contains("___"),-contains("ssi"))->tasks
  #Scanning data frame
  tasks %>% filter(grepl("baseline",redcap_event_name)) %>% 
    select(masterdemoid,redcap_event_name,starts_with("mri"))->scan
    #remove NAs and all unchecked boxes
    scan[which(rowSums(is.na(scan))<max(rowSums(is.na(scan)))),]->scan
    scan[which(rowSums(scan[grepl("___",names(scan))])>0),]->scan
    #Grab arm number variable, if two fMRI screening forms, take highest arm number
    scan$arm_num<-as.numeric(gsub("baseline_arm_","",scan$redcap_event_name))
      if(any(duplicated(scan$masterdemoid))){
      message("Took highest arm number for these, multiple fMRI forms")
        scan[duplicated(scan$masterdemoid),"masterdemoid"]->dups
        print(dups)
      }
    scan %>% group_by(masterdemoid) %>% filter(arm_num==max(arm_num))->scan
    #Remove columns for NAs (ending in ni,nask,asku,or na)
    scan[grepl("ni$|nask$|asku$|na$",names(scan))]<-NULL
    scan$arm_num<-NULL
    #Make sure you didn't add any new variables
    if(any(!names(scan) %in% c("masterdemoid","redcap_event_name",paste0("mri_nomri___",c(0,1:7,10)),
      paste0("mri_nomricurr___",c("mmse","young","weight","decline","subs","pa","covid","other")),
      paste0("mri_noclock___",c("ksoc","bsoc","behav","explore1")),
      paste0("mri_no_spott___",c("bsocial", "behav")),
      paste0("mri_neverscan___",c("history","old","contra","neuro","brain"))))){
      stop("You added a new fmri variable to redcap, please go back and add to code")
    }
    #Only grab checkboxes that are checked
    reshape2::melt(scan,"masterdemoid")->scanmelt
    scanmelt[which(!is.na(scanmelt$value) & scanmelt$value==1),]->scanmelt
    scanmelt[which(grepl("___",scanmelt$variable)),]->scanmelt
    scanmelt$value<-NULL
    #Create map of ineligiblity reasons
    inelireasons<-data.frame(nums=c(0,1:6,7,10),ineli=c("Done","Ineligible currently","Ineligible for clock",
                  "Ineligible forever","Pending Scheduling", "Pending Clearance","Hard to reach","Ineligible for spott",
                  "Other"))
    inelireasons %>% mutate_all(~as.character(.))->inelireasons
    scanmelt %>% mutate(num=gsub("mri_nomri___","",variable))->scanmelt
    as.character(scanmelt$num)->scanmelt$num
    inelireasons$ineli[match(scanmelt$num, inelireasons$nums)]->scanmelt$ineli1
    #Map of all specific and broad reasons allowed for not scanning
    ineli2<-data.frame(spec_reas=c("mmse","young","weight","decline","subs","pa","covid","ksoc","bsoc",
                            "behav","explore1","history","old","contra","neuro","brain","bsocial",NA,NA,NA),
               broad_reas=c("mri_nomricurr___","mri_noclock___","mri_neverscan___","mri_no_spott___"))
    ineli2 %>% mutate_all(~as.character(.))->ineli2
    #Get specific reason for not scanning
    scanmelt %>% mutate(spec_reas=ifelse(is.na(ineli1),gsub(paste(ineli2$broad_reas[1],
                  ineli2$broad_reas[2],ineli2$broad_reas[3],ineli2$broad_reas[4],sep="|"),"",variable),NA))->scanmelt
    #Get a broad reason for not scanning
    specific<-ineli2$spec_reas[1]
    for (i in 1:length(ineli2$spec_reas)-1){
      paste(specific,ineli2$spec_reas[i+1],sep="|")->specific
    }
    scanmelt %>% mutate(broad_reas=ifelse(is.na(ineli1),gsub(specific,"",variable),NA))->scanmelt
    scanmelt[!is.na(scanmelt) & scanmelt=="mri_noclock___"]<-"Ineligible for clock"
    scanmelt[!is.na(scanmelt) & scanmelt=="mri_no_spott___"]<-"Ineligible for spott"
    scanmelt[!is.na(scanmelt) & scanmelt=="mri_nomricurr___"]<-"Ineligible currently"
    scanmelt[!is.na(scanmelt) & scanmelt=="mri_neverscan___"]<-"Ineligible forever"
    
    scanmelt %>% mutate(broad_reas=ifelse(!is.na(ineli1),ineli1,broad_reas))->scanmelt
    scanmelt %>% group_by(masterdemoid) %>% arrange(spec_reas) %>%
      filter(!duplicated(broad_reas)) %>% ungroup->scanmelt
    #Get explore reason for not scanning
    scanmelt %>% mutate(EXPLORE=ifelse(!is.na(spec_reas),
                              paste(broad_reas,spec_reas,sep="-"),broad_reas))->scanmelt
    #If two reasons, combine
    scanmelt %>% arrange(masterdemoid)->scanmelt
    for(i in 2:nrow(scanmelt)-1){
      if(scanmelt[i+1,"masterdemoid"]==scanmelt[i,"masterdemoid"]){
        if(grepl("forever",scanmelt[i+1,"EXPLORE"])){scanmelt[i+1,"EXPLORE"]->scanmelt[i,"EXPLORE"]}
        else if(grepl("forever",scanmelt[i,"EXPLORE"])){scanmelt[i,"EXPLORE"]->scanmelt[i,"EXPLORE"]}
        else{scanmelt[i,"EXPLORE"]<-paste(scanmelt[i,"EXPLORE"],scanmelt[i+1,"EXPLORE"],sep=", ")}
      }
    }
    scanmelt[-which(duplicated(scanmelt$masterdemoid)),]->scanmelt
    scanmelt %>% select(masterdemoid,EXPLORE)->scans
    
    
    #Pi and snake dataframe
      #Snake
      tasks %>% filter(grepl("snake",redcap_event_name)) %>%
        select(masterdemoid,sgcomplete,snakeg_date)->snake
      if(any(duplicated(snake$masterdemoid))){
        print(snake[which(duplicated(snake$masterdemoid)),"masterdemoid"])
        message("Someone did snake twice?")}
      #Supplement
      tasks %>% filter(grepl("supplement",redcap_event_name)) %>%
      select(masterdemoid,starts_with("st_"),-contains(c("ni","nask","asku","na")))->sup
      sup %>% filter(st_complete==1)->sup
    #Put together
    merge(sup,snake, by="masterdemoid",all=T)->tasks
    
#Put tasks and scans together
    merge(tasks, scans, by="masterdemoid",all=T)->behav
    
#Match to new IDs
  merge(behav, p2, by="masterdemoid", all.y = T)->p2
  merge(behav, p2p3, by="masterdemoid", all.y = T)->p2p3
  merge(behav, p3, by="masterdemoid", all.y = T)->p3

#Add SSI
  merge(p2, SSI, by="masterdemoid",all.x=T)->p2
  merge(p2p3, SSI, by="masterdemoid",all.x=T)->p2p3
  merge(p3, SSI, by="masterdemoid",all.x=T)->p3
  
#Remove people consented in past 1 month (causes problems w/ missing info, 
  #no need for them as they won't get a followup)
  p2[which(p2$reg_condate_protect2<enddate-30),]->p2
  p2p3[which(p2p3$reg_condate_protect2<enddate-30),]->p2p3
  p3[which(p3$reg_condate_protect3<enddate-30),]->p3

#Check if anyone has group of unsure yet (88) as followup code below will not work
  if(any(p2$registration_group=="88") | any(p2p3$registration_group=="88") | any(p3$registration_group=="88")){
    if(length(which(p2$registration_group=="88")>0)){
      print(p2[which(p2$registration_group=="88"),"masterdemoid"])
    }
    if(length(which(p2p3$registration_group=="88")>0)){
      print(p2p3[which(p2p3$registration_group=="88"),"masterdemoid"])
    }
    if(length(which(p3$registration_group=="88")>0)){
      print(p3[which(p3$registration_group=="88"),"masterdemoid"])
    }
    stop("Someone has an unknown group, code will not work. Please go to redcap and enter group status")
  }
  
#Who has done which protocol
  games.complete<-function(df){
      #futurenp is a variable that states whether someone is eligible for future np tasks
    ymd(df$reg_condate_snake)->df$reg_condate_snake
    df %>% mutate(Snake=ifelse(!is.na(reg_condate_snake)| (!is.na(sgcomplete) & sgcomplete==1), 
          "DONE",ifelse(!is.na(registration_futurenp) & registration_futurenp!=0, 
          "INELIGIBLE-no more NP3 tasks allowed","NOT DONE")))->df
    df %>% mutate(Supplement=ifelse(rowSums(df[grepl("st_",names(df))],na.rm=T)==4, 
          "DONE",ifelse(!is.na(st_complete) & st_complete==1 & rowSums(df[grepl("task___",names(df))],na.rm=T)==0,
          "ERROR, marked complete but no tasks",ifelse(!is.na(st_complete) & st_complete==1 & rowSums(df[grepl("task___",names(df))],na.rm=T)<4,
          "Partially complete",ifelse(!is.na(registration_futurenp) & registration_futurenp!=0,
          "INELIGIBLE-no more NP3 tasks allowed","NOT DONE")))))->df
    }
  games.complete(p2)->p2
  games.complete(p2p3)->p2p3
  games.complete(p3)->p3
    
#Who needs a followup
  as.Date(p2$reg_condate_protect2)->p2$reg_condate_protect2
  as.Date(p2p3$reg_condate_protect2)->p2p3$reg_condate_protect2
  as.Date(p3$reg_condate_protect3)->p3$reg_condate_protect3
  
  fu_schedule<-function(df, x, threemo=T, IDE=T){
    as.data.frame(df)->df
    num.mo <- function(start_date, end_date=enddate) {
        ed <- as.POSIXlt(end_date)
        sd <- as.POSIXlt(start_date)
        12 * (ed$year - sd$year) + (ed$mon - sd$mon)}
    p3$registration_initials[num.mo(p3$reg_condate_protect3)/12==1]
    
    for (i in 1:nrow(df)){
    if(threemo){
       if(num.mo(df[i, paste0("reg_condate_protect",x)])==3){
         df[i,"This month"]<-"3 Month"}
        if(num.mo(df[i, paste0("reg_condate_protect",x)])==2){
         df[i,"Next month"]<-"3 Month"}}
    if(IDE==T & df[i,"registration_group"] %in% c("DNA","ATT") & 
                          ((is.na(df[i, "max2yr.score"]) |  df[i, "max2yr.score"]>=5) |
                          (!is.na(df[i,"attempt"]) & df[i,"attempt"]==TRUE))){
      if(num.mo(df[i, paste0("reg_condate_protect",x)]) %in% seq(6,600,12)){
        if(num.mo(df[i, paste0("reg_condate_protect",x)])==6){
          df[i,"This month"]<-"6 Months"
        }else{df[i,"This month"]<-paste0(num.mo(df[i, paste0("reg_condate_protect",x)])/12, " Years")}}
      if(num.mo(df[i, paste0("reg_condate_protect",x)]) %in% seq(5,600,12)){
        if(num.mo(df[i, paste0("reg_condate_protect",x)])==5){
          df[i,"Next month"]<-"6 Months"
        }else{df[i,"Next month"]<-paste0((num.mo(df[i, paste0("reg_condate_protect",x)])+1)/12, " Years")}}}
      
    if(num.mo(df[i, paste0("reg_condate_protect",x)]) %in% seq(12,600,12)){
      if(num.mo(df[i, paste0("reg_condate_protect",x)])/12==1){
        df[i,"This month"]<-"1 Year"
      }else{df[i,"This month"]<-paste0(num.mo(df[i, paste0("reg_condate_protect",x)])/12, " Years")}}
    if(num.mo(df[i, paste0("reg_condate_protect",x)]) %in% seq(11,600,12)){
      if((num.mo(df[i, paste0("reg_condate_protect",x)])+1)/12==1){
        df[i,"Next month"]<-"1 Year"
      }else{df[i,"Next month"]<-paste0((num.mo(df[i, paste0("reg_condate_protect",x)])+1)/12, " Years")}}
    }
    if(is.null(df$`This month`)){df$`This month`<-NA}
    if(is.null(df$`Next month`)){df$`Next month`<-NA}
      df[which(!is.na(df$`This month`) | !is.na(df$`Next month`)),]->df
      return(df)}
  as.data.frame(p2)->p2
  fu_schedule(p2, x=2, IDE=F)->p2
  #Protect 3's also in Protect 2 follow old protect 2 schedule of followups
  fu_schedule(p2p3, x=2, threemo = F)->p2p3
  fu_schedule(p3, x=3)->p3
  
#Add variable for which followup
  if(nrow(p2)>0){p2$followup<-"PROTECT2"}
  if(nrow(p2p3)>0){p2p3$followup<-"PROTECT3"}
  if(nrow(p3)>0){p3$followup<-"PROTECT3"}
  

#If p3 person is a catchup, remove their month 3
  if(any(p3$reg_p3catchup=="P2")){
    p3_catchup_mo3 <- p3 %>% filter(p3$reg_p3catchup == "P2" & p3$`Next month` == "3 Month")
    p3_catchup_mo32 <- p3 %>% filter(p3$reg_p3catchup == "P2" & p3$`This month` == "3 Month")
    ID_remove <- paste0(p3_catchup_mo3$masterdemoid, p3_catchup_mo32$masterdemoid, collapse = ",") 
    
    p3 <- p3 %>% filter(!masterdemoid %in% ID_remove)
    
    #p3[-which(p3$reg_p3catchup=="P2" &  ### original code
    #         (p3$`Next month`=="3 Month" | p3$`This month`=="3 Month")),]->p3_old   ### original code
    }

#Put all people together
  ymd(p3$reg_condate_protect2)->p3$reg_condate_protect2
  merge(merge(p2,p2p3,all=T),p3,all=T)->fus
  
  
#Get last visit date and assessor
  as.Date(map$date)->map$date
  map %>% group_by(masterdemoid) %>% filter(date==max(date))->map
  assessdf<-pro %>% select(masterdemoid, redcap_event_name,bq_assessor,fuq_assessor)
  merge(assessdf, map, by=c("masterdemoid","redcap_event_name"),all.y=T)->assessdf
  assessdf %>% mutate(assessor=ifelse(!is.na(fuq_assessor),fuq_assessor,bq_assessor))->assessdf
  assessdf$assessor[match(fus$masterdemoid,assessdf$masterdemoid)]->fus$assessor
  assessdf$date[match(fus$masterdemoid,assessdf$masterdemoid)]->fus$lastseen_date
  
  #Assessor numbers to replace
  as.character(fus$assessor)->fus$assessor
  assess_map<-data.frame(nums=c("99054","50987","50604","50330","50218","50824","50515",
                         "44444","88888","22222", "55555", "60910", "56789", "12345", "14545", "77777", "23456", "45678", "19191"), 
          ppl=c("LAURA","MANDY","MARIA","MICHELLE","JIAZHOU","NATE","MORGAN",
                             "EMILY","CORTNEE","LIZZIE","EMMA","KAYLEE", "AKIRA", "CHRISTIANNA","KATHRINE", "THANDI", "ASHLIE", "GEIXI", "BETH"))
  assess_map %>% mutate_all(~as.character(.))->assess_map
  for (i in 1:nrow(assess_map)){
    fus[which(fus$assessor %in% assess_map$nums[i]),"assessor"]<-assess_map$ppl[i]
  }

#Get df for this month
  thismonth.fu<-fus %>% select(ID=masterdemoid, Initials=registration_initials, Group=registration_group,
                               Age=age, Gender=registration_gender, Schedule=`This month`, Clinician=assessor, 
                               Protocol=followup,Scan=EXPLORE, SG= Snake, Supplement, attempt, max2yr.score)
  thismonth.fu[-which(is.na(thismonth.fu$Schedule)),]->thismonth.fu
  if(any(duplicated(thismonth.fu$ID))){
    print("Error, duplicated people (This month)")
    thismonth.fu[-which(duplicated(thismonth.fu)),]->thismonth.fu}
#Get df for next month
  nextmonth.fu<-fus %>% select(ID=masterdemoid, Initials=registration_initials, Group=registration_group,
                               Age=age, Gender=registration_gender, Schedule=`Next month`, Clinician=assessor, 
                               Protocol=followup,Scan=EXPLORE, SG= Snake, Supplement, attempt, max2yr.score)
  nextmonth.fu[-which(is.na(nextmonth.fu$Schedule)),]->nextmonth.fu
  if(any(duplicated(nextmonth.fu$ID))){
    print("Error, duplicated people (Next month)")
    nextmonth.fu[-which(duplicated(nextmonth.fu)),]->nextmonth.fu}

# Print monthly consent
  message("Monthly consents below (update: ", Sys.Date(), "): ")
  cat("This month consents: ")
  cat(paste0(P3_this_month$registration_initials, collapse = ", "))
  cat("\n")
  cat("Last month consents: ")
  cat(paste0(P3_last_month$registration_initials, collapse = ", "))
  cat("\n")  
 
#Put into "week.protect2.report" spreadsheet in working directory
write.csv(thismonth.fu,"weekly.protect3thismo.csv")
write.csv(nextmonth.fu,"weekly.protect3nextmo.csv")
}

