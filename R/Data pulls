bsrc.snakepull<-function(IDs=NULL){

#Libraries
 require(eeptools);require(tidyr);require(dplyr);require(lubridate);require(pkgcond)
#Setup
  md <- bsrc.checkdatabase2(ptcs$masterdemo, batch_size=500L, online=T)
    idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
    names(idmap)<-c("masterdemoid","wpicid","soloffid")
  pt <- bsrc.checkdatabase2(ptcs$protect, batch_size=500L, online=T)
    pt$data<-bsrc.findid(pt$data,idmap=idmap,"registration_redcapid")
    pt$data<-pt$data[which(pt$data$ifexist),]
  map <- bsrc.getIDDateMap(db=pt)
    map<-bsrc.findid(map,idmap=idmap,"registration_redcapid")
    map<-map %>% select(masterdemoid,redcap_event_name,date)
    
#IDs
  if(is.null(IDs)){
    message("using IDs of EVERYONE consented for Snake")
    IDs<-md$data[which(md$data$registration_ptcstat___snake==1),"registration_redcapid"]
  }


#IDmap id list
  ids<-data.frame(ID=IDs,newvar=T)
  idlist<-bsrc.findid(ids,idmap=idmap, "ID")
  #Check that all IDs exist
    if(any(!idlist$ifexist)){
      message("Some ids don't exist")
      print(idlist[which(idlist$ifexist),"ID"])
    }
  
#Get eligibility and demo
  demo <- md$data[which(md$data$registration_redcapid %in% idlist$masterdemoid),]
  #eligibility
  eligibility <- demo %>%
    select(ID=registration_redcapid, contains("ptcstat___pro"), contains("ptcstat___sui"),
           contains("excl_sui"), contains("excl_pro"),contains("excl_snake")) %>%
    filter(rowSums(.[ grepl("ptcs",names(.))], na.rm=T)>0,
           rowSums(.[ grepl("excl",names(.))], na.rm=T)==0)
  demo2 <- demo[which(demo$registration_redcapid %in% eligibility$ID),]
  demo2->df
  if(any(!demo$registration_redcapid %in% eligibility$ID)){
    message("This/these IDs were removed d/t ineligibility")
    print(demo[which(!demo$registration_redcapid %in% eligibility$ID),"registration_redcapid"])
  } 
  df<-df %>% select(registration_redcapid,registration_group,registration_dob,
                contains("registration_race"),registration_multirace,
                registration_hispanic,registration_edu, registration_gender)
  names(df)[1]<-"masterdemoid"
  #demo
    #Race out of checkbox
    df<-bsrc.checkbox(variablename = "registration_race", dfx = df)
    df$race<-sapply(df$registration_race,function(x){x[1]})
    df$race<-ifelse(df$registration_multirace==1,6,df$registration_race)
    unlist(df$race)->df$race
    #Change DEP and IDE to DNA
    #df[which(df$registration_group %in% c("IDE","DEP")),"registration_group"]<-"DNA"
    #Get age at snake game
      #Get snake game completion date
      sgdates<-data.frame(pt$data[c("masterdemoid","snakeg_date")])
      sgdates<-sgdates[!is.na(sgdates$snakeg_date) & sgdates$snakeg_date!="",]
      if(any(duplicated(sgdates$masterdemoid))){
        sgdates<-sgdates[-which(duplicated(sgdates$masterdemoid)),]
      }
      df<-merge(df,sgdates,by="masterdemoid",all.x=T)
      #Calculate age
      as.Date(df$registration_dob)->df$registration_dob
      as.Date(df$snakeg_date)->df$snakeg_date
      if(nrow(df[which(is.na(df$snakeg_date)),]>0)){
        message("These people are missing snake game completion dates, they will be removed for now so the code will work:")
        print(df[which(is.na(df$snakeg_date)),])
        df<-df[-which(is.na(df$snakeg_date)),]
      }
      df$sg_age<-age_calc(df$registration_dob, enddate=df$snakeg_date, 
               units="years",precise=F)
      #Get final variables
      df<-df %>% select(masterdemoid,registration_group,registration_gender,
           registration_edu,race,sg_age,snakeg_date,registration_dob)
#Suicide history
  if(any(df$registration_group=="ATT")){
  #Get dates and lethalities
  suihx <- demo2[grepl("registration_redcapid|lr|sadate",names(demo2))]
  sahxnew <- reshape(suihx, direction='long', 
                           varying=names(suihx)[-1], 
                           timevar='att',
                           times=1:30,
                           v.names=c("lethality","date"),
                           idvar='registration_redcapid')
  suppress_warnings(
    sahxnew<-sahxnew %>% group_by(registration_redcapid) %>% mutate(maxleth=max(lethality, na.rm=T)))
  sahxnew[which(sahxnew$maxleth==-Inf),c("maxleth")]<-NA
  ymd(sahxnew$date)->sahxnew$date
  #Only dates BEFORE snake game was completed
  sahxnew$snakeg_date<-df$snakeg_date[match(sahxnew$registration_redcapid, df$masterdemoid)]
  sahxnew<-sahxnew[which(sahxnew$date<sahxnew$snakeg_date),]
  #Date of first and last attempt
  sahxnew<-sahxnew %>% group_by(registration_redcapid) %>% mutate(
      mindate=min(date, na.rm=T),maxdate=max(date, na.rm=T))
  #Get number of attempts
  sahxnew<-sahxnew %>% group_by(registration_redcapid) %>% filter(!is.na(date) | !is.na(lethality)) %>%
    add_count()
  names(sahxnew)[names(sahxnew)=="n"]<-"num_atts"
  #Get max lethality and date of max lethality
  newdf<-sahxnew %>% group_by(registration_redcapid) %>% filter(maxleth==lethality)
  newdf<-newdf[order(newdf$date),]
  newdf<-newdf[-which(duplicated(newdf$registration_redcapid)),]
  sahxnew$ml_date<-newdf$date[match(sahxnew$registration_redcapid, newdf$registration_redcapid)]
  #Get all variables per 1 ID, 
  sahxnew2<-sahxnew %>% group_by(registration_redcapid) %>% filter(row_number()==1)
  sahxnew2<-sahxnew2 %>% select(masterdemoid=registration_redcapid,maxleth,mindate,ml_date, maxdate,num_atts)
  }else{
    sahxnew2<-data.frame(masterdemoid=df$masterdemoid,maxleth=NA,mindate=NA, ml_date=NA, maxdate=NA, num_atts=NA)
    }
  #Merge into main dataframe
  df<-merge(df,sahxnew2,by="masterdemoid",all.x=T)
  df<-df %>% mutate(age_lastatt=ifelse(!is.na(df$maxdate), age_calc(registration_dob, snakeg_date, units="years",precise=F),NA))
  #Keep copy for SIS
  sahxnew2->suihx

#Only ids we want for the MAP
  map<-map[which(map$masterdemoid %in% idlist$masterdemoid),]     
  
#SIS
  sis<-pt$data %>% select(masterdemoid, redcap_event_name, contains("sis_date"), contains("sis_recent"), contains("sis_max"),
           -contains("planning_s"), -contains("total_s"), -contains("date"))
  sis<-merge(map, sis, by=c("masterdemoid","redcap_event_name"))
  sis<-sis[which(sis$masterdemoid %in% df$masterdemoid),]
  #Remove NA rows
  sis<-sis[-which(rowSums(is.na(sis[grepl("sis_recent|sis_max",names(sis))]) | 
                       sis[grepl("sis_recent|sis_max",names(sis))]=="")>=38),]
  #Get closest to most lethal date
  sis$ml_date<-suihx$ml_date[match(sis$masterdemoid, suihx$masterdemoid)]
  sis$date_dif<-abs(as.Date(sis$ml_date)-as.Date(sis$date))
  sis<-sis %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif)) %>% ungroup()
  sis<-bsrc.score(df=sis, formname = "sis")
  if(any(duplicated(sis$masterdemoid))){message("Someone is duplicated on SIS")}
  #If max or recent is missing, fill in with the other
  sis2<-sis %>% select(masterdemoid, max_tot=sis_max_total, max_plan=sis_max_plan, recent_tot=sis_recent_total, recent_plan=sis_recent_plan)
  sis2<-sis2 %>% mutate(max_tot=ifelse(is.na(max_tot), recent_tot,max_tot))
  sis2<-sis2 %>% mutate(max_plan=ifelse(is.na(max_plan), recent_plan,max_plan))
  sis2<-sis2 %>% select(masterdemoid, max_tot, max_plan)
  df<-merge(df, sis2, by="masterdemoid",all.x=T)
  if(any(is.na(df$max_tot) & df$registration_group=="ATT")){
    message("Some people are missing SIS but are ATTs, look into them")
    print(df[which(is.na(df$max_tot) & df$registration_group=="ATT"),])
  }

#Income
  income<-pt$data %>% select(masterdemoid, income=macarthur_6) %>% filter(!is.na(income))
  if (any(duplicated(income$masterdemoid))) {
    message("Some people have duplicated income rows... removing...")
    income %>% filter(!duplicated(.)) -> income}
  df<-merge(df,income,by="masterdemoid",all.x=T)
  
#Get HAM
    #1. Grab HAM from protect
      ham<-pt$data %>% select(masterdemoid, redcap_event_name, starts_with("ham"))
    #2. Remove vars we don't want
      ham<-ham %>% select(-contains("ham_done"), -contains("total"),-ham_and_bprs_miss,-ham_complete,-ham_3a_wl,
                            -ham_3b_wd,-ham_3c_rld,-ham_3d_asa,-ham_3e_pdw,-ham_18_q)
    #3. Score
      suppress_warnings(ham<-bsrc.score(df=ham, formname="ham"))
    #3. Remove data with all NAs
      ham<-ham[which(rowSums(is.na(ham[paste0("ham_",c(1:17))]))<17),]
    #4. IDs we want
      ham<-ham[which(ham$masterdemoid %in% idlist$masterdemoid),]
    #5. Get date of snake game
      ham<-ham[which(ham$masterdemoid %in% df$masterdemoid),]
      ham$sg_date<-df$snakeg_date[match(ham$masterdemoid, df$masterdemoid)]
    #6. Find closest to snake game
      as.Date(ham$sg_date)->ham$sg_date
      ymd(ham$ham_date)->ham$ham_date
      ham$datedif<-abs(ham$sg_date-ham$ham_date)
      if(any(is.na(ham$ham_date))){
        message("Someone is missing a Ham date")
        print(ham[which(is.na(ham$ham_date)),])
      }
      ham %>% group_by(masterdemoid) %>% filter(datedif==min(datedif,na.rm = T)) %>% ungroup()->ham
      #These duplicates are on the same date, can remove
      if(any(duplicated((ham[-c(2)])))){ham<-ham[-which(duplicated(ham[-c(2)])),]}
      names(ham)[names(ham)=="datedif"]<-"ham_datedif"
      table(ham$ham_datedif)
    #10. Merge into main df
      ham<-ham %>% select(masterdemoid, ham_sui=ham_17_sui,ham_nosui=ham_17_nosui, ham_datedif)
      df<-merge(df, ham, by="masterdemoid", all.x=T)

#SSI
    #1. Get data
      ssi<-pt$data %>% select(masterdemoid, redcap_event_name, paste0("ssi_",c(1:19),"_worst"),
                              paste0("ssi_",c(1:19),"_curr"))
    #2. Fix characters and remove all NAs
      ssi %>% mutate_all(~as.character(.))->ssi
      ssi[is.na(ssi) | ssi=="" | ssi=="na" | ssi=="dk" | ssi=="refuse"]<-0
      #Change to numeric
      ssi[paste("ssi",c(1:19),"worst", sep="_")]<-lapply(ssi[paste("ssi",c(1:19),"worst", sep="_")],function(x) as.numeric(x))
      ssi[paste("ssi",c(1:19),"curr", sep="_")]<-lapply(ssi[paste("ssi",c(1:19),"curr", sep="_")],function(x) as.numeric(x))
      #Remove all is na
      ssi<-ssi[which(rowSums(is.na(ssi[-c(1:2)]))<38),]
    #3. event map
      ssi<-merge(map, ssi, by=c("masterdemoid","redcap_event_name"), all.y=T)
    #4. Get date of snake game
      ssi<-ssi[which(ssi$masterdemoid %in% df$masterdemoid),]
      df$snakeg_date[match(ssi$masterdemoid, df$masterdemoid)]->ssi$sg_date
    #5. Sum SSI
     rowSums(ssi[paste("ssi",c(1:19),"curr", sep="_")])->ssi$ssi_curr
     rowSums(ssi[paste("ssi",c(1:19),"worst", sep="_")])->ssi$ssi_worst
     pmax(ssi$ssi_curr,ssi$ssi_worst, na.rm=T)->ssi$ssi_top
     ssi %>% group_by(masterdemoid) %>% mutate(ssi_max=max(ssi_top))->ssi
    #7. Date differences
      as.Date(ssi$sg_date)->ssi$sg_date
      as.Date(ssi$date)->ssi$date
      ssi$datedif<-abs(ssi$sg_date-ssi$date)
      ssi %>% group_by(masterdemoid) %>% filter(datedif==min(datedif,na.rm=T))->ssi
      table(ssi$datedif)
      any(duplicated(ssi$masterdemoid))
      ssi<-ssi[-which(duplicated(ssi$masterdemoid)),]
    #8. Remove unnecessary vars
      ssi %>% select(masterdemoid, ssi_curr,ssi_max, ssi_datedif=datedif)->ssi
    #9. Merge into main df
      merge(df, ssi, by="masterdemoid",all.x=T)->df
      sum(df$ssi_datedif>30)
  
#SIDP
    #1. Get SIDP
      sidpdf<- pt$data %>% select(masterdemoid,redcap_event_name, starts_with("sidp"), -contains("unable"))
    #2. Event map data for sidp (only given at BL, not catchup BL)
      sidpdf<-sidpdf[grepl("baseline",sidpdf$redcap_event_name) & ! grepl("catchup", sidpdf$redcap_event_name),]
    #3. Score sidp
      sidpdf<-bsrc.score(df=sidpdf,formname = "sidp")
    #4. Remove NAs
      sidpdf<-sidpdf[which(rowSums(is.na(sidpdf[grepl("sxs|presence",names(sidpdf))]))<12),]
      if(any(duplicated(sidpdf$masterdemoid))){stop("fix code, someone has 2 sidp's")}
    #5. Only scores we want
      sidpdf2<-sidpdf %>% select(masterdemoid,contains("sxs"),contains("presence"))
      names(sidpdf2)<-c("masterdemoid","ASPD_sxs","AVPD_sxs","OCPD_sxs","BPD_sxs","NPD_sxs","SZPD_sxs",
                        "ASPD_presence","AVPD_presence","OCPD_presence","BPD_presence","NPD_presence",
                        "SZPD_presence")
    #5. Merge into main df
        sidpdf2<-sidpdf2[which(sidpdf2$masterdemoid %in% df$masterdemoid),]
        merge(df, sidpdf2, by="masterdemoid", all.x=T)->df

#NARQ
    #1. Get NARQ
    narq<-pt$data %>% select(masterdemoid,paste0("narq_brief_",c(1:6)))
    #Remove if all are NA
    narq<-narq[which(rowSums(is.na(narq[paste0("narq_brief_",c(1:6))]))<6),]
    #3. Check if any duplicated IDs
    if(any(duplicated(narq$masterdemoid))){
      stop("duplicates of narq, must change code")
    }
    #4. Merge into main df
    merge(df, narq, by="masterdemoid", all.x=T)->df      

#PAI-BOR
    #1. Get PAI-BOR
    paibor<-pt$data %>% select(masterdemoid,redcap_event_name,paste0("paibor_",c(1:24)))
    #2. Remove if all NA
    paibor<-paibor[which(rowSums(is.na(paibor[paste0("paibor_",c(1:24))]))<24),]
    #3. Check for dups
    any(duplicated(paibor$masterdemoid))
    paibor<-merge(paibor, map, by=c("masterdemoid","redcap_event_name"), all.x=T)
    paibor$sg_date<-df$snakeg_date[match(paibor$masterdemoid, df$masterdemoid)]
    as.Date(paibor$sg_date)->paibor$sg_date
    as.Date(paibor$date)->paibor$date
    paibor$date_dif<-abs(paibor$sg_date-paibor$date)
    paibor<-paibor %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif))
    any(duplicated(paibor$masterdemoid))
    #4. scoring
    paibor<-bsrc.score(paibor,formname="paibor")
    paibor<-paibor %>% select(masterdemoid, paibor_AI=paibor_affective_instability, paibor_SH=paibor_selfharm,
                      paibor_NR=paibor_neg_relation,paibor_total)
      #5. Merge into main df
        merge(df, paibor, by="masterdemoid", all.x=T)->df
        
#FFNI
  #1. Get FFNI
  ffnidf<-pt$data %>% select(masterdemoid, redcap_event_name,paste0("ffni_",c(1:60)))
  #2. Remove if all NA
  ffnidf<-ffnidf[which(rowSums(is.na(ffnidf[-c(1,2)]))<60),]
  #3. Check for dups (remove baseline dups vs. snake game)
  any(duplicated(ffnidf$masterdemoid))
  ffnidf<-merge(map, ffnidf, all.y=T, by=c("masterdemoid","redcap_event_name"))
      df$snakeg_date[match(ffnidf$masterdemoid, df$masterdemoid)]->ffnidf$sg_date
      as.Date(ffnidf$sg_date)->ffnidf$sg_date
      as.Date(ffnidf$date)->ffnidf$date
      ffnidf$date_dif<-abs(ffnidf$sg_date-ffnidf$date)
      ffnidf %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif))->ffnidf
      any(duplicated(ffnidf$masterdemoid))
  #4. Merge into main df
      ffnidf<-ffnidf %>% select(-sg_date,-date_dif,-date,-redcap_event_name)
      df<-merge(df, ffnidf, by="masterdemoid", all.x=T)
      
#BPNI
  #1. Get BPNI
  bpnidf<-pt$data %>% select(masterdemoid, redcap_event_name, paste0("bpni_",c(1:28)))
  #2. Remove if all NA
  bpnidf<-bpnidf[which(rowSums(is.na(bpnidf[-c(1,2)]))<28),]
  #3. Check for dups (remove baseline dups vs. snake game)
      any(duplicated(bpnidf$masterdemoid))  
      bpnidf<-merge(map, bpnidf, all.y=T, by=c("masterdemoid","redcap_event_name"))
      df$snakeg_date[match(bpnidf$masterdemoid, df$masterdemoid)]->bpnidf$sg_date
      as.Date(bpnidf$sg_date)->bpnidf$sg_date
      as.Date(bpnidf$date)->bpnidf$date
      bpnidf$date_dif<-abs(bpnidf$sg_date-bpnidf$date)
      bpnidf<-bpnidf %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif))
      any(duplicated(bpnidf$masterdemoid))
  #4. Merge into main df
  bpnidf<-bpnidf %>% select(-redcap_event_name,-date,-sg_date,-date_dif)
  df<-merge(df, bpnidf, by="masterdemoid", all.x=T)
  
#Lifetime subs & anxiety
  #1. Gather data for scid (# of anx and substance use d/os)
      sciddf<-pt$data %>% select(masterdemoid,redcap_event_name,starts_with(paste0("scid_",c(17:37))))
      #2. Event map data for scid (only given at BL, not catchup BL)
      sciddf<-sciddf[grepl("baseline",sciddf$redcap_event_name) & !grepl("catchup", sciddf$redcap_event_name),]
      #3. Remove NA's (if all values are NA)
      sciddf<-sciddf[rowSums(is.na(sciddf) | sciddf=="" | sciddf=="NA")<78,]
      #4. Check if any duplicated IDs
        #Remove if the full row is duplicated (same data) besides event name
        any(duplicated(sciddf$masterdemoid))
      #5. Scoring
        sciddf<-bsrc.score(sciddf,formname="scid")
    #6. Merge into main df
        sciddf<-sciddf %>% select(masterdemoid, contains("LP"), contains("PM"))
        df<-merge(df, sciddf, by="masterdemoid", all.x=T)
#NEO
  #1. Get NEO
  neo<-pt$data %>% select(masterdemoid, redcap_event_name, paste0("neoffi_",c(1:60)))
  #2. Remove if all NA
  neo<-neo[which(rowSums(is.na(neo[-c(1,2)]))<60),]
  #3. Check for dups (remove baseline dups vs. snake game)
  any(duplicated(neo$masterdemoid))
      neo<-merge(map, neo, all.y=T, by=c("masterdemoid","redcap_event_name"))
      df$snakeg_date[match(neo$masterdemoid, df$masterdemoid)]->neo$sg_date
      as.Date(neo$sg_date)->neo$sg_date
      as.Date(neo$date)->neo$date
      neo$date_dif<-abs(neo$sg_date-neo$date)
      neo %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif))->neo
      any(duplicated(neo$masterdemoid))
  #4. Merge into main df
      neo<-neo %>% select(-redcap_event_name,-date,-sg_date,-date_dif)
      df<-merge(df, neo,by="masterdemoid", all.x=T)
      
###IPIP-DS
  #1. Get IPIP
  ipipdf<-pt$data %>% select(masterdemoid, redcap_event_name, paste0("ipipds_",c(1:11)))
  #2. Remove if all NA
  ipipdf<-ipipdf[which(rowSums(is.na(ipipdf[-c(1,2)]))<11),]
  #3. Check for dups (remove baseline dups vs. snake game)
  any(duplicated(ipipdf$masterdemoid))
  ipipdf<-merge(map, ipipdf, all.y=T, by=c("masterdemoid","redcap_event_name"))
      df$snakeg_date[match(ipipdf$masterdemoid, df$masterdemoid)]->ipipdf$sg_date
      as.Date(ipipdf$sg_date)->ipipdf$sg_date
      as.Date(ipipdf$date)->ipipdf$date
      ipipdf$date_dif<-abs(ipipdf$sg_date-ipipdf$date)
      ipipdf %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif))->ipipdf
  any(duplicated(ipipdf$masterdemoid))
  #4. Merge into main df
  ipipdf<-ipipdf %>% select(-redcap_event_name,-date,-sg_date,-date_dif)
  merge(df, ipipdf, by="masterdemoid", all.x=T)->df
  

#Final problem fixing
  as.data.frame(df)->df
  return(df)
}
