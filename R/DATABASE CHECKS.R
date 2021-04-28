bsrc.behav_task_check<-function(boxpath=paste("/Users/",Sys.getenv("USER"),"/Box/skinner/data/Behavioral",sep=""),
                                online=T, task=c("snake","supplement"), md=md, pt=pt, idmap=idmap){
  #Libraries
    library(lubridate); library(pkgcond)
  
  #Setup
   
    
    #Snake Game
      #Consents
      snake_consent_df<-md$data %>% 
        select(registration_redcapid, registration_ptcstat___snake,reg_condate_snake) %>% 
        filter(registration_ptcstat___snake==1) %>% mutate_at(vars(reg_condate_snake),ymd)
      if(any(is.na(snake_consent_df$reg_condate_snake))){
        message("This person/these people NEED snake consent dates")
        print(snake_consent_df[which(is.na(snake_consent_df$reg_condate_snake)),])}
      snake_consent<-snake_consent_df$registration_redcapid
      
      #Behavioral data files
          #Function to find snake files (in skinner or PROTECT Box)
          find_snake_files<-function(whichbox="skinner"){
            path<-paste0(boxpath, "/Snake")
            if(whichbox=="PROTECT"){
              path<-gsub("skinner","PROTECT Box",path)
            }
            snake_files<-list.files(path)
            snake_files<-gsub(".csv|_needmod","",snake_files)
            snake_files<-snake_files[!snake_files %in% c("ID mismatches.rtf","Duplicate Snake")]
            #File ID doesn't match ACTUAL ID
            c("215232","222165","211274")->snake_files[snake_files %in% c("042318","11019","221274")]
            snake_files_df<-data.frame(ID=snake_files,extravar=T)
            snake_files_df<-bsrc.findid(snake_files_df,idmap=idmap, id.var = "ID")
            if(any(snake_files_df$ifexist==FALSE)){
              message("This person/these people's IDs don't match the master demo, check them out, 
                      may need to add to ID mismatches")
            print(snake_files_df[snake_files_df$ifexist==FALSE])}
            snake_files_new<-snake_files_df$masterdemoid
            return(snake_files_new)
          }
        
        snake_skinner<-find_snake_files()
        snake_protectbox<-find_snake_files(whichbox="PROTECT")
        
        #Check if any are in Protect box but not in skinner
        if(length(snake_protectbox[!snake_protectbox %in% snake_skinner]>0)){
          message("These people are in protect box but not in skinner, please copy over!")
          print(snake_protectbox[!snake_protectbox %in% snake_skinner])
        }
    
      #Redcap "marked as completed"
        snake_protect_df<-pt$data %>% select(registration_redcapid, sgcomplete,snakeg_date) %>%
          filter(sgcomplete==1)
        snake_protect_df<-bsrc.findid(snake_protect_df, idmap=idmap, "registration_redcapid")
        suppress_warnings(
          snake_protect_df<-snake_protect_df[-which(is.na(as.numeric(snake_protect_df$registration_redcapid))),])
        if(any(!snake_protect_df$ifexist)){
          message("This person/these people do not have a masterdemoid")
          print(snake_protect_df[which(!snake_protect_df$ifexist),])
        }
        #Check if there are any other duplicates other than the one we know about 221715
        if(any(duplicated(snake_protect_df[which(!snake_protect_df$masterdemoid %in% c("221715")),"masterdemoid"]))){
          message("Double check this person/these people, it looks like they did snake twice:")
          print(snake_protect_df[which(duplicated(
            snake_protect_df[which(!snake_protect_df$masterdemoid %in% c("221715")),"masterdemoid"])),"masterdemoid"])
        }
        #Get list of people completed in protect redcap
        snake_protect<-snake_protect_df$masterdemoid
    
    #Supplement
        #Consents
      sup_consent_df<-md$data %>% 
        select(registration_redcapid, registration_ptcstat___supp,reg_condate_supp) %>% 
        filter(registration_ptcstat___supp==1) %>% mutate_at(vars(reg_condate_supp),ymd)
      if(any(is.na(sup_consent_df$reg_condate_supp))){
        message("This person/these people NEED supplement consent dates")
        print(sup_consent_df[which(is.na(sup_consent_df$reg_condate_supp)),])}
      sup_consent<-sup_consent_df$registration_redcapid
        
      #Behavioral files
        find_sup_files<-function(task=NULL){
            if(task=="Grid"){
              path<-paste0(boxpath, "/Grid_Supp")
            }
            if(task=="Helicopter"){
              path<-paste0(boxpath, "/Helicopter_Supp")
            }
            if(task=="Twostep"){
              path<-paste0(boxpath, "/Twostep_Supp")
            }
            sup_files<-list.files(path)
            sup_files<-gsub("gridtask|helicopter|twostep|_|-","",sup_files)
            #combined files are Vanessa's data, not id-wise data
            sup_files<-sup_files[-which(grepl("combine|test",sup_files))]
            sup_file_df<-data.frame(ID=substr(sup_files, 1, 6),date=substr(sup_files, 7, 14))
            ymd(sup_file_df$date)->sup_file_df$date
            sup_file_df<-bsrc.findid(sup_file_df,idmap = idmap, id.var="ID")
             if(any(sup_file_df$ifexist==FALSE)){
              message(paste0("This person/these people's IDs don't match the master demo for ",task,", task. 
              Check them out- may need to add to ID mismatches"))
               print(sup_file_df[which(sup_file_df$ifexist==FALSE),])}
            sup_file_new<-sup_file_df$masterdemoid
            return(sup_file_new)
        }
        sup_grid_behav<-find_sup_files(task="Grid")
        sup_heli_behav<-find_sup_files(task="Helicopter")
        sup_twostep_behav<-find_sup_files(task="Twostep")
      
      #Redcap "marked as completed"
        sup_protect_df<-pt$data %>% select(registration_redcapid, st_complete,st_date, paste0("st_whichtask___",c(1:3))) %>%
          filter(st_complete==1)
        sup_protect_df<-bsrc.findid(sup_protect_df, idmap=idmap, "registration_redcapid")
        if(any(is.na(as.numeric(sup_protect_df$registration_redcapid)))){
          suppress_warnings(
          sup_protect_df<-sup_protect_df[-which(is.na(as.numeric(sup_protect_df$registration_redcapid))),])}
        if(any(!snake_protect_df$ifexist)){
          message("This person/these people do not have a masterdemoid")
          print(sup_protect_df[which(!sup_protect_df$ifexist),])
        }  
        grid_protect<-sup_protect_df[which(sup_protect_df$st_whichtask___1==1),"masterdemoid"]
        heli_protect<-sup_protect_df[which(sup_protect_df$st_whichtask___2==1),"masterdemoid"]
        twostep_protect<-sup_protect_df[which(sup_protect_df$st_whichtask___3==1),"masterdemoid"]
       
      #Check behavioral versus consents versus protect
        check_3_places<-function(consent=NULL, behav_data=NULL, protect_data=NULL,task=NULL){
          ids<-unique(c(consent,behav_data,protect_data))
          con<-ids[!ids %in% consent]
          if(length(con>0)){
            message(paste0("This person/these people are not consented for ",task))
            print(con)
          }
          behav<-ids[!ids %in% behav_data]
          if(length(behav)>0){
            message(paste0("This person/these people have no behavioral data for ",task))
            print(behav)
          }
          pro<-ids[!ids %in% protect_data]
          if(length(pro)>0){
            message(paste0("This person/these people have not been updated in protect as completing ",task))
            print(pro)}
          if(length(con)==0 & length(behav)==0 & length(pro)==0){
          message(paste0("No issues with ",task,", everyone matches up"))
        }
        }
        
          
      check_3_places(consent=snake_consent,behav_data=snake_skinner,protect_data=snake_protect, task="Snake")
      check_3_places(consent=sup_consent,behav_data=sup_grid_behav,protect_data=grid_protect, task="Grid")
      check_3_places(consent=sup_consent,behav_data=sup_heli_behav,protect_data=heli_protect, task="Helicopter")
      check_3_places(consent=sup_consent,behav_data=sup_twostep_behav,protect_data=twostep_protect, task="Two-Step")
}   
