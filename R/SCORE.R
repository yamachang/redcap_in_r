#Protect Scoring
bsrc.score<-function(df=NULL,formname=NULL,...){
  library(dplyr)
  possible_forms<-c("athf","ham","cirsg","sidp","exit","drs","wtar","mmse",
                    "bis","ctq","isel","iip","neo","paibor","spsi","ssd","uppsp","fs", "let", 
                    "swls","maas","ah","bsia","cfcs", "ders", "iri", "nfc", "rand12", "bpni")
  if(is.null(formname)){
    message("No form name supplied, choose one of these options:")
    print(possible_forms)
    stop()
  }else if(tolower(formname)=="wtar"){
    stop("WTAR is already scored (see wtar_s_adj)")
  }else if(tolower(formname)=="athf"){
    stop("ATHF is already scored (see athf_maxnum)")
  }else if(tolower(formname)=="mmse"){
    stop("MMSE is already scored (see mmse_s_adj)")}
  argu <- list(...)
  argu$df = df
  score_func<-get(paste0("score.",tolower(formname)),envir = loadNamespace("bsrc"))
  return(do.call(score_func,argu))
}

#Clinical
#SSI scoring
score.ssi<-function(df=NULL){
  #Remove those that are ONLY missing
  df<-df[which(rowSums(is.na(df[c(paste0("ssi_",c(1:19),"_worst"),paste("ssi_",1:19, "_curr", sep=""))]) | 
                         df[c(paste0("ssi_",c(1:19),"_worst"),paste("ssi_",1:19, "_curr", sep=""))]=="")<38),]
  #Change values to NAs/0s
  suppressWarnings(df <- df %>% 
                     mutate_at(vars(paste0("ssi_",c(6:19),"_worst"),paste("ssi_",6:19, "_curr", sep="")),~replace(.,.=="na"|.=="",0)) %>%
                     mutate_at(vars(paste0("ssi_",c(6:19),"_worst"),paste("ssi_",6:19, "_curr", sep="")),~replace(.,.=="dk"|.=="refuse",NA)) %>%
                     mutate_at(vars(paste0("ssi_",c(1:5),"_worst"),paste("ssi_",1:5, "_curr", sep="")),~replace(.,.=="na"|.=="dk"|.=="refuse",NA)) %>% 
                     mutate_at(vars(paste0("ssi_",c(1:19),"_worst"),paste("ssi_",1:19, "_curr", sep="")), ~as.integer(.)))
  #Score
  df <- df %>% 
    mutate(
      SSI_worst_s = ifelse(rowSums(is.na(df[paste0("ssi_",c(1:19),"_worst")]))==0,
                           rowSums(df[paste0("ssi_",c(1:19),"_worst")]), 
                           ifelse(rowSums(is.na(df[paste0("ssi_",c(1:19),"_worst")])) == 1, 
                                  round(rowSums(df[paste0("ssi_",c(1:19),"_worst")], na.rm = T)*19/18, digits = 0), NA)),
      SSI_current_s = ifelse(rowSums(is.na(df[paste0("ssi_",c(1:19),"_curr")]))==0,
                             rowSums(df[paste0("ssi_",c(1:19),"_curr")]), 
                             ifelse(rowSums(is.na(df[paste0("ssi_",c(1:19),"_curr")])) == 1, 
                                    round(rowSums(df[paste0("ssi_",c(1:19),"_curr")], na.rm = T)*19/18, digits = 0), NA))
    )
  return(df)
}

#SIS scoring
score.sis<-function(df=NULL){
  #Need to be numeric to score, change to numeric
  df<-df %>% mutate_at(vars(paste0("sis_max_",c(1:18)),paste0("sis_recent_",c(1:18))),as.numeric)
  #Reverse score Q8
  df<-df %>% mutate(
    sis_max_8r=2-sis_max_8, sis_recent_8r=2-sis_recent_8
  )
  #Score
  df<-df %>% mutate(
    sis_max_plan=ifelse(rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r'))]))==0,
                        rowSums(df[paste0("sis_max_",c(1:7,'8r'))]),NA),
    sis_recent_plan=ifelse(rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r'))]))==0,
                           rowSums(df[paste0("sis_recent_",c(1:7,'8r'))]),NA),
    sis_max_total=ifelse(rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r',9:15))]))==0,
                         rowSums(df[paste0("sis_max_",c(1:7,'8r',9:15))]),ifelse(
                           rowSums(is.na(df[paste0("sis_max_",c(1:7,'8r',9:15))]))==1,
                           round(rowSums(df[paste0("sis_max_",c(1:7,'8r',9:15))],na.rm=T)*15/14),NA)),
    sis_recent_total=ifelse(rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r',9:15))]))==0,
                            rowSums(df[paste0("sis_recent_",c(1:7,'8r',9:15))]),ifelse(
                              rowSums(is.na(df[paste0("sis_recent_",c(1:7,'8r',9:15))]))==1,
                              round(rowSums(df[paste0("sis_recent_",c(1:7,'8r',9:15))],na.rm=T)*15/14),NA))
  )
  return(df)
}

#HAM scoring
score.ham<-function(df=NULL){
  #Change names to ham_1 through ham_24
  paste0("ham_",1:24)->names(df)[names(df) %in% c("ham_1_dm","ham_2_gf","ham_3_su","ham_4_ii","ham_5_im","ham_6_di","ham_7_wi","ham_8_re",
                                                  "ham_9_ag","ham_10_psya","ham_11_soma","ham_12_gi","ham_12_gi","ham_13_gs","ham_14_sex","ham_15_hd","ham_16_li",
                                                  "ham_17_weight","ham_18_rt","ham_19_dp","ham_20_prsx","ham_21_ocsx","ham_22_xhelp","ham_23_xhope","ham_24_xworth")]
  #Need to be numeric to score, change to numeric
  df<-df %>% mutate_at(vars(paste0("ham_",c(1:24))),as.numeric)
  #Score
  df<-df %>% mutate(
    ham_17_sui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:17))]))==0,
                      rowSums(df[paste0("ham_",c(1:17))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:17))]))==1,
                        round(rowSums(df[paste0("ham_",c(1:17))],na.rm=T)*17/16),NA)),
    ham_17_nosui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:2,4:17))]))==0,
                        rowSums(df[paste0("ham_",c(1:2,4:17))]),ifelse(
                          rowSums(is.na(df[paste0("ham_",c(1:2,4:17))]))==1,
                          round(rowSums(df[paste0("ham_",c(1:2,4:17))],na.rm=T)*16/15),NA)),
    ham_24_sui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:24))]))==0,
                      rowSums(df[paste0("ham_",c(1:24))]),ifelse(
                        rowSums(is.na(df[paste0("ham_",c(1:24))]))==1,
                        round(rowSums(df[paste0("ham_",c(1:24))],na.rm=T)*24/23),ifelse(
                          rowSums(is.na(df[paste0("ham_",c(1:24))]))==2,
                          round(rowSums(df[paste0("ham_",c(1:24))],na.rm=T)*24/22),NA))),
    ham_24_nosui=ifelse(rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==0,
                        rowSums(df[paste0("ham_",c(1:2,4:24))]),ifelse(
                          rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==1,
                          round(rowSums(df[paste0("ham_",c(1:2,4:24))],na.rm=T)*24/23),ifelse(
                            rowSums(is.na(df[paste0("ham_",c(1:2,4:24))]))==2,
                            round(rowSums(df[paste0("ham_",c(1:2,4:24))],na.rm=T)*24/22),NA)))
  )
  return(df)
}

#CIRS-G scoring
score.cirsg<-function(df=NULL){
  #Need to be numeric to score, change to numeric
  df<-df %>% mutate_at(vars(paste("cirsg_",c(1:13),"_s",sep="")),as.numeric)
  #Score
  df<-df %>% mutate(
    cirs_total=ifelse(rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==0,
                      rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")]),ifelse(
                        rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==1,
                        round(rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")],na.rm=T)*13/12),NA)),
    cirs_3or4=ifelse(rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==0,
                     rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")]>2),ifelse(
                       rowSums(is.na(df[paste("cirsg_",c(1:13),"_s",sep="")]))==1,
                       round(rowSums(df[paste("cirsg_",c(1:13),"_s",sep="")]>2,na.rm=T)*13/12),NA))
  )
  return(df)
}

#SCID "scoring"
score.scid<-function(df=NULL){
  #Must not have ALL missingness because we will change values to 1 
  if(!any(is.na(df %>% select(contains(paste0("scid_",c(17:37))))))){
    stop("You MUST first remove rows with all missing values for the scid scoring to work")
  }
  #Change NA values to 1 in key variables
  df <- df %>%
    mutate_at(vars(c(paste("scid_",17:23, "_s", seq(115, 127, 2), sep=""),
                     paste("scid_",24:37,"_s", c(129,139,141,147,151,157,161,165,
                                                 171,173,177,183,189,191),sep=""))), ~ifelse(is.na(.), 1, .))
  #Score number of disorders
  df <- df  %>% mutate(
    LP_numsubs=rowSums(df[grepl(paste0("_s", c(138,seq(114,128,by=2)), collapse="|"),names(df))]>1),
    PM_numsubs=rowSums(df[grepl(paste0("_s", c(seq(115,127,by=2),129,129), collapse="|"),names(df))]>1),
    LP_numanx=rowSums(df[grepl(paste0("_s", c(140,146,150,156,160,164,170,172,176,182,188,190), collapse="|"),
                               names(df))]>1),
    PM_numanx=rowSums(df[grepl(paste0("_s", c(141,147,151,157,161,165,171,173,177,183,189,191), collapse="|"),
                               names(df))]>1)
  )
  #Score presence/absense of disorders
  df <- df %>% mutate(
    LP_pressubs=ifelse(LP_numsubs>0,1,0),
    PM_pressubs=ifelse(PM_numsubs>0,1,0),
    LP_presanx=ifelse(LP_numanx>0,1,0),
    PM_presanx=ifelse(PM_numanx>0,1,0)
  )
  return(df)
  message("This function gives number and presence of lifetime and current anxiety and substance use disorders:
                LP= lifetime prevalence, PM= past month,
                pres= presence (vs. absence), num= number of,
                anx= anxiety disorders, subs= substance use disorders")
}

#SIDP scoring
score.sidp<-function(df=NULL){
  #Remove unnecessary variables, change to numeric, all 4s should be NA (unscorable)
  if(!"masterdemoid" %in% names(df)){
    stop("need to IDmap first for SIDP")
  }
  df<-df %>% select(masterdemoid, redcap_event_name,contains("score")) %>% 
    mutate_at(vars(contains("score")),as.numeric)
  df[!is.na(df) & df=="4"]<-NA
  
  #[number of non-NA criteria met/# of possible criteria-missingness]*possible criteria
  df<-df %>% mutate(
    sidp_antisocial_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("j3","j5","j6","j2","j7","j4","j8","j9"),"score")]))<8,
                               round(
                                 (rowSums(df[paste0("sidp_iv_",c("j3","j5","j6","j2","j7","j4","j8","j9"),"score")]>1,na.rm = T)/(8-
                                                                                                                                    rowSums(is.na(df[paste0("sidp_iv_",c("j3","j5","j6","j2","j7","j4","j8","j9"),"score")]))))*8
                               ),NA),
    sidp_avoidant_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("b2","d3","c3","d5","d6","d1a","a3"),"score")]))<7,
                             round(
                               (rowSums(df[paste0("sidp_iv_",c("b2","d3","c3","d5","d6","d1a","a3"),"score")]>1,na.rm = T)/(7-
                                                                                                                              rowSums(is.na(df[paste0("sidp_iv_",c("b2","d3","c3","d5","d6","d1a","a3"),"score")]))))*7
                             ),NA),
    sidp_obsessivecompulsive_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]))<8,
                                        round(
                                          (rowSums(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]>1,na.rm = T)/(8-
                                                                                                                                             rowSums(is.na(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]))))*8
                                        ),NA),
    sidp_borderline_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("c7","c5","g1","j1","i5","e4","e5","i1","i4"),"score")]))<9,
                               round(
                                 (rowSums(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]>1,na.rm = T)/(9-
                                                                                                                                    rowSums(is.na(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]))))*9
                               ),NA),
    sidp_narcissism_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("g3","g4","d1n","d11","g2","b3","d4","g8","i2"),"score")]))<9,
                               round(
                                 (rowSums(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]>1,na.rm = T)/(9-
                                                                                                                                    rowSums(is.na(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]))))*9
                               ),NA),
    sidp_schizotypal_sxs=ifelse(rowSums(is.na(df[paste0("sidp_iv_",c("h5","h7","h_8","f2","h9","f4","f1","c2","d2"),"score")]))<9,
                                round(
                                  (rowSums(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]>1,na.rm = T)/(9-
                                                                                                                                     rowSums(is.na(df[paste0("sidp_iv_",c("b6","b4","b1","g5","g7","b5","a7","g6"),"score")]))))*9
                                ),NA)
  )
  df<-df %>% mutate(
    sidp_antisocial_presence=ifelse(sidp_antisocial_sxs>2 & sidp_iv_j9score>=2,1,0),
    sidp_avoidant_presence=ifelse(sidp_avoidant_sxs>3,1,0),
    sidp_obsessivecompulsive_presence=ifelse(sidp_obsessivecompulsive_sxs>3,1,0),
    sidp_borderline_presence=ifelse(sidp_borderline_sxs>4,1,0),
    sidp_narcissism_presence=ifelse(sidp_narcissism_sxs>4,1,0),
    sidp_schizotypal_presence=ifelse(sidp_schizotypal_sxs>4,1,0)
  )
  return(df)
}

#Neuropsych
#EXIT scoring
score.exit<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("exit_",c(1:25))),as.numeric)
  df<-df %>% mutate(
    exit_total=ifelse(rowSums(is.na(df[paste0("exit_",c(1:25))]))==0,
                      rowSums(df[paste0("exit_",c(1:25))]),ifelse(
                        rowSums(is.na(df[paste0("exit_",c(1:25))]))==1,
                        round(rowSums(df[paste0("exit_",c(1:25))],na.rm=T)*25/24),ifelse(
                          rowSums(is.na(df[paste0("exit_",c(1:25))]))==2,
                          round(rowSums(df[paste0("exit_",c(1:25))],na.rm=T)*25/23),NA)))
  )
  return(df)
}

#DRS scoring
score.drs<-function(df=NULL){
  drs_vars<-paste0("drs_",c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o',
                            'p','q','r','s','t','u','v','w','x','y','z','ab',
                            'ac','ad','ae','af','ag','ah','ai','aj','ak'))
  df<-df %>% mutate_at(vars(drs_vars),as.numeric)
  df<-df %>% mutate(
    drs_attention=ifelse(rowSums(is.na(df[paste0("drs_",c('a','b','c','d','ad','ae','ah','aj'))]))==0,
                         rowSums(df[paste0("drs_",c('a','b','c','d','ad','ae','ah','aj'))]),NA),
    drs_initandpers=ifelse(rowSums(is.na(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]))==0,
                           rowSums(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]),ifelse(
                             rowSums(is.na(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))]))==1,
                             round(rowSums(df[paste0("drs_",c('e','f','g','h','i','j','k','l','m','n','o'))],na.rm=T)*11/10),NA)),
    drs_construction=ifelse(rowSums(is.na(df[paste0("drs_",c('p','q','r','s','t','u'))]))==0,
                            rowSums(df[paste0("drs_",c('p','q','r','s','t','u'))]),NA),
    drs_conceptualization=ifelse(rowSums(is.na(df[paste0("drs_",c('v','w','x','y','z','ab'))]))==0,
                                 rowSums(df[paste0("drs_",c('v','w','x','y','z','ab'))]),NA),
    drs_memory=ifelse(rowSums(is.na(df[paste0("drs_",c('ac','af','ag','ai','ak'))]))==0,
                      rowSums(df[paste0("drs_",c('ac','af','ag','ai','ak'))]),NA),
    drs_total=ifelse(rowSums(is.na(df))==0, rowSums(df[drs_vars]),ifelse(
      rowSums(is.na(df))==1, round(rowSums(df[drs_vars],na.rm=T)*36/35),ifelse(
        rowSums(is.na(df))==2,round(rowSums(df[drs_vars],na.rm=T)*36/34),ifelse(
          rowSums(is.na(df))==3,round(rowSums(df[drs_vars],na.rm=T)*36/33),NA))))
  )
  return(df)
}

#Self reports
#ARS scoring
score.ars<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("ars_",1:19)),as.numeric)
  df<-df %>% mutate(
    ars_angry_afterthoughts=ifelse(rowSums(is.na(df[paste0("ars_",c(19,18,9,8))]))==0,
                                   rowSums(df[paste0("ars_",c(19,18,9,8))]),NA),
    ars_thoughts_revenge=ifelse(rowSums(is.na(df[paste0("ars_",c(4,16,13,6))]))==0,
                                rowSums(df[paste0("ars_",c(4,16,13,6))]),NA),
    ars_angry_memories=ifelse(rowSums(is.na(df[paste0("ars_",c(2,3,15,1,5))]))==0,
                              rowSums(df[paste0("ars_",c(2,3,15,1,5))]),NA),
    ars_understanding_causes=ifelse(rowSums(is.na(df[paste0("ars_",c(12,17,11,10))]))==0,
                                    rowSums(df[paste0("ars_",c(12,17,11,10))]),NA),
    ars_total=ifelse(rowSums(is.na(df[paste0("ars_",c(1:19))]))==0,
                     rowSums(df[paste0("ars_",c(1:19))]),ifelse(
                       rowSums(is.na(df[paste0("ars_",c(1:19))]))==1,
                       round(rowSums(df[paste0("ars_",c(1:19))],na.rm=T)*19/18), NA))
  )
}

#BIS-36 Scoring
score.bis<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("bis36_",1:36)),as.numeric)
  #Make reverse scores
  df<-df %>% mutate(
    bis36_1r=5-bis36_1, bis36_5r=5-bis36_5,bis36_6r=5-bis36_6,
    bis36_7r=5-bis36_7, bis36_8r=5-bis36_8,bis36_10r=5-bis36_10,
    bis36_11r=5-bis36_11,bis36_13r=5-bis36_13,bis36_19r=5-bis36_19,
    bis36_30r=5-bis36_30,bis36_35r=5-bis36_35
  ) 
  df <- df %>% mutate(
    bis_s_attentional = ifelse(rowSums(is.na(df[paste0("bis36_",c(31, 32, 33, 34, 35, 36))]))==6, # If pts don't have Q31-Q36: they received BIS-11A, do prorated scoring
                               ifelse(rowSums(is.na(df[paste0("bis36_",c(4, '7r', '19r', 27, 29))]))==0,
                                      round(rowSums(df[paste0("bis36_",c(4, '7r', '19r', 27, 29))])*8/5), NA), # Prorated scoring
                               ifelse(rowSums(is.na(df[paste0("bis36_",c(4, 34, 27, 32, '7r', 36, '19r', 29))]))==0,
                                      rowSums(df[paste0("bis36_",c(4, 34, 27, 32, '7r', 36, '19r', 29))]), NA)  # BIS-11 scoring
    ),
    bis_s_motor = ifelse(rowSums(is.na(df[paste0("bis36_",c(31, 32, 33, 34, 35, 36))]))==6, # If pts don't have Q31-Q36: they received BIS-11A, do prorated scoring
                         ifelse(rowSums(is.na(df[paste0("bis36_",c(2, 3, 14, 15, 18, 20, 21, 25, '30r'))]))==0,         # Prorated scoring
                                round(rowSums(df[paste0("bis36_",c(2, 3, 14, 15, 18, 20, 21, 25, '30r'))])*11/9), NA), 
                         ifelse(rowSums(is.na(df[paste0("bis36_",c(2, 31, 3, 15, 18, 21, 25, 14, 20, 33, '30r'))]))==0, # BIS-11 scoring
                                rowSums(df[paste0("bis36_",c(2, 31, 3, 15, 18, 21, 25, 14, 20, 33, '30r'))]),
                                ifelse(rowSums(is.na(df[paste0("bis36_",c(2, 31, 3, 15, 18, 21, 25, 14, 20, 33, '30r'))]))==1, 
                                       round(rowSums(df[paste0("bis36_",c(2, 31, 3, 15, 18, 21, 25, 14, 20, 33, '30r'))],na.rm=T)*11/10), NA))
    ),
    bis_s_nonplanning = ifelse(rowSums(is.na(df[paste0("bis36_",c(31, 32, 33, 34, 35, 36))]))==6, # If pts don't have Q31-Q36: they received BIS-11A, do prorated scoring
                               ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28))]))==0,         # Prorated scoring
                                      round(rowSums(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28))])*11/10), 
                                      ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28))]))==1, 
                                             round(rowSums(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28))], na.rm=T)*11/9), NA)), # 11/9 = 10/9 (10% missingness) * 11/10 (prorated scoring)
                               ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28, '35r'))]))==0,
                                      rowSums(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28, '35r'))]),ifelse(
                                        rowSums(is.na(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28, '35r'))]))==1,
                                        round(rowSums(df[paste0("bis36_",c('1r', '5r', '6r', '8r', '10r', '11r', 12, '13r', 16, 28, '35r'))],na.rm=T)*11/10), NA))
    ),
    bis_total = ifelse(rowSums(is.na(df[paste0("bis36_",c(31, 32, 33, 34, 35, 36))]))==6, # If pts don't have Q31-Q36: they received BIS-11A, do prorated scoring
                       ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))]))==0,         # Prorated scoring
                              round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))])*30/24), 
                              ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))]))==1, 
                                     round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))], na.rm=T)*30/23), # 30/23 = 24/23 (10% missingness) * 30/24 (prorated scoring)
                                     ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))]))==2, 
                                            round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r'))], na.rm=T)*30/22), NA))), # 30/22 = 24/22 (10% missingness) * 30/24 (prorated scoring)
                       ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))]))==0,
                              rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))]),
                              ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))]))==1,
                                     round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))],na.rm=T)*30/29), 
                                     ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))]))==2,
                                            round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))],na.rm=T)*30/28),
                                            ifelse(rowSums(is.na(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))]))==3,
                                                   round(rowSums(df[paste0("bis36_",c('1r', 2, 3, 4, '5r', '6r', '7r', '8r', '10r', '11r', 12, '13r', 14, 15, 16, 18, '19r', 20, 21, 25, 27, 28, 29, '30r', 31, 32, 33, 34, '35r', 36))],na.rm=T)*30/27), NA))
                              )))
  )
  return(df)
}

#CTQ scoring
score.ctq<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("ctq_",1:28)),as.numeric)
  #Reverse scoring
  df <- df %>% mutate(
    ctq_2r=6-ctq_2, ctq_5r=6-ctq_5, ctq_7r=6-ctq_7, ctq_13r=6-ctq_13, ctq_19r=6-ctq_19, 
    ctq_26r=6-ctq_26, ctq_28r=6-ctq_28)
  #Subscale scoring
  df <- df %>% mutate(
    ctq_emotional_abuse = ifelse(rowSums(is.na(df[paste0("ctq_", c(3, 8, 14, 18, 25))]))==0, 
                                 rowSums(df[paste0("ctq_", c(3, 8, 14, 18, 25))]),NA),
    ctq_physical_abuse = ifelse(rowSums(is.na(df[paste0("ctq_", c(9, 11, 12, 15, 17))]))==0, 
                                rowSums(df[paste0("ctq_", c(9, 11, 12, 15, 17))]),NA),
    ctq_sexual_abuse = ifelse(rowSums(is.na(df[paste0("ctq_", c(20, 21, 23, 24, 27))]))==0, 
                              rowSums(df[paste0("ctq_", c(20, 21, 23, 24, 27))]),NA),
    ctq_emotiona_neglect = ifelse(rowSums(is.na(df[paste0("ctq_", c('5r', '7r', '13r', '19r', '28r'))]))==0, 
                                  rowSums(df[paste0("ctq_", c('5r', '7r', '13r', '19r', '28r'))]),NA),
    ctq_physical_neglect = ifelse(rowSums(is.na(df[paste0("ctq_", c(1, '2r', 4, 6, '26r'))]))==0, 
                                  rowSums(df[paste0("ctq_", c(1, '2r', 4, 6, '26r'))]),NA),
    ctq_minimization = ifelse(rowSums(is.na(df[paste0("ctq_", c(10, 16, 22))]))==0, 
                              rowSums(df[paste0("ctq_", c(10, 16, 22))]),NA),
    ctq_total = ifelse(rowSums(is.na(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))]))==0, 
                       rowSums(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))]),
                       ifelse(rowSums(is.na(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))]))==1, 
                              rowSums(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))])*25/24,
                              ifelse(rowSums(is.na(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))]))==2,
                                     rowSums(df[paste0("ctq_", c(3, 8, 14, 18, 25, 9, 11, 12, 15, 17, 20, 21, 23, 24, 27, '5r', '7r', '13r', '19r', '28r', 1, '2r', 4, 6, '26r'))])*25/23, NA)))
  )
  return(df)
}

#ISEL scoring
score.isel<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("isel_",1:16)),as.numeric)
  df<-df %>% mutate(
    isel_selfesteem=ifelse(rowSums(is.na(df[paste0("isel_",c(1,10,13,16))]))==0,
                           1+isel_1+isel_10-isel_13+isel_16,NA),
    isel_belonging=ifelse(rowSums(is.na(df[paste0("isel_",c(2:4,9))]))==0,
                          6-isel_2-isel_3+isel_4+isel_9,NA),
    isel_appraisal=ifelse(rowSums(is.na(df[paste0("isel_",c(5,8,12,15))]))==0,
                          6+isel_5-isel_8+isel_12-isel_15,NA),
    isel_tangible=ifelse(rowSums(is.na(df[paste0("isel_",c(6,7,11,14))]))==0,
                         1+isel_6-isel_7+isel_11+isel_14,NA)
  )
  return(df)
}

#IIP scoring
score.iip<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("iip15_",1:15)),as.numeric)
  df<-df %>% mutate(
    iip_interpersonal_sensitivity=ifelse(rowSums(is.na(df[paste0("iip15_",c(5,8,10,12,13))]))==0,
                                         rowSums(df[paste0("iip15_",c(5,8,10,12,13))]),NA),
    iip_interpersonal_ambivalence=ifelse(rowSums(is.na(df[paste0("iip15_",c(1:4,6))]))==0,
                                         rowSums(df[paste0("iip15_",c(1:4,6))]),NA),
    iip_agression=ifelse(rowSums(is.na(df[paste0("iip15_",c(7,9,11,14,15))]))==0,
                         rowSums(df[paste0("iip15_",c(7,9,11,14,15))]),NA)
  )
  return(df)
}

#NEO scoring
score.neo<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("neoffi_",1:60)),as.numeric)
  df<-df %>% mutate(
    neoffi_1r=6-neoffi_1,neoffi_3r=6-neoffi_3,neoffi_8r=6-neoffi_8, neoffi_9r=6-neoffi_9,
    neoffi_12r=6-neoffi_12, neoffi_14r=6-neoffi_14, neoffi_15r=6-neoffi_15,
    neoffi_16r=6-neoffi_16, neoffi_18r=6-neoffi_18, neoffi_23r=6-neoffi_23,
    neoffi_24r=6-neoffi_24, neoffi_27r=6-neoffi_27, neoffi_29r=6-neoffi_29,
    neoffi_30r=6-neoffi_30, neoffi_31r=6-neoffi_31, neoffi_38r=6-neoffi_38,
    neoffi_39r=6-neoffi_39, neoffi_42r=6-neoffi_42, neoffi_44r=6-neoffi_44,
    neoffi_45r=6-neoffi_45, neoffi_46r=6-neoffi_46, neoffi_48r=6-neoffi_48,
    neoffi_54r=6-neoffi_54, neoffi_55r=6-neoffi_55, neoffi_57r=6-neoffi_57,
    neoffi_59r=6-neoffi_59) #Changed all scoring to 6 minus
  #Scoring
  df<-df %>% mutate(
    neo_neuroticism=ifelse(rowSums(is.na(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]))==0,
                           rowSums(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]),ifelse(
                             rowSums(is.na(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))]))==1,
                             round(rowSums(df[paste0("neoffi_",c('1r',11,'16r','31r','46r',6,21,26,36,41,51,56))],na.rm=T)*12/11),NA)),
    neo_extraversion=ifelse(rowSums(is.na(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]))==0,
                            rowSums(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]),ifelse(
                              rowSums(is.na(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))]))==1,
                              round(rowSums(df[paste0("neoffi_",c(7,'12r',37,'42r',2,17,'27r','57r',22,32,47,52))],na.rm=T)*12/11),NA)),
    neo_openness=ifelse(rowSums(is.na(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]))==0,
                        rowSums(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]),ifelse(
                          rowSums(is.na(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))]))==1,
                          round(rowSums(df[paste0("neoffi_",c(13,'23r',43,'48r',53,58,'3r','8r','18r','38r'))],na.rm=T)*10/9),NA)),
    neo_agreeableness=ifelse(rowSums(is.na(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]))==0,
                             rowSums(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]),ifelse(
                               rowSums(is.na(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))]))==1,
                               round(rowSums(df[paste0("neoffi_",c('9r','14r',19,'24r','29r','44r','54r','59r',4,34,'39r',49))],na.rm=T)*12/11),NA)),
    neo_conscientiousness=ifelse(rowSums(is.na(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]))==0,
                                 rowSums(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]),ifelse(
                                   rowSums(is.na(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))]))==1,
                                   round(rowSums(df[paste0("neoffi_",c(5,10,'15r','30r','55r',25,35,60,20,40,'45r',50))],na.rm=T)*12/11),NA))
  )
  return(df)
}

#PAIBOR scoring
score.paibor<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("paibor_",1:24)),as.numeric)
  #Reverse scoring
  df<-df %>% mutate(
    paibor_7r=3-paibor_7, paibor_12r=3-paibor_12, paibor_14r=3-paibor_14,
    paibor_19r=3-paibor_19, paibor_20r=3-paibor_20, paibor_24r=3-paibor_24
  )
  df<-df %>% mutate(
    paibor_identity_problems=ifelse(rowSums(is.na(df[paste0("paibor_",c(2, 5, 8, 11, 15, '19r'))]))==0,
                                    rowSums(df[paste0("paibor_",c(2, 5, 8, 11, 15, '19r'))]),NA),
    paibor_neg_relation=ifelse(rowSums(is.na(df[paste0("paibor_",c(3, 6, 9, '12r', 16, '20r'))]))==0,
                               rowSums(df[paste0("paibor_",c(3, 6, 9, '12r', 16, '20r'))]),NA),
    paibor_affective_instability=ifelse(rowSums(is.na(df[paste0("paibor_",c(1,4,'7r',10,'14r',18))]))==0,
                                        rowSums(df[paste0("paibor_",c(1,4,'7r',10,'14r',18))]),NA),
    paibor_selfharm=ifelse(rowSums(is.na(df[paste0("paibor_",c(13,17,21,22,23,'24r'))]))==0,
                           rowSums(df[paste0("paibor_",c(13,17,21,22,23,'24r'))]),NA),
    paibor_total=ifelse(rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==0,
                        rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]),ifelse(
                          rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==1,
                          round(rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))],na.rm=T)*24/23),ifelse(
                            rowSums(is.na(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))]))==2,
                            round(rowSums(df[paste0("paibor_",c(1:6,'7r',8:11,'12r',13,'14r',15:18,'19r','20r',21:23,'24r'))],na.rm=T)*24/22),NA)))
  )
  return(df)
}

#PB scoring
score.pb<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("pb_",c('6a','6b','6c','6d','6e','6f'))),as.numeric)
  df<-df %>% mutate(
    pb_6fr=(2-pb_6f)
  )
  
  df<-df %>% mutate(
    pb_total=ifelse(rowSums(is.na(df[c("pb_6a","pb_6b","pb_6c","pb_6d","pb_6e","pb_6f")]))==0,
                    rowSums(df[c("pb_6a","pb_6b","pb_6c","pb_6d","pb_6e","pb_6fr")]),NA)
  )
  return(df)
}

#SPSI scoring
score.spsi<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("spsi_",c(1:25)),as.numeric)  
  df<-df %>% mutate(
    #Subscores (positive and rational are positive, negative, impulsecare and avoid are negative)
    spsi_pos_problemorient=ifelse(rowSums(is.na(df[paste0("spsi_",c(4,5,9,13,15))]))==0,
                                  rowSums(df[paste0("spsi_",c(4,5,9,13,15))]),NA),
    spsi_neg_problemorient=ifelse(rowSums(is.na(df[paste0("spsi_",c(1,3,7,8,11))]))==0,
                                  rowSums(df[paste0("spsi_",c(1,3,7,8,11))]),NA),
    spsi_rational_problemsolve=ifelse(rowSums(is.na(df[paste0("spsi_",c(16,19,12,21,23))]))==0,
                                      rowSums(df[paste0("spsi_",c(16,19,12,21,23))]),NA),
    spsi_impulsecareless=ifelse(rowSums(is.na(df[paste0("spsi_",c(2,14,20,24,25))]))==0,
                                rowSums(df[paste0("spsi_",c(2,14,20,24,25))]),NA),
    spsi_avoidance=ifelse(rowSums(is.na(df[paste0("spsi_",c(10,18,6,17,22))]))==0,
                          rowSums(df[paste0("spsi_",c(10,18,6,17,22))]),NA),
    #Reverse items in negative, impulsecare, and avoidance for total calculation
    spsi_1r=4-spsi_1, spsi_3r=4-spsi_3, spsi_7r=4-spsi_7, spsi_8r=4-spsi_8, spsi_11r=4-spsi_11,
    spsi_2r=4-spsi_2, spsi_14r=4-spsi_14, spsi_20r=4-spsi_20, spsi_24r=4-spsi_24, spsi_25r=4-spsi_25,
    spsi_10r=4-spsi_10, spsi_18r=4-spsi_18, spsi_6r=4-spsi_6, spsi_17r=4-spsi_17, spsi_22r=4-spsi_17)
  
  #Total
  df<-df %>% mutate(
    spsi_total=ifelse(rowSums(is.na(df[paste0("spsi_",c(1:25))]))==0,
                      rowSums(df[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))]),ifelse(
                        rowSums(is.na(df[paste0("spsi_",c(1:25))]))==1,round(rowSums(df[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))],na.rm=T)*25/24),ifelse(
                          rowSums(is.na(df[paste0("spsi_",c(1:25))]))==2,round(rowSums(df[paste0("spsi_",c(4,5,9,13,15,16,19,12,21,23,'1r','3r','7r','8r','11r','2r','14r','20r','24r','25r','10r','18r','6r','17r','22r'))],na.rm=T)*25/23),NA)))
  )
  return(df)
}

#SSD Scoring
score.ssd<-function(df=NULL){
  #Recode variables (ND means for network diversity, ppl= Number of people)
  df<-df %>% mutate_at(vars(paste0("ssd_",c(1:12,'2a','3a','4a','5a','6a','7a','8a','10a','12a','12b'))),as.numeric)
  df<-df %>% mutate(
    #network diversity
    ssd_1nd=ifelse(ssd_1==1,1,0),ssd_2nd=ifelse(ssd_2a>0,1,0),
    ssd_3nd=ifelse(ssd_3a>0,1,0),ssd_4nd=ifelse(ssd_4a>0,1,0),
    ssd_5nd=ifelse(ssd_5a>0,1,0),ssd_6nd=ifelse(ssd_6a>0,1,0),
    ssd_7nd=ifelse(ssd_7a>0,1,0),ssd_8nd=ifelse(ssd_8a>0,1,0),
    ssd_12nd=ifelse(ssd_12a>0 | ssd_12b>0,1,0),ssd_9nd=ifelse(ssd_9>0,1,0),
    ssd_10nd=ifelse(ssd_10a>0,1,0),ssd_11nd=ifelse(ssd_11==1,1,0),
    #number of people in network
    ssd_1ppl=ifelse(ssd_1==1,1,0), ssd_2ppl=ssd_2a, ssd_3ppl=dplyr::recode(ssd_3a,'0'=0,'1'=1,'2'=1,'3'=2),
    ssd_4ppl=dplyr::recode(ssd_4a,'0'=0,'1'=1,'2'=1,'3'=2,'9'=0), ssd_5ppl=ssd_5a,
    ssd_6ppl=ssd_6a, ssd_7ppl=ssd_7a, ssd_8ppl=ssd_8a, ssd_12ppl=ssd_12a+ssd_12b,
    ssd_9ppl=ssd_9, ssd_10ppl=ssd_10,
    #embedded networks
    ssd_1en=ifelse(ssd_1==1,1,0), ssd_2en=ifelse(ssd_2a>0,1,0),
    ssd_3en=ifelse(ssd_3a>0,1,0), ssd_4en=ifelse(ssd_4a>0 & ssd_4a!=9,1,0),
    ssd_5en=ifelse(ssd_5a>0,1,0))
  #Embedded Networks family score
  df<-df %>% mutate(
    ssd_famen1=rowSums(df[c("ssd_1en", "ssd_2en", "ssd_3en", "ssd_4en", "ssd_5en")],na.rm=T),
    ssd_famen2=rowSums(df[c("ssd_1ppl","ssd_2ppl", "ssd_3ppl", "ssd_4ppl", "ssd_5ppl")],na.rm=T))
  #Score
  df<-df %>% mutate(
    ssd_network_diversity=rowSums(df[names(df) %in% paste("ssd_",c(1:12),"nd",sep="")],na.rm=T),
    ssd_num_of_people=rowSums(df[names(df) %in% paste("ssd_",c(1:10,12),"ppl",sep="")],na.rm=T)
  )
  for (i in 1:nrow(df)){
    df[i,"ssd_embedded_network"]<-
      ifelse(!is.na(df[i,"ssd_famen1"]) & df[i,"ssd_famen1"]>2 & !is.na(df[i,"ssd_famen2"]) & df[i,"ssd_famen2"]>3,1,0)+
      ifelse(!is.na(df[i,"ssd_6a"]) & df[i,"ssd_6a"]>3,1,0)+
      ifelse(!is.na(df[i, "ssd_7a"])& df[i, "ssd_7a"]>3,1,0)+
      ifelse(!is.na(df[i, "ssd_8a"])& df[i, "ssd_8a"]>3,1,0)+
      ifelse(!is.na(df[i, "ssd_12a"])& !is.na(df[i, "ssd_12b"]) & df[i, "ssd_12a"]+df[i, "ssd_12b"]>3,1,0)+
      ifelse(!is.na(df[i, "ssd_9"]) & df[i, "ssd_9"]>3,1,0)+
      ifelse(!is.na(df[i, "ssd_10a"])& df[i, "ssd_10a"]>3,1,0)
  }
  return(df)
}

#TA scoring
score.ta<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("ta_",1:7)),as.numeric)
  df<-df %>% mutate(
    ta_total=ifelse(rowSums(is.na(df[paste0("ta_",1:7)]))==0,
                    rowSums(df[paste0("ta_",1:7)]),NA)
  )
}

#UPPSP scoring
score.uppsp<-function(df=NULL){
  df<-df %>% mutate_at(vars(paste0("uppsp_",1:47)),as.numeric)
  #Reverse Scoring
  df<-df %>% mutate(
    uppsp_2r=5-uppsp_2, uppsp_6r=5-uppsp_6, uppsp_10r=5-uppsp_10,
    uppsp_14r=5-uppsp_14, uppsp_18r=5-uppsp_18, uppsp_23r=5-uppsp_23,
    uppsp_27r=5-uppsp_27, uppsp_31r=5-uppsp_31, uppsp_35r=5-uppsp_35,
    uppsp_40r=5-uppsp_40, uppsp_46r=5-uppsp_46, uppsp_7r=5-uppsp_7,
    uppsp_37r=5-uppsp_37, uppsp_4r=5-uppsp_4, uppsp_8r=5-uppsp_8,
    uppsp_12r=5-uppsp_12, uppsp_16r=5-uppsp_16, uppsp_20r=5-uppsp_20,
    uppsp_24r=5-uppsp_24, uppsp_28r=5-uppsp_28, uppsp_32r=5-uppsp_32,
    uppsp_36r=5-uppsp_36, uppsp_39r=5-uppsp_39,uppsp_41r=5-uppsp_41, 
    uppsp_43r=5-uppsp_43, uppsp_45r=5-uppsp_45, uppsp_47r=5-uppsp_47
  )
  df<-df %>% mutate(
    uppsp_neg_urgency=ifelse(rowSums(is.na(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                                                '23r','27r','31r','35r','40r',42,'46r'))]))==0,
                             rowSums(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                                          '23r','27r','31r','35r','40r',42,'46r'))]),ifelse(
                                                            rowSums(is.na(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                                                                               '23r','27r','31r','35r','40r',42,'46r'))]))==1,
                                                            round(rowSums(df[paste0("uppsp_",c('2r','6r','10r','14r','18r',
                                                                                               '23r','27r','31r','35r','40r',42,'46r'))],na.rm=T)*12/11),NA)),
    uppsp_lackof_premed=ifelse(rowSums(is.na(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]))==0,
                               rowSums(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]),ifelse(
                                 rowSums(is.na(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))]))==1,
                                 round(rowSums(df[paste0("uppsp_",c(1,5,9,13,17,22,26,30,34,38,44))],
                                               na.rm=T)*11/10),NA)),
    uppsp_lackof_perserve=ifelse(rowSums(is.na(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]))==0,
                                 rowSums(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]),ifelse(
                                   rowSums(is.na(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))]))==1,
                                   round(rowSums(df[paste0("uppsp_",c(3,'7r',11,15,19,21,25,29,33,'37r'))],
                                                 na.rm=T)*10/9),NA)),
    uppsp_pos_urgency=ifelse(rowSums(is.na(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                '36r','39r','41r','43r','45r','47r'))]))==0,
                             rowSums(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                          '36r','39r','41r','43r','45r','47r'))]),ifelse(
                                                            rowSums(is.na(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                                               '36r','39r','41r','43r','45r','47r'))]))==1,
                                                            round(rowSums(df[paste0("uppsp_",c('4r','8r','12r','16r','20r','24r','28r','32r',
                                                                                               '36r','39r','41r','43r','45r','47r'))],
                                                                          na.rm=T)*14/13),NA))
    
  )
  return(df)
}

#FS scoring
score.fs <- function(df=NULL){
  paste0("fs_",1:8)->names(df)[names(df) %in% c("fs_purpose","fs_rewarding_relationships","fs_engaged","fs_contribute","fs_competent","fs_goodperson","fs_optimistic","fs_respect")]
  
  df <- df %>% 
    mutate_at(vars(paste0("fs_",c(1:8))),as.numeric)%>% 
    mutate(fs_total= ifelse(rowSums(is.na(df[paste0("fs_",c(1:8))]))==0,rowSums(df[paste0("fs_",c(1:8))]), NA))
  return(df)
}

#LET scoring
score.let <- function(df=NULL){
  df <- df %>% mutate_at(vars(paste0("let_",c(1:6))),as.numeric)
  df <- df %>% mutate(let_1=6-let_1, let_3=6-let_3,let_5=6-let_5)
  
  df <- df%>% mutate(LET_total= ifelse(rowSums(is.na(df[paste0("let_",c(1:6))]))==0,
                                       rowSums(df[paste0("let_",c(1:6))]), NA)
  )
  return(df)
}

#SWLS scoring
score.swls <- function (df=NULL){
  paste0("swls_",1:5)->names(df)[names(df) %in% c("swls_ideal","swls_excellent","swls_satisfied","swls_important","swls_no_change")]
  df <- df %>% 
    mutate_at(vars(paste0("swls_",c(1:5))),as.numeric)%>%
    mutate(swls_total= ifelse(rowSums(is.na(df[paste0("swls_",c(1:5))]))==0,rowSums(df[paste0("swls_",c(1:5))]), NA))
  
  return(df)
}

#MAAS scoring 
score.maas <- function(df=NULL){
  df <- df %>% mutate_at(vars(paste0("maas_",1:15)),as.numeric)
  
  df <- df %>% mutate(
    maas_mean=ifelse(rowSums(is.na(df[paste0("maas_",c(1:15))]))==0,
                     rowSums(df[paste0("maas_",c(1:15))])/15,ifelse(
                       rowSums(is.na(df[paste0("maas_",c(1:15))]))==1,
                       round(rowSums(df[paste0("maas_",c(1:15))],na.rm=T)*15/14)/14,NA)
    ))
  return(df)
}

#ah scoring
score.ah <- function(df=NULL){
  df <- df %>% mutate_at(vars(paste0("ah_",c(1:14))),as.numeric)%>% 
    mutate(ah_total=ifelse(rowSums(is.na(df[paste0("ah_",c(1:14))]))==0,
                           rowSums(df[paste0("ah_",c(1:14))]),ifelse(
                             rowSums(is.na(df[paste0("ah_",c(1:14))]))==1,
                             round(rowSums(df[paste0("ah_",c(1:14))],na.rm=T)*14/13),NA))
    )
  return(df)
}

#BSIA scoring
score.bsia <- function (df=NULL){
  df <- df %>% mutate_at(vars(paste0("bsi_a_",c(1:6))),as.numeric)%>% 
    mutate(bsia_total=ifelse(rowSums(is.na(df[paste0("bsi_a_",c(1:6))]))==0,
                             rowSums(df[paste0("bsi_a_",c(1:6))]),ifelse(
                               rowSums(is.na(df[paste0("bsi_a_",c(1:6))]))==1,
                               round(rowSums(df[paste0("bsi_a_",c(1:6))],na.rm=T)*6/5),NA))
    )
  return(df)
}

#CFCS scoring
score.cfcs <- function (df=NULL){
  df <- df %>% mutate_at(vars(paste0("cfcs_",c(1:12))),as.numeric)%>% 
    mutate(cfcs_total=ifelse(rowSums(is.na(df[paste0("cfcs_",c(1:12))]))==0,
                             rowSums(df[paste0("cfcs_",c(1:12))]),ifelse(
                               rowSums(is.na(df[paste0("cfcs_",c(1:12))]))==1,
                               round(rowSums(df[paste0("cfcs_",c(1:12))],na.rm=T)*12/11),NA))
    )
  return(df)
}


#ders scoring
score.ders <- function (df=NULL){
  df <- df %>% mutate_at(vars(paste0("ders_",1:36)),as.numeric)%>%
    mutate(ders_20R=6-ders_20,
           ders_24R=6-ders_24,
           ders_2R=6-ders_2,
           ders_6R=6-ders_6,
           ders_8R=6-ders_8,
           ders_10R=6-ders_10,
           ders_17R=6-ders_17,
           ders_34R=6-ders_34,
           ders_22R=6-ders_22,
           ders_1R=6-ders_1,
           ders_7R=6-ders_7)
  
  
  df <- df %>%mutate(ders_nonacceptance = ifelse(rowSums(is.na(df[paste0("ders_", c(25, 21, 12, 11, 23,29))]))==0, 
                                                 rowSums(df[paste0("ders_", c(25, 21, 12, 11, 23,29))]),NA),
                     ders_goals = ifelse(rowSums(is.na(df[paste0("ders_", c(13,18,'20R',26,33))]))==0, 
                                         rowSums(df[paste0("ders_", c(13,18,'20R',26,33))]),NA),
                     ders_imupsitivty = ifelse(rowSums(is.na(df[paste0("ders_", c(3, 14, 19, '24R', 27, 32))]))==0, 
                                               rowSums(df[paste0("ders_", c(3, 14, 19, '24R', 27, 32))]),NA),
                     ders_awareness = ifelse(rowSums(is.na(df[paste0("ders_", c('2R', '6R', '8R', '10R', '17R', '34R'))]))==0, 
                                             rowSums(df[paste0("ders_", c('2R', '6R', '8R', '10R', '17R', '34R'))]),NA),
                     ders_strategies = ifelse(rowSums(is.na(df[paste0("ders_", c(15, 16, '22R', 28, 30, 31, 35, 36))]))==0, 
                                              rowSums(df[paste0("ders_", c(15, 16, '22R', 28, 30, 31, 35, 36))]),NA),
                     ders_clarity = ifelse(rowSums(is.na(df[paste0("ders_", c('1R', 4, 5, '7R', 9))]))==0, 
                                           rowSums(df[paste0("ders_", c('1R', 4, 5, '7R', 9))]),NA))
  
  df <- df %>% mutate_at(vars(paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                               16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))),as.numeric)%>% 
    mutate(ders_total=ifelse(rowSums(is.na(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                    16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))]))==0,
                             rowSums(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                              16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))]),ifelse(
                                                                rowSums(is.na(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                       16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))]))==1,
                                                                round(rowSums(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                       16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))],na.rm=T)*36/35),ifelse(
                                                                                                         rowSums(is.na(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                                                                16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))]))==2,
                                                                                                         round(rowSums(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                                                                16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))],na.rm=T)*36/34), ifelse(
                                                                                                                                                  rowSums(is.na(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                                                                                                         16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))]))==3,
                                                                                                                                                  
                                                                                                                                                  round(rowSums(dersraw[paste0("ders_",c(25, 21, 12, 11, 23,29,13,18,'20R',26,33,3, 14, 19, '24R', 27, 32,'2R', '6R', '8R', '10R', '17R', '34R',15, 
                                                                                                                                                                                         16, '22R', 28, 30, 31, 35, 36,'1R', 4, 5, '7R', 9))],na.rm=T)*36/33),NA))))
    )
  return (df)
}

#IRI scoring 
score.iri <- function (df=NULL) {
  df <- df %>% mutate_at(vars(paste0("iri_",1:28)),as.numeric)%>%
    mutate(iri_7R=4-iri_7,
           iri_12R=4-iri_12)
  
  
  df <- df %>%mutate(iri_fantasy = ifelse(rowSums(is.na(df[paste0("iri_", c(26,5,'7R','16','1','12R','23'))]))==0, 
                                          rowSums(df[paste0("iri_", c(26,5,'7R','16','1','12R','23'))]),NA),
                     iri_pers_taking = ifelse(rowSums(is.na(df[paste0("iri_", c(28,15,11,21,3,8,25))]))==0, 
                                              rowSums(df[paste0("iri_", c(28,15,11,21,3,8,25 ))]),NA),
                     iri_empath_concern = ifelse(rowSums(is.na(df[paste0("iri_", c(9, 18, 2, 22, 4, 14, 20))]))==0, 
                                                 rowSums(df[paste0("iri_", c(9, 18, 2, 22, 4, 14, 20))]),NA),
                     iri_pers_distress = ifelse(rowSums(is.na(df[paste0("iri_", c(27,10,6,19,17,13,24))]))==0, 
                                                rowSums(df[paste0("iri_", c(27,10,6,19,17,13,24))]),NA)
  )
  return(df)
}

#NFC scoring
score.nfc <- function (df=NULL) {
  df <- df %>% mutate_at(vars(paste0("nfc_",c(1:15))),as.numeric)%>% 
    mutate(nfc_total=ifelse(rowSums(is.na(df[paste0("nfc_",c(1:15))]))==0,
                            rowSums(df[paste0("nfc_",c(1:15))]),ifelse(
                              rowSums(is.na(df[paste0("nfc_",c(1:15))]))==1,
                              round(rowSums(df[paste0("nfc_",c(1:15))],na.rm=T)*15/14),NA))
    )
  return (df)
}


#RAND12 scoring
score.rand12 <- function (df=NULL){
  names(df) <- c("masterdemoid",'redcap_event_name',"gh1", "pf02", "pf04", "rp2", "rp3", "re2", "re3", "bp2",
                 "mh3", "vt2", "mh4", "sf2" )
  
  twopt <- c("rp2", "rp3", "re2", "re3")
  threept <- c("pf02", "pf04")
  fivept <- c("gh1", "bp2", "sf2")
  sixpt <- c("vt2", "mh3", "mh4")
  outRangeNA <- function(x, Min = 1L, Max) replace(x, x < Min | x > Max, NA)
  
  df[, twopt] <- lapply(df[, twopt], outRangeNA, Max = 2L)
  df[, threept] <- lapply(df[, threept], outRangeNA, Max = 3L)
  df[, fivept] <- lapply(df[, fivept], outRangeNA, Max = 5L)
  df[, sixpt] <- lapply(df[, sixpt], outRangeNA, Max = 6L)
  
  
  df$rbp2  <-  6 - df$bp2
  df$rgh1  <-  6 - df$gh1
  df$rvt2  <-  7 - df$vt2
  df$rmh3  <-  7 - df$mh3
  
  
  df$pf02_1 <- as.numeric(df$pf02 == 1L) 
  df$pf02_2 <- as.numeric(df$pf02 == 2L) 
  
  df$pf04_1 <- as.numeric(df$pf04 == 1L) 
  df$pf04_2 <- as.numeric(df$pf04 == 2L) 
  
  
  df$rp2_1 <- as.numeric(df$rp2 == 1L) 
  
  
  df$rp3_1 <- as.numeric(df$rp3 == 1L) 
  
  df$bp2_1 <- as.numeric(df$rbp2 == 1L) 
  df$bp2_2 <- as.numeric(df$rbp2 == 2L) 
  df$bp2_3 <- as.numeric(df$rbp2 == 3L) 
  df$bp2_4 <- as.numeric(df$rbp2 == 4L) 
  
  df$gh1_1 <- as.numeric(df$rgh1 == 1L) 
  df$gh1_2 <- as.numeric(df$rgh1 == 2L) 
  df$gh1_3 <- as.numeric(df$rgh1 == 3L) 
  df$gh1_4 <- as.numeric(df$rgh1 == 4L) 
  
  
  df$vt2_1 <- as.numeric(df$rvt2 == 1L) 
  df$vt2_2 <- as.numeric(df$rvt2 == 2L) 
  df$vt2_3 <- as.numeric(df$rvt2 == 3L) 
  df$vt2_4 <- as.numeric(df$rvt2 == 4L) 
  df$vt2_5 <- as.numeric(df$rvt2 == 5L) 
  
  
  
  df$sf2_1 <- as.numeric(df$sf2 == 1L) 
  df$sf2_2 <- as.numeric(df$sf2 == 2L) 
  df$sf2_3 <- as.numeric(df$sf2 == 3L) 
  df$sf2_4 <- as.numeric(df$sf2 == 4L) 
  
  
  df$re2_1 <- as.numeric(df$re2 == 1L) 
  
  
  df$re3_1 <- as.numeric(df$re3 == 1L) 
  
  
  
  df$mh3_1 <- as.numeric(df$rmh3 == 1L) 
  df$mh3_2 <- as.numeric(df$rmh3 == 2L) 
  df$mh3_3 <- as.numeric(df$rmh3 == 3L) 
  df$mh3_4 <- as.numeric(df$rmh3 == 4L) 
  df$mh3_5 <- as.numeric(df$rmh3 == 5L) 
  
  
  
  df$mh4_1 <- as.numeric(df$mh4 == 1L) 
  df$mh4_2 <- as.numeric(df$mh4 == 2L) 
  df$mh4_3 <- as.numeric(df$mh4 == 3L) 
  df$mh4_4 <- as.numeric(df$mh4 == 4L) 
  df$mh4_5 <- as.numeric(df$mh4 == 5L) 
  
  
  
  df <- df %>% mutate(PCS12=((-7.23216*pf02_1) + (-3.45555*pf02_2) +(-6.24397*pf04_1) + (-2.73557*pf04_2) +
                               (-4.61617*rp2_1) + (-5.51747*rp3_1) +(-11.25544*bp2_1) + (-8.38063*bp2_2) +
                               (-6.50522*bp2_3) + (-3.80130*bp2_4) + (-8.37399*gh1_1) +(-5.56461*gh1_2) + 
                               (-3.02396*gh1_3) + (-1.31872*gh1_4) + (-2.44706*vt2_1) + (-2.02168*vt2_2) + 
                               (-1.6185*vt2_3) +(-1.14387*vt2_4) + (-0.42251*vt2_5) + (-0.33682*sf2_1) +
                               (-0.94342*sf2_2) + (-0.18043*sf2_3) + (0.11038*sf2_4) +(3.04365*re2_1) + 
                               (2.32091*re3_1) + (3.46638*mh3_1) + (2.90426*mh3_2) + (2.37241*mh3_3) + 
                               (1.36689*mh3_4) + (0.66514*mh3_5) + (4.61446*mh4_1) + (3.41593*mh4_2) +
                               (2.34247*mh4_3) + (1.28044*mh4_4) + (0.41188*mh4_5))+56.57706,
                      MCS12=((3.93115*pf02_1) + (1.8684*pf02_2) +(2.68282*pf04_1) + (1.43103*pf04_2) + (1.4406*rp2_1) +
                               (1.66968*rp3_1) + (1.48619*bp2_1) + (1.76691*bp2_2) +(1.49384*bp2_3) + (0.90384*bp2_4) + 
                               (-1.71175*gh1_1) + (-0.16891*gh1_2) + (0.03482*gh1_3) + (-0.06064*gh1_4) + 
                               (-6.02409*vt2_1) + (-4.88962*vt2_2) + (-3.29805*vt2_3) + (-1.65178*vt2_4) + (-0.92057*vt2_5) + (-6.29724*sf2_1) +
                               (-8.26066*sf2_2) + (-5.63286*sf2_3) + (-3.13896*sf2_4) +(-6.82672*re2_1) + (-5.69921*re3_1) + (-10.19085*mh3_1) +
                               (-7.92717*mh3_2) + (-6.31121*mh3_3) + (-4.09842*mh3_4) +(-1.94949*mh3_5) + (-16.15395*mh4_1) + (-10.77911*mh4_2) +
                               (-8.09914*mh4_3) + (-4.59055*mh4_4) + (-1.95934*mh4_5))+60.75781)
  return(df)
  
}

#BPNI scoring
score.bpni <- function(df=NULL){
  df <- df %>% mutate_at(vars(paste0("bpni_",c(1:28))),as.numeric)%>% 
    mutate(bpni_exploit = ifelse(rowSums(is.na(df[paste0("bpni_", c(1,4,6,11))]))==0, 
                                 rowSums(df[paste0("bpni_", c(1,4,6,11))]),NA),
           bpni_enhancement = ifelse(rowSums(is.na(df[paste0("bpni_", c(10,12,19,24))]))==0, 
                                     rowSums(df[paste0("bpni_", c(10,12,19,24))]),NA),
           bpni_grandiosity = ifelse(rowSums(is.na(df[paste0("bpni_", c(13,17,25,26))]))==0, 
                                     rowSums(df[paste0("bpni_", c(13,17,25,26))]),NA),
           bpni_esteem = ifelse(rowSums(is.na(df[paste0("bpni_", c(2,16,18,21))]))==0, 
                                rowSums(df[paste0("bpni_", c(2,16,18,21))]),NA),
           bpni_hiding = ifelse(rowSums(is.na(df[paste0("bpni_", c(3,15,27,28))]))==0, 
                                rowSums(df[paste0("bpni_", c(3,15,27,28))]),NA),
           bpni_devaluing = ifelse(rowSums(is.na(df[paste0("bpni_", c(14,9,27,20))]))==0, 
                                   rowSums(df[paste0("bpni_", c(14,9,27,20))]),NA),
           bpni_rage = ifelse(rowSums(is.na(df[paste0("bpni_", c(5,8,22,23))]))==0, 
                              rowSums(df[paste0("bpni_", c(5,8,22,23))]),NA)
    )
  return(df)
}






