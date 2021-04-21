redcap_upload<-function (ds_to_write, batch_size = 100L, interbatch_delay = 0.5, retry_whenfailed=T,
                         continue_on_error = FALSE, redcap_uri, token, verbose = TRUE, NAoverwrite = F,
                         config_options = NULL) 
{
  start_time <- base::Sys.time()
  if (base::missing(redcap_uri)) 
    base::stop("The required parameter `redcap_uri` was missing from the call to `redcap_write()`.")
  if (base::missing(token)) 
    base::stop("The required parameter `token` was missing from the call to `redcap_write()`.")
  #token <- REDCapR::sanitize_token(token)
  ds_glossary <- REDCapR::create_batch_glossary(row_count = base::nrow(ds_to_write), 
                                                batch_size = batch_size)
  affected_ids <- character(0)
  excluded_ids <- 
    lst_status_code <- NULL
  lst_outcome_message <- NULL
  success_combined <- TRUE
  message("Starting to update ", format(nrow(ds_to_write), 
                                        big.mark = ",", scientific = F, trim = T), " records to be written at ", 
          Sys.time())
  for (i in seq_along(ds_glossary$id)) {
    selected_indices <- seq(from = ds_glossary[i, "start_index"], 
                            to = ds_glossary[i, "stop_index"])
    if (i > 0) 
      Sys.sleep(time = interbatch_delay)
    message("Writing batch ", i, " of ", nrow(ds_glossary), 
            ", with indices ", min(selected_indices), " through ", 
            max(selected_indices), ".")
    
    write_result <- redcap_oneshot_upload(ds = ds_to_write[selected_indices,], previousIDs = NULL,retry_whenfailed = T,
                                          redcap_uri = redcap_uri, token = token, verbose = verbose,
                                          NAoverwrite = NAoverwrite,
                                          config_options = config_options)
    lst_status_code[[i]] <- write_result$status_code
    lst_outcome_message[[i]] <- write_result$outcome_message
    if (!write_result$success) {
      error_message <- paste0("The `redcap_write()` call failed on iteration ", 
                              i, ".")
      error_message <- paste(error_message, ifelse(!verbose, 
                                                   "Set the `verbose` parameter to TRUE and rerun for additional information.", 
                                                   ""))
      if (continue_on_error) 
        warning(error_message)
      else stop(error_message)
    }
    affected_ids <- c(affected_ids, write_result$affected_ids)
    success_combined <- success_combined | write_result$success
    excluded_ids <- c(excluded_ids, write_result$excludedIDs)
    rm(write_result)
  }
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, 
                                         units = "secs"))
  status_code_combined <- paste(lst_status_code, collapse = "; ")
  outcome_message_combined <- paste(lst_outcome_message, collapse = "; ")
  excluded_ids <- excluded_ids[excluded_ids!=""]
  return(list(success = success_combined, status_code = status_code_combined,excluded_ids=excluded_ids,
              outcome_message = outcome_message_combined, records_affected_count = length(affected_ids), 
              affected_ids = affected_ids, elapsed_seconds = elapsed_seconds))
}

redcap_oneshot_upload<-function (ds, redcap_uri, token, verbose = TRUE, NAoverwrite = F,config_options = NULL,retry_whenfailed=F,previousIDs=NULL) {
  overwriteBehavior = ifelse(NAoverwrite,"overwrite","normal")
  start_time <- Sys.time()
  csvElements <- NULL
  if (missing(redcap_uri)) 
    stop("The required parameter `redcap_uri` was missing from the call to `redcap_write_oneshot()`.")
  if (missing(token)) 
    stop("The required parameter `token` was missing from the call to `redcap_write_oneshot()`.")
  #token <- REDCapR::sanitize_token(token)
  con <- base::textConnection(object = "csvElements", open = "w", 
                              local = TRUE)
  utils::write.csv(ds, con, row.names = FALSE, na = "")
  close(con)
  csv <- paste(csvElements, collapse = "\n")
  rm(csvElements, con)
  post_body <- list(token = token, content = "record", format = "csv", 
                    type = "flat", data = csv, overwriteBehavior = overwriteBehavior, 
                    returnContent = "ids", returnFormat = "csv")
  result <- httr::POST(url = redcap_uri, body = post_body, 
                       config = config_options)
  status_code <- result$status_code
  raw_text <- httr::content(result, type = "text")
  
  elapsed_seconds <- as.numeric(difftime(Sys.time(), start_time, 
                                         units = "secs"))
  success <- (status_code == 200L)
  if (success) {
    elements <- unlist(strsplit(raw_text, split = "\\n"))
    affectedIDs <- elements[-1]
    recordsAffectedCount <- length(affectedIDs)
    outcome_message <- paste0(format(recordsAffectedCount, 
                                     big.mark = ",", scientific = FALSE, trim = TRUE), 
                              " records were written to REDCap in ", round(elapsed_seconds, 
                                                                           1), " seconds.")
    raw_text <- ""
  }
  else {
    if(retry_whenfailed){
      outcome_message<-outcome_message <- paste0("The upload was not successful:\n", 
                                                 raw_text,"\n","But we will try again...\n")
      sp_rawtext<-strsplit(raw_text,split = "\\n")[[1]]
      allgx<-lapply(sp_rawtext,function(x){xa<-strsplit(gsub("\"","",x),",")[[1]];})
      mxID<-sapply(allgx,function(sp_rawtext){gsub("ERROR: ","",sp_rawtext[1])})
      allIDs<-c(previousIDs,mxID)
      negPos<-as.numeric(na.omit(sapply(allIDs,function(IDX){
        #print(IDX)
        a<-unique(which(ds==IDX,arr.ind = T)[,1]);
        if(length(a)>0){a}else{NA}
      })))
      ds_new<-ds[-negPos,]
      gx<-redcap_oneshot_upload(ds = ds_new, redcap_uri = redcap_uri, token = token, verbose = verbose, 
                                retry_whenfailed = T,previousIDs = allIDs,
                                config_options = config_options)
      raw_text<-paste(raw_text,gx$raw_text,sep = "re-try: ")
      success<-gx$success
      status_code<-gx$status_code
      outcome_message<-paste(outcome_message,gx$outcome_message,sep = "re-try: ")
      recordsAffectedCount<-gx$records_affected_count
      affectedIDs<-gx$affected_ids
      elapsed_seconds<-gx$elapsed_seconds
      previousIDs<-gx$excludedIDs
    } else {
      affectedIDs <- numeric(0)
      recordsAffectedCount <- NA_integer_
      outcome_message <- paste0("The REDCapR write/import operation was not successful.  The error message was:\n", 
                                raw_text)
    }
  }
  if (verbose) 
    message(outcome_message)
  if (!is.null(previousIDs)){excludedIDs<-previousIDs}else {excludedIDs<-""}
  return(list(success = success, status_code = status_code, 
              outcome_message = outcome_message, records_affected_count = recordsAffectedCount, 
              affected_ids = affectedIDs, elapsed_seconds = elapsed_seconds, excludedIDs = excludedIDs,
              raw_text = raw_text))
}
