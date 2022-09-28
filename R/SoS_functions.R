#------------------------------------------------------------------------------#
#- Help functions
#------------------------------------------------------------------------------#


#- Return version of the API used in this R package
set_Rpkg_apiVersion <- function(){
  version_kod <- "v1"
}

#- Return path of documentation of API
set_apiDoc_url <- function() {
  url_doc <- "https://sdb.socialstyrelsen.se/sdbapi.aspx"
}

#- Set user agent
set_userAgent <- function() {
  ua <- httr::user_agent("https://github.com/kerni714/HealthWelfareStatDB")
}

#- Requests to API
hwdb_api <- function(version, lang, topic, resultquery) {
  
  #- Construct path
  path <- paste0("/",version,"/",lang,"/",topic, "/",resultquery)
  path <- stringr::str_replace_all(path, "//+", "/")
  
  #- Construct url
  url <- update_url("https://sdb.socialstyrelsen.se/api", path = path)
  url <- stringr::str_replace_all(url, "//+", "/")
  
  #- Send request to server
  resp <- httr::GET(url,set_userAgent())
  
  #- Check server response -----------------------------
  if (httr::http_error(resp)) {
    part1 <- "Socialstyrelsen Statistical Database API request failed with server message: "
    part2 <- "Url: "
    part3 <- "API documentation: "
    err_msg <- paste0(part1, httr::status_code(resp), "\n", part2, url, "\n", part3, set_apiDoc_url())
    stop(err_msg, call. = FALSE)
  }
  #- Check for json output
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  #- Parse output
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)
  
  #- Construct output, S3 object
  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "hwdb_api"
  )
}

#- Modify url based on the additional path of the query
update_url <- function (base, path) {
  paste0(base,"/",path)
}

#------------------------------------------------------------------------------#
#- User functions
#------------------------------------------------------------------------------#

#- Print function for hwdb_api object
print.hwdb_api <- function(x, ...) {
  cat("<SoS ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

#- Returns API metadata

return_meta <- function(type,lang,topic,var) {
  #- Checks of input --------------------------------------#
  #- Construct vector of allowed inputs for variable type
  types <- c("api_version","lang","topic","var","var_cat")
  
  #- Check if type is in any of the allowed keywords
  stopifnot(type %in% types)
  
  if (type=="api_version") {
    version <- ""
    lang <- ""
    topic <- ""
    resultquery <- ""
  }
  else {
    version <- set_Rpkg_apiVersion()
    if (type=="lang") {
      lang <- ""
      topic <- ""
      resultquery <- ""
    }
    if (type=="topic") {
      #- Check input for correct value of input variable lang
      languages_obj <- return_meta(type="lang")
      languages <- contentToDataframe_meta(languages_obj)
      langs <- languages[,1]
      stopifnot(lang %in% langs)
      
      topic <- ""
      resultquery <- ""
      
    }  
    if (type=="var") {
      #- Check input for correct value of input variables lang and topic
      languages_obj <- return_meta(type="lang")
      languages <- contentToDataframe_meta(languages_obj)
      langs <- languages[,1]
      topics_obj <- return_meta(type="topic", lang=lang)
      topics <- contentToDataframe_meta(topics_obj)
      topics <- topics[,1]
      stopifnot(lang %in% langs,topic %in% topics)
      
      resultquery <- ""
    }
    if (type=="var_cat") {
      #- Check input for correct value of input variables lang, topic and
      # variable
      languages_obj <- return_meta(type="lang")
      languages <- contentToDataframe_meta(languages_obj)
      langs <- languages[,1]
      topics_obj <- return_meta(type="topic", lang=lang)
      topics <- contentToDataframe_meta(topics_obj)
      topics <- topics[,1]
      vars_obj <- return_meta(type="var", lang=lang, topic=topic)
      vars <- contentToDataframe_meta(vars_obj)
      vars <- vars[,1]
      stopifnot(lang %in% langs,topic %in% topics, var %in% vars)
      
      resultquery <- var
    }
  }
  
  #- Make call to API
  resp_object <- hwdb_api(version=version,lang=lang, topic = topic, 
                         resultquery = resultquery)
}

#- Returns data
return_data <- function (lang,topic,df_input_vars) {
  #- Check input variable lang
  languages_obj <- return_meta(type="lang")
  languages <- contentToDataframe_meta(languages_obj)
  langs <- languages[,1]
  stopifnot(lang %in% langs)
  
  #- Check input variable topic
  topics_obj <- return_meta(type="topic", lang=lang)
  topics <- contentToDataframe_meta(topics_obj)
  topics <- topics[,1]
  stopifnot(topic %in% topics)
  
  #- Check structure of df_input_vars
  stopifnot(is.data.frame(df_input_vars),length(df_input_vars) == 2,
            colnames(df_input_vars) == c("var_list","values_list"))
  
  #- Check contents of df_input_vars:
  #- Check so that variables are as in the list for selected languange and
  #  topic
  vars_obj <- return_meta(type="var", lang=lang, topic=topic)
  vars <- contentToDataframe_meta(vars_obj)
  vars_lang_topic_char <- as.character(vars[1][,1])
  vars_input_char <- as.character(df_input_vars$var_list)
  comparison_var <- (vars_lang_topic_char %in% vars_input_char)
  
  #if (all(comparison_var)!=TRUE) {
  if (!all(comparison_var)) {
    err_msg <- paste0("Need to include all variables in df$input_vars: ",
                      vars_lang_topic_char) 
    stop(err_msg) 
  }
  
  #- Check values for the input variables
  for (i in 1:nrow(df_input_vars))  {
    var <- df_input_vars[i,1]
    value <- df_input_vars[i,2]
    
    #- Manage value vector ---------------------------------#
    #- Split values separated by commas
    split_value <- strsplit(value,",")
    value <- as.vector(split_value[[1]])
    #- Remove spaces if any occuring in values
    value <- gsub(" ", "", value)
    #- Convert to numeric if numeric values, otherwise keep as character
    suppressWarnings(num <- as.numeric(value))
    if (!(any(is.na(num)))) {
      value <- as.numeric(value)  
    }
    
    #- Extract the possible values/categories for the variable i
    var_cats_obj <- return_meta(type="var_cat", lang=lang, topic=topic, var=var)
    var_cats<- contentToDataframe_meta(var_cats_obj)
    
    #- Compare values in df_input_vars with values in the API
    comparison_val <- (value %in% var_cats[,1])
    
    if (!all(comparison_val)) {
      err_msg <- paste0("All values in df_input_vars$values_list for variable ", var, " are not in the value list (categories) for ", var)
      stop(err_msg)
    }
  }
  
  #- Construct result query ---------------------------------#
  resultquery <- "/resultat/"
  for (i in 1:nrow(df_input_vars))  {
    res_i <- paste0(df_input_vars[i,1],"/",df_input_vars[i,2],"/")
    resultquery <- paste0(resultquery,res_i)
  }
  #- Obtain version
  version <- set_Rpkg_apiVersion()
  resp_object <- hwdb_api(version=version,lang=lang, topic = topic,
                         resultquery = resultquery)
  
  #- Put response object in list for output
  resp_object_list <- list(resp_object)
  
  #- Handle pagination ----------------------------------------#
  pages <- resp_object[[1]]$sidor
  if (pages > 1) {
    i <- 2
    while (i <= pages) {
      resultquery_i <- paste0(resultquery,"?sida=",i)
      resp_object <- hwdb_api(version, lang, topic, resultquery)
      resp_object_list[[i]] <- resp_object
      i <- i +1
    }
  }
  return(resp_object_list)
}    

#- Converts meta data list content to data frame
contentToDataframe_meta <- function(resp_object) {
  #- Check input
  stopifnot(class(resp_object)=="hwdb_api")
  df <- do.call(rbind.data.frame, resp_object[[1]])
}

#- Converts data list content to data frame
contentToDataframe_data <- function(resp_object_list) {
  
  #- Check first that input is a list
  stopifnot(is.list(resp_object_list))
  
  #- Loop over list objects to extract data
  for (i in 1:length(resp_object_list)) {
    resp_object_i = resp_object_list[[i]]
    
    #- Check input
    stopifnot(class(resp_object_i)=="hwdb_api")
    
    df_i <- do.call(rbind.data.frame, resp_object_i[[1]][[1]])
    if (i==1) {
      df <- df_i
    }
    else {
      df <- rbind(df,df_i)
    }
  }
  return(df)
}

#- END OF FILE ----------------------------------------------------------------#