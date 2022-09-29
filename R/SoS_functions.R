#------------------------------------------------------------------------------#
#- Help functions
#------------------------------------------------------------------------------#


#' get_Rpkg_apiVersion
#' 
#' Returns version of the API used in this R package
#'
#' @return string with version of API for which the package is 
#'         developed
#'
#' examples 
#' get_Rpkg_apiVersion()
get_Rpkg_apiVersion <- function(){
  version_kod <- "v1"
}

#' get_apiDoc_url
#'
#' Return path of documentation of API
#'
#' @return string with url to API documentation
#'
#' examples
#' get_apiDoc_url()
get_apiDoc_url <- function() {
  url_doc <- "https://sdb.socialstyrelsen.se/sdbapi.aspx"
}

#' set_userAgent
#'
#' Set user agent
#'
#' @return
#'
#' examples
#' set_userAgent()
set_userAgent <- function() {
  ua <- httr::user_agent("https://github.com/kerni714/HealthWelfareStatDB")
}

#' hwdb_api
#' 
#' Sends request to server
#'
#' @param version - API version
#' @param lang - language
#' @param topic - topic that should be queried
#' @param resultquery - part of the query that is the remainder after topic
#'
#' @return server response S3 object of class "hwdb_api"
#'
#' examples
hwdb_api <- function(version, lang, topic, resultquery) {
  
  #- Construct path
  path <- paste0("/",version,"/",lang,"/",topic, "/",resultquery)
  path <- stringr::str_replace_all(path, "//+", "/")
  
  #- Construct url
  url <- update_url("https://sdb.socialstyrelsen.se/api", path = path)
  url <- stringr::str_replace_all(url, "//+", "/")
  #print(url)
  #- Send request to server
  resp <- httr::GET(url,set_userAgent())
  
  #- Check server response -----------------------------
  if (httr::http_error(resp)) {
    part1 <- "Socialstyrelsen Statistical Database API request failed with server message: "
    part2 <- "Url: "
    part3 <- "API documentation: "
    #- Check if more than one measure (matt) in query
    matt_ind <- unlist(gregexpr('matt', url))[1]
    next_str <- substr(url, matt_ind+5, 1000)
    fsl_ind <- unlist(gregexpr('/', next_str))[1]
    if (fsl_ind > 2) {
      part4 <- "It appears that more than one measure (matt) has been queried, which is not allowed by the API"  
    }
    
    err_msg <- paste0(part1, httr::status_code(resp), "\n", part2, url, "\n", part3, get_apiDoc_url(),
                      "\n", part4)
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

#' update_url
#' Modify url based on the additional path of the query
#'
#' @param base base url to the API 
#' @param path API url path
#'
#' @return string with the complete url
#'
#' examples
update_url <- function (base, path) {
  paste0(base,"/",path)
}

 
#' print
#'
#' Print function for hwdb_api object
#'
#' @param x object of type hwdb_api
#' @param ... arguments to be passed to methods
#'
#' examples
print.hwdb_api <- function(x, ...) {
  cat("<SoS ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}

#' contentToDataframe_meta
#'
#' Converts content of meta data S3 objects of class hwdb_api  to data frame
#'
#' @param resp_object S3 object of class hwdb_api
#'
#' @return dataframe
#'
#' examples: see function return_meta
contentToDataframe_meta <- function(resp_object) {
  #- Check input
  stopifnot(class(resp_object)=="hwdb_api")
  df <- do.call(rbind.data.frame, resp_object[[1]])
}

#' contentToDataframe_data
#'
#' Converts content of list of S3 objects of class hwdb_api  to data frame
#' @param resp_object_list list of S3 objects of class hwdb_api
#' @param addText logical indicating whether variables containing text labels 
#'                should be added to the id variables, the former variables
#'                will be returned as ordered factors   
#'
#' @return data frame
#'
#' examples see function return_data
contentToDataframe_data <- function(resp_object_list, addText) {
  
  #- Check first that input is a list, and value of addText
  # (class of list elements checked below)
  stopifnot(is.list(resp_object_list), addText %in% c(TRUE,FALSE))
  
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
  
  #- If commas in the value variable, replace to dot
  df[,"varde"] <- gsub(",", ".", gsub("\\.", "", df[,"varde"]))
  
  if (addText==TRUE) {
    #- Extract path components (lang & topic)
    path_comp <- strsplit(resp_object_i$path,"/")[[1]]
    lang <- path_comp[3]
    topic <-path_comp[4]
    
    df <- addTextToData(df=df, lang=lang, topic=topic)
  }
  else {
    return(df)  
  }
}

#' addTextToData
#'
#' Adds variables containing text labels to the id variables.
#' @param df dataframe 
#' @param lang language
#' @param topic topic
#'
#' @return dataframe
#'
#' examples see function return_data
addTextToData <- function(df, lang, topic){
  
  #- Check of input: df
  stopifnot(is.data.frame(df))
  
  #- Check of input: lang
  languages <- return_meta(type="lang")
  langs <- languages[,1]
  stopifnot(lang %in% langs)
  
  #- Check of input: topic
  topics <- return_meta(type="topic", lang=lang)
  topics <- topics[,1]
  stopifnot(topic %in% topics)
  
  #- Extract variables
  vars <- return_meta(type="var", lang=lang, topic=topic)
  
  #- Add text
  for (i in 1:length(vars[,1])) {
    var_i <- vars[i,1]
    #print(var_i)
    if(var_i != "diagnos" & var_i != "ar") {
      #if(var_i != "ar") {
      #- Find variable categories
      var_cats <- return_meta(type="var_cat", lang=lang, topic=topic, var=var_i)
      #print(var_cats)
      nameId <- paste0(var_i,"Id")
      nameText <- paste0(var_i,"Text")
      #- Change name temporarily to enable left_join
      nd <- names(df)
      ind_tmp <- which(nd==nameId)
      nd[ind_tmp] <- "tmp"
      names(df) <- nd
      #- Join dataset with text to dataset with id
      df <- dplyr::left_join(df, var_cats[,c("id","text")], by = c("tmp"="id"))
      #- Change name back
      nd <- names(df)
      nd[ind_tmp] <- nameId
      names(df) <- nd
      #- Make text variable as ordered factor
      df[,"text"] <- factor(df[,"text"], ordered = TRUE, levels = var_cats[,"text"])
      nd <- names(df)
      nd[length(nd)] <- nameText
      names(df) <- nd
    }
  }
  return(df)
}

#------------------------------------------------------------------------------#
#- User functions
#------------------------------------------------------------------------------#

#' return_meta
#'
#' Returns API metadata, lists all values for the queried parameter
#' 
#' @param type type of metadata, should be one of: 
#'             "api_version" for version of api
#'             "lang" for language
#'             "topic" of data
#'             "var" for the variable
#' @param lang language (depends on version)
#' @param topic topic of data (depends on language)
#' @param var   variable (depends on topic) 
#'
#' @return dataframe with id and text variables, sometimes also additional 
#'         variables
#' @export
#'
#' @examples
#' 
#' versions <- return_meta(type="api_version")
#' print(versions)
#'
#' languages <- return_meta(type="lang")
#' print(languages)
#'
#' topics <- return_meta(type="topic", lang="en")
#' print(topics)

#' vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
#' print(vars[,1:2])

#' var_cats_alder <- return_meta(type="var_cat", lang="en", 
#'                    topic="diagnoserislutenvard", var="alder")
#' print(var_cats_alder)

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
    version <- get_Rpkg_apiVersion()
    if (type=="lang") {
      lang <- ""
      topic <- ""
      resultquery <- ""
    }
    if (type=="topic") {
      #- Check input for correct value of input variable lang
      languages <- return_meta(type="lang")
      langs <- languages[,1]
      stopifnot(lang %in% langs)
      
      topic <- ""
      resultquery <- ""
      
    }  
    if (type=="var") {
      #- Check input for correct value of input variables lang and topic
      languages <- return_meta(type="lang")
      langs <- languages[,1]
      stopifnot(lang %in% langs)
      topics <- return_meta(type="topic", lang=lang)
      topics <- topics[,1]
      stopifnot(topic %in% topics)
      
      resultquery <- ""
    }
    if (type=="var_cat") {
      #- Check input for correct value of input variables lang, topic and
      # variable
      languages <- return_meta(type="lang")
      langs <- languages[,1]
      stopifnot(lang %in% langs)
      topics <- return_meta(type="topic", lang=lang)
      topics <- topics[,1]
      stopifnot(topic %in% topics)
      vars <- return_meta(type="var", lang=lang, topic=topic)
      vars <- vars[,1]
      stopifnot(var %in% vars)
      
      resultquery <- var
    }
  }
  
  #- Make call to API
  resp_object <- hwdb_api(version=version,lang=lang, topic = topic, 
                          resultquery = resultquery)
  
  df <- contentToDataframe_meta(resp_object)
}


#' return_data
#'
#' Returns data from query
#'
#' @param lang language
#' @param topic topic of data 
#' @param df_input_vars data frame with two variables: var_list and values_list,
#'                      for each of the variables available for the topic it
#'                      should contain the values for which data is desired, see
#'                      example below 
#' @param addText logical indicating whether variables containing text labels 
#'                should be added to the id variables, the former variables
#'                will be returned as ordered factors
#'
#' @return data frame
#' @export
#'
#' @examples
#' vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
#' var_list <- vars[,1]
#' #- Region=entire Sweden, age group=, sex= males and females, measure=
#' # Number of patients per 100,000 inhabitants years = 2012 and 2013, 
#' # diagnoses= J13 and J14
#' values_list <- c("0",
#'                  "45-49",
#'                  "1,2",
#'                   "7",
#'                   "2012,2013",
#'                   "J13,J14")
#'
#' df_input_vars <- as.data.frame(cbind(var_list,values_list))
#' print(df_input_vars)
#'
#' data <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars, 
#' addText=TRUE)
#'

return_data <- function (lang,topic,df_input_vars, addText) {
  #- Check input variable lang
  languages <- return_meta(type="lang")
  langs <- languages[,1]
  stopifnot(lang %in% langs)
  
  #- Check input variable topic
  topics <- return_meta(type="topic", lang=lang)
  topics <- topics[,1]
  stopifnot(topic %in% topics)
  
  #- Check structure of df_input_vars
  stopifnot(is.data.frame(df_input_vars),length(df_input_vars) == 2,
            colnames(df_input_vars) == c("var_list","values_list"))
  
  #- Check addText
  stopifnot(addText %in% c(TRUE,FALSE))
  #- Check contents of df_input_vars:
  #- Check so that variables are as in the list for selected languange and
  #  topic
  vars <- return_meta(type="var", lang=lang, topic=topic)
  vars_lang_topic_char <- as.character(vars[1][,1])
  vars_input_char <- as.character(df_input_vars$var_list)
  comparison_var <- (vars_lang_topic_char %in% vars_input_char)
  
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
    var_cats <- return_meta(type="var_cat", lang=lang, topic=topic, var=var)
    
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
  version <- get_Rpkg_apiVersion()
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
  
  df <- contentToDataframe_data(resp_object_list, addText=addText)
}    

#- END OF FILE ----------------------------------------------------------------#