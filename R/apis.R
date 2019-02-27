

# Parameters of the script -----------------------------------------------
defaultTimeout <- 5 #Timeout for the API calls
base_url <- "http://rxnav.nlm.nih.gov/"




#' Get RxNorm Approximate Terms
#'
#' Do an approximate match search to determine the strings in the data set that
#' most closely match the search string. See
#' \href{https://rxnav.nlm.nih.gov/RxNormAPIs.html#uLink=RxNorm_REST_getApproximateMatch}{RxNorm}.
#' @param term Search string.
#' @param maxEntries The maximum number of entries to return (default 20)
#' @param option Special processing options.  0 - no special processing. 1 -
#'   return only information for terms contained in valid RxNorm concepts. That
#'   is, the term must either be from the RxNorm vocabulary or a synonym of a
#'   (non-suppressed) term in the RxNorm vocabulary.
#' @return Approximate terms.
#' @export
#' @examples
#' rx_approximate_term("lipitor")
#' rx_approximate_term(term = "zocor 2010 20mm", trunc = F)
#' rx_approximate_term("lipitor", max_rank = 6)
#' rx_approximate_term("lipitor", max_rank = 6, min_match_score = 40)
rx_approximate_term <- function(term,
                                maxEntries = 20,
                                option = 0,
                                trunc = T,
                                max_rank = 1,
                                min_match_score = 0) {
  params <- list(term = term,
                 maxEntries = maxEntries,
                 option = option)
  r <- GET(base_url, path = "REST/approximateTerm.json", query = params)
  if(fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$approximateGroup$comment=="No drugs identified; ") {
    return(paste("No drugs identified for given search term"))
  }

  if(trunc == T){
    r <- fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$`approximateGroup`$candidate
    r$rank <- as.numeric(r$rank)
    r$score <- as.numeric(r$score)
    r %>%  dplyr::filter(rank <= max_rank,
                 score >= min_match_score) %>%
      dplyr::select(-rxaui) %>%
          dplyr::distinct(rxcui, .keep_all = T)
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Retrieve class information from a class identifier.
#'
#' Intakes a classId and outputs the class name and class type
#'
#' @param classId
#' @return
#' @export
#' @examples
#' class_by_id(classId = "B01AA")
#' class_by_id("D000998", trunc = FALSE)
class_by_id <- function(classId, trunc = T){
  params <- list(classId = classId)
  r <- httr::GET(base_url, path = "REST/rxclass/class/byId.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassMinConceptList$`rxclassMinConcept`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }

}




#'  Retrieve class information from a class name.
#' @param className  The name of the class
#' @param classTypes (optional) a list of class types to check. See
#'   /classtypes for a list of the possible class types. If none are specified,
#'   all class types are checked for the name.
#' @return
#' @export
#' @examples
#' class_by_name(className = "radiopharmaceuticals")
#' class_by_name(className = "Alkylating Agents", classTypes = 'CHEM')
#' class_by_name(className = "Alkylating Agents", classTypes = c('CHEM', 'MESHPA'))

class_by_name <- function(className, classTypes = NULL, trunc = T){
  query_string <- paste0("className=", className,
                         ifelse(!is.null(classTypes), paste0('&classTypes=', paste0(classTypes, collapse = "+")),""))
  query_string <- gsub(" ", "%20", query_string)

  r <- GET(paste0(base_url, "REST/rxclass/class/byName.json?", query_string))
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassMinConceptList$`rxclassMinConcept`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Find similar classes of drug members
#'
#' The input drug class is defined by the class identifier (classId), the source
#' of drug relations (relaSource) and the relationship (rela) of the drugs to
#' the class.
#'
#' @param classId
#' @param relaSource
#' @param rela
#' @param scoreType
#' @param top
#' @param equivalenceThreshold
#' @param inclusionThreshold
#' @return
#' @export
#' @examples
#' class_similar(classId ='L01XE', relaSource='ATC', scoreType=1, top=4)
#' class_similar(classId ='D002318 ', relaSource='MEDRT', rela = 'CI_with')
class_similar <- function(classId,
                          relaSource = NULL,
                          rela = NULL,
                          scoreType = 0,
                          top = 10,
                          equivalenceThreshold = NULL,
                          inclusionThreshold = NULL,
                          trunc = T) {
  params <- list(classId = classId, relaSource = relaSource, rela = rela,
                 scoreType = scoreType, top = top, equivalenceThreshold = equivalenceThreshold,
                 inclusionThreshold = inclusionThreshold)
  r <- GET(base_url, path = 'REST/rxclass/class/similar.json', query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$similarityMember
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Find similar classes for a list of drugs specified by RxNorm identifiers.
#'
#'
#' @param rxcuis a list of RxNorm drug identifiers (RxCUIs). A maximum of 500 are permitted.
#' @param relaSource
#' @param rela
#' @param scoreType
#' @param top
#' @param equivalenceThreshold
#' @param inclusionThreshold
#' @return
#' @export
#' @examples
#' similar_by_rxcuis(rxcuis = 7052, top = 10)
#' similar_by_rxcuis(rxcuis = c(7052, 7676, 7804, 23088), top = 3)
#' similar_by_rxcuis(rxcuis = c(7052, 7676, 7804, 23088), top = 3, relaSource = 'DAILYMED')
similar_by_rxcuis <- function(rxcuis,
                              relaSource = NULL,
                              rela = NULL,
                              scoreType = 0,
                              top = 10,
                              equivalenceThreshold = NULL,
                              inclusionThreshold = NULL,
                              trunc = T){
  query_string = paste0("rxcuis=", paste(rxcuis, collapse = "+"),
                        ifelse(!is.null(relaSource), paste0('&relaSource=', relaSource),""),
                        ifelse(!is.null(rela), paste0('&rela=', rela), ""),
                        ifelse(!is.null(scoreType), paste0('&scoreType=', scoreType), ""),
                        ifelse(!is.null(top), paste0('&top=', top), ""),
                        ifelse(!is.null(equivalenceThreshold), paste0('&equivalenceThreshold=', equivalenceThreshold), ""),
                        ifelse(!is.null(inclusionThreshold), paste0('&inclusionThreshold=', inclusionThreshold), ""))
  query_string <- gsub(" ", "%20", query_string)

  r <- GET(paste0(base_url, "REST/rxclass/class/similarByRxcuis.json?", query_string))
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$similarityMember$rankClassConcept
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get all classes for each specified class type.
#' @param classTypes
#' @return
#' @export
#' @examples
#'classes_all('MOA')
#'
classes_all <- function(classTypes, trunc = T){
  params <- list(classTypes = classTypes)
  r <- GET(base_url, path = "REST/rxclass/allClasses.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassMinConceptList$`rxclassMinConcept`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Returns the class type and value of a specified drug
#'
#' Intakes a RxCUI identifier and outputs all the classes for the specified
#' class type and database
#'
#' @param rxcui
#' @param relaSource
#' @param relas
#' @return Dataframe of all the classes for the given drug RxCUI, database(s) and relationship
#' @export
#' @examples
#' class_by_rxcui(rxcui = 7052, relaSource = "DAILYMED")
#' class_by_rxcui(rxcui = 7052, relaSource = "MEDRT", relas = "may_treat")
#' class_by_rxcui(rxcui = 7052, relaSource = "MEDRT", relas = c("may_treat", "ci_with"))

class_by_rxcui <- function(rxcui,
                           relaSource = NULL,
                           relas = NULL,
                           trunc = T) {
  query_string = paste0("rxcui=", rxcui,
                        ifelse(!is.null(relaSource), paste0('&relaSource=', relaSource),""),
                        ifelse(!is.null(relas), paste0('&relas=', paste0(relas, collapse = "+")), ""))
  query_string <- gsub(" ", "%20", query_string)
  r <- GET(paste0(base_url, "REST/rxclass/class/byRxcui.json?", query_string))
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassDrugInfoList$`rxclassDrugInfo`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get the classes containing a specifed drug name as a member.
#'
#' The user may specify a source of drug-class relations and relationship values
#' to filter the output returned.
#'
#' @param drugName
#' @param relaSource
#' @param relas
#' @return
#' @export
#' @examples
#' class_by_drug_name(drugName = 'armodafinil')
#' class_by_drug_name(drugName = 'armodafinil', relaSource = 'MEDRT')
#' class_by_drug_name(drugName = 'armodafinil', relaSource = 'MEDRT', relas = c('has_MOA', 'has_PE'))

class_by_drug_name <- function(drugName,
                               relaSource = NULL,
                               relas = NULL,
                               trunc = T) {
    query_string = paste0("drugName=", drugName,
                        ifelse(!is.null(relaSource), paste0('&relaSource=', relaSource),""),
                        ifelse(!is.null(relas), paste0('&relas=', paste0(relas, collapse = "+")), ""))
  query_string <- gsub(" ", "%20", query_string)
  r <- GET(paste0(base_url, "REST/rxclass/class/byDrugName.json?", query_string))
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassDrugInfoList$`rxclassDrugInfo`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get the class contexts
#'
#' A context is a path from the root of the class hierarchy down to the specified class.
#' @param classId
#' @return
#' @export
#' @examples
#'class_context('D019275')
#'
class_context <- function(classId, trunc = T){
  params <- list(classId = classId)
  r <- GET(base_url, path = "REST/rxclass/classContext.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$classPathList$classPath$rxclassMinConcept

  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get the graph of a specified class.
#'
#' Returns the ancestors of a specified class in a graph form
#'
#' @param classId The class identifier
#' @param source The class types. See class_types() function for list of all
#'   possible class types
#' @return
#' @export
#' @examples
#' class_graph(classId = 'D003879', source = 'MESHPA')
class_graph <- function(classId, source, trunc = T){
  params = list(classId = classId, source = source)
  r <- GET(base_url, path = "REST/rxclass/classGraph.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassGraph
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get the drug members of a specified class.
#'
#' This function requires the specification of a source of drug-class
#' relationships (for example: DailyMed) as well as a relationship to the class.
#' See the table in the input section for the list of possible values for these
#' parameters.
#'
#' @param classId the class identifier. Note that this is NOT an RxNorm
#'   identifier, but an identifier from the source vocabulary
#' @param relaSource the source asserting the relationships between the drug
#'   members and the drug class.
#' @param relas the relationship of the drug class to its members. See table
#'   below for valid relationship values.
#' @param trans (optional) 0 = include indirect and direct relations (the
#'   default). 1 = direct relations only.
#' @param ttys (optional) include only drugs with the specified RxNorm term
#'   types. Default is IN, PIN and MIN.
#' @return
#' @export
#' @examples
#' class_members(classId = 'N0000008638', relaSource = 'MEDRT', rela = 'has_PE', ttys = 'IN')
#' class_members(classId = 'N0000008638', relaSource = 'MEDRT', rela = 'has_PE', ttys = c('IN', 'PIN'))

class_members <- function(classId,
                          relaSource = NULL,
                          rela = NULL,
                          trans = NULL,
                          ttys = NULL,
                          trunc = T) {

  query_string = paste0("classId=", classId,
                        ifelse(!is.null(relaSource), paste0('&relaSource=', paste0(relaSource, collapse = "+")),""),
                        ifelse(!is.null(rela), paste0('&rela=', paste0(rela, collapse = "+")), ""),
                        ifelse(!is.null(trans), paste0('&trans=', trans, collapse = "+"), ""),
                        ifelse(!is.null(ttys), paste0('&ttys=', paste0(ttys, collapse = "+")), ""))
  r <- GET(paste0(base_url, "REST/rxclass/classMembers.json?", query_string))
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$drugMemberGroup$`drugMember`
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Get the class tree for a specified class identifier.
#'
#' The class tree represents the classes which are descendents of the specified class.
#' @param classId
#' @return
#' @export
#' @examples
#' class_tree('D019275')
#'
class_tree <- function(classId, trunc = T){
  params <- list(classId = classId)
  r <- GET(base_url, path = "REST/rxclass/classTree.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$rxclassTree$rxclassMinConceptItem

  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#'Get the class types.
#'
#'The resources /class/byName and /allClasses use the class types as filters for the output.
#'
#' @param
#' @return
#' @export
#' @examples
#' class_types()
class_types <- function(){
r<- GET("https://rxnav.nlm.nih.gov/REST/rxclass/classTypes.json")

unlist(fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$classTypeList$classTypeName)
}




#' Get the relationships for a source of drug relations.
#'
#' @param relaSource
#' @return
#' @export
#' @examples
#' class_relas('DAILYMED')
#' class_relas('medrt')
#'
class_relas <- function(relaSource , trunc = T){
  params <- list(relaSource  = relaSource )
  r <- GET(base_url, path = "REST/rxclass/relas.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$relaList$rela

  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#' Return the similarity information between two drug classes.
#'
#' @param classId1
#' @param relaSource1
#' @param rela1
#' @param classId2
#' @param relaSource2
#' @param rela2
#' @return
#' @export
#' @examples
#' class_similar_info(classId1 = 'N02AA', relaSource1 = 'ATC', classId2 = 'D009294', relaSource2 = 'MESH')
class_similar_info <- function(classId1,
                               relaSource1 = NULL,
                               rela1 = NULL,
                               classId2,
                               relaSource2 = NULL,
                               rela2 = NULL,
                               trunc = T){

  params <- list(classId1 = classId1, relaSource1 = relaSource1, rela1 = rela1,
                 classId2 = classId2, relaSource2 = relaSource2, rela2 = rela2)
  r <- GET(base_url, path = "REST/rxclass/class/similarInfo.json", query = params)
  if(trunc == T){
    fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$SimilarityInformation
  }

  else if (trunc == F){
    content(r, encoding = 'ISO-8859-1')
  }
}




#'Get the list of sources that associate generic drugs to the class types.
#'
#' @param
#' @return
#' @export
#' @examples
#' class_relaSources()

class_relaSources <- function(){
  r<- GET("https://rxnav.nlm.nih.gov/REST/rxclass/relaSources.json")

  fromJSON(content(r, 'text', encoding = 'ISO-8859-1'))$relaSourceList$`relaSourceName`
}








