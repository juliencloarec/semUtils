#' @param model
#'
#' @param database
#' @param confidence_interval
#' @param bootstrap_samples
#'
#' @importFrom lavaan sem standardizedSolution
#' @importFrom stringr str_split str_replace str_replace_all
#' @import magrittr
#' @importFrom psych headTail
#' @importFrom knitr kable
#'
#' @export


mediation <- function(model, database, confidence_interval, bootstrap_samples){

  model <- str_split(model, "\n", simplify = TRUE)
  model <- model[model != ""]
  model <- str_replace_all(model, " " , "")

  measurementModel <- model[grepl("=~", model)]

  model <- model[!grepl("=~", model)]

  number_of_lines <- as.numeric(length(model))

  simplifiedmodel = ""

  for (i in 1:number_of_lines) {

    DV <- str_split(model[i], "~", simplify = TRUE)[1]

    IV <- str_split(model[i], "~", simplify = TRUE)[2]

    list_of_IV <- str_split(IV, "\\+", simplify = TRUE)

    number_of_IV <- as.numeric(length(list_of_IV))

    for (j in 1:number_of_IV) {
      simplifiedmodel <- paste(simplifiedmodel, DV, "~", list_of_IV[j], "\n")
    }
  }

  simplifiedmodel <- str_split(simplifiedmodel, "\n", simplify = TRUE)
  simplifiedmodel <- simplifiedmodel[simplifiedmodel != ""]
  simplifiedmodel <- str_replace_all(simplifiedmodel, " " , "")

  simplifiedmodel <- as.data.frame(simplifiedmodel)

  names(simplifiedmodel)[1] <- "regression"

  for (i in 1:nrow(simplifiedmodel)) {
    simplifiedmodel$DV[i] <- str_split(simplifiedmodel$regression[i], "~", simplify = TRUE)[1]
    simplifiedmodel$IV[i] <- str_split(simplifiedmodel$regression[i], "~", simplify = TRUE)[2]
  }

  list_of_SEM_IV <- simplifiedmodel$IV[!simplifiedmodel$IV %in% simplifiedmodel$DV]

  list_of_SEM_IV <- levels(as.factor(list_of_SEM_IV))

  list_of_SEM_DV <- simplifiedmodel$DV[!simplifiedmodel$DV %in% simplifiedmodel$IV]

  list_of_SEM_DV <- levels(as.factor(list_of_SEM_DV))

  simplifiedmodel$Coefficient <- ""

  simplifiedmodel$Is_SEM_IV <- FALSE
  simplifiedmodel$Is_SEM_DV <- FALSE

  for (i in 1:nrow(simplifiedmodel)) {

    simplifiedmodel$Coefficient[i] <- paste0(simplifiedmodel$IV[i],
                                             "_",
                                             simplifiedmodel$DV[i])

    simplifiedmodel$Expression[i] <- paste0(simplifiedmodel$DV[i],
                                            "~",
                                            simplifiedmodel$Coefficient[i],
                                            "*",
                                            simplifiedmodel$IV[i])

    if(simplifiedmodel$IV[i] %in% list_of_SEM_IV){
      simplifiedmodel$Is_SEM_IV[i] <- TRUE
    }

    if(simplifiedmodel$DV[i] %in% list_of_SEM_DV){
      simplifiedmodel$Is_SEM_DV[i] <- TRUE
    }

  }

  path = ""

  for (i in 1:nrow(simplifiedmodel)) {
    for (j in 1:nrow(simplifiedmodel)) {

      if(simplifiedmodel$DV[i] == simplifiedmodel$IV[j] &
         simplifiedmodel$Is_SEM_DV[j] == TRUE &
         simplifiedmodel$Is_SEM_IV[i] == TRUE){

        path <- paste0(path,
                       simplifiedmodel$Coefficient[i],
                       "*",
                       simplifiedmodel$Coefficient[j],
                       " \n")}

      for (k in 1:nrow(simplifiedmodel)) {

        if(simplifiedmodel$DV[i] == simplifiedmodel$IV[j] &
           simplifiedmodel$DV[j] == simplifiedmodel$IV[k] &
           simplifiedmodel$Is_SEM_DV[k] == TRUE &
           simplifiedmodel$Is_SEM_IV[i] == TRUE){

          path <- paste0(path,
                         simplifiedmodel$Coefficient[i],
                         "*",
                         simplifiedmodel$Coefficient[j],
                         "*",
                         simplifiedmodel$Coefficient[k],
                         " \n")}

        for (l in 1:nrow(simplifiedmodel)) {

          if(simplifiedmodel$DV[i] == simplifiedmodel$IV[j] &
             simplifiedmodel$DV[j] == simplifiedmodel$IV[k] &
             simplifiedmodel$DV[k] == simplifiedmodel$IV[l] &
             simplifiedmodel$Is_SEM_DV[l] == TRUE &
             simplifiedmodel$Is_SEM_IV[i] == TRUE){

            path <- paste0(path,
                           simplifiedmodel$Coefficient[i],
                           "*",
                           simplifiedmodel$Coefficient[j],
                           "*",
                           simplifiedmodel$Coefficient[k],
                           "*",
                           simplifiedmodel$Coefficient[l],
                           " \n")}

          for (m in 1:nrow(simplifiedmodel)) {

            if(simplifiedmodel$DV[i] == simplifiedmodel$IV[j] &
               simplifiedmodel$DV[j] == simplifiedmodel$IV[k] &
               simplifiedmodel$DV[k] == simplifiedmodel$IV[l] &
               simplifiedmodel$DV[l] == simplifiedmodel$IV[m] &
               simplifiedmodel$Is_SEM_DV[m] == TRUE &
               simplifiedmodel$Is_SEM_IV[i] == TRUE){

              path <- paste0(path,
                             simplifiedmodel$Coefficient[i],
                             "*",
                             simplifiedmodel$Coefficient[j],
                             "*",
                             simplifiedmodel$Coefficient[k],
                             "*",
                             simplifiedmodel$Coefficient[l],
                             "*",
                             simplifiedmodel$Coefficient[m],
                             " \n")}
          }
        }
      }
    }
  }

  path <- str_split(path, "\n", simplify = TRUE)
  path <- levels(as.factor(path))
  path <- str_replace_all(path, " " , "")
  path <- path[path != ""]


  structuralModel <- ""
  for (i in 1:nrow(simplifiedmodel)) {
    structuralModel <- paste0(structuralModel, simplifiedmodel$Expression[i], "\n")
  }

  structuralModel <- str_replace(structuralModel, ",", "\n")

  path <- as.data.frame(path)
  nbMediatingEffects <- nrow(path)+1
  names(path)[1] <- "indEffect"

  path$indEffectName <- path$indEffect

  for (i in 1:nrow(path)) {
    path$indEffectName[i] <- str_replace_all(path$indEffectName[i], "\\*" , "_")
  }

  mediationModel <- ""
  for (i in 1:nrow(path)) {
    mediationModel <- paste0(mediationModel, path$indEffectName[i],":=",
                             path$indEffect[i], "\n")
  }

  mediationModel <- toString(mediationModel)
  mediationModel <- str_replace(mediationModel, ",", "\n")

  measurementModel <- toString(measurementModel)
  measurementModel <- str_replace_all(measurementModel, ",", "\n")

  finalModel <- paste(measurementModel, structuralModel,
                      mediationModel, sep = "\n ")

  fit <- sem(finalModel, data=database, se="bootstrap", bootstrap=bootstrap_samples)

  levelAnalysis = (1-(1-confidence_interval))/100

  mediationResults <- standardizedSolution(fit, type="std.all", se=TRUE, zstat=FALSE,
                                           pvalue=FALSE, ci=TRUE, level=levelAnalysis) %>%
    headTail(bottom=nbMediatingEffects, top=0, digits=4)

  mediationResults <- data.frame(mediationResults[,4:5], mediationResults[,7:8])

  mediationResults <- mediationResults[-(1:2),]

  mediationResults[2:4] <- sapply(mediationResults[2:4],as.numeric)

  for(i in 1:nrow(mediationResults)){
    if(sign(mediationResults[i,3])==sign(mediationResults[i,4])){
      mediationResults[i,5] <- "Yes"
    }else{
      mediationResults[i,5] <- "No"
    }
  }

  names(mediationResults)[1:5] <- c("Indirect Effects",
                                    "Beta",
                                    paste0(confidence_interval, "% CI (lower)", ""),
                                    paste0(confidence_interval, "% CI (upper)", ""),
                                    "Significant")

  rownames(mediationResults) <- NULL

  for (i in 1:nrow(mediationResults)) {

    a <- str_split(mediationResults$`Indirect Effects`[i], "_")
    a <- unlist(a)
    a <- unique(a)
    a <- toString(a)
    mediationResults$`Indirect Effects`[i] <- str_replace_all(a, ",", " =>")

  }

  kable(mediationResults)
}
