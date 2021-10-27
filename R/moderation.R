
#'
#' @param database
#'
#' @param dependent_variable_variable
#' @param independent_variable_variable_variable
#' @param moderating_variable
#' @param model
#'
#' @importFrom lavaan sem standardizedSolution
#' @importFrom stringr str_split str_replace str_replace_all
#' @import magrittr
#' @importFrom knitr kable
#' @importFrom dplyr filter
#' @export
#'
#'
moderation <- function(model, database, dependent_variable_variable, independent_variable_variable_variable, moderating_variable){

  model <- str_split(modelInput, "\n", simplify = TRUE)
  model <- model[model != ""]
  model <- str_replace_all(model, " " , "")

  measurementModel <- model[grepl("=~", model)]

  structuralModel <- model[!grepl("=~", model)]


  var1_items <- 0
  var2_items <- 0

  for (i in 1:length(measurementModel)) {
    if(moderating_variable == str_split(measurementModel[i], "=~", simplify = TRUE)[1]){
      var1_items <- str_split(measurementModel[i], "~", simplify = TRUE)[2]
      var1_items <- str_split(var1_items, "\\+")
      var1_items <- unlist(var1_items)
      var1_items <- as.data.frame(var1_items)
    }
    if(independent_variable_variable == str_split(measurementModel[i], "=~", simplify = TRUE)[1]){
      var2_items <- str_split(measurementModel[i], "~", simplify = TRUE)[2]
      var2_items <- str_split(var2_items, "\\+")
      var2_items <- unlist(var2_items)
      var2_items <- as.data.frame(var2_items)
    }

    names(var1_items)[1] <- "items"
    names(var2_items)[1] <- "items"
  }

  nbNewColumns <- 0

  for (i in 1:nrow(var1_items)) {
    for (j in 1:nrow(var2_items)) {

      interactionName <- paste0(var1_items$items[i],"x", var2_items$items[j])

      var1_items_center <- as.numeric(scale(database[,var1_items$items[i]], scale=FALSE))
      var2_items_center <- as.numeric(scale(database[,var2_items$items[j]], scale=FALSE))

      database[,interactionName] <- var1_items_center*var2_items_center

      nbNewColumns <- nbNewColumns+1

    }
  }

  database_last_columns <- database[,(ncol(database)-nbNewColumns+1):ncol(database)]

  covarianceSyntax <- ""

  for (i in 1:ncol(database_last_columns)) {
    for (j in 1:ncol(database_last_columns)) {
      if(names(database_last_columns)[i] != names(database_last_columns)[j]){
        if(!grepl(paste0(names(database_last_columns)[i],"~~",names(database_last_columns)[j]), covarianceSyntax) &
           !grepl(paste0(names(database_last_columns)[j],"~~",names(database_last_columns)[i]), covarianceSyntax)){
          covarianceSyntax <- paste0(covarianceSyntax, names(database_last_columns)[i],"~~",names(database_last_columns)[j], "\n")
        }
      }
    }
  }

  measurementModel <- toString(measurementModel)
  measurementModel <- str_replace_all(measurementModel, ",", "\n")

  measurementModel <- paste(measurementModel, "\n", paste0(moderating_variable,"x",independent_variable_variable), "=~")
  for (i in 1:ncol(database_last_columns)) {
    measurementModel <- paste(measurementModel, names(database_last_columns)[i], "+")
  }
  measurementModel <- str_sub(measurementModel,1,nchar(measurementModel)-1)

  structuralModel <- toString(structuralModel)
  structuralModel <- str_replace_all(structuralModel, ",", "\n")

  structuralModel <- paste(structuralModel, "\n", dependent_variable, "~", paste0(moderating_variable,"x",independent_variable_variable))

  modelFinal <- paste(measurementModel, "\n", covarianceSyntax, "\n", structuralModel)

  fit <- lavaan::sem(modelFinal, data=database)

  moderationResults <- standardizedSolution(fit, type="std.all", pvalue=TRUE)
  moderationResults <- moderationResults %>% filter(moderationResults$op == "~")
  moderationResults <- moderationResults[nrow(moderationResults),]

  if(moderationResults$pvalue <= 0.05 & moderationResults$est.std > 0){
    print(paste0("The moderating effect is positive and significant (Beta = ", round(moderationResults$est.std[1],2), ", p = ", round(moderationResults$pvalue[1],3),")"))
  }

  if(moderationResults$pvalue > 0.05 & moderationResults$est.std > 0){
    print(paste0("The moderating effect is positive but not significant (Beta = ", round(moderationResults$est.std[1],2), ", p = ", round(moderationResults$pvalue[1],3),")"))
  }

  if(moderationResults$pvalue <= 0.05 & moderationResults$est.std < 0){
    print(paste0("The moderating effect is negative and significant (Beta = ", round(moderationResults$est.std[1],2), ", p = ", round(moderationResults$pvalue[1],3),")"))
  }

  if(moderationResults$pvalue > 0.05 & moderationResults$est.std < 0){
    print(paste0("The moderating effect is negative but not significant (Beta = ", round(moderationResults$est.std[1],2), ", p = ", round(moderationResults$pvalue[1],3),")"))
  }

}
