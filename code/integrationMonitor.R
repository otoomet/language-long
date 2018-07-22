
langModel <- function(data, reg="lpm", outcome="need",
                      subset=k102 >= 25 & k102 <=55) {
   ## Estimate the language need/usage model on IntegrationMonitor data
   subset <- eval(substitute(subset), envir=data)
   if(!is.null(subset)) {
      subset <- which(subset)
      cat("Taking subset: original", nrow(data), "obs, subset", length(subset), "obs\n")
      data <- data[subset,]
   }
   ## Modeling language use at work based on IntegrationMonitor
   data <- data[tolower(trim(data$k104.1)) != "eestlane",]
                           # ethnic backround
   cat("Ethnic background of the sample:\n")
   print(table(data$k104.1))
   ETEnvir <- tolower(trim(data$k12)) == "peamiselt eesti keeles"
                           # communication language at workplace
   RUEnvir <- tolower(trim(data$k12)) == "peamiselt vene keeles"
   needET <- tolower(trim(data$k78.1)) == "eesti"
                           # which languages do you need at work
   needRU <- (tolower(trim(data$k78.1)) == "vene") | (tolower(trim(data$k78.2)) == "vene")
   needEN <- (tolower(trim(data$k78.1)) == "inglise") | (tolower(trim(data$k78.2)) == "inglise") |
       (tolower(trim(data$k78.3)) == "inglise")
   needEN[is.na(needEN)] <- FALSE
   woman <- tolower(trim(data$k101)) == "naine"
                           # gender
   ##
   region <- rep("ROC", nrow(data))
   region[tolower(trim(data$mk)) == "harju"] <- "Capital"
   region[tolower(trim(data$mk)) == "ida-viru"] <- "NE"
   region <- factor(region, levels=c("ROC", "Capital", "NE"))
   ##
   occ <- rep("L", nrow(data))
   occ[tolower(trim(data$k5)) == "asutuse või allüksuse juht"] <- "upper"
                           # occupation
   occ[tolower(trim(data$k5)) == "spetsialist ametil, mis eeldab kõrgharidust"] <- "specialist"
   occ <- factor(occ)
   ##
   public <- tolower(trim(data$k6)) == "riiklikus asutuses/ettevõttes"
                           # private/public/NGO...
   highWage <- tolower(trim(data$k8)) %in% c("2-3 alampalka ( 9001 - 13 500)", "3-4 alampalka (13 501- 18 000)",
                                             "4-5 alampalka (18 001- 22 500)", "Üle 5 alampalga (üle 22 500)")
                           # wage group
   ageGroup <- cut(data$k102, c(0,15,25,35,50,65,100))
   ageGroup <- factor(ageGroup)
   ##
   edu <- rep("<HS", nrow(data))
   edu[data$k109 %in% c("kutse(kesk)haridus (kutsekool, kutsekeskkool, tööstuskool)",
                        "keskeriharidus (tehnikum)",
                        "kutse(kesk)haridus (kutsekool, kutsekeskkool, tööstuskool)",
                        "keskeriharidus (tehnikum)")] <- "HS"
   edu[data$k109 %in% c("rakendus- või kutsekõrgharidus", "ülikooliharidus (endine kõrgharidus) või teaduslik kraad")] <- "college"
   edu <- factor(edu, levels=c("HS", "<HS", "college"))
   ##
   married <- tolower(trim(data$k108)) == "abielus, vabaabielus"
   ## Print descriptive table
   cat("Descriptive tables:\n")
   cat("Gender distribution: woman\n")
   print(table(woman))
   cat("Women vs need ET at work\n")
   print(prop.table(t <- table(woman, needET), margin=1))
   cat(sum(t), "observations\n")
   cat("Women vs ET environmnet at work\n")
   print(prop.table(table(woman, ETEnvir), margin=1))
   ##
   cat("Women vs need RU at work\n")
   print(prop.table(table(woman, needRU), margin=1))
   cat("Women vs RU environmnet at work\n")
   print(prop.table(table(woman, RUEnvir), margin=1))
   ##
   cat("Women vs need EN at work\n")
   print(prop.table(table(woman, needEN), margin=1))
   ##
   if(outcome == "needET")
       f <- "needET"
   else if(outcome == "ETEnvir")
       f <- "ETEnvir"
   else if(outcome == "needEN")
       f <- "needEN"
   f <- paste(f, "~ woman*married + edu + region + occ + public + ageGroup")
   print(f)
   if(reg == "lpm") {
      m <- lm(as.formula(f))
   }
   else
       m <- glm(as.formula(f), family=binomial(link="probit"))
   return(m)
}
         
