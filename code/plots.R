
loadData <- function() {
   library(data.table)
   load("~/tyyq/andmebaasiq/ETU/mergeELFS/ETU_panel.Rdat")
   setDT(panel)
   panel
}

maleWage <- function(data) {
   library(magrittr)
   library(ggplot2)
   subset <- data[year(date) == 2012 & sex == 1 & !is.na(wage) & nonEst, .(wage, estLevel, residenceCounty, edu)][
      wage > 0, ][
    , metro := residenceCounty == 37][
    , et := estLevel %in% c("1", "2", "home")]
   plain <- subset %>%
      ggplot(aes(x=et, y=log(wage))) +
      geom_violin(aes(col=et), size=2) +
      stat_summary(fun.y=mean, geom="point")
   metro <- subset %>%
      ggplot(aes(x=et, y=log(wage))) +
      geom_violin(aes(col=et, fill=metro), size=2) +
      stat_summary(aes(group=metro), fun.y=mean, geom="point")
   edu <- subset %>%
      ggplot(aes(x=et, y=log(wage))) +
      geom_violin(aes(col=et, fill=metro, alpha=edu), size=2) +
      stat_summary(aes(group=metro), fun.y=mean, geom="point")
   gridExtra::grid.arrange(plain, metro, edu, ncol=3)
}
