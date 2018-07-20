
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

maleWageF <- function(data, years=2000:2012, ages=25:50) {
   library(magrittr)
   library(ggplot2)
   subset <- data[sex == 1 & !is.na(wage) & !is.na(residenceCounty) & nonEst,
                  .(wage, estLevel, engLevel, residenceCounty, edu, age, date)][
      wage > 0, ][
    , metro := sapply(as.character(residenceCounty), function(c) switch(c, "37"="Tallinn", "44"="Other", "Other"))][
    , edu := sapply(as.character(edu), function(c) switch(c, "<=basic"="< HS", "highSchool"="HS", college="college degree"))][
    , edu := factor(edu, levels=c("< HS", "HS", "College Degree"))][
    , et := estLevel %in% c("1", "2", "home")][
    , en := engLevel %in% c("1", "2", "home")][
      year(date) %in% years & age >= min(ages) & age <= max(ages), ]
   deg <- length(unique(years)) - 1
   lm(log(wage) ~ poly(year(date), deg) + metro*edu*et, data=subset) %>%
      summary() %>%
      print()
   m2 <- lm(log(wage) ~ poly(year(date), deg), data=subset)
   subset[, residual := residuals(m2)]
   plain <- subset %>%
      ggplot(aes(x=et, y=residual)) +
      geom_violin(aes(fill=et), alpha=0.6) +
      stat_summary(fun.y=mean, geom="point") +
      coord_cartesian(ylim=c(-2,2)) +
      theme(legend.position="none")
   metro <- subset %>%
      ggplot(aes(x=et, y=residual)) +
      geom_violin(aes(fill=et), alpha=0.6) +
      stat_summary(fun.y=mean, geom="point") +
      facet_grid(metro ~ .) +
      coord_cartesian(ylim=c(-2,2)) +
      theme(legend.position="none")
   edu <- subset %>%
      ggplot(aes(x=et, y=residual)) +
      geom_violin(aes(fill=et), alpha=0.6) +
      stat_summary(fun.y=mean, geom="point") +
      facet_grid(metro ~ edu) +
      coord_cartesian(ylim=c(-2,2)) +
      labs(fill="Speaks\nEstonian")
   t1 <- subset[, .(wage = mean(wage), residual=mean(residual), n = .N), keyby=.(et)] %>%
      print()
   t2 <- subset[, .(wage = mean(wage), residual=mean(residual), n = .N), keyby=.(metro, et)] %>%
      print()
   t3 <- subset[, .(wage = mean(wage), residual=mean(residual), n = .N), keyby=.(metro, edu, et)] %>%
      print()
   hlay <- matrix(c(1,2,3,3,3), nrow=1)
   gridExtra::grid.arrange(plain, metro, edu, ncol=3, layout_matrix=hlay)
   res <- dcast(t3, metro + edu ~ et, value.var=c("residual", "n"))[
    , `:=`(n = n_FALSE + n_TRUE, n_FALSE = NULL, n_TRUE = NULL)]
   setnames(res, c("residual_FALSE", "residual_TRUE"), c("ET0", "ET1"))
   res[, premium := ET1 - ET0]
   res[, .(metro, edu, n, ET0, ET1, premium)]
}
