library(tidyverse)
library(ggformula)
library(gtsummary)
library(jmv)

set.seed(8034812)

pbc <- readRDS("data/examples/mod01_pbc.rds") |> 
  mutate(stage = factor(stage, levels=c(1, 2, 3, 4),
                        labels = c("Stage 1", "Stage 2", "Stage 3", "Stage 4")),
         sex = factor(sex, levels=c(1,2),
                      labels=c("Male", "Female")))

gf_bar(filter(pbc, is.na(stage) == FALSE), ~ stage,
       xlab = "", ylab = "Number of participants") |> 
  gf_theme(theme_classic())


age <- select(pbc, age) |> 
  slice_sample(n=35) |> 
  mutate(age = round(age))

age
arrange(age, by=age)

hist(age$age)
plot(density(age$age))

descriptives(age, age, pcEqGr = TRUE)

sum(age$age)


age_appended <- bind_rows(age, tibble(age = 110))
descriptives(age_appended, age)


gf_histogram(pbc, ~ age, boundary=0, binwidth = 5, colour = "black",
             xlab="Age (years)", ylab="Frequency") |> 
  gf_theme(theme_classic())

gf_dhistogram(pbc, ~ age, boundary=0, binwidth = 5, colour = "black",
             xlab="Age (years)", ylab="Density") |> 
  gf_theme(theme_classic())

gf_boxplot(data=pbc, age ~ .,
           ylab = "Age (years)") |> 
  gf_theme(theme_classic()) |> 
  gf_theme(axis.text.x = element_blank(),
           axis.ticks.x = element_blank())


gf_boxplot(data=age, age ~ .,
           ylab = "Age (years)") |> 
  gf_theme(theme_classic()) |> 
  gf_theme(axis.text.x = element_blank(),
           axis.ticks.x = element_blank())


t<- gtsummary::tbl_cross(pbc, sex, stage, missing="no",
                     percent="column",
                     label = list(sex ="Sex", stage="Stage"),
                     digits = 0)
as_hux_table(t)


t_row <- gtsummary::tbl_cross(pbc, sex, stage, missing="no",
                         percent="row",
                         label = list(sex ="Sex", stage="Stage"),
                         digits = 0)
as_hux_table(t_row)
