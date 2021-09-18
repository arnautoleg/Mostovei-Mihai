library(rstatix)
library(ggplot2)
library(table1)
library(readxl)
install.packages("ggstatsplot")
install.packages("ggpubr")
library(ggstatsplot)
library(ggpubr)


indicii_tezei_statistica_modificati <- read_excel("indicii tezei statistica modificati.xlsx")

df <- indicii_tezei_statistica_modificati

df$Lotul <- as.factor(df$Lotul)
df$Sex   <- as.factor(df$Sex)

library(dplyr)
df$Sex <- recode_factor(df$Sex, "0" = "F", "1" = "B")
levels(df$Sex)




#Este diferențe intre electroactivitatea musculara a grupei de studiu initial (0) si grupa de control?
#Este diferențe intre electroactivitatea musculara a grupei de studiu la etapa de control (1) si grupa de control?
#Este diferențe intre electroactivitatea musculara a grupei de studiu initial (0) si etapa de control dupa 6 luni (1)?
  
tb <- table1::table1(~. | df$Lotul, data = df[, c(6:9, 34:37)], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)


lshap <- lapply(df[, 34:37], shapiro.test) # LSF distribution analysis

df_1 <- subset(df, Lotul == "C")
lshap_1 <- lapply(df_1[, 6:9], shapiro.test) # LC distribution analysis

df_2 <- subset(df, Lotul == "S")
lshap_2 <- lapply(df_1[, 6:9], shapiro.test) # LS distribution analysis



longer_data <- df %>%
  pivot_longer(34:37, names_to = "Parameters", values_to = "Value")
print(longer_data)

df %>%
  pivot_longer(34:37, names_to = "Parameters", values_to = "Value") %>%
  ggplot(aes(x = Value)) +
  geom_histogram(color = "Blue", fill = "lightblue") +
  theme_get()+
  facet_wrap(vars(Parameters), ncol = 2) +
  labs(x = "Valoare")

longer_data$Parameters

wilcox.test(TAL0 ~ Lotul, data = df)
wilcox.test(TAR0 ~ Lotul, data = df)
wilcox.test(MML0 ~ Lotul, data = df)
wilcox.test(MMR0 ~ Lotul, data = df)

wilcox.test(df$TAL1, df_1$TAL0)
wilcox.test(df$TAR1, df_1$TAR0)
wilcox.test(df$MML1, df_1$MML0)
wilcox.test(df$MMR1, df_1$MMR0)

wilcox.test(df_2$TAL1, df_2$TAL0, paired = T)
wilcox.test(df_2$TAR1, df_2$TAR0, paired = T)
wilcox.test(df_2$MML1, df_2$MML0, paired = T)
wilcox.test(df_2$MMR1, df_2$MMR0, paired = T)

## LC and LS Comparative evalution visualisation

p <-ggboxplot(df, x = "Lotul", y = "TAL0", 
          color = "Lotul", palette = c("#00AFBB", "#E7B800"),
          ylab = "TAL0", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "TAR0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAR0", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MML0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MML0", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMR0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MMR0", xlab = "Lotul", add = "jitter")
p + stat_compare_means()


df_2_long <- df_2 %>%
  pivot_longer(c(6:9, 34:37), names_to = "Parameters", values_to = "Value")
print(df_2_long)

plt_0 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TAR1" | Parameters == "TAR0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TAL1" | Parameters == "TAL0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MML1" | Parameters == "MML0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMR1" | Parameters == "MMR0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

p <-ggboxplot(df, x = "Lotul", y = "TAL1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAL1", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "TAR1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAR1", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MML1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MML1", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMR1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MMR1", xlab = "Lotul", add = "jitter")
p + stat_compare_means()


#Este diferențe intre electroactivitatea musculara a grupei de studiu initial (0) si masticatia la inițială (0)?
#Este diferențe intre electroactivitatea musculara a grupei de studiu la etapa de control (1) si masticatia la etapa de control (1)?
#Este diferențe intre electroactivitatea musculara a grupei de control si masticatia grupei de control ?
#Este diferențe intre electroactivitatea musculara in timpul masticatiei a grupei de studiu la etapa inițială (0) si masticatia la etapa de control (1)?
#Este diferențe intre electroactivitatea musculara la masticatie a grupei de control si masticatia grupei de studiu la etapa 0 si etapa 1?
  


tb <- table1::table1(~. | df$Lotul, data = df[, c(6:9,  34:37, 68:71, 96:99)], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)



lshap <- lapply(df_1[, 68:71], shapiro.test) # distribution analysis for mastication control group

lshap <- lapply(df_2[, 68:71], shapiro.test) # distribution analysis for mastication study group (initial)

lshap <- lapply(df_2[, 96:99], shapiro.test) # distribution analysis for mastication study group (follow up)


wilcox.test(df_2$TALch0, df_2$TAL0, paired = T)
wilcox.test(df_2$TARch0, df_2$TAR0, paired = T)
wilcox.test(df_2$MMLch0, df_2$MML0, paired = T)
wilcox.test(df_2$MMRch0, df_2$MMR0, paired = T)


wilcox.test(df_2$TALch1, df_2$TAL1, paired = T)
wilcox.test(df_2$TARch1, df_2$TAR1, paired = T)
wilcox.test(df_2$MMLch1, df_2$MML1, paired = T)
wilcox.test(df_2$MMRch1, df_2$MMR1, paired = T)

wilcox.test(df_2$TALch0, df_2$TAL0, paired = T)
wilcox.test(df_2$TARch0, df_2$TAR0, paired = T)
wilcox.test(df_2$MMLch0, df_2$MML0, paired = T)
wilcox.test(df_2$MMRch0, df_2$MMR0, paired = T)


wilcox.test(df_2$TALch1, df_2$TAL0, paired = T)
wilcox.test(df_2$TARch1, df_2$TAR0, paired = T)
wilcox.test(df_2$MMLch1, df_2$MML0, paired = T)
wilcox.test(df_2$MMRch1, df_2$MMR0, paired = T)


wilcox.test(df_2$TALch0, df_2$TALch1, paired = T)
wilcox.test(df_2$TARch0, df_2$TARch1, paired = T)
wilcox.test(df_2$MMLch0, df_2$MMLch1, paired = T)
wilcox.test(df_2$MMRch0, df_2$MMRch1, paired = T)


wilcox.test(df_1$TALch0, df_1$TAL0, paired = T)
wilcox.test(df_1$TARch0, df_1$TAR0, paired = T)
wilcox.test(df_1$MMLch0, df_1$MML0, paired = T)
wilcox.test(df_1$MMRch0, df_1$MMR0, paired = T)


wilcox.test(df_1$TALch0, df_2$TALch0)
wilcox.test(df_1$TARch0, df_2$TARch0)
wilcox.test(df_1$MMLch0, df_2$MMLch0)
wilcox.test(df_1$MMRch0, df_2$MMRch0)


wilcox.test(df_1$TALch0, df_2$TALch1)
wilcox.test(df_1$TARch0, df_2$TARch1)
wilcox.test(df_1$MMLch0, df_2$MMLch1)
wilcox.test(df_1$MMRch0, df_2$MMRch1)


df_1_long <- df_1 %>%
  pivot_longer(c(6:9, 68:71), names_to = "Parameters", values_to = "Value")
print(df_1_long)

df_2_long <- df_2 %>%
  pivot_longer(c(6:9, 68:71), names_to = "Parameters", values_to = "Value")
print(df_2_long)

plt_0 <- ggwithinstats(
  data = subset(df_1_long,Parameters == "TARch0" | Parameters == "TAR0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_1_long,Parameters == "TALch0" | Parameters == "TAL0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_1_long,Parameters == "MMLch0" | Parameters == "MML0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


df_2_long <- df_2 %>%
  pivot_longer(c(6:9,  34:37, 68:71, 96:99), names_to = "Parameters", values_to = "Value")
print(df_2_long)

plt_0 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TARch0" | Parameters == "TAR0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TALch0" | Parameters == "TAL0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMLch0" | Parameters == "MML0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMRch0" | Parameters == "MMR0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_0 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TARch1" | Parameters == "TAR1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TALch1" | Parameters == "TAL1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMLch1" | Parameters == "MML1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMRch1" | Parameters == "MMR1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_0 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TARch1" | Parameters == "TARch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "TALch1" | Parameters == "TALch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMLch1" | Parameters == "MMLch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "MMRch1" | Parameters == "MMRch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

p <-ggboxplot(df, x = "Lotul", y = "TALch0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAL, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "TARch0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAR, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMLch0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MML, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMRch0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MMR, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()




p <-ggboxplot(df, x = "Lotul", y = "TALch1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAL, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "TARch1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "TAR, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMLch1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MML, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "MMRch1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "MMR, mastication", xlab = "Lotul", add = "jitter")
p + stat_compare_means()



#Cum sau modificat valorile deviatiilor procentuale pentru parametrii (PocTA, PocMM, BAR, Impact, Tors și Asym) la etapa 0 si etapa de control 1. 
#Care este diferenta deviației procentuale medii la la etapa inițială (0) si etapa de control (1).
#Cum este deviația procentuală medie a grupei de control vs grupa de studiu la etapa 0 si etapa 1. 
#Cum este deviația procentulă a parametrilor (PocTA, PocMM, BAR, Impact, Tors și Asym) la etapa 0 și masticația 0.
#Cum se midifică deviația procentuală medie la etapa 0 și masticația 0. 
#Cum este deviația procentulă a parametrilor (PocTA, PocMM, BAR, Impact, Tors și Asym) la etapa 1 și masticația 1.
#Cum se midifică deviația procentuală medie la etapa 1 și masticația 1. 
#Cum este deviația procentulă a parametrilor (PocTA, PocMM, BAR, Impact, Tors și Asym) la grupa de control in statica si masticatie.
#Cum se modifica deviatia procentuala medie in statica si msticatie in grupa de control. 

tb <- table1::table1(~. | df$Lotul, data = df[, c(10, 13, 14, 17, 18, 21, 22, 24, 25, 28, 29, 31)], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)

a <- c(10, 13, 14, 17, 18, 21, 22, 24, 25, 28, 29, 31)

b <- a + 28
b

tb <- table1::table1(~. | df$Lotul, data = df[, b], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)


c <- a + 62
c

tb <- table1::table1(~. | df$Lotul, data = df[, c], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)

d <-  a + 90
d

tb <- table1::table1(~. | df$Lotul, data = df[, d], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)


tb <- table1::table1(~. | df$Lotul, data = df[, c(11, 15, 19, 26)], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)

df$`L0/R1_POCMM0` <- as.numeric(df$`L0/R1_POCMM0`)
df$`L0/R1_POCMM1` <- as.numeric(df$`L0/R1_POCMM1`)


df[, c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116)] <- lapply(df[,c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116)], factor)


tb <- table1::table1(~. | df$Lotul, data = df[, c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116)], 
                     render.continuous=c(.="FREQ (PCT)"),
                     topclass="Rtable1-grid"
)



# The first variant

library(gtsummary)
library(tidyverse)

t1 <-
  df %>%
  select(c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116)) %>%
  tbl_summary(statistic = all_categorical() ~ "{n} / {N} , {p}%") %>%
  modify_header(stat_0 ~ "n group / N total, %")


t1 <- df %>%
  select(c(11, 15, 19, 26, 39, 43, 47, 73, 77, 81, 88, 101, 105, 109, 116)) %>%   # variable 54 deleted
  tbl_summary(statistic = all_categorical() ~ "{n} / {N} , {p}%",
              type = everything() ~ "categorical")


categorical_ci <- function(variable, tbl, ...) {
  
  filter(tbl$meta_data, variable == .env$variable) %>%
    pluck("df_stats", 1) %>%
    mutate(
      # calculate and format 95% CI
      prop_ci = map2(n, N, ~prop.test(.x, .y)$conf.int %>%
                       style_percent(symbol = TRUE)),
      ci = map_chr(prop_ci, ~glue::glue("{.x[1]}, {.x[2]}"))
    ) %>%
    pull(ci)
}

t1 %>%
  add_stat(
    fns = everything() ~ "categorical_ci",
    location = "level", 
    modify_header(starts_with("add_stat_1") ~ "**95% CI**")
  ) %>%
  modify_footnote(everything() ~ NA)



# The second variant (optimal)

library(gtsummary)
library(tidyverse)

ci_function_cat <- function(data, variable, by, tbl, ...) {
  # first calculate CIs for all levels
  result <-
    data %>% 
    freqtables::freq_table(!!sym(by), !!sym(variable)) %>% 
    mutate(ci = str_glue("{style_percent(lcl_row / 100, symbol = TRUE)}, {style_percent(ucl_row / 100, symbol = TRUE)}")) %>%
    select(row_cat, col_cat, ci) %>%
    pivot_wider(id_cols = col_cat, names_from = row_cat, values_from = ci) 
  
  # if variable is type 'dichotomous', only keep one row
  var_meta_data <- tbl$meta_data %>% filter(.data$variable %in% .env$variable)
  if (var_meta_data$summary_type %in% "dichotomous") {
    result <- result %>% filter(col_cat %in% var_meta_data$dichotomous_value[[1]])
  }
  
  result %>%
    select(-1) %>%
    set_names(paste0("add_stat_", seq_len(ncol(.))))
}

a = subset(df, Lotul == "S")

tbl <-
  a %>%
  select(c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116)) %>%
  tbl_summary(missing =  "no",
              statistic = all_categorical() ~ "{n}/{N} ({p}%)")%>%
  add_stat(
    fns = all_categorical() ~ categorical_ci,
    location = all_categorical(FALSE) ~ "level"
  ) %>%
  modify_header(starts_with("add_stat_1") ~ "**95% CI**")

# Graphs 

e <- c(10, 13, 14, 17, 18, 21, 22, 24, 25, 28, 29, 31,                #continuos variable list
       38, 41, 42, 45, 46, 49, 50, 52, 53, 56, 57, 59,
       72, 75, 76, 79, 80, 83, 84, 86, 87, 90, 91, 93,
       100, 103, 104, 107, 108, 111, 112, 114, 115, 118, 119, 121)
  
  
df_3_long <- df_1 %>%                                                 #control group
  pivot_longer(all_of(e), names_to = "Parameters", values_to = "Value")
print(df_3_long)

df_3_long$Parameters

plt_0 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "POCTA0" | Parameters == "POCTAch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_1 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_POCTA0%" | Parameters == "CD_POCTAch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "POCMM0" | Parameters == "POCMMch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_POCMM0%" | Parameters == "CD_POCMMch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_4 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "BAR0" | Parameters == "BARch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_5 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_BAR0%" | Parameters == "CD_BARch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_6 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "IMPACT0"  | Parameters == "IMPACTch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_7 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_IMPACT0%"  | Parameters == "CD_IMPACTch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_8 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "TORS0"  | Parameters == "TORSch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_9 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_TORS0 %"  | Parameters == "CD_TORSch0 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_10 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "ASYM0"  | Parameters == "ASYMch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

       
plt_11 <- ggwithinstats(
  data = subset(df_3_long,Parameters == "CD_Asym0 %"  | Parameters == "CD_Asymch0 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

df_4_long <- df_2 %>%                                                 #study group
  pivot_longer(all_of(e), names_to = "Parameters", values_to = "Value")
print(df_4_long)


# Valori initiale

plt_0 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCTA0" | Parameters == "POCTAch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_1 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCTA0%" | Parameters == "CD_POCTAch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCMM0" | Parameters == "POCMMch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCMM0%" | Parameters == "CD_POCMMch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_4 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "BAR0" | Parameters == "BARch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_5 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_BAR0%" | Parameters == "CD_BARch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_6 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "IMPACT0"  | Parameters == "IMPACTch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_7 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_IMPACT0%"  | Parameters == "CD_IMPACTch0%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_8 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "TORS0"  | Parameters == "TORSch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_9 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_TORS0 %"  | Parameters == "CD_TORSch0 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_10 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "ASYM0"  | Parameters == "ASYMch0"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_11 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_Asym0 %"  | Parameters == "CD_Asymch0 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

# follow up

plt_0 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCTA1" | Parameters == "POCTAch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_1 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCTA1%" | Parameters == "CD_POCTAch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCMM1" | Parameters == "POCMMch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCMM1%" | Parameters == "CD_POCMMch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_4 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "BAR1" | Parameters == "BARch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_5 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_BAR1%" | Parameters == "CD_BARch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_6 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "IMPACT1"  | Parameters == "IMPACTch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_7 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_IMPACT1%"  | Parameters == "CD_IMPACTch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_8 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "TORS1"  | Parameters == "TORSch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_9 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_TORS1 %"  | Parameters == "CD_TORSch1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_10 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "ASYM1"  | Parameters == "ASYMch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_11 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_Asym1 %"  | Parameters == "CD_Asymch1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

# LS vs LSF

plt_0 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCTA0" | Parameters == "POCTA1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_1 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCTA0%" | Parameters == "CD_POCTA1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCMM0" | Parameters == "POCMM1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCMM0%" | Parameters == "CD_POCMM1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_4 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "BAR0" | Parameters == "BAR1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_5 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_BAR0%" | Parameters == "CD_BAR1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_6 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "IMPACT0"  | Parameters == "IMPACT1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_7 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_IMPACT0%"  | Parameters == "CD_IMPACT1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_8 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "TORS0"  | Parameters == "TORS1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_9 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_TORS0 %"  | Parameters == "CD_TORS1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_10 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "ASYM0"  | Parameters == "ASYM1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_11 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_Asym0 %"  | Parameters == "CD_Asym1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

# LSch vs LSFch

plt_0 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCTAch0" | Parameters == "POCTAch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_1 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCTAch0%" | Parameters == "CD_POCTAch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "POCMMch0" | Parameters == "POCMMch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_POCMMch0%" | Parameters == "CD_POCMMch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_4 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "BARch0" | Parameters == "BARch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_5 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_BARch0%" | Parameters == "CD_BARch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_6 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "IMPACTch0"  | Parameters == "IMPACTch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_7 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_IMPACTch0%"  | Parameters == "CD_IMPACTch1%"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_8 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "TORSch0"  | Parameters == "TORSch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_9 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_TORSch0 %"  | Parameters == "CD_TORSch1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_10 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "ASYMch0"  | Parameters == "ASYMch1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


plt_11 <- ggwithinstats(
  data = subset(df_4_long,Parameters == "CD_Asymch0 %"  | Parameters == "CD_Asymch1 %"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


# LC vs LS and LSF

e <- c(10, 13, 14, 17, 18, 21, 22, 24, 25, 28, 29, 31,                #continuos variable list
       38, 41, 42, 45, 46, 49, 50, 52, 53, 56, 57, 59,
       72, 75, 76, 79, 80, 83, 84, 86, 87, 90, 91, 93,
       100, 103, 104, 107, 108, 111, 112, 114, 115, 118, 119, 121)

#LC vs LS

plt_12 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCTA0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_13 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCTA0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_14 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCMM0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_15 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCMM0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_16 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = BAR0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_17 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_BAR0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_18 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = IMPACT0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_19 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_IMPACT0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_20 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = TORS0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_21 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_TORS0 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_22 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = ASYM0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_23 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_Asym0 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

#LC vs LSF

plt_12 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCTA1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_13 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCTA1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_14 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCMM1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_15 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCMM1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_16 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = BAR1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_17 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_BAR1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_18 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = IMPACT1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_19 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_IMPACT1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_20 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = TORS1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_21 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_TORS1 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_22 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = ASYM1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_23 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_Asym1 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


#LCch vs LSch

plt_12 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCTAch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_13 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCTAch0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_14 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCMMch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_15 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCMMch0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_16 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = BARch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_17 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_BARch0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_18 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = IMPACTch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_19 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_IMPACTch0%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_20 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = TORSch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_21 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_TORSch0 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_22 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = ASYMch0, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_23 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_Asymch0 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

#LCch vs LSFch

plt_12 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCTAch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_13 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCTAch1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_14 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = POCMMch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_15 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_POCMMch1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_16 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = BARch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_17 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_BARch1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)


plt_18 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = IMPACTch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_19 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_IMPACTch1%`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_20 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = TORSch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_21 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_TORSch1 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_22 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = ASYMch1, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "significant", # display only significant pairwise comparisons
)

plt_23 <-  ggstatsplot::ggbetweenstats(
  data = df,
  x = Lotul, # grouping/independent variable
  y = `CD_Asymch1 %`, # dependent variables
  type = "neparametric", # type of statistics
  xlab = "Lotul", # label for the x-axis
  pairwise.display = "all" # display only significant pairwise comparisons
)

df_5_long <- df_1 %>%
  pivot_longer(c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116), names_to = "Parameters", values_to = "Value")
print(df_1_long)


mcnemar.test(df_2$`L0/R1_POCTA0`, df_2$`L0/R1_POCTA1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_POCMM0`, df_2$`L0/R1_POCMM1`, correct = TRUE)
mcnemar.test(df_2$`A0/P1_BAR0`, df_2$`A0/P1_BAR1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_TORS0`, df_2$`L0/R1_TORS1`, correct = TRUE)

mcnemar.test(df_2$`L0/R1_POCTAch0`, df_2$`L0/R1_POCTAch1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_POCMMch0`, df_2$`L0/R1_POCMMch1`, correct = TRUE)
mcnemar.test(df_2$`A0/P1_BARch0`, df_2$`A0/P1_BARch1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_TORSch0`, df_2$`L0/R1_TORSch1`, correct = TRUE)

mcnemar.test(df_2$`L0/R1_POCTA0`, df_2$`L0/R1_POCTAch0`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_POCMM0`, df_2$`L0/R1_POCMMch0`, correct = TRUE)
mcnemar.test(df_2$`A0/P1_BAR0`, df_2$`A0/P1_BARch0`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_TORS0`, df_2$`L0/R1_TORSch0`, correct = TRUE)

mcnemar.test(df_1$`L0/R1_POCTA0`, df_1$`L0/R1_POCTAch0`, correct = TRUE)
mcnemar.test(df_1$`L0/R1_POCMM0`, df_1$`L0/R1_POCMMch0`, correct = TRUE)
mcnemar.test(df_1$`A0/P1_BAR0`, df_1$`A0/P1_BARch0`, correct = TRUE)
mcnemar.test(df_1$`L0/R1_TORS0`, df_1$`L0/R1_TORSch0`, correct = TRUE)

mcnemar.test(df_2$`L0/R1_POCTA1`, df_2$`L0/R1_POCTAch1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_POCMM1`, df_2$`L0/R1_POCMMch1`, correct = TRUE)
mcnemar.test(df_2$`A0/P1_BAR1`, df_2$`A0/P1_BARch1`, correct = TRUE)
mcnemar.test(df_2$`L0/R1_TORS1`, df_2$`L0/R1_TORSch1`, correct = TRUE)


M1<-matrix(c(13,20,16,14),nrow=2)
fisher.test(M1)

M1<-matrix(c(13,20,14,16),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,17,13),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,14,16),nrow=2)
fisher.test(M1)



M1<-matrix(c(17,16,12,18),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,10,20),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,17,13),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,14,16),nrow=2)
fisher.test(M1)




M1<-matrix(c(17,16,12,18),nrow=2)
fisher.test(M1)

M1<-matrix(c(17,16,10,20),nrow=2)
fisher.test(M1)

M1<-matrix(c(10,23,15,15),nrow=2)
fisher.test(M1)

M1<-matrix(c(10,23,14,16),nrow=2)
fisher.test(M1)



M1<-matrix(c(22,11,16,14),nrow=2)
fisher.test(M1)

M1<-matrix(c(22,11,19,11),nrow=2)
fisher.test(M1)

M1<-matrix(c(22,11,17,13),nrow=2)
fisher.test(M1)

M1<-matrix(c(22,11,16,14),nrow=2)
fisher.test(M1)



M1<-matrix(c(16,17,16,14),nrow=2)
fisher.test(M1)

M1<-matrix(c(16,17,17,13),nrow=2)
fisher.test(M1)

M1<-matrix(c(20,13,16,14),nrow=2)
fisher.test(M1)

M1<-matrix(c(20,13,14,16),nrow=2)
fisher.test(M1)



fisher.test(df$`L0/R1_POCMM1`, df$Lotul, conf.int = TRUE)


df_6_long <- df_2 %>%
  pivot_longer(c(11, 15, 19, 26, 39, 43, 47, 54, 73, 77, 81, 88, 101, 105, 109, 116), names_to = "Parameters", values_to = "Value")
print(df_2_long)


#Despre eficiența masticatorie
#Cum s-a modificat timpul de masticatie la grupa de studiu in etapa initială (timpmas0 si timpmas1)
#Care este diferenta dintre timpul masticatiei la etapa 0 si 1 vs cele din grupa de control.
#Care este diferenta dintre numarul de cicluri la grupa de studiu etapa initiala si control (cicluri 0 vs cicluri 1)
#Care este diferenta dintre numarul de cicluri grupa de studiu si cea de control.
#Care este diferenta in frecventa msticatie (timpmas0/cicluri0) la etapa 0 vs etapa 1 (timpmas 1/cicluri1). 
#Care este diferenta in frecventa masticatiei la grupa de studiu (timpmas0/cicluri0);  (timpmas 1/cicluri1) si cea din grupa de control. 
#Care este diferenta in eficienta masticatorie (mastic0 și mastic1) la etapa 0 si 1 in grupa de studiu. (cu cit cifra e mai mare cu atit mai bine maninca)
#Care este diferenta dintr eficienta masticatorie in grupa de studiu (mastic0 și mastic1)  si cea de control.


tb <- table1::table1(~. | df$Lotul, data = df[, c(62:69)], 
                     render.continuous=c(.="Mean (SD)", 
                                         .="Median (IQR)", 
                                         .="[Min, Max]"),
                     topclass="Rtable1-grid"
)


wilcox.test(`mastic0(%)` ~ Lotul, data = df)
wilcox.test(`timpmas0(s)` ~ Lotul, data = df)
wilcox.test(`cicluri0(n)` ~ Lotul, data = df)
wilcox.test(frecventa0 ~ Lotul, data = df)

wilcox.test(`mastic1(%)` ~ Lotul, data = df)
wilcox.test(`timpmas1(s)` ~ Lotul, data = df)
wilcox.test(`cicluri1(n)` ~ Lotul, data = df)
wilcox.test(frecventa1 ~ Lotul, data = df)

wilcox.test(df_2$`mastic0(%)`, df_2$`mastic1(%)`, paired = T)
wilcox.test(df_2$`timpmas0(s)`, df_2$`timpmas1(s)`, paired = T)
wilcox.test(df_2$`cicluri0(n)`, df_2$`cicluri1(n)`, paired = T)
wilcox.test(df_2$frecventa0, df_2$frecventa1, paired = T)


df_2_long <- df_2 %>%
  pivot_longer(c(62:69), names_to = "Parameters", values_to = "Value")
print(df_2_long)

plt_0 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "frecventa0" | Parameters == "frecventa1"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_1 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "mastic0(%)"  | Parameters == "mastic1(%)"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_2 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "timpmas0(s)" | Parameters == "timpmas1(s)"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)

plt_3 <- ggwithinstats(
  data = subset(df_2_long,Parameters == "cicluri0(n)" | Parameters == "cicluri1(n)"),
  type = "nonparametric", 
  x = Parameters,
  y = Value,
  xlab = "Measurements",
  ylab = "Value"
)


p <-ggboxplot(df, x = "Lotul", y = "mastic0(%)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "mastic0(%)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "timpmas0(s)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "timpmas0(s)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "cicluri0(n)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "cicluri0(n)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "frecventa0", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "frecventa0", xlab = "Lotul", add = "jitter")
p + stat_compare_means()




p <-ggboxplot(df, x = "Lotul", y = "mastic1(%)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "mastic1(%)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "timpmas1(s)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "timpmas1(s)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "cicluri1(n)", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "cicluri1(n)", xlab = "Lotul", add = "jitter")
p + stat_compare_means()

p <-ggboxplot(df, x = "Lotul", y = "frecventa1", 
              color = "Lotul", palette = c("#00AFBB", "#E7B800"),
              ylab = "frecventa1", xlab = "Lotul", add = "jitter")
p + stat_compare_means()


## Install package BiocManager
install.packages("BiocManager")
## Use BiocManager to install limma
BiocManager::install("limma")


## Installation of CRAN version
install.packages("MKmisc")

## Or the development version from GitHub
# install.packages("remotes")
remotes::install_github("stamats/MKmisc")

library(MKmisc)

power.diagnostic.test(spec = 0.8,
                     delta = 0.1,
                     sig.level = 0.05, 
                     power = 0.8)

power.diagnostic.test(spec = 0.95,
                      delta = 0.1,
                      prev = 0.001,
                      sig.level = 0.05, 
                      power = 0.8)

power.diagnostic.test(sens = 0.99, delta = 0.14, power = 0.95) # 40
