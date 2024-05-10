# Set working directory

root <- setwd("C:/Users/User/OneDrive - Universität Potsdam/IZW/biodiversity_dynamics/d6_teaching_collection-main")

# Load packages

package.list=c("here",
               "marked",
               "skimr",
               "sf",
               "tmap",
               "devtools",
               "rnaturalearthdata", 
               "ggplot2",
               "readr",
               "tidyr",
               "dplyr",
               "ggridges",
               "patchwork")

for (package in package.list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Set theme for some ggplots:
theme_set(
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.ticks = element_line(colour = "black", linewidth = 1),
    axis.ticks.length = unit(0.2, "cm")
  )
)

# Load data

bird_data <- read.csv("data/data_brandenburg/bird_data_rietzer_see_2000_2022_filled_min_4326.csv")

# Preparing for capture recapture

## change Geschlecht NA to "unbekannt"

bird_data$Geschlecht[is.na(bird_data$Geschlecht)]<-"unbekannt"

## Add column with initials of observer

get_initials <- function(name) {
  s_f_name <- strsplit(name, ", ")[[1]]
  initials <- paste0(substr(s_f_name[[1]], 1, 1), substr(s_f_name[[2]], 1, 1))
  return(initials)
}

bird_data$Initials <- sapply(bird_data$Beobachter,get_initials)

## calculate the times each bird has been captured

data_plot<-list()

for (i in unique(bird_data$Artbezeichnung.deutsch)){
  data_filtered <- bird_data |> 
    filter(Artbezeichnung.deutsch %in% i) |> 
    group_by(Ring_Inschrift, year) |> 
    mutate(n = n()) |> 
    filter(row_number()==1) |> 
    ungroup()
  
  data_plot[[i]]<-data_filtered
}

## capture-recapture history for each bird species

cmr_hist <- list()

for (i in seq(data_plot)){
  data <- table(data_plot[[i]]$Ring_Inschrift, data_plot[[i]]$year)   #table(data_plot[[i]][c(2,12)])
  cmr_hist[[i]] <- data}

# check if there are more than one observation per year (0 or 1)

for (i in seq(cmr_hist)){print(table(cmr_hist[i]))}


## check if some bird individuals have been tagged more than once.

rs <- list()

for (i in seq(cmr_hist)){
  data <- cmr_hist[[i]] |> data.frame() |> 
    pivot_wider(names_from = Var2, values_from = Freq) |> 
    mutate(capt_sum = rowSums(across(where(is.numeric)))) |> 
    unite(col = ch, -Var1, -capt_sum, sep = "", remove = TRUE) |>
    rename(Ring_Inschrift = "Var1")
  rs[[i]] <- data}

## to do: check how many recaptures there are

for (i in seq(rs)){print(dim(table(rs[[i]]$capt_sum)))}  #dim(table(rs[[1]][3]))

## plot capture histories

ggplot(data = rs[[103]], mapping = aes(x = ch)) +
  geom_bar(colour = "#262674", fill = "#262674") +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, by = 10),
    expand = c(.001, .001)
  ) +
  labs(x = "capture histories") +
  theme(axis.text.x = element_text(angle = 45))


# Merge the recapture data together with the data_plot data
### drop species where recapture too low

data_cmr = list()

for (i in seq(cmr_hist)){if (dim(table(rs[[i]]$capt_sum))>1 & 
                             sum(rs[[i]]$capt_sum == "2") > 4){
  data <- left_join(rs[[i]],data_plot[[i]] |> 
                        dplyr::select( Ring_Inschrift, Artbezeichnung.deutsch, 
                                       Gewicht_num_min, Fluegellaenge_num_min, 
                                       Teilfederlaenge_num_min, Geschlecht, 
                                       Beobachter, date, year, month), 
                      by = "Ring_Inschrift")
  data_cmr <- c(data_cmr, list(data))}}

### change variables to factor variables

for (i in seq(data_cmr)){
  data_cmr[[i]]$Geschlecht <- as.factor(data_cmr[[i]]$Geschlecht) 
  data_cmr[[i]]$Gewicht_num_min <- as.factor(data_cmr[[i]]$Gewicht_num_min)
  data_cmr[[i]]$Fluegellaenge_num_min <- as.factor(data_cmr[[i]]$Fluegellaenge_num_min)
  data_cmr[[i]]$Teilfederlaenge_num_min <- as.factor(data_cmr[[i]]$Teilfederlaenge_num_min)
  data_cmr[[i]]$Beobachter <- as.factor(data_cmr[[i]]$Beobachter)
  data_cmr[[i]]$year <- as.factor(data_cmr[[i]]$year)
  data_cmr[[i]]$month <- as.factor(data_cmr[[i]]$month)
  data_cmr[[i]]$date <- as.factor(data_cmr[[i]]$date)
  data_cmr[[i]]$Initials <- as.factor(data_cmr[[i]]$Initials)}

# Plot capturing data

## Sex

ggplot_sex <- list()
for (i in seq(data_cmr)){ggplot_sex[[i]] <- ggplot(
  data = data_cmr[[i]],mapping = aes(x = Geschlecht, fill = Geschlecht)) + 
  geom_bar() + 
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex, nrow = 4)
ggsave("ggplot_sex.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)

## Sex and years

ggplot_sex_yrs <- list()

for (i in seq(data_cmr)){ggplot_sex_yrs[[i]] <- 
  ggplot(data_cmr[[i]], mapping = aes(x = year, fill = Geschlecht)) +
  geom_bar(position = "stack") +
  labs(x = "years", fill = "sex") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex_yrs, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_sex_yrs.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)

## Sex and capture histories

ggplot_sex_ch <- list()

for (i in seq(data_cmr)){ggplot_sex_ch[[i]] <- 
  ggplot(data_cmr[[i]], mapping = aes(x = ch, fill = Geschlecht)) +
  geom_bar(position = "stack") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), expand = c(.001, .001)) +
  labs(x = "capture histories", fill = "sex") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex_ch, nrow = 4)
ggsave("ggplot_sex_ch.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)

## Sex and capture sum

ggplot_sex_cs <- list()

for (i in seq(data_cmr)){ggplot_sex_cs[[i]] <- 
  ggplot(data_cmr[[i]], mapping = aes(x = capt_sum, fill = Geschlecht)) +
  geom_bar(position = "stack") +
  #scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), expand = c(.001, .001)) +
  labs(x = "capture sum", fill = "sex") +
  theme(legend.position = "none") + #axis.text.x = element_text(angle = 45)) 
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex_cs, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_sex_cs.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


## clean data:
### drop species where R can not fit model

data_cmr <- data_cmr[-3]
data_cmr <- data_cmr[-8]
data_cmr <- data_cmr[-11]

# Change species order to show relation

arten_sort <- read.csv("data/data_brandenburg/arten_sort.csv")
arten <- arten_sort$X
data_cmr <- data_cmr[arten]

# Explore interval of observations

##Wieviele sample tage pro Jahr

df <-data.frame(year = levels(data_cmr[[1]]$year), smplng_days = 0)

days_p_yr <- list()
for (i in df$year){
  data <- length(unique(filter(data_cmr[[1]], year == i)$date))
  df$smplng_days <- c(df$smplng_days,data)}


for (i in seq(days_p_yr)){print(length(days_p_yr[[i]]))}


##Wieviele sample tage pro Monat (pro Jahr??)

##Wieviele sample tage pro dekanat


# Basic Cormack-Jolly-Seber model

model_crm <- list()

for (i in seq(data_cmr)){
  data <- crm(as.data.frame(data_cmr[[i]]), hessian  = TRUE)
  model_crm <- c(model_crm, list(data))
}

model_res = list()

for (i in seq(model_crm)){
  data <- predict(model_crm[[i]])
  model_res <- c(model_res, list(data))
}

for (i in seq(model_res)){print(model_res[[i]]$Phi)}
for (i in seq(model_res)){print(model_res[[i]]$p)}

### drop NAs

data_cmr_nona = list()

for (i in seq(data_cmr)){data <- drop_na(data_cmr[[i]], c(Gewicht_num_min))
data_cmr_nona <- c(data_cmr_nona,list(data))}

## Initializing variables

data_mod = list()

for (i in seq(data_cmr_nona)){data <- process.data(as.data.frame(data_cmr_nona[[i]]))
data_mod <- c(data_mod,list(data))}

## Design matrix for detection

design.Phi <- list(static = c("Geschlecht", "Gewicht_num_min", "Beobachter"))
design.p <- list(static = c("Geschlecht", "Gewicht_num_min", "Beobachter"))

### combine in one list
design.parameters <- list(Phi = design.Phi, p = design.p)

## Create design df for crm: processed data and design parameters into model design matrix

m3_design = list()

for (i in seq(data_mod)){
  data <- make.design.data(data_mod[[i]], design.parameters)
  m3_design <- c(m3_design, list(data))}


Phi.sex <- list(formula =  ~ Geschlecht) #response ~ predictor
Phi.weight <- list(formula =  ~ Gewicht_num_min)
Phi.ppl <- list(formula =  ~ Beobachter)
p.sex <- list(formula =  ~ Geschlecht)
p.weight <- list(formula =  ~ Gewicht_num_min)
p.ppl <- list(formula =  ~ Beobachter)

## fit the model

m3 = list()

for (i in seq(data_mod)){
  data <- crm(data_mod[[i]], 
              m3_design[[i]], 
              hessian = TRUE, 
              model.parameters = list(Phi = Phi.weight,p = p.sex))
  m3 <- c(m3, list(data))}


for (i in seq(m3)){print(predict(m3[[i]]))}


# Fit multiple models simultaneously

## Group data

multi_proc = list()

for (i in seq(data_cmr_nona)){data <- process.data(as.data.frame(data_cmr_nona[[i]]), 
                                                   groups = c("Geschlecht","Beobachter"))#,
                                                              #unique("date")))
multi_proc <- c(multi_proc,list(data))}

## Create design data

multi_design = list()

for (i in seq(multi_proc)){
  data <- make.design.data(multi_proc[[i]])
  multi_design <- c(multi_design, list(data))}


## Model formulas as function

fit.birds.cjs.models <- function(){
  
  Phi.dot <- list(formula = ~ 1)          # constant survival
  Phi.sex <- list(formula = ~ Geschlecht) # differs between males and females
  #Phi.time <- list(formula =  ~ time)     #time dependent survival
  #Phi.gewicht <- list(formula = ~ Gewicht_num_min) # differs because of weight
  Phi.ppl <- list(formula = ~  Beobachter)# differs because of observer

  p.dot <- list(formula = ~ 1)          # constant detection
  p.sex <- list(formula = ~ Geschlecht) # differs between males and females
  #p.time <- list(formula =  ~ time)     # time dependent detection
  #p.gewicht <- list(formula = ~ Gewicht_num_min) # differs because of weight
  p.ppl <- list(formula = ~  Beobachter)# differs because of observer
  
  cml <- create.model.list(c("Phi", "p"))
  results <- crm.wrapper(cml,
                         data = multi_proc[[i]],
                         ddl = multi_design[[i]],
                         external = FALSE, accumulate = FALSE,
                         hessian = TRUE)
  return(results)
  }

# Run function

birds_cjs_models <- list()
for (i in seq(multi_proc)){birds_cjs_models[[i]] <- fit.birds.cjs.models()}
saveRDS(birds_cjs_models, file = "birds_cjs_models.rds")

# Visualize output

View(birds_cjs_models[[1]][["model.table"]])
View(birds_cjs_models[[1]][["Phi.dot.p.dot"]])
predict(birds_cjs_models[[1]][["Phi.dot.p.dot"]])
summary(birds_cjs_models[[1]])

## Plot Phi of basic CJS model

Phi_dot <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(1,3)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["Phi"]]
  data_x <- rbind(data_x,data)
  Phi_dot[[i]] <- data_x}
  Phi_dot[[i]]$model <- c("Phi.dot.p.dot", "Phi.dot.p.ppl", "Phi.dot.p.sex")
  data_x <- NULL}

ggplot_Phi_dot <- list()
for (i in seq(Phi_dot)){ggplot_Phi_dot[[i]] <- 
  ggplot(Phi_dot[[i]],aes(model, estimate, ymin = lcl, ymax = ucl)) + #color = model
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("Phi\n") +
  xlab("Model\n") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_Phi_dot, nrow = 4)
ggsave("ggplot_Phi_dot.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)

###Plot density

ggplot_Phi_dot <- list()
for (i in seq(Phi_dot)){ggplot_Phi_dot[[i]] <- 
  ggplot(Phi_dot[[i]], aes(x = estimate)) +
  geom_density(alpha = 0.2, fill = "lightgrey") +
  labs(x = "Phi\n #no further assumption", 
       y = "Density") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch) +
  xlim(0.1,0.7)}
# geom_vline(xintercept = Phi_dot$estimate[1,], color = "black", linetype = "dashed")
combined_plot <- wrap_plots(ggplot_Phi_dot, nrow = 4)
ggsave("ggplot_Phi_dot_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


##Plot Phi influenced by people

Phi_ppl <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(4,6)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["Phi"]]
  data_x <- rbind(data_x,data)
  Phi_ppl[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(Phi_ppl)){
  Phi_ppl[[i]]$model[1:length(Phi_ppl[[i]][,1])/3] <- "Phi.ppl.p.dot"
  Phi_ppl[[i]]$model[(1+length(Phi_ppl[[i]][,1])/3):(2*length(Phi_ppl[[i]][,1])/3)] <- "Phi.ppl.p.ppl"
  Phi_ppl[[i]]$model[(1+2*length(Phi_ppl[[i]][,1])/3):(length(Phi_ppl[[i]][,1]))] <- "Phi.ppl.p.sex"}

### Add column with only initials of observer

for (i in seq(Phi_ppl)){  Phi_ppl[[i]]$Beobachter <- as.character(Phi_ppl[[i]]$Beobachter)}
  
get_initials <- function(name) {
  s_f_name <- strsplit(name, ", ")[[1]]
  initials <- paste0(substr(s_f_name[[1]], 1, 1), substr(s_f_name[[2]], 1, 1))
  return(initials)
}

for (i in seq(Phi_ppl)){data <- sapply(Phi_ppl[[i]]$Beobachter, get_initials)
Phi_ppl[[i]]$Initials <- data
}

### Plot

ggplot_Phi_ppl <- list()
for (i in seq(Phi_ppl)){ggplot_Phi_ppl[[i]] <- 
ggplot(Phi_ppl[[i]],aes(Initials, estimate, ymin = lcl, ymax = ucl, color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("Phi\n") +
  xlab("Observer\n") +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + #facet_grid(model ~ .) +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_Phi_ppl, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_Phi_ppl_I.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


###Plot density

ggplot_Phi_ppl <- list()
for (i in seq(Phi_ppl)){ggplot_Phi_ppl[[i]] <- 
  ggplot(Phi_ppl[[i]],
         aes(x = estimate, y = Initials, fill = Initials)) +
  geom_density_ridges(alpha = 0.2) +
  labs(x = "Phi #depending on observer", y = "Observer") +
  theme(legend.position = "none") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_Phi_ppl, nrow = 4)
ggsave("ggplot_Phi_ppl_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


##Plot Phi influenced by sex

Phi_sex <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(7,9)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["Phi"]]
  data_x <- rbind(data_x,data)
  Phi_sex[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(Phi_sex)){
  Phi_sex[[i]]$model[1:length(Phi_sex[[i]][,1])/3] <- "Phi.sex.p.dot"
  Phi_sex[[i]]$model[(1+length(Phi_sex[[i]][,1])/3):(2*length(Phi_sex[[i]][,1])/3)] <- "Phi.sex.p.ppl"
  Phi_sex[[i]]$model[(1+2*length(Phi_sex[[i]][,1])/3):(length(Phi_sex[[i]][,1]))] <- "Phi.sex.p.sex"}


ggplot_Phi_sex <- list()
for (i in seq(Phi_sex)){ggplot_Phi_sex[[i]] <- 
  ggplot(Phi_sex[[i]],aes(Geschlecht, estimate, ymin = lcl, ymax = ucl, color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("Phi\n") +
  xlab("Sex\n") +
  theme(legend.position = "none") + #facet_grid(model ~ .) +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_Phi_sex, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_Phi_sex_I.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


###Plot density

ggplot_Phi_sex <- list()
for (i in seq(Phi_sex)){ggplot_Phi_sex[[i]] <- 
  ggplot(Phi_sex[[i]],
       aes(x = estimate, y = Geschlecht, fill = Geschlecht)) +
  geom_density_ridges(alpha = 0.2) +
  labs(x = "Phi", y = "Sex") +
  theme(legend.position = "none") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}
  #xlim(0.34,0.4) +
  #geom_vline(xintercept = Phi_sex$estimate, color = "black", linetype = "dashed") +
  #facet_wrap(~ Geschlecht, scales = "free")

combined_plot <- wrap_plots(ggplot_Phi_sex, nrow = 4)
ggsave("ggplot_Phi_sex_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


##Plot p for basic CJS model

p_dot <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in c(1,4,7)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["p"]]
  data_x <- rbind(data_x,data)
  p_dot[[i]] <- data_x}
  p_dot[[i]]$model <- c("Phi.dot.p.dot", "Phi.ppl.p.dot", "Phi.sex.p.dot")
  data_x <- NULL}

ggplot_p_dot <- list()
for (i in seq(p_dot)){ggplot_p_dot[[i]] <- 
  ggplot(p_dot[[i]],aes(model, estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("p\n") +
  xlab("Model\n") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_p_dot, nrow = 4)
ggsave("ggplot_p_dot.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


###Plot density

ggplot_p_dot <- list()
for (i in seq(p_dot)){ggplot_p_dot[[i]] <-
  ggplot(p_dot[[i]], aes(x = estimate)) +
  geom_density(alpha = 0.2, fill = "lightgrey") +
  labs(x = "p\n #no further assumption", 
       y = "Density") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch) +
  xlim(0.15,0.7)}

combined_plot <- wrap_plots(ggplot_p_dot, nrow = 4)
ggsave("ggplot_p_dot_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


##Plot p influenced by people

p_ppl <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in c(2,5,8)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["p"]]
  data_x <- rbind(data_x,data)
  p_ppl[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_ppl)){
  p_ppl[[i]]$model[1:length(p_ppl[[i]][,1])/3] <- "Phi.dot.p.ppl"
  p_ppl[[i]]$model[(1+length(p_ppl[[i]][,1])/3):(2*length(Phi_ppl[[i]][,1])/3)] <- "Phi.ppl.p.ppl"
  p_ppl[[i]]$model[(1+2*length(p_ppl[[i]][,1])/3):(length(Phi_ppl[[i]][,1]))] <- "Phi.sex.p.ppl"}

### Add column with only initials of observer

for (i in seq(p_ppl)){p_ppl[[i]]$Beobachter <- as.character(p_ppl[[i]]$Beobachter)}
for (i in seq(p_ppl)){data <- sapply(p_ppl[[i]]$Beobachter, get_initials)
p_ppl[[i]]$Initials <- data}

###Plot

ggplot_p_ppl <- list()
for (i in seq(p_ppl)){ggplot_p_ppl[[i]] <- 
  ggplot(p_ppl[[i]],aes(Initials, estimate, ymin = lcl, ymax = ucl, color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("p\n") +
  xlab("Observer\n") +
  theme(axis.text.x = element_text(angle=90), legend.position = "none") + #facet_grid(model ~ .) +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_p_ppl, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_p_ppl_I.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


###Plot density

ggplot_p_ppl <- list()
for (i in seq(p_ppl)){ggplot_p_ppl[[i]] <- 
  ggplot(p_ppl[[i]],
         aes(x = estimate, y = Initials, fill = Initials)) +
  geom_density_ridges(alpha = 0.2) +
  labs(x = "p #depending on observer", y = "Observer") +
  theme(legend.position = "none") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_p_ppl, nrow = 4)
ggsave("ggplot_p_ppl_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


##Plot p influenced by sex

p_sex <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in c(3,6,9)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["p"]]
  data_x <- rbind(data_x,data)
  p_sex[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_sex)){
  p_sex[[i]]$model[1:length(p_sex[[i]][,1])/3] <- "Phi.dot.p.sex"
  p_sex[[i]]$model[(1+length(p_sex[[i]][,1])/3):(2*length(p_sex[[i]][,1])/3)] <- "Phi.ppl.p.sex"
  p_sex[[i]]$model[(1+2*length(p_sex[[i]][,1])/3):(length(p_sex[[i]][,1]))] <- "Phi.sex.p.sex"}


ggplot_p_sex <- list()
for (i in seq(p_sex)){ggplot_p_sex[[i]] <- 
  ggplot(p_sex[[i]],aes(Geschlecht, estimate, ymin = lcl, ymax = ucl, color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  ylab("p\n") +
  xlab("Sex\n") +
  theme(legend.position = "none") + #facet_grid(model ~ .) +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_p_sex, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_p_sex_I.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)


###Plot density

ggplot_p_sex <- list()
for (i in seq(p_sex)){ggplot_p_sex[[i]] <- 
  ggplot(p_sex[[i]],
         aes(x = estimate, y = Geschlecht, fill = Geschlecht)) +
  geom_density_ridges(alpha = 0.2) +
  labs(x = "p", y = "Sex") +
  theme(legend.position = "none") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_p_sex, nrow = 4)
ggsave("ggplot_p_sex_den.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)



