# Set working directory

root <- setwd("C:/Users/User/OneDrive - Universität Potsdam/IZW/biodiversity_dynamics/d6_teaching_collection-main/data")

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
               "patchwork",
               "ggdist")

for (package in package.list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}



# Load data

bird_data <- read.csv("data_brandenburg/raw.data/bird_data_rietzer_see_2000_2022_filled_min_4326.csv")


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
  data <- bird_data |> 
    filter(Artbezeichnung.deutsch %in% i) |> 
    group_by(Ring_Inschrift, year) |> 
    mutate(n = n()) |> 
    filter(row_number()==1) |> 
    ungroup()
  
  data_plot[[i]]<-data
}

## capture-recapture history for each bird species

cmr_hist <- list()
for (i in seq(data_plot)){
  data <- table(data_plot[[i]]$Ring_Inschrift, data_plot[[i]]$year)
  cmr_hist[[i]] <- data}


rs <- list()
for (i in seq(cmr_hist)){
  data <- cmr_hist[[i]] |> data.frame() |> 
    pivot_wider(names_from = Var2, values_from = Freq) |> 
    mutate(capt_sum = rowSums(across(where(is.numeric)))) |> 
    unite(col = ch, -Var1, -capt_sum, sep = "", remove = TRUE) |>
    rename(Ring_Inschrift = "Var1")
  rs[[i]] <- data}


# Merge the recapture data together with the data_plot data
### drop species where recapture too low

data_cmr = list()

for (i in seq(cmr_hist)){if (dim(table(rs[[i]]$capt_sum))>1 & 
                             sum(rs[[i]]$capt_sum == "2") > 4){
  data <- left_join(rs[[i]],data_plot[[i]] |> 
                      dplyr::select( Ring_Inschrift, Artbezeichnung.deutsch, 
                                     Geschlecht, year, Initials), 
                    by = "Ring_Inschrift")
  data_cmr <- c(data_cmr, list(data))}}


## clean data:
### drop species where R can not fit model

#data_cmr <- data_cmr[-13]#Neuntöter
#data_cmr <- data_cmr[-9]#KOhlmeise
#data_cmr <- data_cmr[-3]#Schwanzmeise

# Change species order to show relation

arten_sort <- read.csv("data_brandenburg/output/arten_sort.csv")
data_cmr <- data_cmr[arten_sort$X]

arten_sort$clr <- c("darkblue","darkcyan","dodgerblue","deepskyblue","deepskyblue4","cyan","blueviolet","orchid",
                    "darkorange","red","yellow","orange","gold")


### change variables to factor variables

for (i in seq(data_cmr)){
  data_cmr[[i]]$Geschlecht <- as.factor(data_cmr[[i]]$Geschlecht)
  data_cmr[[i]]$year <- as.factor(data_cmr[[i]]$year)
  data_cmr[[i]]$Initials <- as.factor(data_cmr[[i]]$Initials)}


# Fit multiple models simultaneously

## Group data

multi_proc = list()

for (i in seq(data_cmr)){
  data <- process.data(as.data.frame(data_cmr[[i]]),
                       groups = c("Geschlecht","year"))
  multi_proc <- c(multi_proc,list(data))}

## Create design data

multi_design = list()

for (i in seq(multi_proc)){
  data <- make.design.data(multi_proc[[i]])
  multi_design <- c(multi_design, list(data))}


## Model formulas as function: Sex, Year, Observer, Temp etc.

fit.birds.cjs.models <- function(){
  
  Phi.sex <- list(formula = ~ Geschlecht)
  Phi.dot <- list(formula =  ~ 1)
  Phi.year <- list(formula =  ~ year)
  
  
  p.year <- list(formula =  ~ year)
  #p.year.ppl <- list(formula = ~ year*Initials)
  
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

#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_yr_p.rds")

birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_yr_p.rds")


# Visualize output


## Plot p

### Store in df

p_yr <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(1,3)){
  data <- birds_cjs_models[[i]][[j]]$results$reals$p
  data_x <- rbind(data_x,data)
  p_yr[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_yr)){
  p_yr[[i]]$model[1:length(p_yr[[i]][,1])/3] <- "Phi.dot.p.year"
  p_yr[[i]]$model[(1+length(p_yr[[i]][,1])/3):(2*length(p_yr[[i]][,1])/3)] <- "Phi.sex.p.year"
  p_yr[[i]]$model[(1+2*length(p_yr[[i]][,1])/3):(length(p_yr[[i]][,1]))] <- "Phi.year.p.year"}


### Change years to numeric variable

for (i in seq(p_yr)){
  p_yr[[i]]$year <- as.character(p_yr[[i]]$year)}

for (i in seq(p_yr)){
  p_yr[[i]]$year <- as.numeric(p_yr[[i]]$year)}


### Plot p influenced by year

ggplot_p_yr <- list()
for (i in seq(p_yr)){ggplot_p_yr[[i]] <-
  ggplot(p_yr[[i]],aes(year, estimate, ymin = lcl, ymax = ucl, color=model)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  ylab("p\n") +
  xlab("Year\n") +
  theme(legend.position = "none") + 
  facet_grid(model ~ .) +
  ggtitle(birds_cjs_models[[i]]$Phi.dot.p.year$data$data$Artbezeichnung.deutsch[1])}

combined_plot <- wrap_plots(ggplot_p_yr, nrow = 4, byrow=FALSE) + theme(legend.position = "bottom")
ggsave("ggplot_p_yr.png", plot = combined_plot, path = "data_brandenburg/plots", width = 14, height = 15, units = "in", dpi = 300)


##Plot Phi

### extract estimates and change to numeric variable

phi_yr <- list()
for (i in seq(birds_cjs_models)){
  data <- birds_cjs_models[[i]]$Phi.year.p.year$results$reals$Phi
  phi_yr[[i]] <- data}

for (i in seq(phi_yr)){
  phi_yr[[i]]$year <- as.character(phi_yr[[i]]$year)
  phi_yr[[i]]$year <- as.numeric(phi_yr[[i]]$year)}

### Plot Phi influenced by year


ggplot_phi_yr <- list()
for (i in seq(phi_yr)){ggplot_phi_yr[[i]] <-
  ggplot(phi_yr[[i]], aes(year, estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  xlab("\nyear") + ylab("Phi\n") +
  ggtitle(birds_cjs_models[[i]][["Phi.year.p.year"]][["data"]][["data"]]$Artbezeichnung.deutsch[1])}

combined_plot <- wrap_plots(ggplot_phi_yr, nrow = 4, byrow = FALSE)
ggsave("ggplot_phi_yr.png", plot = combined_plot, path = "data_brandenburg/plots", width = 14, height = 15, units = "in", dpi = 300)