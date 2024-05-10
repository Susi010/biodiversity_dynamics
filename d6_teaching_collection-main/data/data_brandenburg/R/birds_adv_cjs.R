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

temp <- read_csv("data_brandenburg/raw.data/temp/data/data_OBS_DEU_P1M_T2M.csv")

# Merge bird_data and temp

## Identifier

bird_data$y.m <- format(as.Date(bird_data$date), "%Y-%m")

temp$y.m <- format(as.Date(temp$Zeitstempel), "%Y-%m")

## Merge

bird_data_merge <- merge(bird_data, temp[,c("y.m","Wert")], by="y.m", all.x =TRUE)

colnames(bird_data_merge)[colnames(bird_data_merge)== "Wert"]<-"Temp"

# Preparing for capture recapture

## change Geschlecht NA to "unbekannt"

bird_data_merge$Geschlecht[is.na(bird_data_merge$Geschlecht)]<-"unbekannt"

## Add column with initials of observer

get_initials <- function(name) {
  s_f_name <- strsplit(name, ", ")[[1]]
  initials <- paste0(substr(s_f_name[[1]], 1, 1), substr(s_f_name[[2]], 1, 1))
  return(initials)
}

bird_data_merge$Initials <- sapply(bird_data_merge$Beobachter,get_initials)

## calculate the times each bird has been captured

data_plot<-list()

for (i in unique(bird_data_merge$Artbezeichnung.deutsch)){
  data <- bird_data_merge |> 
    filter(Artbezeichnung.deutsch %in% i) |> 
    group_by(Ring_Inschrift, year) |> 
    mutate(n = n()) |> 
    filter(row_number()==1) |> 
    ungroup()
  
  data_plot[[i]]<-data}

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
                                     Geschlecht, Temp, Initials), 
                    by = "Ring_Inschrift")
  data_cmr <- c(data_cmr, list(data))}}


## clean data:
### drop species where R can not fit model

data_cmr <- data_cmr[-16]#Neuntöter
data_cmr <- data_cmr[-12]#Schwanzmeise
#data_cmr <- data_cmr[-1]#Kohlmeise


### change variables to factor variables

for (i in seq(data_cmr)){
  data_cmr[[i]]$Geschlecht <- as.factor(data_cmr[[i]]$Geschlecht) 
  data_cmr[[i]]$Temp <- as.factor(data_cmr[[i]]$Temp)
  data_cmr[[i]]$Initials <- as.factor(data_cmr[[i]]$Initials)}

### drop NA ##Only for temperature!!

data_cmr_nona = list()

for (i in seq(data_cmr)){data <- drop_na(data_cmr[[i]], c(Temp))
data_cmr_nona <- c(data_cmr_nona,list(data))}

# Fit multiple models simultaneously

## Group data

multi_proc = list()

for (i in seq(data_cmr_nona)){
  data <- process.data(as.data.frame(data_cmr_nona[[i]]),
                       groups = c("Geschlecht","Temp","Initials"))
  multi_proc <- c(multi_proc,list(data))}

## Create design data

multi_design = list()

for (i in seq(multi_proc)){
  data <- make.design.data(multi_proc[[i]])
  multi_design <- c(multi_design, list(data))}


## Model formulas as function: Sex, Year, Observer, Temp etc.

fit.birds.cjs.models <- function(){
  
  #Phi.sex.tmp <- list(formula = ~ Geschlecht*Temp)
  Phi.dot <- list(formula =  ~ 1)
  #Phi.sex.time <- list(formula =  ~ Geschlecht*time)
  #Phi.tmp.time <- list(formula = ~ Temp*time)

  p.tmp.time <- list(formula =  ~ Temp*time)
  #p.time <- list(formula =  ~ time)
  #p.dot <- list(formula =  ~ 1)
  #p.sex <- list(formula =  ~ Geschlecht)
  
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


#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_tmp_sex.rds")
#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_time.sex_phi.rds")

birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_tmp_sex.rds")

# Visualize output

## Change temp to numeric variable

for (i in seq(1,13)){
  birds_cjs_models[[i]]$Phi.sex.p.dot$results$reals$Phi$Temp <- 
    as.character(birds_cjs_models[[i]]$Phi.sex.p.dot$results$reals$Phi$Temp)}

for (i in seq(1,13)){
  birds_cjs_models[[i]]$Phi.sex.p.dot$results$reals$Phi$Temp <- 
    as.numeric(birds_cjs_models[[i]]$Phi.sex.p.dot$results$reals$Phi$Temp)}


## Plot Phi for Temp & Sex

ggplot_phi_tmpsex <- list()
for (i in seq(1,13)){ggplot_phi_tmpsex[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.sex.p.dot$results$reals$Phi, 
         aes(Temp, estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line(group=1) +
  facet_grid(Geschlecht ~.)+
  theme(legend.position = "none") +
  labs(x = "Temperature\n", 
       y = "Phi\n", 
       title= birds_cjs_models[[i]]$Phi.sex.p.dot$data$data$Artbezeichnung.deutsch[1])}
combined_plot <- wrap_plots(ggplot_phi_tmpsex, nrow = 8, byrow=FALSE) + 
  theme(plot.title = element_text("Model: Phi.sex.tmp.p.dot"), legend.position="bottom")
ggsave("ggplot_phi_tmp_sex.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 7, height = 15, units = "in", dpi = 300)

