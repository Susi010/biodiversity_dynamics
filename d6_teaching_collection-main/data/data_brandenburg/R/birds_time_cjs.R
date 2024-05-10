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


### uneven sampling interval ###

sampling_dates <- as.Date(sort(unique(bird_data$date)))
years <- format(sampling_dates, format="%Y")
table(years)[i]-1
sampling_interval <- diff(sampling_dates)/(365)


## calculate the times each bird has been captured

data_plot<-list()

for (i in unique(bird_data$Artbezeichnung.deutsch)){
  data <- bird_data |> 
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

for (i in seq(cmr_hist)){if (dim(table(rs[[i]]$capt_sum))>1 & #nur Arten wo mind. ein Wiederfang
                             sum(rs[[i]]$capt_sum == "2") > 4){ #nur Arten wo mind. 5 Indiv einen Wiederfang
  data <- left_join(rs[[i]],data_plot[[i]] |> 
                      dplyr::select( Ring_Inschrift, Artbezeichnung.deutsch,
                                     Geschlecht, Initials), 
                    by = "Ring_Inschrift")
  data_cmr <- c(data_cmr, list(data))}}


## clean data:
### drop species where R can not fit model

data_cmr <- data_cmr[-13]#Neuntöter
data_cmr <- data_cmr[-9]#Kohlmeise
data_cmr <- data_cmr[-3]#Schwanzmeise


### change variables to factor variables

for (i in seq(data_cmr)){
  data_cmr[[i]]$Geschlecht <- as.factor(data_cmr[[i]]$Geschlecht)
  data_cmr[[i]]$Initials <- as.factor(data_cmr[[i]]$Initials)}


# Fit multiple models simultaneously

## Group data

multi_proc = list()

for (i in seq(data_cmr)){
  data <- process.data(as.data.frame(data_cmr[[i]]),
                       groups = c("Geschlecht","Initials"))
  multi_proc <- c(multi_proc,list(data))}

## Create design data

multi_design = list()

for (i in seq(multi_proc)){
  data <- make.design.data(multi_proc[[i]])
  multi_design <- c(multi_design, list(data))}


## Model formulas as function: Sex, Year, Observer, Temp etc.

fit.birds.cjs.models <- function(){
  
  #Phi.dot <- list(formula =  ~ 1)
  #Phi.sex <- list(formula =  ~ Geschlecht)
  Phi.time <- list(formula =  ~ time)

  
  #p.time <- list(formula =  ~ time)
  p.dot <- list(formula =  ~ 1)
  p.sex <- list(formula =  ~ Geschlecht)
  p.ppl <- list(formula =  ~ Initials)
  
  cml <- create.model.list(c("Phi", "p"))
  results <- crm.wrapper(cml,
                         data = multi_proc[[i]],
                         ddl = multi_design[[i]],
                         time.intervals = sampling_interval, ## for uneven time interval
                         external = FALSE, accumulate = FALSE,
                         hessian = TRUE)
  return(results)
}

# Run function

birds_cjs_models <- list()
for (i in seq(multi_proc)){birds_cjs_models[[i]] <- fit.birds.cjs.models()}


#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_time.rds")
#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_uneven_time.rds")
#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_phi_time.rds")
#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_uneven_time_p.rds")
#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_uneven_time_phi.rds")

birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_time.rds")
birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time.rds")
birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_phi_time.rds")
birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time_p.rds")
birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time_phi.rds")

## Extract AIC

taxa<-data.frame()
for(i in seq(birds_cjs_models)){taxa <- 
  rbind(taxa,data.frame(taxa=birds_cjs_models[[i]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]))}

AIC<-data.frame()
for(i in seq(birds_cjs_models)){AIC <- 
  rbind(AIC, birds_cjs_models[[i]]$model.table)}

#AIC$taxa<- taxa$taxa
#write.csv(AIC, file = "data_brandenburg/output/AIC_time_uneven.csv")

#AIC$taxa<- rep(taxa$taxa,each=3)
#write.csv(AIC, file = "data_brandenburg/output/AIC_time_even.csv")
AIC<-read.csv("data_brandenburg/output/AIC_time_even.csv")

# Visualize output
  

# Plot p for time interval

p_time <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(1,3)){
  data <- birds_cjs_models[[i]][[j]]$results$reals$p
  data_x <- rbind(data_x,data)
  p_time[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_time)){
  p_time[[i]]$model[1:length(p_time[[i]][,1])/3] <- "Phi.dot.p.time"
  p_time[[i]]$model[(1+length(p_time[[i]][,1])/3):(2*length(p_time[[i]][,1])/3)] <- "Phi.sex.p.time"
  p_time[[i]]$model[(1+2*length(p_time[[i]][,1])/3):(length(p_time[[i]][,1]))] <- "Phi.time.p.time"}

for (i in seq(p_time)){
  p_time[[i]]$time <- as.numeric(p_time[[i]]$time)}

ggplot_p_time <- list()
for (i in seq(1,13)){ggplot_p_time[[i]] <-
  ggplot(p_time[[i]], 
         aes(time, estimate, ymin = lcl, ymax = ucl,color=model)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line(group=1) +
  facet_grid(model ~ .) +
  theme(legend.position = "none",
        legend.text= element_text(size=11)) +
  labs(x = "Time interval\n", 
       y = "p\n", 
       title= element_text(birds_cjs_models[[i]]$Phi.dot.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_p_time[[8]]$theme$legend.position<-"bottom"
combined_plot <- wrap_plots(ggplot_p_time, nrow = 4, byrow=FALSE)
ggsave("ggplot_p_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


# Plot p for uneven time interval


for (i in seq(1,14)){
  birds_cjs_models[[i]]$Phi.time.p.time$results$reals$p$time <- 
    as.numeric(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$p$time)}

ggplot_p_un_time <- list()
for (i in seq(1,14)){ggplot_p_un_time[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$p, 
         aes(time, estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line(group=1) +
  labs(x = "Time interval\n", 
       y = "p\n", 
       subtitle= element_text(birds_cjs_models[[i]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_p_un_time[[1]]$labels<-
  labs(x = "Time interval\n", 
       y = "p\n",
       subtitle= element_text(birds_cjs_models[[1]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]),
       title = "Model: Phi.time.p.time: \nuneven time interval")
combined_plot <- wrap_plots(ggplot_p_un_time, nrow = 4, byrow=FALSE)
ggsave("ggplot_p_un_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


# Plot Phi for uneven time interval


for (i in seq(1,14)){
  birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi$time <- 
    as.numeric(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi$time)}

ggplot_phi_un_time <- list()
for (i in seq(1,14)){ggplot_phi_un_time[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi, 
         aes(time, estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line(group=1) +
  labs(x = "Time interval\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[i]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_phi_un_time[[1]]$labels<-
  labs(x = "Time interval\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[1]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]),
       title = "Model: Phi.time.p.time: \nuneven time interval")
combined_plot <- wrap_plots(ggplot_phi_un_time, nrow = 4, byrow=FALSE)
ggsave("ggplot_phi_un_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


# Plot Phi for even time interval
##Phi_sex_p_time

ggplot_phi_sex_p_time <- list()
for (i in seq(1,13)){ggplot_phi_sex_p_time[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.sex.p.time$results$reals$Phi, 
         aes(Geschlecht, estimate, ymin = lcl, ymax = ucl)) +
  #geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_errorbar() +
  labs(x = "Geschlecht\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[i]]$Phi.sex.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_phi_sex_p_time[[1]]$labels<-
  labs(x = "Geschlecht\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[1]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]),
       title = "Model: Phi.sex.p.time: \neven time interval")
combined_plot <- wrap_plots(ggplot_phi_sex_p_time, nrow = 4, byrow=FALSE)
ggsave("ggplot_phi_sex_p_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


##Phi_dot_p_time

ggplot_phi_dot_p_time <- list()
for (i in seq(1,13)){ggplot_phi_dot_p_time[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.dot.p.time$results$reals$Phi, 
         aes(x="Phi.dot_p.time", estimate, ymin = lcl, ymax = ucl)) +
  #geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_errorbar() +
  labs(x = "Phi.dot_p.time\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[i]]$Phi.dot.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_phi_dot_p_time[[1]]$labels<-
  labs(x = "Phi.dot_p.time\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[1]]$Phi.dot.p.time$data$data$Artbezeichnung.deutsch[1]),
       title = "Model: Phi.dot.p.time: \neven time interval")
combined_plot <- wrap_plots(ggplot_phi_dot_p_time, nrow = 4, byrow=FALSE)
ggsave("ggplot_phi_dot_p_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


##Phi_time_p_time


for (i in seq(1,13)){
  birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi$time <- 
    as.numeric(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi$time)}

ggplot_phi_ev_time <- list()
for (i in seq(1,13)){ggplot_phi_ev_time[[i]] <-
  ggplot(birds_cjs_models[[i]]$Phi.time.p.time$results$reals$Phi, 
         aes(time, estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line(group=1) +
  labs(x = "Time interval\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[i]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]))}
ggplot_phi_ev_time[[1]]$labels<-
  labs(x = "Time interval\n", 
       y = "Phi\n",
       subtitle= element_text(birds_cjs_models[[1]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1]),
       title = "Model: Phi.time.p.time: \neven time interval")
combined_plot <- wrap_plots(ggplot_phi_ev_time, nrow = 8, byrow=FALSE)
ggsave("ggplot_phi_ev_time.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)
