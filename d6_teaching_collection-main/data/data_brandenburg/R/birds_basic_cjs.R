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

bird_data <- read.csv("data_brandenburg/data.raw/bird_data_rietzer_see_2000_2022_filled_min_4326.csv")

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


## Merge recapture data with data_plot data

### drop species where recapture too low

data_cmr = list()

for (i in seq(cmr_hist)){if (dim(table(rs[[i]]$capt_sum))>1 & 
                             sum(rs[[i]]$capt_sum == "2") > 4){
  data <- left_join(rs[[i]],data_plot[[i]] |> 
                        dplyr::select( Ring_Inschrift, Artbezeichnung.deutsch, 
                                       Geschlecht, Initials, year), 
                      by = "Ring_Inschrift")
  data_cmr <- c(data_cmr, list(data))}}

### change variables to factor variables

for (i in seq(data_cmr)){
  data_cmr[[i]]$Geschlecht <- as.factor(data_cmr[[i]]$Geschlecht)
  data_cmr[[i]]$Initials <- as.factor(data_cmr[[i]]$Initials)
  data_cmr[[i]]$year <- as.factor(data_cmr[[i]]$year)}
  

## clean data:
### drop species where R can not fit model

data_cmr <- data_cmr[-13]#Neuntöter
data_cmr <- data_cmr[-9]#Kohlmeise
data_cmr <- data_cmr[-3]#Schwanzmeise

# Change species order to show relation

arten_sort <- read.csv("data_brandenburg/output/arten_sort.csv")
data_cmr <- data_cmr[arten_sort$X]


# Basic Cormack-Jolly-Seber model

model_crm <- list()

for (i in seq(data_cmr)){
  data <- crm(as.data.frame(data_cmr[[i]]), hessian  = TRUE)
  model_crm <- c(model_crm, list(data))
}

saveRDS(model_crm, file = "data_brandenburg/output/birds_cjs_model_dot.rds")

model_crm <- readRDS("data_brandenburg/output/birds_cjs_model_dot.rds")

## Extract AIC

AIC<- data.frame()
for(i in seq(model_crm)){AIC <- rbind(AIC,data.frame(AIC=model_crm[[i]]$results$AIC,
                  neg2lnl=model_crm[[i]]$results$neg2lnl,
                  convergence=model_crm[[i]]$results$convergence))}

taxa<-data.frame()
for(i in seq(model_crm)){taxa <- 
  rbind(taxa,data.frame(taxa=model_crm[[i]]$data$data$Artbezeichnung.deutsch[1]))}

AIC$taxa<-taxa$taxa

#write.csv(AIC, file = "data_brandenburg/output/AIC_dot.csv")

## Plot estimation of parameter of basic model

df_basic <- data.frame()
for (i in seq(model_crm)){df_basic <- rbind(
  df_basic,model_crm[[i]]$results$reals$Phi)
df_basic <- rbind(
  df_basic,model_crm[[i]]$results$reals$p)}
df_basic$species <- rep(arten_sort$bird,each =2)
df_basic$par <- rep(c("Phi","p"), 13)
df_basic$species <- factor(df_basic$species, levels = arten_sort$bird)

est_basic  <-
  ggplot(df_basic, aes(x= estimate,
                       y = species, 
                       xmin = lcl, 
                       xmax = ucl, 
                       color=species))+
  labs(y="",x = "Parameter estimate", title = "Model: Phi.dot.p.dot")+
  geom_errorbar() +
  geom_point()+
  facet_grid(df_basic$par~.)+
  theme(legend.position = "none")+
  scale_color_manual(values = setNames(arten_sort$clr, arten_sort$bird))
ggsave("ggplot_est_basic.png", plot = est_basic, 
       path = "data_brandenburg/plots", 
       width = 12, height = 12, units = "in", dpi = 300)


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


## Model formulas as function

fit.birds.cjs.models <- function(){
  
  Phi.dot <- list(formula = ~ 1)
  Phi.sex <- list(formula = ~ Geschlecht)
  #Phi.time <- list(formula =  ~ time)    

  p.dot <- list(formula = ~ 1)
  p.sex <- list(formula = ~ Geschlecht)
  #p.time <- list(formula =  ~ time)
  p.ppl <- list(formula = ~  Initials)
  
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

#saveRDS(birds_cjs_models, file = "data_brandenburg/output/birds_cjs_models_basic.rds")

birds_cjs_models <- readRDS("data_brandenburg/output/birds_cjs_models_basic.rds")

# Change order to match phylo tree

birds_cjs_models <- birds_cjs_models[arten_sort$X]

#Extract AIC

AIC<-data.frame()
for(i in seq(birds_cjs_models)){AIC <- 
  rbind(AIC, birds_cjs_models[[i]]$model.table)}

AIC$taxa<- rep(arten_sort$bird,each=6)
#write.csv(AIC, file = "data_brandenburg/output/AIC_basic.csv")


# Visualize output

##Plot p influenced by people

p_ppl <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in c(2,5)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["p"]]
  data_x <- rbind(data_x,data)
  p_ppl[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_ppl)){
  p_ppl[[i]]$model[1:length(p_ppl[[i]][,1])/2] <- "Phi.dot.p.ppl"
  p_ppl[[i]]$model[(1+length(p_ppl[[i]][,1])/2):length(p_ppl[[i]][,1])] <- "Phi.sex.p.ppl"
 }

###Plot

ggplot_p_ppl <- list()
for (i in seq(p_ppl)){ggplot_p_ppl[[i]] <- 
  ggplot(p_ppl[[i]],aes(Initials, 
                        estimate, 
                        ymin = lcl, 
                        ymax = ucl, 
                        color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  facet_grid(model ~ .) +
  theme(axis.text.x = element_text(angle=90,
                                   color="black"), 
        legend.position = "none",
        legend.text= element_text(size=10)) +
  labs(x = "Observer\n", 
       y = "p\n", 
       title= element_text(arten_sort$bird[i]))}
ggplot_p_ppl[[8]]$theme$legend.position<-"bottom"
combined_plot <- wrap_plots(ggplot_p_ppl, nrow = 4, byrow=FALSE)
ggsave("ggplot_p_ppl.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


##Plot p influenced by sex

p_sex <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in c(3,6)){
  data <- birds_cjs_models[[i]][[j]][["results"]][["reals"]][["p"]]
  data_x <- rbind(data_x,data)
  p_sex[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(p_sex)){
  p_sex[[i]]$model[1:length(p_sex[[i]][,1])/2] <- "Phi.dot.p.sex"
  p_sex[[i]]$model[(1+length(p_sex[[i]][,1])/2):(length(p_sex[[i]][,1]))] <- "Phi.sex.p.sex"
}


ggplot_p_sex <- list()
for (i in seq(p_sex)){ggplot_p_sex[[i]] <- 
  ggplot(p_sex[[i]],aes(Geschlecht, 
                        estimate, 
                        ymin = lcl, 
                        ymax = ucl, 
                        color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  facet_grid(model ~ .) +
  theme(axis.text.x = element_text(color="black"), 
        legend.position = "none",
        legend.text= element_text(size=11))+
  labs(x = "Sex", 
       y = "p\n", 
       title= element_text(arten_sort$bird[i]))}
ggplot_p_sex[[8]]$theme$legend.position<-"bottom"
combined_plot <- wrap_plots(ggplot_p_sex, nrow = 4, byrow=FALSE)
ggsave("ggplot_p_sex.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)


##Plot phi influenced by sex

phi_sex <- list()
data_x <- data.frame()
for (i in seq(birds_cjs_models)){for (j in seq(4,6)){
  data <- birds_cjs_models[[i]][[j]]$results$reals$Phi
  data_x <- rbind(data_x,data)
  phi_sex[[i]] <- data_x}
  data_x <- NULL}

for (i in seq(phi_sex)){
  phi_sex[[i]]$model[1:length(phi_sex[[i]][,1])/3] <- "Phi.sex.p.dot"
  phi_sex[[i]]$model[(1+length(phi_sex[[i]][,1])/3):(2*length(phi_sex[[i]][,1])/3)] <- "Phi.sex.p.ppl"
  phi_sex[[i]]$model[(1+2*length(phi_sex[[i]][,1])/3):(length(phi_sex[[i]][,1]))] <- "Phi.sex.p.sex"}


ggplot_phi_sex <- list()
for (i in seq(phi_sex)){ggplot_phi_sex[[i]] <- 
  ggplot(phi_sex[[i]],aes(Geschlecht, 
                          estimate, 
                          ymin = lcl, 
                          ymax = ucl, 
                          color=model)) +
  geom_errorbar() +
  geom_point() +
  geom_line() +
  facet_grid(model ~ .) +
  theme(legend.position = "none",
        legend.text= element_text(size=11)) +
  labs(x = "Sex", 
       y = "Phi\n", 
       title= element_text(arten_sort$bird[i]))}
ggplot_phi_sex[[8]]$theme$legend.position<-"bottom"
combined_plot <- wrap_plots(ggplot_phi_sex, nrow = 4, byrow=FALSE)
ggsave("ggplot_phi_sex.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 13, height = 13, units = "in", dpi = 300)
