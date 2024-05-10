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


## COllect AIC of all models

model_basic <- readRDS("data_brandenburg/output/birds_cjs_models_basic.rds")

taxa_basic <- c()
for(i in seq(model_basic)){taxa_basic <- 
  c(taxa_basic,model_basic[[i]]$Phi.dot.p.dot$data$data$Artbezeichnung.deutsch[1])}

model_time <- readRDS("data_brandenburg/output/birds_cjs_models_time.rds")

taxa_time <- c()
for(i in seq(model_time)){taxa_time <- 
  c(taxa_time,model_time[[i]]$Phi.dot.p.time$data$data$Artbezeichnung.deutsch[1])}

model_phi_time <- readRDS("data_brandenburg/output/birds_cjs_models_phi_time.rds")

taxa_phi_time <- c()
for(i in seq(model_phi_time)){taxa_phi_time <- 
  c(taxa_phi_time,model_phi_time[[i]]$Phi.time.p.dot$data$data$Artbezeichnung.deutsch[1])}

model_un_time <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time.rds")

taxa_un_time <- c()
for(i in seq(model_un_time)){taxa_un_time <- 
  c(taxa_un_time,model_un_time[[i]]$Phi.time.p.time$data$data$Artbezeichnung.deutsch[1])}

model_un_time_p <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time_p.rds")

taxa_un_time_p <- c()
for(i in seq(model_un_time_p)){taxa_un_time_p <- 
  c(taxa_un_time_p,model_un_time_p[[i]]$Phi.dot.p.time$data$data$Artbezeichnung.deutsch[1])}

model_un_time_phi <- readRDS("data_brandenburg/output/birds_cjs_models_uneven_time_phi.rds")

taxa_un_time_phi <- c()
for(i in seq(model_un_time_phi)){taxa_un_time_phi <- 
  c(taxa_un_time_phi,model_un_time_phi[[i]]$Phi.time.p.dot$data$data$Artbezeichnung.deutsch[1])}

model_yr <- readRDS("data_brandenburg/output/birds_cjs_models_yr_p.rds")

taxa_yr <- c()
for(i in seq(model_yr)){taxa_yr <- 
  c(taxa_yr,model_yr[[i]]$Phi.dot.p.year$data$data$Artbezeichnung.deutsch[1])}

model_tmp_p <- readRDS("data_brandenburg/output/birds_cjs_models_temp_p.rds")

taxa_tmp_p <- c()
for(i in seq(model_tmp_p)){taxa_tmp_p <- 
  c(taxa_tmp_p,model_tmp_p[[i]]$Phi.dot.p.tmp$data$data$Artbezeichnung.deutsch[1])}

model_tmp_phi <- readRDS("data_brandenburg/output/birds_cjs_models_temp_phi.rds")

taxa_tmp_phi <- c()
for(i in seq(model_tmp_phi)){taxa_tmp_phi <- 
  c(taxa_tmp_phi,model_tmp_phi[[i]]$Phi.tmp.p.dot$data$data$Artbezeichnung.deutsch[1])}

model_tmp_sex <- readRDS("data_brandenburg/output/birds_cjs_models_tmp_sex.rds")

taxa_tmp_sex <- c()
for(i in seq(model_tmp_sex)){taxa_tmp_sex <- 
  c(taxa_tmp_sex,model_tmp_sex[[i]]$Phi.sex.p.dot$data$data$Artbezeichnung.deutsch[1])}

## Rohrammer -> AIC  and plot best model

AIC_Rohrammer <- rbind(model_basic[[1]]$model.table, 
                       model_time[[13]]$model.table,
                       model_phi_time[[7]]$model.table,
                       #model_un_time[[8]]$model.table,
                       #model_un_time_p[[7]]$model.table,
                       #model_un_time_phi[[7]]$model.table,
                       model_yr[[8]]$model.table,
                       model_tmp_p[[4]]$model.table,
                       model_tmp_phi[[4]]$model.table,
                       model_tmp_sex[[3]]$model.table)

write.csv(AIC_Rohrammer,file="data_brandenburg/output/AIC_Rohrammer.csv")

rownames(AIC_Rohrammer) <- AIC_Rohrammer$model

###Plot Phi.dot_p.tmp:


rohrammer <- list()

rohrammer[[1]] <-ggplot(model_tmp_p[[4]]$Phi.dot.p.tmp$results$reals$Phi,
         aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.tmp",
       title = "Rohrammer")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

rohrammer[[2]]<- ggplot(model_tmp_p[[4]]$Phi.dot.p.tmp$results$reals$p,
       aes(x= as.numeric(as.character(Temp)), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature", 
       y = "p\n")
combined_plot <- wrap_plots(rohrammer,nrow=2)
ggsave("rohrammer.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)

## Goldammer -> AIC  and plot best model

AIC_Goldammer <- rbind(model_basic[[2]]$model.table, 
                       model_time[[12]]$model.table, 
                       model_phi_time[[9]]$model.table,
                       #model_un_time[[10]]$model.table,
                       #model_un_time_p[[9]]$model.table,
                       #model_un_time_phi[[9]]$model.table,
                       model_yr[[11]]$model.table,
                       model_tmp_p[[11]]$model.table,
                       model_tmp_phi[[11]]$model.table,
                       model_tmp_sex[[10]]$model.table)

rownames(AIC_Goldammer) <- AIC_Goldammer$model

###Plot Phi.sex_p.time:


goldammer <- list()
goldammer[[1]] <-ggplot(model_time[[12]]$Phi.sex.p.time$results$reals$Phi,
                        aes(x= Geschlecht, y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "Sex", 
       y = "Phi\n",
       subtitle= "Model: Phi.sex_p.time",
       title = "Goldammer")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

goldammer[[2]]<- ggplot(model_time[[12]]$Phi.sex.p.time$results$reals$p,
                        aes(x= as.numeric(time), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Years: even time interval", 
       y = "p\n")
combined_plot <- wrap_plots(goldammer,nrow=2)
ggsave("goldammer.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Gartengrasmücke -> AIC  and plot best model

AIC_Gartengrasmücke <- rbind(model_basic[[3]]$model.table, 
                       model_time[[2]]$model.table, 
                       model_phi_time[[8]]$model.table,
                       #model_un_time[[9]]$model.table,
                       #model_un_time_p[[8]]$model.table,
                       #model_un_time_phi[[8]]$model.table,
                       model_yr[[10]]$model.table,
                       model_tmp_p[[13]]$model.table,
                       model_tmp_phi[[13]]$model.table,
                       model_tmp_sex[[12]]$model.table)

rownames(AIC_Gartengrasmücke) <- AIC_Gartengrasmücke$model

###Plot Phi.tmp_p.dot:

gartengrasmücke <- list()
gartengrasmücke[[1]] <-ggplot(model_tmp_phi[[13]]$Phi.tmp.p.dot$results$reals$Phi,
                        aes(x= as.numeric(as.character(Temp)), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line()+
  labs(x = "Temperature", 
       y = "Phi\n",
       subtitle= "Model: Phi.tmp_p.dot",
       title = "Gartengrasmücke")+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

gartengrasmücke[[2]]<- ggplot(model_tmp_phi[[13]]$Phi.tmp.p.dot$results$reals$p[3,],
                        aes(x= "p.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  ylim(0,1)+
  labs(x = "", 
       y = "p\n")
combined_plot <- wrap_plots(gartengrasmücke,nrow=2)
ggsave("gartengrasmücke.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Fitis -> AIC  and plot best model

AIC_Fitis <- rbind(model_basic[[4]]$model.table, 
                       model_time[[1]]$model.table,
                      model_phi_time[[6]]$model.table,
                       #model_un_time[[7]]$model.table,
                   #model_un_time_p[[6]]$model.table,
                   #model_un_time_phi[[6]]$model.table,
                       model_yr[[7]]$model.table,
                       model_tmp_p[[7]]$model.table,
                       model_tmp_phi[[7]]$model.table,
                       model_tmp_sex[[6]]$model.table)

rownames(AIC_Fitis) <- AIC_Fitis$model


###Plot Phi.sex_p.dot:

fitis <- list()
fitis[[1]] <-ggplot(model_basic[[4]]$Phi.sex.p.dot$results$reals$Phi,
                              aes(x= Geschlecht, y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "Sex", 
       y = "Phi\n",
       subtitle= "Model: Phi.sex_p.dot",
       title = "Fitis")+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

fitis[[2]]<- ggplot(model_basic[[4]]$Phi.sex.p.dot$results$reals$p,
                              aes(x= "p.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  ylim(0,1)+
  labs(x = "", 
       y = "p\n")
combined_plot <- wrap_plots(fitis,nrow=2)
ggsave("fitis.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Feldsperling -> AIC  and plot best model
AIC_Feldsperling <- rbind(model_basic[[5]]$model.table, 
                       model_time[[11]]$model.table,
                       model_phi_time[[5]]$model.table,
                       #model_un_time[[6]]$model.table,
                       #model_un_time_p[[5]]$model.table,
                       #model_un_time_phi[[5]]$model.table,
                       model_yr[[6]]$model.table,
                       model_tmp_p[[2]]$model.table,
                       model_tmp_phi[[2]]$model.table,
                       model_tmp_sex[[1]]$model.table)


rownames(AIC_Feldsperling) <- AIC_Feldsperling$model

###Plot Phi.tmp_p.dot:

feldsperling <- list()
feldsperling[[1]] <-ggplot(model_tmp_phi[[2]]$Phi.tmp.p.dot$results$reals$Phi,
                              aes(x= as.numeric(as.character(Temp)), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line()+
  labs(x = "Temperature", 
       y = "Phi\n",
       subtitle= "Model: Phi.tmp_p.dot",
       title = "Feldsperling")+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

feldsperling[[2]]<- ggplot(model_tmp_phi[[2]]$Phi.tmp.p.dot$results$reals$p[2,],
                              aes(x= "p.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  ylim(0,1)+
  labs(x = "", 
       y = "p\n")
combined_plot <- wrap_plots(feldsperling,nrow=2)
ggsave("feldsperling.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Rohrschwirl -> AIC  and plot best model
AIC_Rohrschwirl <- rbind(model_basic[[6]]$model.table, 
                       model_time[[6]]$model.table,
                       model_phi_time[[4]]$model.table,
                       #model_un_time[[5]]$model.table,
                       #model_un_time_p[[4]]$model.table,
                       #model_un_time_phi[[4]]$model.table,
                       model_yr[[5]]$model.table,
                       model_tmp_p[[8]]$model.table,
                       model_tmp_phi[[8]]$model.table,
                       model_tmp_sex[[7]]$model.table)

rownames(AIC_Rohrschwirl) <- AIC_Rohrschwirl$model

###Plot Phi.dot_p.tmp:

p_tmp_rohrschwirl<-data.frame()
for (i in seq(1,76)){p_tmp_rohrschwirl <- 
  rbind(p_tmp_rohrschwirl,model_tmp_p[[8]]$Phi.dot.p.tmp$results$reals$p[3*i,])}

rohrschwirl <- list()

rohrschwirl[[1]] <-ggplot(model_tmp_p[[8]]$Phi.dot.p.tmp$results$reals$Phi,
                        aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.tmp",
       title = "Rohrschwirl")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

rohrschwirl[[2]]<- ggplot(p_tmp_rohrschwirl,
                        aes(x= as.numeric(as.character(Temp)), 
                            y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature", 
       y = "p\n")
combined_plot <- wrap_plots(rohrschwirl,nrow=2)
ggsave("rohrschwirl.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Blaumeise -> AIC  and plot best model
AIC_Blaumeise <- rbind(model_basic[[7]]$model.table, 
                       model_time[[8]]$model.table,
                       model_phi_time[[3]]$model.table,
                       #model_un_time[[4]]$model.table,
                       #model_un_time_p[[3]]$model.table,
                       #model_un_time_phi[[3]]$model.table,
                       model_yr[[4]]$model.table,
                       model_tmp_p[[12]]$model.table,
                       model_tmp_phi[[12]]$model.table,
                       model_tmp_sex[[11]]$model.table)


rownames(AIC_Blaumeise) <- AIC_Blaumeise$model

###Plot Phi.time_p.sex:

blaumeise <- list()
blaumeise[[1]]<- ggplot(model_phi_time[[3]]$Phi.time.p.sex$results$reals$Phi,
                        aes(x= as.numeric(time), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Years: even time interval", 
       y = "Phi\n",
       subtitle= "Model: Phi.time_p.sex",
       title = "Blaumeise")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

blaumeise[[2]] <-ggplot(model_phi_time[[3]]$Phi.time.p.sex$results$reals$p,
                        aes(x= Geschlecht, y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "Sex", 
       y = "p\n")
combined_plot <- wrap_plots(blaumeise,nrow=2)
ggsave("blaumeise.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Bartmeise -> AIC  and plot best model
AIC_Bartmeise <- rbind(model_basic[[8]]$model.table, 
                       model_time[[7]]$model.table,
                       model_phi_time[[1]]$model.table,
                       #model_un_time[[1]]$model.table,
                       #model_un_time_p[[1]]$model.table,
                       #model_un_time_phi[[1]]$model.table,
                       model_yr[[1]]$model.table,
                       model_tmp_p[[3]]$model.table,
                       model_tmp_phi[[3]]$model.table,
                       model_tmp_sex[[2]]$model.table)

rownames(AIC_Bartmeise) <- AIC_Bartmeise$model

###Plot Phi.dot_p.tmp:

bartmeise <- list()

bartmeise[[1]] <-ggplot(model_tmp_p[[3]]$Phi.dot.p.tmp$results$reals$Phi,
                          aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.tmp",
       title = "Bartmeise")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

bartmeise[[2]]<- ggplot(model_tmp_p[[3]]$Phi.dot.p.tmp$results$reals$p,
                          aes(x= as.numeric(as.character(Temp)), 
                              y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature", 
       y = "p\n")
combined_plot <- wrap_plots(bartmeise,nrow=2)
ggsave("bartmeise.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Amsel -> AIC  and plot best model
AIC_Amsel <- rbind(model_basic[[9]]$model.table, 
                       model_time[[10]]$model.table,
                      model_phi_time[[13]]$model.table,
                       #model_un_time[[14]]$model.table,
                   #model_un_time_p[[13]]$model.table,
                   #model_un_time_phi[[13]]$model.table,
                       model_yr[[16]]$model.table,
                       model_tmp_p[[10]]$model.table,
                       model_tmp_phi[[10]]$model.table,
                       model_tmp_sex[[9]]$model.table)

rownames(AIC_Amsel) <- AIC_Amsel$model

###Plot Phi.sex_p.time:

amsel <- list()
amsel[[1]] <-ggplot(model_time[[10]]$Phi.sex.p.time$results$reals$Phi,
                        aes(x= Geschlecht, y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "Sex", 
       y = "Phi\n",
       subtitle= "Model: Phi.sex_p.time",
       title = "Amsel")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

amsel[[2]]<- ggplot(model_time[[10]]$Phi.sex.p.time$results$reals$p,
                        aes(x= as.numeric(time), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Years: even time interval", 
       y = "p\n")
combined_plot <- wrap_plots(amsel,nrow=2)
ggsave("amsel.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Weißsterniges Blaukehlchen -> AIC  and plot best model
AIC_Weißstern <- rbind(model_basic[[10]]$model.table, 
                       model_time[[9]]$model.table,
                       model_phi_time[[12]]$model.table,
                       #model_un_time[[13]]$model.table,
                       #model_un_time_p[[12]]$model.table,
                       #model_un_time_phi[[12]]$model.table,
                       model_yr[[15]]$model.table,
                       model_tmp_p[[14]]$model.table,
                       model_tmp_phi[[14]]$model.table,
                       model_tmp_sex[[13]]$model.table)

rownames(AIC_Weißstern) <- AIC_Weißstern$model

###Plot Phi.dot_p.ppl:

weißstern <- list()

weißstern[[1]] <-ggplot(model_basic[[10]]$Phi.dot.p.ppl$results$reals$Phi,
                        aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.ppl",
       title = "Weißsterniges Blaukehlchen")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

weißstern[[2]]<- ggplot(model_basic[[10]]$Phi.dot.p.ppl$results$reals$p,
                        aes(x= Initials, 
                            y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "Observer", 
       y = "p\n")
combined_plot <- wrap_plots(weißstern,nrow=2)
ggsave("weißstern.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)

## Teichrohrsänger -> AIC  and plot best model
AIC_Teichrohrsänger <- rbind(model_basic[[11]]$model.table, 
                       model_time[[5]]$model.table,
                       model_phi_time[[2]]$model.table,
                       #model_un_time[[2]]$model.table,
                       #model_un_time_p[[2]]$model.table,
                       #model_un_time_phi[[2]]$model.table,
                       model_yr[[2]]$model.table,
                       model_tmp_p[[6]]$model.table,
                       model_tmp_phi[[6]]$model.table,
                       model_tmp_sex[[5]]$model.table)

rownames(AIC_Teichrohrsänger) <- AIC_Teichrohrsänger$model

###Plot Phi.dot_p.tmp:

teichrohrsänger <- list()

teichrohrsänger[[1]] <-ggplot(model_tmp_p[[6]]$Phi.dot.p.tmp$results$reals$Phi,
                        aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.tmp",
       title = "Teichrohrsänger")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

teichrohrsänger[[2]]<- ggplot(model_tmp_p[[6]]$Phi.dot.p.tmp$results$reals$p,
                        aes(x= as.numeric(as.character(Temp)), 
                            y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature", 
       y = "p\n")
combined_plot <- wrap_plots(teichrohrsänger,nrow=2)
ggsave("teichrohrsänger.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Drosselrohrsänger -> AIC  and plot best model
AIC_Drosselrohrsänger <- rbind(model_basic[[12]]$model.table, 
                       model_time[[4]]$model.table,
                       model_phi_time[[10]]$model.table,
                       #model_un_time[[11]]$model.table,
                       #model_un_time_p[[10]]$model.table,
                       #model_un_time_phi[[10]]$model.table,
                       model_yr[[12]]$model.table,
                       model_tmp_p[[9]]$model.table,
                       model_tmp_phi[[9]]$model.table,
                       model_tmp_sex[[8]]$model.table)

rownames(AIC_Drosselrohrsänger) <- AIC_Drosselrohrsänger$model

###Plot Phi.tmp_p.dot:

drosselrohrsänger <- list()
drosselrohrsänger[[1]] <-ggplot(model_tmp_phi[[9]]$Phi.tmp.p.dot$results$reals$Phi,
                           aes(x= as.numeric(as.character(Temp)), y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line()+
  labs(x = "Temperature", 
       y = "Phi\n",
       subtitle= "Model: Phi.tmp_p.dot",
       title = "Drosselrohrsänger")+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

drosselrohrsänger[[2]]<- ggplot(model_tmp_phi[[9]]$Phi.tmp.p.dot$results$reals$p[3,],
                           aes(x= "p.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  ylim(0,1)+
  labs(x = "", 
       y = "p\n")
combined_plot <- wrap_plots(drosselrohrsänger,nrow=2)
ggsave("drosselrohrsänger.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)



## Schilfrohrsänger -> AIC  and plot best model
AIC_Schilfrohrsänger <- rbind(model_basic[[13]]$model.table, 
                       model_time[[3]]$model.table,
                       model_phi_time[[11]]$model.table,
                       #model_un_time[[12]]$model.table,
                       #model_un_time_p[[11]]$model.table,
                       #model_un_time_phi[[11]]$model.table,
                       model_yr[[14]]$model.table,
                       model_tmp_p[[5]]$model.table,
                       model_tmp_phi[[5]]$model.table,
                       model_tmp_sex[[4]]$model.table)

rownames(AIC_Schilfrohrsänger) <- AIC_Schilfrohrsänger$model

###Plot Phi.dot_p.tmp:

p_tmp_schilfrohr<-data.frame()
for (i in seq(1,72)){p_tmp_schilfrohr <- 
  rbind(p_tmp_schilfrohr,model_tmp_p[[5]]$Phi.dot.p.tmp$results$reals$p[2*i,])}

schilfrohrsänger <- list()

schilfrohrsänger[[1]] <-ggplot(model_tmp_p[[5]]$Phi.dot.p.tmp$results$reals$Phi,
                              aes(x= "Phi.dot", y = estimate, ymin = lcl, ymax = ucl)) +
  geom_errorbar() +
  geom_point() +
  labs(x = "", 
       y = "Phi\n",
       subtitle= "Model: Phi.dot_p.tmp",
       title = "Schilfrohrsänger")+
  ylim(0,1)+
  theme(axis.text.x= element_text(size=10),
        axis.text.y= element_text(size=10),
        axis.title.y = element_text(size=12))

schilfrohrsänger[[2]]<- ggplot(p_tmp_schilfrohr,
                              aes(x= as.numeric(as.character(Temp)), 
                                  y = estimate, ymin = lcl, ymax = ucl)) +
  geom_ribbon(alpha=0.2) +
  geom_point() +
  geom_line() +
  labs(x = "Temperature", 
       y = "p\n")
combined_plot <- wrap_plots(schilfrohrsänger,nrow=2)
ggsave("schilfrohrsänger.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 8, height = 7, units = "in", dpi = 300)


## Kohlmeise -> AIC  and plot best model
AIC_Kohlmeise <- rbind(model_yr[[9]]$model.table,
                       model_tmp_p[[1]]$model.table,
                       model_tmp_phi[[1]]$model.table)


## Schwanzmeise -> AIC  and plot best model
AIC_Schwanzmeise <- rbind(model_un_time[[3]]$model.table,
                       model_yr[[3]]$model.table)


## Neuntöter -> AIC  and plot best model
AIC_Neuntöter <- rbind(model_yr[[13]]$model.table)