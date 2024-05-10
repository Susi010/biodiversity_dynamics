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
               "ggdist",
               "readr")

for (package in package.list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# Load data

birds_cjs_models_basic <- readRDS("data_brandenburg/output/birds_cjs_models_basic.rds")

bird_data <- read.csv("data_brandenburg/raw.data/bird_data_rietzer_see_2000_2022_filled_min_4326.csv")

temp <- read_csv("data_brandenburg/raw.data/temp/data/data_OBS_DEU_P1M_T2M.csv")

# Merge bird_data and temp

## Identifier

bird_data$y.m <- format(as.Date(bird_data$date), "%Y-%m")

temp$y.m <- format(as.Date(temp$Zeitstempel), "%Y-%m")

## Merge

bird_data <- merge(bird_data, temp[,c("y.m","Wert")], by="y.m", all.x =TRUE)

colnames(bird_data)[colnames(bird_data)== "Wert"]<-"Temp"

### Plot temperature

df_temp <- na.omit(bird_data|> group_by(y.m) |> summarise(Temp=mean(Temp), year=first(year)))

ggplot_temp<- ggplot(df_temp,aes(y=Temp, x= y.m))+
  geom_line(group=1)+
  geom_point()+
  labs(x="Sampling period: 5.2004 - 11.2023", y= "Temperature in °C")+
  #scale_x_date(breaks = df_temp$y.m[seq(1, nrow(df_temp), by = 2)])
  theme(axis.text.x = element_blank())
ggsave("ggplot_temp.png", plot = ggplot_temp, 
       path = "data_brandenburg/plots", 
       width = 7, height = 5, units = "in", dpi = 300)


### Plot temp for certain species

ggplot_tmp_srs <- ggplot(
  na.omit(df|>filter(Artbezeichnung.deutsch=="Schilfrohrsänger")),
  aes(y=Temp, x= year_month))+
  geom_bar(aes(y = length(year_month)/1000),stat="identity")+
  geom_line(group=1)+
  geom_point()+
  labs(x="Sampling date", y= "Temperature in °C")+
  scale_y_continuous(sec.axis = sec_axis(~., name = "Sampled individuals")) +
  theme(axis.text.x = element_text(angle=90))
ggsave("ggplot_tmp_srs.png", plot = ggplot_tmp_srs, 
       path = "data_brandenburg/plots", 
       width = 7, height = 5, units = "in", dpi = 300)

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
                                     Geschlecht, Initials, year, year_month,month, Temp), 
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
data_cmr <- data_cmr[-9]#KOhlmeise
data_cmr <- data_cmr[-3]#Schwanzmeise

### drop species where R can not fit model when temperature added
data_cmr <- data_cmr[-16]#Neuntöter
data_cmr <- data_cmr[-12]#Schwanzmeise
data_cmr <- data_cmr[-1]#Kohlmeise


# Change species order to show relation -> not when Temp added

arten_sort <- read.csv("data_brandenburg/output/arten_sort.csv")
data_cmr <- data_cmr[arten_sort$X]

### Change list into data frame
df <- data.frame()
for (i in seq(data_cmr)){
  data <- as.data.frame(data_cmr[[i]])
  df <- rbind(df, data)}
df$Artbezeichnung.deutsch <- factor(df$Artbezeichnung.deutsch, levels = arten_sort$bird)


# Plot capturing data

## Barplots

### Sex
####FIX ncol??

ggplot_sex <- list()
for (i in seq(data_cmr)){ggplot_sex[[i]] <- 
  ggplot(data_cmr[[i]], aes(x = Geschlecht, fill = Geschlecht)) +
  geom_bar() +
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  theme(legend.position = "none") +
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex, ncol=3)
ggsave("ggplot_sex.png", plot = sex, path = "data_brandenburg/plots", width = 6, height = 15, units = "in", dpi = 300)

### Sex and capture histories

ggplot_sex_ch <- list()
for (i in seq(data_cmr)){ggplot_sex_ch[[i]] <- 
  ggplot(data_cmr[[i]], mapping = aes(x = ch, fill = Geschlecht)) +
  geom_bar(position = "stack") +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), expand = c(.001, .001)) +
  labs(x = "capture histories", fill = "sex", y = data_cmr[[i]]$Artbezeichnung.deutsch) +
  theme(axis.text.x = element_blank(), legend.position = "none") +
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey"))}

combined_plot <- wrap_plots(ggplot_sex_ch, ncol = 1) + theme(legend.position = "right")
ggsave("ggplot_sex_ch.png", plot = combined_plot, path = "data_brandenburg/plots", width = 6, height = 15, units = "in", dpi = 300)


## Sex and capture sum

ggplot_sex_cs <- list()

for (i in seq(data_cmr)){ggplot_sex_cs[[i]] <- 
  ggplot(data_cmr[[i]], mapping = aes(x = capt_sum, fill = Geschlecht)) +
  geom_bar(position = "stack") +
  #scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 10), expand = c(.001, .001)) +
  labs(x = "capture sum", fill = "sex") +
  theme(legend.position = "bottom") + #axis.text.x = element_text(angle = 45)) 
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) + 
  ggtitle(data_cmr[[i]]$Artbezeichnung.deutsch)}

combined_plot <- wrap_plots(ggplot_sex_cs, nrow = 4) + theme(legend.position = "bottom")
ggsave("ggplot_sex_cs.png", plot = combined_plot, path = "data/data_brandenburg", width = 12, height = 12, units = "in", dpi = 300)

for (i in seq(ggplot_sex_cs)){
  filename <- sprintf("ggplot_sex_cs_%02d.png", i)
  ggsave(filename, 
         plot = ggplot_sex_cs[[i]],
         path = "data_brandenburg/plots", 
         width = 5, height = 5, units = "in", dpi = 300)}


### Species each year

ggplot_sp_yr <- list()
for (i in seq(data_cmr)){
  ggplot_sp_yr[[i]] <- ggplot(data_cmr[[i]], 
                                  aes(x = year)) +
    stat_count() +
    labs(x = "Years", y = element_text(arten_sort$bird[i])) +
    theme(plot.margin = margin(0,0,0,0, "in"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(vjust = 0.5,
                                      hjust = 1,
                                      size=9,
                                      angle=0))}
combined_plot <- wrap_plots(ggplot_sp_yr, nrow = 13, byrow=FALSE) + 
  theme(axis.title.x.bottom = element_text(),
        axis.text.x.bottom = element_text(angle = 10))
ggsave("ggplot_sp_yr_bar.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 7, height = 13, units = "in", dpi = 300)



ggplot_sp_yr_16 <- ggplot(df) +
  geom_bar(aes(x = year, fill = Artbezeichnung.deutsch), position = "stack") +
  labs(x = "year", fill = "Art") +
  scale_fill_manual(values = rainbow(length(unique(df$Artbezeichnung.deutsch)))) +
  #scale_fill_manual(values = setNames(arten_sort$clr, arten_sort$bird))+ 
  theme(legend.position = "right")
ggsave("ggplot_sp_yr_16.png", plot = ggplot_sp_yr_16, 
       path = "data_brandenburg/plots", 
       width = 12, height = 15, units = "in", dpi = 300)


### Species sex each year

ggplot_sp_sex_yr <- list()
for (i in seq(data_cmr)){
  ggplot_sp_sex_yr[[i]] <- ggplot(data_cmr[[i]], 
                                  aes(x = year, fill = Geschlecht)) +
    geom_bar(position ="stack") +
    scale_fill_manual(values = c(weiblich = "salmon", 
                                 männlich = "lightblue", 
                                 unbekannt = "grey")) +
    labs(x = "Years", y = "Sampled individuals", 
         title = element_text(data_cmr[[i]]$Artbezeichnung.deutsch[1]), 
         fill = "sex") +
    theme(legend.position = "bottom")}
#ggplot_sp_sex_yr[[11]]$theme$legend.position<-"right"
#combined_plot <- wrap_plots(ggplot_sp_sex_yr, nrow = 5, byrow=FALSE)
#ggsave("ggplot_sp_sex_yr.png", plot = combined_plot, 
#       path = "data_brandenburg/plots", 
#       width = 10, height = 11, units = "in", dpi = 300)


for (i in seq(ggplot_sp_sex_yr)){
  filename <- sprintf("ggplot_sp_sex_yr_%02d.png", i)
  ggsave(filename, 
         plot = ggplot_sp_sex_yr[[i]],
         path = "data_brandenburg/plots", 
         width = 6, height = 5, units = "in", dpi = 300)}


ggplot_sp_sex_yr <- ggplot(df,aes(x = year, fill = Geschlecht)) +
    geom_bar(position ="stack") +
    scale_fill_manual(values = c(weiblich = "salmon", 
                                 männlich = "lightblue", 
                                 unbekannt = "grey")) +
  facet_wrap(df$Artbezeichnung.deutsch, scales ="free_y",
             strip.position = "right",ncol=1, axis.labels = "margins")+
  labs(y="Number of sampled individuals")+
  theme(legend.title = element_text("Sex"))
  
ggsave("ggplot_sp_sex_yr.png", plot = ggplot_sp_sex_yr, 
       path = "data_brandenburg/plots", 
       width = 8, height = 12, units = "in", dpi = 300)


### Species capt_sum each year

ggplot_sp_capt_yr <- list()
for (i in seq(data_cmr)){
  ggplot_sp_capt_yr[[i]] <- ggplot(data_cmr[[i]], 
                                  aes(x = year, fill = factor(capt_sum))) +
    geom_bar(position ="stack") +
    labs(x = "Years", y = "Sampled individuals", 
         title = element_text(data_cmr[[i]]$Artbezeichnung.deutsch[1]), 
         fill = "Times captured") +
    theme(legend.position = "bottom")}
for (i in seq(ggplot_sp_capt_yr)){
  filename <- sprintf("ggplot_sp_capt_yr_%02d.png", i)
  ggsave(filename, 
         plot = ggplot_sp_capt_yr[[i]],
         path = "data_brandenburg/plots", 
         width = 6, height = 5, units = "in", dpi = 300)}


## Species sex each year plus temp -> does not work!

#ggplot_sp_sex_yr_tmp <- 
#ggplot(df,aes(x = year, fill = Geschlecht)) +
#  geom_bar(position ="stack") +
#  geom_line(y=df$Temp)
#  scale_fill_manual(values = c(weiblich = "salmon", 
#                               männlich = "lightblue", 
#                               unbekannt = "grey")) #+
  #facet_wrap(df$Artbezeichnung.deutsch, scales ="free_y",
  #           axis.labels = "margins")+
  #labs(y="Number of sampled individuals")+
  #theme(legend.title = element_text("Sex"))

#ggsave("ggplot_sp_sex_yr.png", plot = ggplot_sp_sex_yr_tmp, 
#       path = "data_brandenburg/plots", 
#       width = 8, height = 12, units = "in", dpi = 300)



### Species sex and Temperature

ggplot_sp_sex_tmp <- list()
for (i in seq(data_cmr)){ggplot_sp_sex_tmp[[i]] <- 
  ggplot(data_cmr[[i]], aes(x=Temp,fill=Geschlecht)) +
  geom_histogram(position ="stack", binwidth=1) +
  scale_fill_manual(values = c(weiblich = "salmon", 
                               männlich = "lightblue", 
                               unbekannt = "grey"))+
  labs(x = "Temperature", y = "Sampled individuals", 
       title = element_text(data_cmr[[i]]$Artbezeichnung.deutsch[1]), 
       fill = "sex") +
  theme(legend.position = "bottom")}
#ggplot_sp_sex_tmp[[11]]$theme$legend.position<-"right"
#combined_plot <- wrap_plots(ggplot_sp_sex_tmp, nrow = 5, byrow=FALSE)
#ggsave("ggplot_sp_sex_tmp.png", plot = combined_plot, 
#         path = "data_brandenburg/plots", 
#         width = 10, height = 11, units = "in", dpi = 300)


for (i in seq(ggplot_sp_sex_tmp)){
  filename <- sprintf("ggplot_sp_sex_tmp_%02d.png", i)
  ggsave(filename, 
         plot = ggplot_sp_sex_tmp[[i]],
         path = "data_brandenburg/plots", 
         width = 6, height = 5, units = "in", dpi = 300)}

### Species capt_sum and Temperature

ggplot_sp_capt_tmp <- list()
for (i in seq(data_cmr)){ggplot_sp_capt_tmp[[i]] <-
  ggplot(data_cmr[[i]], aes(x=Temp,fill=factor(capt_sum))) +
  geom_histogram(position ="stack", binwidth=1) +
  labs(x = "Temperature", y = "Sampled individuals", 
       title = element_text(data_cmr[[i]]$Artbezeichnung.deutsch[1]), 
       fill = "Times captured") +
  theme(legend.position = "bottom")}
for (i in seq(ggplot_sp_capt_tmp)){
  filename <- sprintf("ggplot_sp_capt_tmp_%02d.png", i)
  ggsave(filename, 
         plot = ggplot_sp_capt_tmp[[i]],
         path = "data_brandenburg/plots", 
         width = 6, height = 5, units = "in", dpi = 300)}


### Histogram

ggplot_sp_hist <- list()
for (i in seq(data_cmr)){
  ggplot_sp_hist[[i]] <- ggplot(data_cmr[[i]],aes(x = year))+
  geom_histogram(aes(y=..density..),binwidth = 1, col="white")+
    labs(x = "Years", y = element_text(arten_sort$bird[i]), fill = "sex") +
    theme(legend.position = "none",
          plot.margin = margin(0,0,0,0, "in"),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(vjust = 0.5,
                                      hjust = 1,
                                      size=9,
                                      angle=0))}
combined_plot <- wrap_plots(ggplot_sp_hist, nrow = 13, byrow=FALSE)+ 
  theme(axis.title.x.bottom = element_text(),
        axis.text.x.bottom = element_text(angle = 10))
ggsave("ggplot_sp_hist.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 7, height = 13, units = "in", dpi = 300)


#  stat_histinterval(fill_type = "segments", alpha = 0.9, interval_alpha=0.9, point_alpha=0.9,aes(fill = after_stat(level)))+
#  stat_interval(alpha=0)+
#  stat_summary(geom = "point", fun = median)+
#  scale_fill_brewer(na.translate = FALSE)+
#  theme(axis.title.y = element_blank())


## Variation meassures for plotting

abundance_per_yr<-list()
for(i in seq(data_cmr)){abundance_per_yr[[i]] <- 
  data_cmr[[i]] %>%group_by(year) %>%
  summarize(abundance = length(Artbezeichnung.deutsch))}

abundance_per_year <- df|> group_by(Artbezeichnung.deutsch, year) |>
  summarize(abundance = length(Artbezeichnung.deutsch))

taxa_summarize<-abundance_per_year|> group_by(Artbezeichnung.deutsch)|>
  summarize(min = min(abundance),
          max = max(abundance),
          median = median(abundance),
          mean = mean(abundance),
          sd = sd(abundance),
          cv = sd / mean)

## Boxplot

ggplot_sp_box<- ggplot(abundance_per_year, 
                       aes(y = Artbezeichnung.deutsch, x = abundance)) +
  geom_boxplot() +
  stat_summary(fun=median, geom="errorbar",
               aes(xmax = ..x.., xmin = ..x.., color="Median"), 
               linetype = "solid") +
  stat_summary(fun=mean, geom="errorbar",
               aes(xmax = ..x.., xmin = ..x.., color="Mean"), 
               linetype = "dashed") +
  labs(y = "", x = "Sampled individuals per year") +
  scale_colour_manual("Values", 
                      values = c(Median = "black", Mean = "red")) +
  theme(legend.position = "right")
ggsave("ggplot_sp_box.png", plot = ggplot_sp_box, 
       path = "data_brandenburg/plots", 
       width = 10, height = 9, units = "in", dpi = 300)

## Mean and SD

ggplot_sp_sd <- ggplot(taxa_summarize, aes(y = Artbezeichnung.deutsch, x = mean)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  geom_errorbar(aes(xmin=mean-sd, xmax=mean+sd), width=.2) +
  labs(y = "", x = "Mean and Standard deviation")
ggsave("ggplot_sp_sd.png", plot = ggplot_sp_sd, 
       path = "data_brandenburg/plots", 
       width = 10, height = 9, units = "in", dpi = 300)

## Connected scatterplot

df_scattr <- df|> select(year, Artbezeichnung.deutsch) |> 
  table() |> as.data.frame() #|> 
  pivot_wider(names_from = Artbezeichnung.deutsch, values_from = Freq)

tmp <- df_scattr |> mutate(name2=Artbezeichnung.deutsch)


ggplot_scttr <- ggplot(tmp, aes(x=year,y=Freq))+
  geom_line(data=tmp|> dplyr::select(-Artbezeichnung.deutsch),
            aes(group=name2,color=name2),size=0.4,alpha=0.4)+
  geom_line(aes(color=Artbezeichnung.deutsch,group=1), size=1.4)+
  scale_color_viridis(discrete = TRUE) +
  scale_color_manual(values = setNames(arten_sort$clr, arten_sort$bird))+
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.title.x.bottom = element_text(),
        axis.text.x = element_text(angle=270, size=5))+
  facet_wrap(~ Artbezeichnung.deutsch, strip.position = "right")+
  labs(y="No. of individuals sampled", x= "Year")
ggsave("ggplot_sp_sctr.png", plot = ggplot_scttr, 
       path = "data_brandenburg/plots", 
       width = 8, height = 8, units = "in", dpi = 300)


### Species per people (= Observer) -> only certain species

ggplot_ppl_wbk <- ggplot(
  df|>filter(Artbezeichnung.deutsch=="Weißsterniges Blaukehlchen"),
  aes(x= Initials, fill=factor(capt_sum)))+
  geom_bar()+
  labs(x="Observer", y= "Sampled individuals", fill="Times captured")+
  theme(legend.position = "bottom")
ggsave("ggplot_ppl_wbk.png", plot = ggplot_ppl_wbk, 
       path = "data_brandenburg/plots", 
       width = 6, height = 4, units = "in", dpi = 300)



### Species per year with capt_sum -> only certain species

ggplot_capt_yr_brm <- ggplot(
  df|>filter(Artbezeichnung.deutsch=="Bartmeise"),
  aes(x= year, fill=factor(capt_sum)))+
  geom_bar()+
  labs(x="Years", y= "Sampled individuals", 
       fill="Times captured", title = "Bartmeise")+
  theme(legend.position = "bottom")
ggsave("ggplot_capt_yr_brm.png", plot = ggplot_capt_yr_brm, 
       path = "data_brandenburg/plots", 
       width = 6, height = 5, units = "in", dpi = 300)


## Densities

ggplot_sp_yr_den <- list()
for (i in seq(data_cmr)){ggplot_sp_yr_den[[i]] <- 
  ggplot(data_cmr[[i]],aes(x = year, y=Artbezeichnung.deutsch)) +
  labs(x = "Years", y = element_text(arten_sort$bird[i])) +
  stat_halfeye(fill_type = "segments", alpha = 0.6, height= 5, point_alpha=0)+
  stat_interval(size=2)+
  stat_summary(geom = "point", fun = median) +
  theme(legend.position = "none",
        plot.margin = margin(0,0,-2,0, "in"), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 0.1,
                                    hjust = 1,
                                    size=9,
                                    angle=0))}
combined_plot <- wrap_plots(ggplot_sp_yr_den, nrow = 13, byrow=FALSE) + 
  theme(axis.title.x.bottom = element_text(),
        axis.text.x.bottom = element_text())
ggsave("ggplot_sp_yr_den.png", plot = combined_plot, 
       path = "data_brandenburg/plots", 
       width = 6, height = 13, units = "in", dpi = 300)



ggplot_sp_sex_yr_den <- list()
for (i in seq(data_cmr)){ggplot_sp_sex_yr_den[[i]] <- 
  ggplot(data_cmr[[i]], aes(x = year, y=Artbezeichnung.deutsch, 
                            fill = Geschlecht)) +
  labs(x = "Years", y = "", title= element_text(arten_sort$bird[i])) +
  stat_halfeye(alpha = 0.4, height= 5, point_alpha=0, interval_alpha=0)+
  stat_interval(position="dodge", size=1)+
  stat_pointinterval(position="dodge", size=0.3)+
  #stat_summary(geom = "point", fun = median) +
  scale_fill_manual(values = c(weiblich = "salmon", männlich = "lightblue", unbekannt = "grey")) +
  geom_vline(xintercept = median_yr, col = "grey30", lty = "dashed")+
  theme(legend.position = "none", 
        axis.text.y = element_blank(), 
        axis.title.x = element_blank(), 
        title = element_text(color = arten_sort$clr[i], size=13))}
combined_plot <- wrap_plots(ggplot_sp_sex_yr_den, ncol = 2, byrow=FALSE) + theme(axis.title.x.bottom = element_text())
ggsave("ggplot_sp_sex_yr_den.png", plot = combined_plot, path = "data_brandenburg/plots", width = 6, height = 15, units = "in", dpi = 300)

