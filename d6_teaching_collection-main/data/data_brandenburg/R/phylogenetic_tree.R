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


# Phylogenetic tree
install.packages("ape")
install.packages("U.PhyloMaker")
devtools::install_github("jinyizju/U.PhyloMaker")
library(ape)
library('U.PhyloMaker')

# Make up a species list
bird <- c("Bartmeise","Teichrohrsänger","Blaumeise","Rohrschwirl","Feldsperling","Fitis","Rohrammer", "Gartengrasmücke", "Goldammer", "Drosselrohrsänger", "Schilfrohrsänger", "Weißsterniges Blaukehlchen", "Amsel")
species <- c("Panurus biarmicus","Acrocephalus scirpaceus", "Cyanistes caeruleus", "Locustella luscinioides", "Passer montanus", "Phylloscopus trochilus", "Emberiza schoeniclus", "Sylvia borin", "Emberiza citrinella", "Acrocephalus arundinaceus", "Acrocephalus schoenobaenus", "Luscinia svecica cyanecula", "Turdus merula")
genus <- c("Panurus", "Acrocephalus", "Cyanistes", "Locustella", "Passer", "Phylloscopus", "Emberiza", "Sylvia", "Emberiza", "Acrocephalus", "Acrocephalus", "Luscinia", "Turdus")
family <- c("Panuridae", "Acrocephalidae", "Paridae", "Locustellidae", "Passeridae", "Phylloscopidae", "Emberizidae", "Sylviidae", "Emberizidae", "Acrocephalidae", "Acrocephalidae", "Muscicapidae", "Turdidae")

arten <- data.frame(bird = bird, species = species, genus = genus, family = family)
tree <- data.frame(species = species, genus = genus, family = family)

# Make up a backbone phylogeny

newick <- '(("Phylloscopus trochilus":4, "Sylvia borin":4, 
(("Acrocephalus arundinaceus":4, "Acrocephalus schoenobaenus":4,"Acrocephalus scirpaceus":4), 
"Locustella luscinioides":4):2,"Panurus biarmicus":4,"Cyanistes caeruleus":4):1, 
(("Luscinia svecica cyanecula", "Turdus merula"),
("Passer montanus":4, ("Emberiza schoeniclus":6,"Emberiza citrinella":6):5):2):1);'

cat(newick, file = "data_brandenburg/output/tree.tre", sep = "\n")

megatree <- read.tree("data_brandenburg/output/tree.tre")

genus_labels <- c("Phylloscopus","Sylvia",rep("Acrocephalus",3),  "Locustella", "Panurus", "Cyanistes","Luscinia","Turdus","Passer", rep("Emberiza",2))

megatree$tip.label <- paste(genus_labels, sort(megatree$tip.label), sep = "_")

# Make up the genus-family relationship file
genus_list <- data.frame(genus = tree$genus, family = tree$family)

Phyl_tree <- phylo.maker(sp.list = tree, tree = megatree, gen.list = genus_list, nodes.type = 1, scenario = 3)

# Plot Tree
plot_tree <-  plot.phylo(Phyl_tree$phylo)
plot.phylo(Phyl_tree$phylo)
Phyl_tree[["phylo"]][["tip.label"]]

sort_vector <- c("Phylloscopus trochilus","Sylvia borin","Acrocephalus schoenobaenus",
"Acrocephalus arundinaceus", "Acrocephalus scirpaceus", "Locustella luscinioides",   
"Panurus biarmicus","Cyanistes caeruleus","Luscinia svecica cyanecula",
"Turdus merula","Passer montanus","Emberiza citrinella","Emberiza schoeniclus")


arten_sort <- arten[match(sort_vector, arten$species), ]
arten_sort$clr <- c("darkblue","darkcyan","dodgerblue","deepskyblue","deepskyblue4","cyan","blueviolet","orchid",
                    "darkorange","red","yellow","orange","gold")
write.csv(arten_sort, "data_brandenburg/output/arten_sort.csv")

png(filename = "data_brandenburg/plots/phylo_tree.png",  width = 12, 
    height = 12, units = "in", res = 300)

plot.phylo(Phyl_tree$phylo)
dev.off()



