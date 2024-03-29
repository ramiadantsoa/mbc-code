1 + 1

km <- 10 + 29 + 14 + 18

kmjour <- km / 30
 1  2
kmjour * 365

km  <- "this is a text"
 1 + 2
1 + "1"

1 = 2
1 == 2


v1  <- c(1, 2, 3, 4)
v2 <- 1:4
v3 <- seq(1, 4, by = 1)

v4 <- c()
for(i in 1:4){
    v4  <- c(v4, i)
}

v4
m1 <- matrix(1:6, nrow = 2)

name <- c("Tj", "To", "Ce", "Ka", "Ma", "SF", "Va")
isFemale <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
hairlength  <- runif(7, 0, 30)

data <- data.frame(name, isFemale, hairlength)
View(data)

c(1, 2, 3, 4.2)

# add new line

plot(1:4)


####### Exercise in R #####

# this clears the memory
rm(list=ls())

# creating a dataset to play with

nn <- 200
temperature <- round(rnorm(nn, 273, 20), 3)
eggs <- rpois(nn, 100)
type <- sample(c("s1", "s2", "s3"), nn, replace = TRUE)

data <- data.frame(temperature, eggs, type)
View(data)
write.csv(data, "/Users/tanjona/Documents/asa/misc/MBC/mbc-code/data_exo1.csv")


#### Solution to the problem

filename <- "/Users/tanjona/Documents/asa/misc/MBC/mbc-code/data_exo1.csv"

data_exo <- read.csv(filename)
View(data_exo)

# first approach
mea_temp <- mean(data_exo$temperature)
mea_eggs <- mean(data_exo$eggs)

med_temp <- median(data_exo$temperature)
med_eggs <- median(data_exo$eggs)

max_temp <- max(data_exo$temperature)
max_eggs <- max(data_exo$eggs)

min_temp <- min(data_exo$temperature)
min_eggs <- min(data_exo$eggs)

std_temp <- sd(data_exo$temperature)
std_eggs <- sd(data_exo$eggs)

temp <- c(mea_temp, med_temp, max_temp, min_temp, std_temp)
eggs <- c(mea_eggs, med_eggs, max_eggs, min_eggs, std_eggs)

df_exo <- data.frame(temperature = temp, n_eggs = eggs)

rownames(df_exo) <- c("mean", "median", "max", "min", "std")

View(df_exo)

newfile  <- "/Users/tanjona/Documents/asa/misc/MBC/mbc-code/data_exo1_result.csv"

write.csv(df_exo, newfile)

# Second approach

data_exo[, "type"]
data_exo[, 4]
data_exo$type

data_exo2 <- data_exo[, c(-1, -4)]
View(data_exo2)

m1 <- apply(data_exo2, 2, mean)
m2 <- apply(data_exo2, 2, median)
m3 <- apply(data_exo2, 2, max)
m4 <- apply(data_exo2, 2, min)
m5 <- apply(data_exo2, 2, sd)

df_exo2 <- rbind(mean = m1, median = m2, max = m3, min = m4, std = m5)

View(df_exo2)

# third approach
library(dplyr)

d2  <- data_exo |>
    summarise(across(c(temperature, eggs),
        list(mean = mean, median = median, max = max, min = min,  sd = sd)))

df_exo3 <- as.data.frame(matrix(d2, nrow = 5))
rownames(df_exo3) <- c("mean", "median", "max", "min", "std")
colnames(df_exo3) <- c("temperature", "eggs")

View(df_exo3)

matrix(1:10, nrow = 5 )
matrix(1:10, nrow = 5, byrow = TRUE )


#### generating data for Twins ####

# pool of species
all_species <- paste("sp", 1:20, sep = "")

# random number of individuals per combination of field and age
n_lines <- rpois(9, 7)
age3 <- rep("3", sum(n_lines[1:3]))
age6 <- rep("6", sum(n_lines[4:6]))
age9 <- rep("9", sum(n_lines[7:9]))

# combining the vectors
age <- c(age3, age6, age9)

# generating field ID
field <- c()
id_field <- paste(rep(1:3, 3))
for (i in 1:9){
    temp <- rep(id_field[i], n_lines[i])
    field <- c(field, temp)
}

# sampling species per field and age
species <- c()
for (i in 1:9){
    temp <- sample(all_species, n_lines[i], replace = TRUE)
    species <- c(species, temp)
}

# final data
data <- data.frame(age, field, species)

library(tidyverse)

# calculating number of distinct species and individuals per age
data_nsp  <- data |>
    group_by(age)  |>
    summarise(n_sp = n_distinct(species), n_ind = n())
View(data_nsp)


## play with Maya's data
library(dplyr)
library(ggplot2)

#import data example
data  <- read.csv("/Users/tanjona/Downloads/Data_Maya - Mounting.csv")
View(data)

# look at the number of rows and columns
dim(data)

# calculate the number of individuals and genra per transect
d1  <- data  |>
    group_by(Collectors.code)  |>
    summarise(n_ind = n(), n_gen = n_distinct(Genres))


# calculate the number of individuals and genra per transect per pitfall
d1b  <- data  |>
    group_by(Collectors.code, subsample)  |>
    summarise(n_ind = n(), n_gen = n_distinct(Genres))

# total number of individuals
sum(d1$n_ind)
View(d1b)

# plot number of genera per transect
ggplot(d1, aes(x = Collectors.code, y = n_gen))+
    geom_col()

# same as above
ggplot(data, aes(x = Collectors.code)) +
    geom_bar()

# multiplanel figure: panel = transec
ggplot(data, aes(x = subsample)) +
    geom_bar()+
    facet_wrap(~Collectors.code)


## importing and more visualization of Maya's data
# setting working directy to shorten filename
setwd("/Users/tanjona/Library/Mobile Documents/com~apple~CloudDocs/misc/MBC/mbc-code/")
filename  <- "data/Data_Maya.csv"
data <- read.csv(filename)

library(ggplot2)
library(dplyr)
View(data)

# convert as factor so R knows it is not numbers.
data$Type <- as.factor(data$Type)
data$Sites <- as.factor(data$Sites)

# partitioning the bar so a color represents a genus
ggplot(data, aes(x = Type, fill = Genus)) +
    geom_bar()

# number of individuals per type and per site
ggplot(data, aes(x = Type, fill = Sites)) +
    geom_bar(position = "dodge") +
    theme_bw()

# creating a new dataset to the last column
# represents the number of individuals per pitfall

data2 <- data |>
    group_by(Type, Sites, Transect, Subsample)  |>
    summarize(n_ind = n())

# making sure the code does want I want
View(data2)

# distribution of number of individual per site
ggplot(data2, aes(x = Sites, y = n_ind)) +
    geom_violin() +
    facet_wrap(~Type)

# another way of plotting using boxplot and adding raw data
fig <- ggplot(data2, aes(x = Sites, y = n_ind)) +
    geom_boxplot() +
    geom_jitter(width = 0.05) +
    facet_wrap(~Type)

# exporting figures
ggsave("fig/fig_box_demo.pdf", fig)

ggsave("fig/fig_box_demo.png", fig,
        dpi = 300, width = 2, height = 1, units = c("cm"))

# creating a personalized theme
mytheme <- theme(plot.title = element_text(hjust = 0.5),
				axis.text = element_text(size = 10),
				axis.title = element_text(size = 16,
                                        face = "bold",
                                        color = "black"),
				panel.background = element_blank(),
				panel.grid.minor = element_blank(),
                    panel.grid.major = element_line(color = "gray30",
                                                    size = 0.1,
                                                    linetype = "dashed"),
				axis.line = element_line(color = "black"),
				legend.key = element_rect(fill = "transparent"))

fig  <- ggplot(data2, aes(x = Sites, y = n_ind)) +
    geom_boxplot() +
    geom_jitter(width = 0.05) +
    facet_wrap(~Type) +
    labs(x = "Maya", y = "Tojo", title = "a figure") +
    mytheme

ggsave("fig/fig_box_demo.png", fig,
        dpi = 300, width = 10, height = 7, units = c("cm"))
