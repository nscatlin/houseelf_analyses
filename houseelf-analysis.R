# This analysis adds a size and GC column to elf data
library(dplyr)
library(ggplot2)

#Import the data using a function
elf <- read.csv("data/houseelf_earlength_dna_data.csv",header=TRUE)
earlength <- elf$earlength
elfdnaseq <- elf$dnaseq
#Seeing if ear length is large (>10cm) or small (<10cm)
elf_ear_size <- function(length){
  if (length>10){
    size_class = "large"
  }else{
    size_class = "small"
  }
  return(size_class)
}
#The above should be correct

for (length in earlength){
  large_or_small <- elf_ear_size(length)
  print(large_or_small)
}
#For loop to find if the ear lengths are large or small... why can't you specify earlength as a data.frame?


#Calculate the GC Content of Elf DNA
elf_gc_content_fun <- function(sequence){
  G_count <- (str_count(sequence, 'g') + str_count(sequence, 'G'))
  C_count <- (str_count(sequence,'c') + str_count(sequence, 'C'))
  GC <- ((G_count + C_count)/str_length(sequence))*100
  return(GC)
}

for (seq in elfdnaseq){
  GC_elf <-  elf_gc_content_fun(seq)
  print(GC_elf)
}

#Add size class data and GC Content data to a data frame

add_size_content <- function(df){
  elf_ear_size <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% 
    mutate(size=elf_ear_size(earlength))
  return(elf_ear_size)
}  
elf <- add_size_content(elf)
#Adding GC Content
add_gc_content <- function(df){
  elf_gc_content_fun <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% 
    mutate(gc_percent=elf_gc_content_fun(elfdnaseq))
  return(elf_gc_content_fun)
}  

elf <- add_gc_content(elf)


# elf_gc_content_fun <- function(sequence){
#   G_count <- (str_count(sequence, 'g') + str_count(sequence, 'G'))
#   C_count <- (str_count(sequence,'c') + str_count(sequence, 'C'))
#   GC <- ((G_count + C_count)/str_length(sequence))*100
#   return(GC)
# }
# 


subset <- subset(elf, select=c("id", "size", "GC_percents"))

#Subsetting data and sending it to a CSV
write.csv(elf_subset, file = "grangers_analysis.csv", row.names=FALSE)

GC_content_large <- mean(subset$GC_percents[subset$size == "large"])
GC_content_small <- mean(subset$GC_percents[subset$size == "small"])

Size_GC_percents <- data.frame(Size = c("Large", "Small"), Mean_GC_content = c(GC_content_large, GC_content_small))
###class
get_data <- function(){
  data <- read.csv("data/houseelf_earlength_dna_data.csv")
  return(data)
}

get_size_class <- function(weight, threshold){
  if (weight > threshold){
    size_class <- "large"
  } else {
    size_class <- "small"
  }
  return(size_class)
}

#4 for Git Homework
get_data(elf)
file.rename("data/houseelf_earlength_dna_data.csv", "data/houseelf_earlength_dna_data1.csv")

#5

#6
GC_content <- function(seq){
  seq <- str_to_upper(seq)
  G <- str_count(seq, 'G')
  C <- str_count(seq, 'C')
  GC <- ((G + C)/str_length(seq))*100
  return(GC)
}


for (line in elfdnaseq){
  GC_elf <-  GC_content(line)
  print(GC_elf)
}





##class 2015_10_30
# Git
# 
# Local repository
# 
# make changes and then commit them back to our local repository.
# 
# If you see origin popping up, it's talking about GitHubs repository that you are trying to remote to.
# 
# git remote add origin https://github.com/nscatlin/houseelf_analyses.git
# git push -u origin master

library(dplyr)

get_data <- function(){
  data <- read.csv("data/houseelf_earlength_dna_data.csv")
  return(data)
}

get_size_class <- function(weight, threshold){
  if (weight > threshold){
    size_class <- "large"
  } else {
    size_class <- "small"
  }
  return(size_class)
}

add_size_class <- function(){
  data_w_size_class <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% #can work one row at a time and pass it thorugh our get_size function
    mutate(size_class = get_size_class(weight,50))
  return(data_w_size_class)
}


#7, Pulling from GitHub

gc_content <- function(seq){
   #Calculate the GC-content for one or more sequences
   ear_lengths <- ifelse(seq > 10, "large", "small")
   return(ear_lengths)
}

df_id_lengthclass_gc <-  subset(elf, select=c("id", "size", "GC_percents"))

#Subsetting data and sending it to a CSV
write.csv(elf_subset, file = "grangers_analysis.csv", row.names=FALSE)

GC_content_large <- mean(subset$GC_percents[subset$size == "large"])
GC_content_small <- mean(subset$GC_percents[subset$size == "small"])

Size_GC_percents <- data.frame(Size = c("Large", "Small"), Mean_GC_content = c(GC_content_large, GC_content_small))
