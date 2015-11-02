# This analysis adds a size and GC column to elf data
setwd("~/Documents/UF_Fall_2015/Data_Carpentry/git/")
library(dplyr)
library(ggplot2)

#2 For these questions, I just copy and pasted what I did for Assignment 6 (except for problem 4 and 6 in this homework.
#Import the data using a function
elf <- read.csv("data/houseelf_earlength_dna_data.csv",header=TRUE)



#4 for Git Homework

file.rename("data/houseelf_earlength_dna_data.csv", "data/houseelf_earlength_dna_data1.csv")

###If you want to change it back to the original name, run command below
#file.rename("data/houseelf_earlength_dna_data1.csv", "data/houseelf_earlength_dna_data.csv")


#5. Remote added

#6

GC_content <- function(seq){
  seq <- str_to_upper(seq)
  G <- str_count(seq, 'G')
  C <- str_count(seq, 'C')
  GC <- ((G + C)/str_length(seq))*100
  return(GC)
}


#7, Pulling and pushing

get_size_class <- function(ear_length){ #Dr. Granger's code for making size classes based on earlength
# Calculate the size class for one or more earth lengths
  ear_lengths <- ifelse(ear_length > 10, "large", "small")
  return(ear_lengths)
}

#Adding size class to Elf dataset
add_size_class <- function(df){
  get_size_class <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% #can work one row at a time and pass it thorugh our get_size function
    mutate(size_class = get_size_class(earlength))
  return(get_size_class)
}

elf <- add_size_class(elf)


#Adding GC Content to Elf dataset
add_gc_content <- function(df){
  GC_content <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% 
    mutate(GC_percents=GC_content(dnaseq))
  return(GC_content)
}  

elf <- add_gc_content(elf)

df_id_lengthclass_gc <-  subset(elf, select=c("id", "size_class", "GC_percents"))

#Subsetting data and sending it to a CSV
write.csv(df_id_lengthclass_gc, file = "data/Catlin_Granger.csv", row.names=FALSE)

GC_content_large <- mean(df_id_lengthclass_gc$GC_percents
                          [df_id_lengthclass_gc$size == "large"])
GC_content_small <- mean(df_id_lengthclass_gc$GC_percents
                          [df_id_lengthclass_gc$size == "small"])

Size_GC_percents <- data.frame(Size = c("Large", "Small"),
                               Mean_GC_content = c(GC_content_large, GC_content_small))
