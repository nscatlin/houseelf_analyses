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
  elf_gc_content <- 
    df %>% 
    na.omit() %>% 
    rowwise() %>% 
    mutate(gc_percent=elf_gc_content_fun(elfdnaseq))
  return(elf_gc_content)
}  
elf <- add_gc_content(elf)

subset <- subset(elf, select=c("id", "size", "GC_percents"))

#Subsetting data and sending it to a CSV
write.csv(elf_subset, file = "grangers_analysis.csv", row.names=FALSE)

GC_content_large <- mean(subset$GC_percents[subset$size == "large"])
GC_content_small <- mean(subset$GC_percents[subset$size == "small"])

Size_GC_percents <- data.frame(Size = c("Large", "Small"), Mean_GC_content = c(GC_content_large, GC_content_small))


###class
get_data <- function(){
  ata <- read.csv("surveys.csv")
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






