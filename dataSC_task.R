#-----------------------------------------------------------------------------------------------------------------------
#
# Program: dataSC_task.R
#
# Purpose: Automatic extraction of the list of licence holders that should be registered with GAMSTOP
#
# Author: Olusegun Oshota
#
# Date: 6th June, 2020
#
# Usage: Run program from work directory as follows using "Rscript dataSC_task.R inputfile"
#
# Output file description: "not_found_on_register.csv": List of licence holders that require registration with GAMSTOP
#
#-----------------------------------------------------------------------------------------------------------------------

#Import libraries
suppressPackageStartupMessages({
library(readxl)
library(stringr)
library(dplyr)
library(data.table)
})

# Main function for input arguement, checks and running process()
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  filename <- args[1]
  
  #check if result folder exists
  ifelse(!dir.exists("results"), dir.create("results"), "results folder exitss already")
  
  # Ckeck if file exists
  if(file.exists(filename)){
    process(filename)
  }else{
    cat('\nThe file',paste(filename,'does not exist. Please supply the file!\n'))
  }
}

process <- function(file){
  # Read in Licensed Activities data 
  Lic_Act.df <- read_excel(file,sheet = "Licensed Activities",na = c("",NA), trim_ws = T)#catch NA and leading and trailing spaces
  dim(Lic_Act.df) #1667    5
  
  # Activities that are regulated
  reg_act <- c("Casino - R","Bingo - R","External Lottery Manager - R","General Betting Standard - Virtual Event - R",
               "General Betting Standard - RealEvent - R","Pool Betting - R")
  reg_act.df <- Lic_Act.df[Lic_Act.df$Activity %in% reg_act,]
  reg_act.df <- reg_act.df[!(reg_act.df$Status %in% "Surrendered"), ]  #Exclude "surrendered" licences
  
  # Exclude Licence holders with non-UK domains,
  dom_names = read_excel(file, sheet = "Domain Names",na = c("",NA), trim_ws = T)#catch NA and leading and trailing spaces
  reg_dom_names <- merge(dom_names, reg_act.df, by = "Account Number" )
  reg_dom_names.uk <- reg_dom_names[reg_dom_names$Url %in%  str_extract(reg_dom_names$Url,".*\\.uk.*"),  ] #extract data subset with UK only domain names
  
  # Check if any uk domain was found above
  if(nrow(reg_dom_names.uk) == 0){
    print("No Licence holders with UK domain name was found!")
  }else{
  
  # remove duplicate urls
  no_url_dups <- distinct(reg_dom_names.uk, Url, .keep_all = T)
  dim(no_url_dups) #605   7
  head(no_url_dups)
  
  # Determine the list of licence holders that should be registered with GAMSTOP
  Pub_Reg.df = read_excel(file, sheet = "Public Register", skip = 5, na = c("",NA), trim_ws = T)#catch NA and leading and trailing spaces, skip 5 rows
  common <- intersect(Pub_Reg.df$`Account Number`, no_url_dups$`Account Number`)
  found_on_register <- Pub_Reg.df[Pub_Reg.df$`Account Number` %in% common, ]
  not_found_on_register <- Pub_Reg.df[!(Pub_Reg.df$`Account Number` %in% common), ]
  
  #Save results to results folder
  fwrite(found_on_register,paste0("results/","found_on_register.csv"), quote = F, sep = "\t")# List of licence holders that are already registered with GAMSTOP 
  fwrite(not_found_on_register,paste0("results/","not_found_on_register.csv"), quote = F, sep = "\t") # List of licence holders that require registration with GAMSTOP
  }
}

main()
