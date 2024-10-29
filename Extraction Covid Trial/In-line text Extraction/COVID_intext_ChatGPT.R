
#################################################
################## load packages ################
#################################################
library(pdftools)
library(dplyr)
library(stringr)
library(httr)
library(TheOpenAIR)

#################################################
############### PDF import function #############
#################################################
read.pdf.EE <- function(src)
{
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  QTD_COLUMNS <- 2
  read_text <- function(text) {
    result <- ''
    #Get all index of " " from page.
    lstops <- gregexpr(pattern =" ",text)
    #Puts the index of the most frequents ' ' in a vector.
    stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
    #Slice based in the specified number of columns (this can be improved)
    for(i in seq(1, QTD_COLUMNS, by=1))
    {
      temp_result <- sapply(text, function(x){
        start <- 1
        stop <-stops[i] 
        if(i > 1)            
          start <- stops[i-1] + 1
        if(i == QTD_COLUMNS)#last column, read until end.
          stop <- nchar(x)+1
        substr(x, start=start, stop=stop)
      }, USE.NAMES=FALSE)
      temp_result <- trim(temp_result)
      result <- append(result, temp_result)
    }
    result
  }
  
  txt <- pdf_text(src)
  result <- ''
  for (i in 1:length(txt)) { 
    page <- txt[i]
    t1 <- unlist(strsplit(page, "\n"))      
    maxSize <- max(nchar(t1))
    
    t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
    result = append(result,read_text(t1))
  }
  result <- result[!result %in% ""]
  return(result)
}


testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

######### Extraction start

######### Extraction start
pdf.extraction <- function(src){
  
temp.x <- read.pdf.EE(src = src)

######### Key Word Setting
Keyword_EX <- " Die|die|Mortal|mortal|Death|death"
Field_type_key <- "Placebo|placebo|Favipira|favipira|Control|control|FPV|standard|Standard|SOC|AVIFAVIR"


####### Result Session
sub_title_loc1 <- grep("^[A-Z]+$", temp.x, ignore.case = T)
sub_title_loc2 <- grep(r"{^\d\. [A-Z]+$}", temp.x, ignore.case = T)
sub_title_loc <- sort(unique(c(sub_title_loc1,sub_title_loc2)))
result_loc <- sub_title_loc[grep("result|Result", temp.x[sub_title_loc], ignore.case = T)]
discussion_loc <- sub_title_loc[grep("discussion|DISCUSSION|Discussion", temp.x[sub_title_loc], ignore.case = T)]

temp.x_field <- temp.x[result_loc:length(temp.x)]



########### Data Cleaning for Numeric data format 
temp.x_number1 <- str_replace_all(temp.x_field,"one|One ", " 1 ")
temp.x_number2 <- str_replace_all(temp.x_number1,"two|Two", " 2 ")
temp.x_number3 <- str_replace_all(temp.x_number2,"three|Three", " 3 ")
temp.x_number4 <- str_replace_all(temp.x_number3,"four|Four ", " 4 ")
temp.x_number5 <- str_replace_all(temp.x_number4,"five|Five ", " 5 ")
temp.x_number6 <- str_replace_all(temp.x_number5,"six|Six ", " 6 ")
temp.x_number7 <- str_replace_all(temp.x_number6,"seven|Seven ", " 7 ")


######## outcome Key word screening
keyword_s <- grep(Keyword_EX, temp.x_number7, ignore.case = T)

# First String Clean based on Number exist or not

key_string <- vector()
keyword_keep <- vector()

keyword_string_test1 <- vector()
keyword_string_test <- vector()
key_string_prev_add <- vector()


for (i in 1:length(keyword_s)) {
  key_string_test <- strsplit(temp.x_number7[keyword_s[i]],split = "      ")
  key_string_test_prev <- strsplit(temp.x_number7[keyword_s[i]-1],split = "      ")
  key_string_test_follow <- strsplit(temp.x_number7[keyword_s[i]+1],split = "      ")
  key_string_add <- vector()
  if (length(key_string_test[[1]]) != 1) {
    key_string_loc <- grep(Keyword_EX,key_string_test[[1]])
    if (identical(key_string_loc,integer(0)) == "TRUE") {
      keyword_string_test[i] <- ""
    } else  {
      if (identical(grep("^[A-Z]",key_string_test[[1]][key_string_loc]),integer(0)) != "TRUE") {
        
        key_peirod_cnt <- 0
        repeat {
          key_peirod_cnt <- key_peirod_cnt + 1
          key_string_test_new <- temp.x_number7[keyword_s[i]+key_peirod_cnt]
          key_string_test_target <- strsplit(key_string_test_new,split = "      ")[[1]][key_string_loc]
          key_string_add <- paste(key_string_add,key_string_test_target, collapse = " ")
          if(identical(grep(r"{\. }",key_string_add), integer(0)) != "TRUE" |
             identical(grep(r"{\.$}",key_string_add), integer(0)) != "TRUE") break
        }
        keyword_string_test[i] <- paste(key_string_test[[1]][key_string_loc],key_string_add,collapse = "")
        
      } else if (identical(grep(r"{\. }",key_string_test[[1]][key_string_loc]),integer(0)) == "TRUE" & 
                 identical(grep(r"{\.$}",key_string_test[[1]][key_string_loc]),integer(0)) == "TRUE") {
        key_peirod_prev_cnt <- 0
        repeat {
          key_peirod_prev_cnt <- key_peirod_prev_cnt + 1
          key_string_prev_test_new <- temp.x_number7[keyword_s[i]-key_peirod_prev_cnt]
          key_string_prev_test_target <- strsplit(key_string_prev_test_new,split = "      ")[[1]][key_string_loc]
          key_string_prev_add <- paste(key_string_prev_test_target, key_string_prev_add, collapse = " ")
          if(identical(grep(r"{\. }",key_string_prev_add), integer(0)) != "TRUE" |
             identical(grep(r"{\.$}",key_string_prev_add), integer(0)) != "TRUE") break
        }
        
        
        key_peirod_cnt <- 0
        repeat {
          key_peirod_cnt <- key_peirod_cnt + 1
          key_string_test_new <- temp.x_number7[keyword_s[i]+key_peirod_cnt]
          key_string_test_target <- strsplit(key_string_test_new,split = "      ")[[1]][key_string_loc]
          key_string_add <- paste(key_string_add,key_string_test_target, collapse = " ")
          if(identical(grep(r"{\. }",key_string_add), integer(0)) != "TRUE" |
             identical(grep(r"{\.$}",key_string_add), integer(0)) != "TRUE") break
        }
        keyword_string_test[i] <- paste(key_string_prev_add,key_string_test[[1]][key_string_loc],key_string_add,collapse = "")
      }
      
    }
    
  } else if (identical(grep(r"{^\d\.|^\d\.\d\.}",temp.x_number7[keyword_s[i]]),integer(0)) != "TRUE") {
    keyword_string_test[i] <- ""
  } else {
    keyword_line_loc <- gregexpr("died",temp.x_number7[keyword_s[i]])
    if (keyword_line_loc[[1]][1] == 1) {
      if (identical(grep(Field_type_key, temp.x_number7[keyword_s[i]],
                         ignore.case = T),integer(0)) == "TRUE") {
        cnt <- 1
        keyword_string_test1 <- temp.x_number7[keyword_s[i]]
        repeat{
          keyword_string_test1 <- c(temp.x_number7[keyword_s[i]-cnt],keyword_string_test1)
          cnt <- cnt +1
          if(identical(grep(Field_type_key, keyword_string_test1, ignore.case = T), integer(0)) != "TRUE"| cnt > 5) break
        }
        keyword_string_test[i] <- paste(keyword_string_test1,collapse = " ")
      } else {
        keyword_string_test[i] <- temp.x_number7[keyword_s[i]]
      } 
    } else {
      key_period_end_test1 <- grep(r"{\. }",temp.x_number7[keyword_s[i]])
      key_period_end_test2 <- grep(r"{\.$}",temp.x_number7[keyword_s[i]])
      key_period_end_test3 <- grep("^[A-Z]",temp.x_number7[keyword_s[i]])
      
      key_peirod_cnt <- 0
      key_cnt_del <- vector()
      repeat {
        key_peirod_cnt <- key_peirod_cnt + 1
        
        if (key_peirod_cnt < 3) {
          repeat{
            
            if (nchar(temp.x_number7[keyword_s[i]]) > (nchar(temp.x_number7[keyword_s[i]+key_peirod_cnt]) + 10)) {
              if (identical(grep(Field_type_key, temp.x_number7[keyword_s[i]+key_peirod_cnt], ignore.case = T), integer(0)) != "TRUE") {
                key_peirod_cnt <- key_peirod_cnt
              } else {
                key_cnt_del <- c(key_cnt_del,key_peirod_cnt)
                key_peirod_cnt <- key_peirod_cnt + 1
              }
              
            } else {
              key_peirod_cnt <- key_peirod_cnt
            }
            
            if (nchar(temp.x_number7[keyword_s[i]]) <= (nchar(temp.x_number7[keyword_s[i]+key_peirod_cnt]) + 10)|
                identical(grep(Field_type_key, temp.x_number7[keyword_s[i]+key_peirod_cnt], ignore.case = T), integer(0)) != "TRUE") break
          }
        }
        
        if(identical(grep(r"{\. }",temp.x_number7[keyword_s[i]+key_peirod_cnt]), integer(0)) != "TRUE" |
           identical(grep(r"{\.$}",temp.x_number7[keyword_s[i]+key_peirod_cnt]), integer(0)) != "TRUE") break
      }
      if (identical(key_period_end_test1,integer(0)) == "TRUE" & 
          identical(key_period_end_test2,integer(0)) == "TRUE" &
          identical(key_period_end_test3,integer(0)) == "TRUE") {
        key_prev_test <- temp.x_number7[keyword_s[i]-1]
        if (identical(grep(r"{\. }",key_prev_test), integer(0)) != "TRUE") {
          key_prev_part_test <- strsplit(key_prev_test,r"{\. }")[[1]]
          key_prev_part <- key_prev_part_test[length(key_prev_part_test)]
        } else {
          key_prev_part <- temp.x_number7[keyword_s[i]-1]
        }
        if (identical(grep(r"{\.$}",temp.x_number7[keyword_s[i]+key_peirod_cnt]), integer(0)) != "TRUE") {
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(key_prev_part,temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+(1:key_peirod_cnt)]),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(key_prev_part,temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+((1:key_peirod_cnt)[-key_cnt_del])]),collapse = " ")
          }
          
        } else {
          key_peirod_part <- strsplit(temp.x_number7[keyword_s[i]+key_peirod_cnt],r"{\. [A-Z]}")[[1]][1]
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(key_prev_part,temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+(1:(key_peirod_cnt-1))],key_peirod_part),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(key_prev_part,temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+((1:(key_peirod_cnt-1))[-key_cnt_del])],key_peirod_part),collapse = " ")
          }
          
        }
        
      } else if (identical(key_period_end_test1,integer(0)) != "TRUE") {
        key_peirod_part <- strsplit(temp.x_number7[keyword_s[i]],r"{\. }")[[1]]
        key_peirod_prev_test <- strsplit(temp.x_number7[keyword_s[i]],r"{\. }")
        key_peirod_prev <- key_peirod_prev_test[[1]][length(key_peirod_prev_test[[1]])]
        if (length(key_peirod_part) > 2) {
          key_period_target <- which.max(nchar(key_peirod_part))
          keyword_string_test[i] <- key_peirod_part[key_period_target]
        } else if (identical(grep(r"{\.$}",temp.x_number7[keyword_s[i]+key_peirod_cnt]), integer(0)) != "TRUE") {
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(key_peirod_prev,
                                              temp.x_number7[keyword_s[i]+(1:key_peirod_cnt)]),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(key_peirod_prev,
                                              temp.x_number7[keyword_s[i]+((1:key_peirod_cnt)[-key_cnt_del])]),collapse = " ")
          }
          
        } else {
          key_peirod_part <- strsplit(temp.x_number7[keyword_s[i]+key_peirod_cnt],r"{\. [A-Z]}")[[1]][1]
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(key_peirod_prev,
                                              temp.x_number7[keyword_s[i]+(1:(key_peirod_cnt-1))],key_peirod_part),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(key_peirod_prev,
                                              temp.x_number7[keyword_s[i]+((1:(key_peirod_cnt-1))[-key_cnt_del])],key_peirod_part),collapse = " ")
          }
          
        }
      } else if (identical(key_period_end_test3,integer(0)) != "TRUE") {
        if (identical(grep(r"{\.$}",temp.x_number7[keyword_s[i]+key_peirod_cnt]), integer(0)) != "TRUE") {
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+(1:key_peirod_cnt)]),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+((1:key_peirod_cnt)[-key_cnt_del])]),collapse = " ")
          }
          
        } else {
          key_peirod_part <- strsplit(temp.x_number7[keyword_s[i]+key_peirod_cnt],r"{\. [A-Z]|\. [0-9]}")[[1]][1]
          if (identical(key_cnt_del,logical(0)) == "TRUE") {
            keyword_string_test[i] <- paste(c(temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+(1:(key_peirod_cnt-1))],key_peirod_part),collapse = " ")
            
          } else {
            keyword_string_test[i] <- paste(c(temp.x_number7[keyword_s[i]],
                                              temp.x_number7[keyword_s[i]+((1:(key_peirod_cnt-1))[-key_cnt_del])],key_peirod_part),collapse = " ")
          }
          
        }
      } else {
        keyword_string_test[i] <- paste(temp.x_number7[keyword_s[i]-1],temp.x_number7[keyword_s[i]],
                                        temp.x_number7[keyword_s[i]+1],temp.x_number7[keyword_s[i]+2])
      }
    }
  }
  
  if (identical(grep(r"{\d}", keyword_string_test[i], ignore.case = T), integer(0)) != "TRUE" |
      identical(grep(Field_type_key, keyword_string_test[i], ignore.case = T), integer(0)) != "TRUE") {
    keyword_keep <- c(keyword_keep,i)
  } 
}


keyword_clean <- keyword_s[keyword_keep]



keyword_string <- vector()
if (length(keyword_clean) == 1){
  if (identical(grep(temp.x_number7[keyword_clean], keyword_string_test1),integer(0)) != "TRUE" ) {
    keyword_string <- paste(keyword_string_test1,collapse = " ")
  }
} else if (identical(keyword_string_test,logical(0)) != "TRUE") {
  keyword_string <- keyword_string_test[keyword_keep]
} else {
  for (i in 1:length(keyword_clean)) {
    keyword_string[i] <- paste(temp.x_number7[keyword_clean[i]-1],temp.x_number7[keyword_clean[i]],
                               temp.x_number7[keyword_clean[i]+1])
  }
}


keep_del <- vector()
for (i in 1:length(keyword_string)) {
  keep_test <- strsplit(keyword_string[i],split = " ")
  if (identical(grep(Field_type_key, keep_test[[1]], ignore.case = T), integer(0)) == "TRUE") {
    keep_del <- c(keep_del,i)
  } else {
    Field_keep <- grep(Field_type_key, keep_test[[1]], ignore.case = T)
    Keyword_keep <- grep(Keyword_EX, keep_test[[1]], ignore.case = T)
    num_keep <- grep("^[0-9]+$", keep_test[[1]], ignore.case = T)
    Day_month_year_test <- grep("day|year|month|DAY|Year|Month", keep_test[[1]], ignore.case = T)
    if (identical(Field_keep, integer(0)) == "TRUE" | identical(Keyword_keep, integer(0)) == "TRUE") {
      keep_del <- c(keep_del,i)
    }else if (length(num_keep) <= length(Day_month_year_test)) {
      keep_del <- c(keep_del,i)
    } else if (length(num_keep) <= 2) {
      if (identical(num_keep,integer(0)) == "TRUE") {
        keep_del <- c(keep_del,i)
      } else {
        keep_del <- keep_del
      }
    }
    
  }
}

if (identical(keep_del, logical(0)) == "TRUE") {
  key_Result_string1 <- keyword_string
} else {
  key_Result_string1 <- keyword_string[-keep_del]
  
}

key_keep_del <- vector()
key_num_sum <- vector()
if (length(key_Result_string1) > 1) {
  for (i in 1:length(key_Result_string1)) {
    keyword_keep_test <- strsplit(key_Result_string1[i],split = " ")
    key_num_keep <- grep("^[0-9]+$", keyword_keep_test[[1]], ignore.case = T)
    key_num_elment <- as.numeric(keyword_keep_test[[1]][key_num_keep])
    if (length(key_num_keep) < 2 | length(key_num_keep) > 7) {
      key_keep_del <- c(key_keep_del,i)
    } else { 
      if (max(key_num_elment) > 1000) {
        key_keep_del <- c(key_keep_del,i)
      } else {
        key_num_sum1 <- sum(as.numeric(key_num_elment))
        key_num_sum <- c(key_num_sum,key_num_sum1)
      }
    }
  }
  keyword_string_1 <- key_Result_string1[-key_keep_del]
  key_Result_string <- keyword_string_1[which.max(key_num_sum)]
} else {
  key_Result_string <- key_Result_string1
}



Field_type_key <- "Placebo|placebo|Favipira|favipira|Control|control|FPV|standard|Standard|SOC|AVIFAVIR|lopinavir|ritonavir|Lopinavir|Ritonavir"


field_s <- grep(Field_type_key, temp.x_number7, ignore.case = T)

# First String Clean based on Number exist or not
field_del <- vector()
for (i in 1:length(field_s)) {
  if (identical(grep(r"{ \d+ }", temp.x_number7[field_s[i]], ignore.case = T), integer(0)) == "TRUE" &
      identical(grep(r"{\d\/\d}", temp.x_number7[field_s[i]], ignore.case = T), integer(0)) == "TRUE" &
      identical(grep("    ", temp.x_number7[field_s[i]]), integer(0)) == "TRUE") {
    field_del <- c(field_del,i)
  }  
}


field_clean <- field_s[-field_del]


field_del_new <- vector()
for (i in 1:length(field_clean)) {
  field_rep_test <- strsplit(temp.x_number7[field_clean[i]],split = " ")
  field_rep <- grep(Field_type_key,field_rep_test[[1]],ignore.case = T)
  if (length(field_rep) < 2) {
    field_del_new <- c(field_del_new,i)
  }
  
}


temp.x.clean <- field_clean[-field_del_new]

field_test <- grep("      ", temp.x_number7[temp.x.clean])


field_Result_string1 <- vector()
field_info <- vector()
num_sum <- vector()
if (identical(field_test,integer(0)) != "TRUE") {
  for (i in 1:length(field_test)) {
    split_field_test <- strsplit(temp.x_number7[temp.x.clean[field_test[i]]],split = "   ")
    split_field_num <- grep(r"{\d}",split_field_test[[1]])
    split_field_target <- grep(Field_type_key,split_field_test[[1]])
    
    if (identical(split_field_num,integer(0)) == "TRUE") {
      cnt <- 1
      repeat {
        field_test_new <- temp.x_number7[temp.x.clean[field_test[i]]+cnt]
        cnt <- cnt + 1
        
        if(identical(grep(r"{\d}", field_test_new, ignore.case = T), integer(0)) != "TRUE") break
      }
      split_test_new <- strsplit(field_test_new,split = "   ")
      
      num_target <- grep(r"{\d}",split_test_new[[1]][which(split_test_new[[1]] != "")])
      field_target_1 <- grep(Field_type_key,split_test_new[[1]])
      if (length(num_target) < 2) {
        field_info <- ""
      } else if (identical(field_target_1,integer(0)) != "TRUE") {
        if (length(field_target_1) < length(split_field_target)) {
          field_info <- ""
        }
      } else {
        if (length(num_target) != length(split_field_target)) {
          field_target <- grep(Field_type_key,split_field_test[[1]][which(split_field_test[[1]] != "")])
        } else {
          field_target <- num_target
        }
        for (j in 1:length(field_target)) {
          field_info[j] <- paste("enrolled in", split_field_test[[1]][which(split_field_test[[1]] != "")][field_target[j]], ": ",
                                 split_test_new[[1]][which(split_test_new[[1]] != "")][field_target[j]])
        }
        if (identical(grep(" NA", field_info, ignore.case = T), integer(0)) != "TRUE") {
          field_info <- ""
        } else {
          field_info <- field_info
        }
      }
      field_Result_string1[i] <- paste(field_info,collapse = ";")
    } else if (length(split_field_num) < 2) {
      field_Result_string1[i] <- ""
    }
  }
} else if (length(field_del_new) != length(field_clean)) {
  field_string <- vector()
  for (i in 1:length(temp.x.clean)) {
    
    field_string_test <- strsplit(temp.x_number7[field_clean[i]],split = "    ")
    field_string_test_prev <- strsplit(temp.x_number7[field_clean[i]-1],split = "    ")
    field_string_test_follow <- strsplit(temp.x_number7[field_clean[i]+1],split = "    ")
    if (length(field_string_test[[1]]) != 1) {
      field_string_loc <- grep(Field_type_key,field_string_test[[1]])
      if (identical(field_string_loc,integer(0)) == "TRUE") {
        field_string[i] <- ""
      } else if (length(field_string_loc) == length(field_string_test[[1]])) {
        split_field_num <- grep(r"{\d}",field_string_test[[1]])
        if (identical(split_field_num,integer(0)) == "TRUE") {
          cnt <- 1
          repeat {
            field_test_new <- temp.x_number7[field_clean[i]+cnt]
            cnt <- cnt + 1
            
            if(identical(grep(r"{\d}", field_test_new, ignore.case = T), integer(0)) != "TRUE") break
          }
        }
        
        split_test_new <- strsplit(field_test_new,split = "   ")
        
        field_info <- vector()
        for (i in 1:length(field_string_loc)) {
          field_info[i] <- paste("enrolled in", field_string_test[[1]][which(field_string_test[[1]] != "")][field_string_loc[i]], ":",
                                 split_test_new[[1]][which(split_test_new[[1]] != "")][field_string_loc[i]])
        }
        field_string[i] <- paste(field_info,collapse = ";")
      } else {
        if (length(field_string_test_prev[[1]]) != 1) {
          field_combine_prev <- paste(field_string_test_prev[[1]][field_string_loc],
                                      field_string_test[[1]][field_string_loc])
          
        } else {
          field_combine_prev <- paste(field_string_test_prev[[1]][field_string_loc],
                                      field_string_test[[1]][1])
        }
        if (length(field_string_test_follow[[1]]) != 1 ) {
          field_string[i] <- paste(field_combine_prev,
                                   field_string_test_follow[[1]][field_string_loc])
        } else {
          field_string[i] <- paste(field_combine_prev,field_string_test_follow[[1]][1])
        }
      } 
    } else {
      if (i != length(field_clean)) {
        ############ Test First since the period sign in original string
        
        follow_string <- temp.x_number7[field_clean[i]+1]
        follow_target <- temp.x_number7[field_clean[i+1]]
        if (follow_string == follow_target) {
          period_end_test1 <- grep(r"{\. }",temp.x_number7[field_clean[i]+2])
          period_end_test2 <- grep(r"{\.$}",temp.x_number7[field_clean[i]+2])
          if (identical(period_end_test1,integer(0)) == "TRUE" & 
              identical(period_end_test2,integer(0)) == "TRUE") {
            
            peirod_cnt <- 1
            repeat {
              peirod_cnt <- peirod_cnt + 1
              
              if(identical(grep(r"{\. }",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE" |
                 identical(grep(r"{\.$}",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE") break
            }
            if (identical(grep(r"{\.$}",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE") {
              field_string[i] <- paste(c(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                         temp.x_number7[field_clean[i]+(1:peirod_cnt)]),collapse = " ")
            } else {
              peirod_part <- strsplit(temp.x_number7[field_clean[i]+peirod_cnt],r"{\. [A-Z]}")[[1]][1]
              field_string[i] <- paste(c(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                         temp.x_number7[field_clean[i]+(1:(peirod_cnt-1))],peirod_part),collapse = " ")
            }
            
          } else {
            field_string[i] <- paste(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                     temp.x_number7[field_clean[i]+1],temp.x_number7[field_clean[i]+2])
          }
          
        } else {
          period_end_test1 <- grep(r"{\. }",temp.x_number7[field_clean[i]+1])
          period_end_test2 <- grep(r"{\.$}",temp.x_number7[field_clean[i]+1])
          if (identical(period_end_test1,integer(0)) == "TRUE" & 
              identical(period_end_test2,integer(0)) == "TRUE") {
            
            peirod_cnt <- 0
            repeat {
              peirod_cnt <- peirod_cnt + 1
              
              if(identical(grep(r"{\. }",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE" |
                 identical(grep(r"{\.$}",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE") break
            }
            if (identical(grep(r"{\.$}",temp.x_number7[field_clean[i]+peirod_cnt]), integer(0)) != "TRUE") {
              field_string[i] <- paste(c(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                         temp.x_number7[field_clean[i]+(1:peirod_cnt)]),collapse = " ")
            } else {
              peirod_part <- strsplit(temp.x_number7[field_clean[i]+peirod_cnt],r"{\. [A-Z]}")[[1]][1]
              field_string[i] <- paste(c(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                         temp.x_number7[field_clean[i]+(1:min(c(1,(peirod_cnt-1))))],peirod_part),collapse = " ")
            }
            
          } else {
            field_string[i] <- paste(temp.x_number7[field_clean[i]-1],temp.x_number7[field_clean[i]],
                                     temp.x_number7[field_clean[i]+1])
          }
        }
      } 
      
    }
  }
  
  
  field_keep_del <- vector()
  
  field_keep_test_n <- grep("n =|n=",field_string)
  if (identical(field_keep_test_n, integer(0)) != "TRUE") {
    field_Result_string1 <- field_string[field_keep_test_n]
  } else {
    for (i in 1:length(field_string)) {
      
      field_keep_test <- strsplit(field_string[i],split = " ")
      num_keep <- grep("^[0-9]+$", field_keep_test[[1]], ignore.case = T)
      num_elment <- as.numeric(field_keep_test[[1]][num_keep])
      if (length(num_keep) < 2 | length(num_keep) > 7) {
        field_keep_del <- c(field_keep_del,i)
      } else { 
        if (max(num_elment) > 1000) {
          field_keep_del <- c(field_keep_del,i)
        } else {
          num_sum1 <- sum(as.numeric(num_elment))
          num_sum <- c(num_sum,num_sum1)
        }
        
      }
      
    }
    field_Result_string1 <- field_string[-field_keep_del]
  }
  
  
}





if (length(field_Result_string1) > 1) {
  if (identical(num_sum,logical(0)) == TRUE) {
    if (length(field_Result_string1[which(field_Result_string1 != "")]) < 2) {
      field_Result_string <- field_Result_string1[which(field_Result_string1 != "")]
    }else {
      field_result_sort <- field_Result_string1[which(field_Result_string1 != "")]
      field_result_s <- which.min(nchar(field_result_sort))
      field_Result_string <- field_result_sort[field_result_s]
    } 
  }else {
    field_Result_string <- field_Result_string1[which(num_sum == max(num_sum))]
  }
  
  
} else {
  field_Result_string <- field_Result_string1
} 




Chat_prompt <- "Extract how many patients enrolled and died in each treatment group and provide the data with following format: name of treatment, number died, number enrolled:'"

extract_target <- paste(key_Result_string,field_Result_string,sep = ";")

chat_command <- paste(Chat_prompt,extract_target, "'")



openai_api_key("OPEN-API-KEY")



response <- chat(chat_command,output = "message")



response_0 <- str_replace_all(response, " no | not |No |Not", " 0 ")
response_1 <- str_replace_all(response_0,"\n"," ")
response_2 <- str_remove_all(response_1,r"{\(}")
response_3 <- str_replace_all(response_2, ",|;|:", " ")


response_word <- strsplit(response_3,split = " ")


response_word_num <- grep("^[0-9]+$", response_word[[1]], ignore.case = T)
response_word_field <- grep(Field_type_key, response_word[[1]], ignore.case = T)

for (i in 1:length(response_word_field)) {
  field_name_test_num <- grep("[0-9]",response_word[[1]][response_word_field[i]+1])
  if (identical(field_name_test_num,integer(0)) != "TRUE") {
    response_word[[1]][response_word_field[i]] <- paste(response_word[[1]][response_word_field[i]],
                                                        response_word[[1]][response_word_field[i]+1])
  } 
}
# response_word_keyword <- grep(str_remove_all(Keyword_EX, " "), response_word[[1]], ignore.case = T)

response_word_keep <- response_word[[1]][sort(c(response_word_num,response_word_field))]


#### According to the place of field word to create data
field_rule <- grep(Field_type_key, response_word_keep, ignore.case = T)

field_rule_del <- vector()
for (i in 1:(length(field_rule)-1)) {
  if (as.numeric(field_rule[i])+1 == field_rule[i+1]) {
    response_word_keep[field_rule[i]] <- paste(response_word_keep[field_rule[i]],"+",
                                               response_word_keep[field_rule[i+1]])
    field_rule_del <- c(field_rule_del,i+1)
  } 
}
if (identical(field_rule_del,logical(0)) != "TRUE") {
  response_word_keep <- response_word_keep[-field_rule[field_rule_del]]
  field_rule <- grep(Field_type_key, response_word_keep, ignore.case = T)
  
} else {
  response_word_keep <- response_word_keep
  field_rule <- field_rule
}


table_info <- vector()
cnt <- 1
repeat{
  
  # if (length(field_rule) != 2 & (field_rule[cnt] + 1) == field_rule[cnt + 1]) {
  #   trt_info_test <- paste(response_word_keep[(field_rule[cnt])],response_word_keep[(field_rule[cnt+1])],sep = "+")
  #   num_info_all <- as.numeric(c(response_word_keep[(field_rule[cnt]+2)],
  #                                response_word_keep[(field_rule[cnt]+3)]))
  #   cnt <- cnt + 1
  # } else {
  trt_info_test <- response_word_keep[(field_rule[cnt])]
  num_info_all <- as.numeric(c(response_word_keep[(field_rule[cnt]+1)],
                               response_word_keep[(field_rule[cnt]+2)]))
  # }
  
  died_info_test <- num_info_all[which.min(num_info_all)]
  size_info_test <- num_info_all[which.max(num_info_all)]
  num_info_test <- c(died_info_test,size_info_test,trt_info_test)
  table_info <- rbind(table_info,num_info_test)
  cnt <- cnt + 1
  if( cnt > length(field_rule)) break
}


title <- vector()

file.title.temp <- str_extract(src, regex("/.+", ignore_case = T))
file.title <- strsplit(file.title.temp, split = "/")
title <- file.title[[1]][grep(".pdf", file.title[[1]], ignore.case = T)]



title_1 <- sub(".pdf.*", "",title)

return.x <- cbind(c(rep(title_1,length(table_info[,1]))),table_info)
colnames(return.x) <- c("Reference", "Events", "Total", 
                        "Treatment")
return(return.x)

}

pdf.rob.vector <- list.files("~/In-line text Extraction", full.names = T)
pdf.rob.vector <- pdf.rob.vector[grep("pdf", pdf.rob.vector, ignore.case = T)]
pdf.rob.vector <- pdf.rob.vector[!grepl("pdftools", pdf.rob.vector)]
pdf.rob.vector

# start.time <- Sys.time()

mined.rob1 <- vector()

for(pdf.i in c(index)) {######## Use the index for articles for in-line data extraction
                     ######## Enter a single index each time to avoid
                     ######## the limit request per minute of OPENAI 
  print(pdf.i)
  print(pdf.rob.vector[pdf.i])
  mined.rob1 <-  rbind(mined.rob1, pdf.extraction(src = pdf.rob.vector[pdf.i]))
}

# end.time <- Sys.time()


# time.taken <- round(end.time - start.time,2)
# time.taken

mined.rob1



