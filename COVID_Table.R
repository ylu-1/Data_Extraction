
#################################################
################## load packages ################
#################################################
library(pdftools)
library(dplyr)
library(stringr)

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



######### Table Extraction Function

pdf.extraction <- function(src){
  
  temp.x <- read.pdf.EE(src = src)


Keyword_EX <- "Die|die|Mortal|mortal|Death|death"
Field_type_key <- "Placebo|placebo|Favipira|favipira|Control|Treat|control|treat|SOC"


### Data Cleaning Trial - Try to clean the truncated line
table_s <- grep("Table \\d", temp.x, ignore.case = T)
table_del <- which(nchar(temp.x) < 10)


table_delete <- which(table_del %in% table_s != "TRUE")
temp.x.new <- temp.x[-table_del[table_delete]]


table_s_new <- grep("Table \\d", temp.x.new, ignore.case = T)

### Table location Search
table_loc <- gregexpr("Table \\d", temp.x.new[table_s_new])



loc_test1 <- vector()
for (i in 1:length(table_loc)) {
  loc_test <- which(min(table_loc[[i]]) == 1)
  if (identical(loc_test, integer(0)) == "TRUE") {
    loc_test1[i] <- 0
  } else {
    loc_test1[i] <- loc_test
  }
}

table_location <- table_s_new[which(loc_test1 == 1)]



table_start <- vector()
for (i in 1:length(table_location)) {
  cnt <- 1
  repeat {
    if (identical(grep(r"{\d\)}", temp.x.new[table_location[i]+cnt], ignore.case = T), integer(0)) != "TRUE") {
      table_start[i] <- table_location[i]+cnt
    } else if (identical(grep("%) ", temp.x.new[table_location[i]+cnt], ignore.case = T), integer(0)) != "TRUE") {
      table_start[i] <- table_location[i]+cnt
    } else if (identical(grep(r"{\d}", temp.x.new[table_location[i]+cnt], ignore.case = T), integer(0)) != "TRUE") {
      table_start[i] <- table_location[i]+cnt
    } 
    cnt <- cnt + 1
    if(cnt > 20 |is.na(table_start[i]) != "TRUE") break
  }
  
}

table_start <- table_start[which(table_start != "NA")]


# Check where the Table Value End & consider the truncated line in table

table_end <- vector()
for (i in 1:length(table_start)) {
  cnt <- 0 
  repeat {
    table_end[i] <- table_start[i] + cnt
    cnt <- cnt+1
    if (cnt > 1 & nchar(temp.x.new[table_end[i]]) < nchar(temp.x.new[table_end[i] + 1]) ) {
      table_end[i] <- table_end[i] + 1
    }  else {
      table_end[i] <- table_end[i]
    }
    
    if(identical(grep(r"{\d\)}", temp.x.new[table_end[i]], ignore.case = T), integer(0)) == "TRUE" &
       identical(grep("%) ", temp.x.new[table_end[i]], ignore.case = T), integer(0)) == "TRUE" &
       identical(grep("[0-9]+$", temp.x.new[table_end[i]], ignore.case = T), integer(0)) == "TRUE") break
  }
}
table_end <- table_end - 1




point_s <- vector()
start_point <- vector()
table_start_new <- vector()
if (length(table_start) > 1) {
  start_point <- grep("%)", temp.x.new[table_start], ignore.case = T)
  
  if (identical(start_point,integer(0)) == "TRUE") {
    start_point <- grep("n =", temp.x.new[table_start], ignore.case = T)
    if (identical(start_point,integer(0)) == "TRUE") {
      for (i in 1:length(table_start)) {
        if (identical(grep(Keyword_EX, temp.x.new[table_start[i]:table_end[i]], ignore.case = T), integer(0)) != "TRUE") {
          point_s[i] <- min(grep(Keyword_EX, temp.x.new[table_start[i]:table_end[i]], ignore.case = T))
          start_point[i] <- i
        }
      }
      
      if (length(start_point) != 1) {
        start_point <- which.max(table_end-table_start)
      }
    } else if (length(start_point) > 1) {
      start_point <- grep(r"{\d\)}", temp.x.new[table_start+1], ignore.case = T)
    }
  } else if (length(start_point) > 1) {
    start_point_field <- grep(Field_type_key, temp.x.new[table_start[start_point]], ignore.case = T)
      if (identical(start_point_field,integer(0)) == "TRUE") {
        start_point <- grep(r"{\d\)}", temp.x.new[table_start+1], ignore.case = T)
      } else {
        start_point <- start_point[start_point_field]
        
      }
  } else{
    start_point <- start_point
  }
  if (table_start[start_point] != table_end[start_point]) {
    table_content <- temp.x.new[table_start[start_point]:table_end[start_point]]
    
  } else {
    ########### Reformat the table content caused by the linked string
    table.new <- strsplit(temp.x.new[table_start[start_point]:length(temp.x.new)],split = "     ")
    test1.table <- vector()
    
   
    test1.table_del <- vector()
    
    for (i in 1:length(table.new)) {
      
      if (length(table.new[[i]]) > 1) {
        end_test <- table.new[[i]][length(table.new[[i]])]
        blank_test1 <- table.new[[i]][which(table.new[[i]] != "")]
        if (nchar(end_test) < 30) {
          blank_test2 <- blank_test1
        } else {
          blank_test2 <- blank_test1[-length(blank_test1)]
        }    
        test1.table[i] <- paste(blank_test2,collapse =  " ")
      } else if (identical(grep("Table \\d", table.new[[i]], ignore.case = T), integer(0)) != "TRUE") {
        test1.table[i] <- table.new[[i]]
      } else {
        test1.table[i] <- table.new[[i]]
        test1.table_del[i] <- i
      }
      
    }
    table.format.new <- test1.table[-which(test1.table_del != "NA")]
    

      cnt <- 1
      repeat {
        if (identical(grep(r"{\d\)}", table.format.new[1+cnt], ignore.case = T), integer(0)) != "TRUE") {
          table_start_new <- 1+cnt
        } else if (identical(grep(r"{\d\%}", table.format.new[1+cnt], ignore.case = T), integer(0)) != "TRUE") {
          table_start_new <- 1+cnt
        } else if (identical(grep(r"{\d}", table.format.new[1+cnt], ignore.case = T), integer(0)) != "TRUE") {
          table_start_new <- 1+cnt
        } 
        cnt <- cnt + 1
        if(cnt > 20 |identical(table_start_new, logical(0)) != "TRUE") break
      }
      
  
    table_end_new <- vector()
      cnt <- 0 
      repeat {
        table_end_new <- table_start_new + cnt
        cnt <- cnt+1
        if (cnt > 1 & nchar(table.format.new[table_end_new]) < nchar(table.format.new[table_end_new + 1]) ) {
          table_end_new <- table_end_new + 1
        }  else {
          table_end_new <- table_end_new
        }
        
        if(identical(grep(r"{\d\)}", table.format.new[table_end_new], ignore.case = T), integer(0)) == "TRUE" &
           identical(grep("%) ", table.format.new[table_end_new], ignore.case = T), integer(0)) == "TRUE" &
           identical(grep("[0-9]+$", table.format.new[table_end_new], ignore.case = T), integer(0)) == "TRUE") break
      }
  
      table_end_new <- table_end_new - 1
    table_content <- table.format.new[table_start_new:table_end_new]
  }

} else{
  start_point <- 1
  table_content <- temp.x.new[table_start:table_end]
  if (identical(grep(Keyword_EX, table_content, ignore.case = T), integer(0)) == "TRUE") {
    table_end_cnt <- 1
    table_end_add <- vector()
    repeat {
      table_end_new <- table_end + table_end_cnt
      table_end_cnt <- table_end_cnt + 1
      if (identical(grep(Keyword_EX, temp.x.new[table_end_new], ignore.case = T), integer(0)) != "TRUE") {
        table_end_add <- temp.x.new[table_end_new]
      }  else {
        table_end_add <- table_end_add
      }
      
      if(identical(table_end_add, logical(0)) != "TRUE" ) break
    }
    table_content <- c(table_content,table_end_add)
  }
  
}




if (identical(point_s, logical(0)) != "TRUE") {
  point_choie <- grep(Keyword_EX, temp.x.new[table_start[start_point]:table_end[start_point]], ignore.case = T)
  table_content <- temp.x.new[(table_start[start_point]+min(point_choie)-1):(table_start[start_point]+max(point_choie)-1)]
  if (length(table_content) < 2) {
    table_content <- temp.x.new[(table_start[start_point]+min(point_choie)-1):table_end[start_point]]
  }
} else {
  table_content <- table_content
}


test1 <- strsplit(table_content,split = "  ")

cnt <- 1
table_val <- vector()

repeat {
  # if (length(test1[[cnt]]) < 5) {
  #   cnt <- cnt+1
  # }else 
    if (cnt > 1) {
    test_1 <- which(test1[[cnt]] != "")
    val <- test1[[cnt]][test_1]
    
    if (length(val) < 2) {
      cnt <- cnt + 1
    } else if (length(val) > length(table_val[1,])) {
      table_val <- rbind(c("",table_val),val)
      cnt <- cnt + 1
    } else if (length(val) < length(table_val[1,])) {
      test1_new <- strsplit(table_content[cnt],split = " ")
      test_11 <- which(test1_new[[1]] != "")
      val1 <- test1_new[[1]][test_11]
      if (identical(grep("total|Total", val1, ignore.case = T),integer(0)) != "TRUE") {
        trt_num <- length(table_val[1,-1] ) / length(val1[-1]) 
        
        val_total1 <- vector()
        for (total_i in 1:trt_num) {
          val_total <- c(val1[1+total_i],"")
          val_total1 <- c(val_total1,val_total)
        }
        
        val <- c(val1[1],val_total1)
      } else {
        val_num <- grep("^[0-9]+$|%)", val1, ignore.case = T)
        val_num2 <- grep(r"{\d\)}", val1, ignore.case = T)
        val_1 <- paste(val1[-c(val_num,val_num2)],collapse = " ")
        
        if (identical(grep("%)", val1, ignore.case = T),integer(0)) == "TRUE") {
          val_2 <- paste(val1[val_num2-1],val1[val_num2])
        } else {
          val_prop <- grep("%)", val1, ignore.case = T)
          val_2 <- paste(val1[val_prop-1],val1[val_prop])
        }
        val <- c(val_1,val_2)
      }
      
      table_val <- rbind(table_val,val)
      cnt <- cnt + 1
    } 
    
    else if (length(val) == length(table_val[1,]) & cnt < 3 & length(val) < 3 &
               identical(grep("[A-Z]", val, ignore.case = T),integer(0)) != "TRUE") {
      test1_new <- strsplit(table_content[cnt],split = " ")
      test_11 <- which(test1_new[[1]] != "")
      val1 <- test1_new[[1]][test_11]
      val_num <- grep("^[0-9]+$|%)", val1, ignore.case = T)
      val_num2 <- grep(r"{\d\)}", val1, ignore.case = T)
      val_1 <- paste(val1[-c(val_num,val_num2)],collapse = " ")

        if (identical(grep("%)", val1, ignore.case = T),integer(0)) == "TRUE") {
          val_2 <- paste(val1[val_num2-1],val1[val_num2])
        } else {
          val_prop <- grep("%)", val1, ignore.case = T)
          val_2 <- paste(val1[val_prop-1],val1[val_prop])
        }
        val <- c(val_1,val_2)
        table_val <- rbind(c("",table_val),val)
        cnt <- cnt + 1
    }
    else {
      table_val <- rbind(table_val,val)
      cnt <- cnt + 1
    }
    
  } else {
    test_1 <- which(test1[[cnt]] != "")
    val <- test1[[cnt]][test_1]
    cnt <- cnt+1
    table_val <- rbind(table_val,val)
  }
  
  if(cnt > length(test1)) break
}

if (identical(table_start_new,logical(0)) != "TRUE") {
  temp.x.new <- table.format.new
  table_start <- table_start_new
  start_point <- 1
} else {
  temp.x.new <- temp.x.new
  table_start <- table_start
  start_point <- start_point
  
}


if (identical(grep(Field_type_key, temp.x.new[table_start[start_point]], ignore.case = T),integer(0)) != "TRUE") {
  table_source_s <- table_start[start_point] 
} else {
    cnt <- 1
    repeat {
    table_source_s <- table_start[start_point] - cnt
    cnt <- cnt+1
  
    if(identical(grep(r"{\d\)}", temp.x.new[table_source_s], ignore.case = T), integer(0)) == "TRUE" &
     identical(grep("%)", temp.x.new[table_source_s], ignore.case = T), integer(0)) == "TRUE" &
     identical(grep("[0-9] ", temp.x.new[table_source_s], ignore.case = T), integer(0)) == "TRUE") break
    }
}

table_source <- temp.x.new[table_source_s]
table_test <- strsplit(table_source,split = "  ")
table_test1 <- which(table_test[[1]] != "")
table_source1 <- table_test[[1]][table_test1]


# Source adjustment


if (table_source_s == table_start[start_point]) {
  field_word <- vector()
  for (i in 1:length(table_val[1,])) {
    field_test <- strsplit(table_val[1,i],split = " ")
    if (identical(field_test[[1]],character(0)) == "TRUE") {
      field_word[i] <- ""
    } else if (length(field_test[[1]]) < 2) {
      field_word[i] <- field_test[[1]]
    } else {
      field_word_s <- grep(Field_type_key, field_test[[1]], ignore.case = T)
      if (identical(field_word_s,integer(0)) == "TRUE") {
        field_word[i] <- str_extract(table_val[1,i], "[A-Z]+[a-z]+")
      } else if (length(field_word_s) > 1) {
        field_word[i] <- paste(field_test[[1]][field_word_s], collapse = "+")
      } else {
        field_word[i] <- field_test[[1]][field_word_s]
      }
    } 

  }
  table_source1 <- field_word
  
} else if (min(nchar(table_source1)) > 20) {
  table_source1 <- c(rep(table_source1[1],2),rep(table_source1[2],2),rep(table_source1[3],2))
} else if (max(nchar(table_source1)) > 16) {
  table_test <- strsplit(table_source,split = " ")
  table_test1 <- which(table_test[[1]] != "")
  table_source1 <- table_test[[1]][table_test1]
} else {
  table_source1 <- table_source1
}

if (length(table_source1) < length(table_val[1,])) {
  table_source1 <- c("",table_source1)
} else{
  table_source1 <- table_source1
}



table_source_ss <- vector()
### Due to the table design of source name or truncated line
if (nchar(table_source) < 12) {
  table_val[1,1] <- paste(table_source, table_val[1,1])
  table_source_ss <- table_source_s - 1
  table_source <- temp.x.new[table_source_ss]
}  else {
  table_source <- temp.x.new[table_source_s]
}




table_source_test <- vector()
if (identical(grep(Field_type_key, table_source1,ignore.case = T),integer(0)) == "TRUE") {
  if (identical(table_source_ss, logical(0)) == "TRUE") {
    cnt <- 1
  } else {
    cnt <- 2
  } 
  repeat {
    table_source_test <- table_source_s - cnt
    cnt <- cnt+1
    if (identical(grep(Field_type_key, temp.x.new[table_source_test],ignore.case = T), integer(0)) == "TRUE") {
      source_add <- strsplit(temp.x.new[table_source_test],split = " ")
      source_add1 <- source_add[[1]][which(source_add[[1]] != "")]
      source_add11 <- unique(source_add1)
      if (length(source_add11) == 1 & identical(grep("Total|total", source_add1,ignore.case = T),integer(0)) == "FALSE") {
        source_add_loc <- grep("Calves|calves",table_source1,ignore.case = T)
        table_source1[source_add_loc] <- paste(source_add11, table_source1[source_add_loc])
      }
    }
    
    if(identical(grep(Field_type_key, temp.x.new[table_source_test],ignore.case = T), integer(0)) == "FALSE") break
  }
}

table_source_new <- vector()
if (identical(table_source_test,logical(0)) != "TRUE") {
  table_source_new <- temp.x.new[table_source_test]
  source_test <- strsplit(table_source_new,split = "  ")
  source_test1 <- which(source_test[[1]] != "")
  table_source2 <- source_test[[1]][source_test1]
  if (length(table_source2) < length(table_source1) ) {
    table_source1s <- which(table_source1 != "")[-1]
    rep_num <- length(unique(str_replace(table_source1[table_source1s], " ","")))
    table_source2s <- vector()
    for (source_i in 1:(length(table_source2)-1)) {
      table_source2s <- c(table_source2s,rep(table_source2[source_i],rep_num))
    }
    table_source_vac <- c("", table_source2s,rep(table_source2[length(table_source2)],length(table_source1s) - length(table_source2s)))
    
    if (length(table_source_vac) != length(table_source1)) {
      table_source1 <- rbind(c(rep("",length(which(table_source1[[1]] == ""))),table_source_vac),table_source1)
    } else {
      table_source1 <- rbind(table_source_vac,table_source1)
    }
    
  }
} else {
  table_source1 <- table_source1
}
  



table_result <- rbind(table_source1,table_val)



row.names(table_result) <- c(rep("",length(table_result[,1])))



cnt <- 2
arm_test <- vector()
overall_test <- vector()
repeat {
  
  if (identical(grep("n =|n=", table_result[,cnt], ignore.case = T), integer(0)) == "TRUE" &
      identical(grep("%)", table_result[,cnt], ignore.case = T), integer(0)) == "TRUE" &
      identical(grep("N", str_extract(table_result[,cnt], "[A-Z]"), ignore.case = T), integer(0)) == "TRUE")  {
    arm_test <- c(arm_test,cnt)
  }else if (identical(grep("Overall|Total", table_result[,cnt], ignore.case = T), integer(0)) != "TRUE") {
    overall_test <- c(overall_test,cnt)
  }
  cnt <- cnt+1
  if(cnt > length(table_result[1,])) break
}

if (identical(arm_test, logical(0)) == "TRUE" & identical(overall_test, logical(0)) == "TRUE") {
  table_result1 <- table_result
} else if (identical(overall_test, logical(0)) != "TRUE" & identical(arm_test, logical(0)) == "TRUE") {
  table_result1 <- table_result[,-overall_test]
} else if (identical(overall_test, logical(0)) == "TRUE" & identical(arm_test, logical(0)) != "TRUE") {
  table_result1 <- table_result[,-arm_test]
} else {
  table_result1 <- table_result[,-unique(c(arm_test,overall_test))]
}

# Clean the source value rows, only left the important row
if (identical(table_source_test,logical(0)) != "TRUE") {
  if (table_source != table_source_new) {
    table_result1 <- table_result[-2,-arm_test]
  }
  
} else {
  table_result1 <- table_result1
}


IBK_loc <- grep(Keyword_EX, table_result1[,1], ignore.case = T)
IBK_table <- table_result1[c(1,IBK_loc),]

size_table <- table_result1[-IBK_loc,]



meta_table <- vector()


for (i in 2:length(size_table[1,])) {
  source_txt <- str_extract(IBK_table[,i], "[A-Z]+[a-z]*")
  if (identical(grep("^V|^v",source_txt,ignore.case = T),integer(0)) != "TRUE" ) {
    trt_txt <- "Vaccinated"
  } else if (identical(grep("^U|^u",source_txt,ignore.case = T),integer(0)) != "TRUE") {
    trt_txt <- "Unvaccinated"
  } else if (identical(grep("^P|^p",source_txt,ignore.case = T),integer(0)) != "TRUE") {
    trt_txt <- "Placebo"
  } else if (identical(grep("[A-Z]",source_txt,ignore.case = T),integer(0)) != "TRUE") {
    trt_txt <- IBK_table[grep("[A-Z]",source_txt,ignore.case = T),i]
  }
  
  value_info <- str_extract(IBK_table[,i], "[0-9]+")
  
  size_info_test <- str_extract(size_table[,i], "[0-9]+")
  size_info_test_n <- grep("n =|n=", size_table[,i], ignore.case = T)
  if (identical(size_info_test_n,integer(0)) != "TRUE") {
    size_info <- size_info_test[size_info_test_n]
  } else if (length(size_info_test) > 2) {
    size_info <- max(as.integer(size_info_test),na.rm = TRUE)
  } else {
    size_info <- size_info_test
  }
  trial <- cbind(source_txt,value_info,size_info,trt_txt)
  meta_table <- rbind(meta_table,trial)
}


del_row <- vector()
for (i in 1:length(meta_table[,1])) {
  if (is.na(meta_table[i,2]) == "TRUE") {
    del_row <- c(del_row,i)
  }
}

table_s <- unique(meta_table[-del_row,])



table_s

title <- vector()

file.title.temp <- str_extract(src, regex("/.+", ignore_case = TRUE))
file.title <- strsplit(file.title.temp, split = "/")
title <- file.title[[1]][grep(".pdf", file.title[[1]], ignore.case = T)]



title_1 <- sub(".pdf.*", "",title)

return.x <- cbind(c(rep(title_1,length(table_s[,2]))),table_s)
colnames(return.x) <- c("Reference", "", "Events", "Total", 
                     "Treatment")
return(return.x)



}





