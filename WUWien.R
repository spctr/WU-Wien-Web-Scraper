####################

#### Webscraping in R

####################


####Alpertunga Ertin, last updated: 13.11.2022


####Preparing Workspace

rm(list=ls()) # Clears memory

graphics.off() # Clears graphs


if (!require("pacman")) install.packages("pacman") #Installs package for package installation if not already here

pacman::p_load("tidyverse","rvest","stringr", "rebus", "lubridate", "xml2","RSelenium","jsonlite","tidyjson","RJSONIO","rjson","dplyr","writexl","data.table")

#Step 1: Extract International Courses for Exchange Students from script json object

first_feedback_page_url <- "https://www.wu.ac.at/en/programs/incoming-students/exchange-semester/academics/course-catalog/filter/79070/0/0/0//"

webpage <- read_html(first_feedback_page_url)
script_element2 <- html_nodes(webpage, '#content > div > div.content-main.col-md-8.col-lg-7 > div.tx-wu-zas-courses > script:nth-child(1)')



json2 = html_text(script_element2)

allCoursesIndex <- unlist(gregexpr('allCourses', json2))[1]
allCoursesEndIndex <- unlist(gregexpr('}}]', json2))[1]
json_string <- substr(json2, allCoursesIndex+13, allCoursesEndIndex+2)
currentSemesterIndex <- unlist(gregexpr('currentSemester', json2))[1]
current_semester <- substr(json2, currentSemesterIndex+19, currentSemesterIndex+23)

RJSONIO::isValidJSON(json_string, TRUE)

json_object <- jsonlite::fromJSON(json_string)

json_object %>%
  filter(pid == as.numeric(current_semester)) -> current_json

write_xlsx(current_json, "current_json.xlsx")

#Step 2 Download course syllabuses from course catalog
catalog_source<-"https://vvz.wu.ac.at/cgi-bin/vvz.pl?C=V;LV=4;L2=S;L3=V;L4=A;S=22W;LANG=DE;U=H"


selServ <- wdman::selenium(
  
  port = 4444L,
  
  version = 'latest',
  
  chromever = 'latest'
  
)
remDr <- remoteDriver()

remDr$open()
remDr$navigate(catalog_source)
landing_page <- xml2::read_html(remDr$getPageSource()[[1]])
institutes<-xml_attr(html_nodes(landing_page, xpath = '//*[@id="content_durchgehend"]/table/tbody/tr/td[1]/b/a'), "href")
courses=c()
for (i in institutes){
  remDr$navigate(paste("https://vvz.wu.ac.at",i,sep=""))
  institute_page <-xml2::read_html(remDr$getPageSource()[[1]])
  
  course_numbers<-html_text(html_nodes(institute_page, xpath = '//*[@id="content_durchgehend"]/table/tbody/tr/td[1]/b'))
  for (j in 1:length(course_numbers))
    courses<-c(courses,course_numbers[j])
  
}

unique_courses<-unique(courses)

#Step 3 Download unique course syllabuses to local hard drive
#for(x in unique_courses){
#  course_number<-sprintf("%04d", x)
#  course<-read_html(paste("https://learn.wu.ac.at/vvz/22w/",course_number,sep = ""))
#  write_html(course,paste("course",course_number,sep = ""))
#  Sys.sleep(0.1)
#}

#Step 4 extract attributes and values from downloaded course pages
df_list <- list()
for(x in unique_courses){
  x<-as.numeric(x)
  course_number<-sprintf("%04d", x)
  current_page<-read_html(x=paste("course",course_number,sep = ""))
  current_nodes<-html_nodes(current_page, xpath='//*[@id="bachsyllabus"]/div[1]/div[2]/div/ul/li')
  
  print(x)
  
  value_list<-list()
  attribute_list<-list()
  if(length(current_nodes)>0){
    
    value_list<-append(value_list,x)
    attribute_list<-append(attribute_list,"verid")
    for(i in 1:9){
      attribute<- html_nodes(current_page, xpath=paste("//*[@id='bachsyllabus']/div[1]/div[1]/div[",i,"]/div[1]",sep= ""))
      if(length(attribute)>0){
        string<-gsub("[\r\n]", "",str_trim(html_text(html_nodes(current_page, xpath=paste("//*[@id='bachsyllabus']/div[1]/div[1]/div[",i,"]/div[2]",sep= "")))))
        attribute_text<-gsub("[\r\n]", "",str_trim(html_text(attribute)))
        if(nchar(string)>0 && nchar(attribute_text)>0){
          if(length(string))
          value_list<-append(value_list,string[1])
          attribute_list<-append(attribute_list,attribute_text[1])
        }
      }
    }
    if(length(current_nodes)>1){
      for(i in 1:(length(current_nodes)-1)){
        attribute<-gsub("[\r\n]", "",str_trim(html_text(html_nodes(current_page, xpath=paste("//*[@id='bachsyllabus']/div[",1+3*i,"]/div[1]",sep= "")))))
        value<-gsub("[\r\n]", "",str_trim(html_text(html_nodes(current_page, xpath=paste("//*[@id='bachsyllabus']/div[",1+3*i,"]/div[2]",sep= "")))))
        if(length(attribute)>0 && length(value)>0){
          value_list<-append(value_list,value[1])
          attribute_list<-append(attribute_list,attribute[1])
        }
      }
    }

    current_df<-data.frame(matrix(ncol = length(attribute_list), nrow = 0))
    colnames(current_df)<-attribute_list
    current_df[nrow(current_df) + 1,] =value_list
    df_list[[length(df_list)+1]]<-current_df
  }

}
df_downloaded<-rbindlist(df_list, fill = TRUE)
write_xlsx(df_downloaded, "df_downloaded.xlsx")
#write.csv(df_downloaded,"df_downloaded.csv")

#Step 5 merge the two different course catalogues and save the result in a spreadsheed
current_json$verid<-as.numeric(current_json$verid)
current_json<-select(current_json, -starts_with("zas"))
current_json<-select(current_json, -starts_with("persons"))
df_merged<- merge(df_downloaded, current_json, by="verid",all = TRUE)
write_xlsx(df_merged, "df_merged.xlsx")
