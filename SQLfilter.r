    library(dplyr)
    library(tidyr)
	library(RSQLite)
    setwd( "C:\\Users\\usato\\Documents\\YandexDisk\\isr")
    SQLite_path1= "C:\\Users\\usato\\Documents\\YandexDisk\\CURRENT WORK\\ISR-SSLDIET\\tblsIN\\ISR_20210701_as20211025.db"
    sqlite    <- dbDriver("SQLite")
   SSL <- dbConnect(sqlite,   SQLite_path1)
   Sys.setlocale("LC_ALL","Russian_Russia.20866")

#######
 
  cf=as_tibble(dbGetQuery(SSL,paste0("select * from Result_update2021")))
  cf$year=substr(cf$date,1,4)
                      fshSSL=c("терпуги",
		                     "минтай",
							 "бычки",
							 "анчоусы",
		                    "треска",
							"треска ярусная",
							
							"корюшка азиатская зубастая",
							"корюшка малоротая морская",
							"корюшка малоротая",
							"корюшка",	
							
							"камбала",
							"камбала-ерш",
							 "камбалы дальневосточные",
							 
							 "сельдь тихоокеанская",
							 "сельдь",
							 
							 
							 "кальмар командорский",
							 "кальмар тихоокеанский",
							 "кальмар Бартрама",
							 "кальмары прочие",		
							 						 
							 "горбуша",
							 "кета",
							 "нерка",							 
							 "чавыча",
							 "кижуч",
							 "лососевые т-о")
  
  
     cf1=  cf %>% #filter (fish %in% fshSSL) %>% 
	             # filter (catch_depth < 150) %>%
				  filter(year > 1999) %>% filter (year < 2011)
				  
			
 
 bd=as_tibble(dbGetQuery(SSL,paste0("select * from  Budget_time")))
   write.csv(bd,"Budget_time_2000_2010.csv", row.names=F)
   write.csv(cf1,"catch_2000_2010.csv", row.names=F)

 