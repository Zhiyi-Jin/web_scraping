# Billboard Japan Hot 100: http://www.billboard-japan.com/charts/detail?a=hot100

#Step 1: web scraping
#Gnerate a vector of URLS
#from "http://www.billboard-japan.com/charts/detail?a=hot100&year=2010&month=01&day=18"
#to "http://www.billboard-japan.com/charts/detail?a=hot100&year=2020&month=12&day=28"
#and seperate dates in to year, month and day
date <- seq(as.Date("2010/01/18"), as.Date("2020/12/28"), by = "week")
year <- format(date, format = "%Y")
month <- format(date, format = "%m")
day <- format(date, format = "%d")

urls <- paste0('http://www.billboard-japan.com/charts/detail?a=hot100&year=', year,
               '&month=', month,
               '&day=', day)

#Collect pages
pages <- lapply(urls, GET)
pages2 <- lapply(pages, function(x) content(x, as = 'text', encoding = 'UTF-8'))

#Parse the page with html parse
t_pages <- lapply(pages2, function(x) htmlParse(x))

#Collect charts info including ranks, titles, artists name and date
info <- list()
total <- list()
node1 <- list()
node2 <- list()
for (i in 1:length(t_pages)){ 
  node1[[i]] <- xpathApply(t_pages[[i]], '//div[@class="rank_detail sp_obj"]')
  node2[[i]] <- xpathApply(t_pages[[i]], '//div[@class="name_detail"]')
  
  if (length(node1[[i]]) > 0 & length(node2[[i]]) > 0){
    
    for (j in 1:10){
      ranks <- xpathApply(node1[[i]][[j]], './/p[@class="rank"]', xmlValue)
      titles <- xpathApply(node2[[i]][[j]], './/p[@class="musuc_title"]', xmlValue)
      artists <- xpathApply(node2[[i]][[j]], './/p[@class="artist_name"]', xmlValue)
      date <- xpathApply(t_pages[[i]], '//p[@class="date"]', xmlValue)
      info[[j]] <- cbind(ranks, titles, artists, date)
    }} 
  
  else {
    ranks <- NA
    titles <- NA
    artists <- NA
    date <- NA
  }
  
  total[[i]] <- info
}

#make a data frame
dt <- lapply(total, function(x) as.data.frame(do.call(rbind, x)))
dt2 <- unlist(dt, recursive = FALSE)
dt3 <- as.data.frame(do.call(cbind, dt2))
dt4 <- as.data.frame(lapply(split(lapply(dt3, as.character), names(dt3)), unlist))
dt4 <- dt4[c("ranks","titles","artists","date")] # reorder the columns

#check NAs, complete
nrow(dt4[!complete.cases(dt4),]) 


#Step 2: Data cleaning
#clean the 'date' column remove space, bracket and useless Japanese characters
#extract date number
dt4$date <- gsub(".*(\\d{4}).*(\\d{2}).*(\\d{2}).*", "\\1-\\2-\\3", dt4$date)

#seperate the date column into 'year', 'month', 'day'
dt4 %>% separate(date, into = c("year", "month", "day"), sep = "-") -> dt5

#output the data frame
Sys.setlocale("LC_ALL","ja_JP.UTF-8") # export Japanese character
write_excel_csv(dt5, "chart.csv")



#Step 3: Simple descriptive analysis
##(1) Who made it to Top 10 the most in the half decades?
by_artist <- dt5 %>% 
  group_by(artists) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20) 

theme_set(theme_gray(base_size = 12, base_family = "HiraKakuProN-W3")) # display Japanese character in the plot

ggplot(by_artist, aes(x = reorder(artists, count), y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "sky blue") +
  coord_flip() +
  labs(title = "Number of Billboard Japan Top 20 Occurrences by Artists",
       x = "Artist", y = "Number of Occurrences",
       caption = "Source: Billboard Japan Hot 100 (Jan 2010~Dec 2020)")
ggsave("Figure1.png", width=10, height=6)

##(2) Most ranked songs in the half decades
by_title <- dt5 %>% 
  group_by(titles) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  top_n(20) 

ggplot(by_title, aes(x = reorder(titles, count), y = count)) +
  geom_bar(stat = "identity") +
  geom_col(fill = "sky blue") +
  coord_flip() +
  labs(title = "Number of Billboard Japan Top 20 Occurrences by Songs",
       x = "Song titles", y = "Number of Occurrences",
       caption = "Source: Billboard Japan Hot 100 (Jan 2010~Dec 2020)")
ggsave("Figure2.png", width=10, height=6)

#make a table with titles, artists and frequency
by_title2 <- dt5 %>% 
  select(titles, artists) %>% 
  distinct(titles, artists)

left_join(by_title, by_title2, by = "titles") %>% 
  select(artists, titles, count) %>% 
  kbl(caption = "Table: Number of Billboard Japan Top 20 Occurrences by Songs", 
      booktabs = T, align = "c") %>% 
  kable_styling(font_size = 11, bootstrap_options = c("striped", "condensed")) %>% 
  kable_minimal() %>% 
  save_kable("table.png")
