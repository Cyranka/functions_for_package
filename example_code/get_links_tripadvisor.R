rm(list =ls())
library(rvest)

url <- paste0("https://www.tripadvisor.com/Hotel_Review-g60763-d611947-Reviews-or",seq(5,50,by = 5),"-New_York_Hilton_Midtown-New_York_City_New_York.html")

retrieve_hrefs <- function(my_url){
    print(my_url)
    #Sys.sleep(2)
    return(reviews <- my_url %>%
        read_html() %>%html_nodes("a") %>%
        html_attr("href"))
    
}

p <- lapply(url, function(i)retrieve_hrefs(i))
l <- unlist(p) %>% stringr::str_subset("/ShowUserReviews")
l <- paste0("https://www.tripadvisor.com/", l) %>% unique()

##Grabs full text

test_1 <- "https://www.tripadvisor.com/ShowUserReviews-g60763-d611947-r650559310-New_York_Hilton_Midtown-New_York_City_New_York.html"
j <- test_1 %>%
    read_html() %>% html_nodes(".fullText") %>%
    html_text()
    

m <- test_1 %>%
    read_html() %>% html_nodes(".bubble_20") 