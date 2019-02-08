
library(tidyverse)
library(RJSONIO)
k <- l[[1]] %>%
    read_html() %>% html_nodes("script")

k2 <- sapply(k, as.character)
n2 <- RJSONIO::fromJSON(str_extract(str_subset(k2,"@context"),"\\{.*\\}"))

##Retrieve JSON file
retrieve_json_with_review <- function(my_link){
    print(my_link)
    Sys.sleep(1)
    k <- my_link %>%
        read_html() %>% html_nodes("script")
    
    k2 <- sapply(k, as.character)
    n2 <- RJSONIO::fromJSON(str_extract(str_subset(k2,"@context"),"\\{.*\\}"))
    frame_to_return <- tibble(
        name = n2$name,
        date = n2$datePublished,
        author = n2$author,
        review = n2$reviewBody,
        rating = unname(n2$reviewRating['ratingValue'])
    )
    return(frame_to_return)
}


test_1 <- lapply(l, function(i)retrieve_json_with_review(i))
test_2 <- bind_rows(test_1)

test_2 %>% 
    mutate_all(rtweet::plain_tweets) %>%
    unique()

##regex: gsub(pattern = "\\\\n"," ",test_2$review[[1]])