references <- data.frame(read.delim("crumpbib_2.bib", sep = "@"))
references$X <- as.character(references$X)
references$X.1 <- as.character(references$X.1)

j <- 1
article_id <- c()
for (i in 1:length(references$X.1)) {
  if (references$X.1[i] != "") {
    article_id[j] <- references$X.1[i]
    j <- j + 1
  }
}
num_citations <- length(article_id)

#article_id <- as.character(unique(levels(references$X.1))[-1])
references$X.1 <- as.character((references$X.1))
for(i in 1:length(article_id)) {
  article_id[i] <- substr(article_id[i], start = 9, stop = nchar(article_id[i])-1)
}

# Generate NA values for fields that may not have relevant information
citations <- data.frame(article_id = article_id,
                        title = rep(NA,num_citations),
                        author = rep(NA,num_citations),
                        journal = rep(NA,num_citations),
                        pages = rep(NA,num_citations),
                        year = rep(NA,num_citations))


loop_references <- references
loop_count <- 0
for (i in 1:num_citations) {
  for (j in 1:100) {
    if (length(loop_references) == 0) {
      break
    }
    if (loop_references$X[j] == "}") {
      loop_count <- loop_count + 1
      loop_references <- loop_references[-c(1:loop_count),]
      loop_count <- 0
      break
    }
    if (substr(loop_references$X[j],start=2,stop=6) == "title") {
      citations$title[i] <- substr(loop_references$X[j], start=11, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=7) == "author") {
      citations$author[i] <- substr(loop_references$X[j], start=12, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=8) == "journal") {
      citations$journal[i] <- substr(loop_references$X[j], start=13, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=13) == "journaltitle") {
      citations$journal[i] <- substr(loop_references$X[j], start=18, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=6) == "pages") {
      citations$pages[i] <- substr(loop_references$X[j], start=11, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=5) == "year") {
      citations$year[i] <- substr(loop_references$X[j], start=10, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=2,stop=5) == "date") {
      citations$year[i] <- substr(loop_references$X[j], start=10, stop=13)
    }
    loop_count <- loop_count + 1
    print(loop_count)
  }
}

author_list = strsplit(citations$author, split = "and")
for (i in 1:length(author_list)) {
  for (j in 1:length(author_list[[i]])) {
    if (length(author_list[[i]]) == 1) {
      break
    }
    if (j == 1) {
      author_list[[i]][j] <- substr(author_list[[i]][j], start=0, stop=nchar(author_list[[i]][j])-1)
    }
    else if (j == length(author_list[[i]])) {
      author_list[[i]][j] <- substr(author_list[[i]][j], start=2, stop=nchar(author_list[[i]][j]))
    }
    else {
      author_list[[i]][j] <- substr(author_list[[i]][j], start=2, stop=nchar(author_list[[i]][j])-1)
    }
  }
}

# Citations by author 
unlist_of_authors <- unlist(author_list)

# Citations by title word
title_word_list <- tolower(unlist(strsplit(citations$title, split = " ")))
comma_words <- grep(",",title_word_list, fixed = TRUE)
comma_words <- c(comma_words, grep(":",title_word_list, fixed = TRUE))
comma_words <- c(comma_words, grep(".",title_word_list, fixed = TRUE))
comma_words <- c(comma_words, grep("?",title_word_list, fixed = TRUE))
comma_words <- unique(comma_words)
for (i in comma_words) {
  title_word_list[i] <- substr(title_word_list[i], start=1, stop=nchar(title_word_list[i])-1)
}
prepositions <- which(title_word_list %in% c("to","a","of","in","i","for","and","its","the","is"))
title_word_list_c <- title_word_list[-prepositions]


# Citations by year
year_of_citations_c <- as.numeric(citations$year)

# Citations by journal
journal_of_citations_c <- citations$journal

# Word cloud
library(wordcloud2)
word_cloud_c <- wordcloud2(data.frame(table(title_word_list_c)), color = "limegreen")

# Citations by author
author_list_c <- author_list


