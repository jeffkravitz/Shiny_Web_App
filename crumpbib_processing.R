references <- data.frame(read.delim("crumpbib.bib", sep = "@"))
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
    if (substr(loop_references$X[j],start=3,stop=7) == "title") {
      citations$title[i] <- substr(loop_references$X[j], start=12, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=3,stop=8) == "author") {
      citations$author[i] <- substr(loop_references$X[j], start=13, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=3,stop=9) == "journal") {
      citations$journal[i] <- substr(loop_references$X[j], start=14, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=3,stop=7) == "pages") {
      citations$pages[i] <- substr(loop_references$X[j], start=12, stop=nchar(loop_references$X[j])-2)
    }
    if (substr(loop_references$X[j],start=3,stop=6) == "year") {
      citations$year[i] <- substr(loop_references$X[j], start=11, stop=nchar(loop_references$X[j])-2)
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
title_word_list <- gsub("[{]", "", title_word_list)
title_word_list <- gsub("[}]", "", title_word_list)
title_word_list <- gsub("[(]", "", title_word_list)
title_word_list <- gsub("[)]", "", title_word_list)
comma_words <- grep(",",title_word_list, fixed = TRUE)
comma_words <- c(comma_words, grep(":",title_word_list, fixed = TRUE))
comma_words <- c(comma_words, grep(".",title_word_list, fixed = TRUE))
comma_words <- c(comma_words, grep("?",title_word_list, fixed = TRUE))
comma_words <- unique(comma_words)
for (i in comma_words) {
  title_word_list[i] <- substr(title_word_list[i], start=1, stop=nchar(title_word_list[i])-1)
}
prepositions <- which(title_word_list %in% c("to","a","of","in","i","for","and","its","the","is","as","an","it","or"))
title_word_list_b <- title_word_list[-prepositions]


# Citations by year
year_of_citations_b <- as.numeric(citations$year)

# Citations by journal
journal_of_citations_b <- citations$journal



# Word cloud
library(wordcloud2)
word_cloud_b <- wordcloud2(data.frame(table(title_word_list_b)), color = "limegreen")

# Citations by author
author_list_b <- author_list

# Citations by journal
journal_list_b <- citations$journal

  