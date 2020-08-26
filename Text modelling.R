
library(readxl)
library(tm)
library(stringr)
library(textstem)
library(textmineR)


read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
} 

mysheets <- read_excel_allsheets("C:/Users/sharm/Documents/Bang the Table/Austin/E-bikes and E-scooters new file.xlsx")

survey_tool_2 <- mysheets[['SurveyTool 2']]

exp <- survey_tool_2$`What would improve your trail experience?`


exp <- str_replace(exp, '/', ' ')
exp <- str_replace(exp, '-', ' ')
exp <- str_replace(exp, '\'s', '')


#corpus <- iconv(exp, to = "utf-8")

corpus <- Corpus(VectorSource(exp))
#inspect(corpus[1:5])

corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleandata <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleandata[1:5])

cleandata <- tm_map(cleandata, stripWhitespace)
inspect(cleandata[1:5])

dataframe <- data.frame(text=sapply(cleandata, identity), 
                        stringsAsFactors=F)

#Creating Document Term Matrix
dtm <- CreateDtm(dataframe$text, 
                 ngram_window = c(1, 2))

#Explore basic frequency
tf <- TermDocFreq(dtm = dtm)
tf
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm

k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines


#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")



model <- model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
top20_wide <- as.data.frame(model$top_terms)

model$theta
