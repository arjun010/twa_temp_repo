score.sentiment = function(sentences)
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  i<-1
  temp = integer()
  temp[i]=0
  temp1 = integer()
  temp1[i]=0
  # create simple array of scores with laply
  scores = laply(sentences,
                 list[a,b]<-function(sentence) #, pos.words, neg.words
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   #pos.matches = match(words, pos.words)
                   #neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   #pos.matches = !is.na(pos.matches)
                   #neg.matches = !is.na(neg.matches)
                   
                   # final score
                   #score = sum(pos.matches) - sum(neg.matches)
                   #return(score)
                   #---------------------------------------------
                   #new code
                   data<-read.csv("/home/arjun/Desktop/twa/sentiment analysis/anew-1999/all.csv")
                   for(word in words){
                     ans = match(word,data$Description,nomatch=-1)
                     if(ans==-1){
                       #print(-1)
                     }else{
                       #return(data[ans])
                       temp[i]<-data$Valence.Mean[ans]
                       temp1[i]<-data$Dominance.Mean[ans] 
                       i<-i+1
                     }
                   }
                   #print(head(data))
                   pos=neg=neutral=0
                   if(mean(temp)<5){
                     score = -1
                   }else if(mean(temp)==5){
                     score =0
                   }else{
                     score=1
                   }
                   a=mean(temp)
                   b=mean(temp1)
                   #----------------------------------------------
                 })
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}