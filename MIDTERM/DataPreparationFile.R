# YOU DO NOT NEED TO RUN THIS CODE!!!

# THIS CODE PREPARES THE DATASETS
# WordsFreqFinal and WordsFinal

library(readtext)
library(SnowballC)

setwd("/Users/vrockova/Desktop/Big Data/Midterm/headlines")

data<-readtext("RedditNews.csv",skip=1)

date<-data[2]

dj<-read.csv("DJIA.csv")

ndays<-nrow(dj)

# Let's look at news on one day

dj[1,1]

day<-"2016-07-01"
	
data_subset<-subset(data,
			as.Date(date[,1],"%m/%d/%y")==day)

data_subset[,3]

# Let's chop the news into words

words<-NULL

for(k in (1:ndays)){
	
	print(k)

	day<-as.Date(dj[k,1])
	
	data_subset<-subset(data,
			as.Date(date[,1],"%m/%d/%y")==day)

	dim(data_subset)

	for (i in (1:nrow(data_subset))){
	
		vec<-word_process(data_subset[i,3])
	
		words<-c(words,vec)
			
								}

}

words_copy<-words

words<-wordStem(words) # we use only word stems


# Keep only words that occur at least 10 times

t<-table(words)

words_names<-names(t[t>10])

write.table(words_names,"WordsFinal.csv",col.names=FALSE,row.names=FALSE)

words<-words_names

# **** Relate words to texts

# Create a matrix of document-words-counts

doc_word<-NULL

for(k in (1:ndays)){
	
	print(k)

	day<-as.Date(dj[k,1])
	
	data_subset<-subset(data,
			as.Date(date[,1],"%m/%d/%y")==day)

	dim(data_subset)
	
	vec<-NULL

	for (i in (1:nrow(data_subset))){
	
		text<-data_subset[i,3]
		
		vec<-c(vec,word_process(text))
			
								}

	indices<-(1:length(words))[words%in%vec]
	

	for(j in indices){
			
			triplet<-c(k,j,sum(words[j]==vec))
	
			doc_word<-rbind(doc_word,triplet)
			
		}
}

# write a table with doc IDs and word IDs and counts


write.table(doc_word,"WordFreqFinal.csv",col.names=FALSE,row.names=FALSE)



# Processing function, removing special characters and keeping only words with at least three letters

word_process<-function(vec){
	
	vec<-tolower(unlist(strsplit(vec," ")))
	vec<-gsub("b'","",vec)
	vec<-gsub("b\"\"","",vec)
	vec<-gsub("0","",vec)
	vec<-gsub("1","",vec)
	vec<-gsub("2","",vec)
	vec<-gsub("3","",vec)
	vec<-gsub("4","",vec)
	vec<-gsub("5","",vec)
	vec<-gsub("6","",vec)
	vec<-gsub("7","",vec)
	vec<-gsub("8","",vec)
	vec<-gsub("9","",vec)
	vec<-gsub("!","",vec)
	vec<-gsub(",","",vec)
	vec<-gsub("?","",vec)
	vec<-gsub("[?]","",vec)
	vec<-gsub("[+]","",vec)
	vec<-gsub("[=]","",vec)
	vec<-gsub("'","",vec)
	vec<-gsub(";","",vec)
	vec<-gsub(":","",vec)
	vec<-gsub("/","",vec)
	vec<-gsub("|","",vec)
	vec<-gsub("\"","",vec)
	vec<-gsub("\r","",vec)
	vec<-gsub("\t","",vec)
	vec<-gsub("\n","",vec)
	vec<-gsub("\\\\","\\",vec)
	vec<-gsub("<br","",vec)
	vec<-gsub("[-]","",vec)
	vec<-gsub("$","",vec)
	vec<-gsub("<","",vec)
	vec<-gsub("[*]","",vec)
	vec<-gsub("[$]","",vec)
	vec<-gsub("[@]","",vec)
	vec<-gsub("[~]","",vec)
	vec<-gsub("[{]","",vec)
	vec<-gsub("[}]","",vec)
	vec<-gsub("[%]","",vec)
	vec<-gsub("]","",vec)
	vec<-gsub("_","",vec)
	vec<-gsub("[#]","",vec)
	vec<-gsub("`","",vec)
	vec<-gsub("[&]","",vec)
	vec<-gsub("[[]","",vec)
	vec<-gsub("[(]","",vec)
	vec<-gsub(")","",vec)
	vec<-gsub(">","",vec)
	vec<-gsub("[.]","",vec)
	vec<-vec[grepl("^[[:digit:]]",vec)==FALSE]
	vec<-vec[nchar(vec)>2]
	vec
}


