#where do you want to cut off the abstracts?
cutoff <- 700
#where do you want to decide if an abstract is a duplicate?
criticalval <- .45
#Input the AuYrTi output style in endnote
citations <- read.table("/Users/zacharyclement/Desktop/AuTiYr.txt", sep = "^", quote=NULL, comment='', header=FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c("author", "title", "year"), strip.white = TRUE)
#Input the Annotated Copy style in endnote to go with the references
annotated <- read.table("/Users/zacharyclement/Desktop/Annotated Copy.txt", quote = NULL, comment = '', header=FALSE, fill = TRUE, stringsAsFactors = FALSE, sep = "^", col.names = c("bad","Keep","junk"))
#Input the Abstracts Only style in endnote to 
Abstracts <- read.table("/Users/zacharyclement/Desktop/Abstracts Only.txt", quote = NULL, comment = '', header=FALSE, fill = TRUE, stringsAsFactors = FALSE, col.names = c(1:cutoff), flush = TRUE)
#only keeping rows that come from a unique record
Abstracts <- Abstracts[(Abstracts[ ,1] == "Summary:"), ]
annotated <- annotated[(annotated[ ,1]) == "junk", ]

#Put the annotated bibliography in with the other citationso
citations$annotated <- annotated$Keep

#producing preliminary numbers
n <- length(citations$author)
r <-1
removelist <- 0
for (i in 2:n){
  #Creating a vector that only reads true if the year is not blank, the author is not anonymous, the years match, and the first two letters of the author match
  match <- ((citations$year[1:i-1] == citations$year[i]) & (substring(citations$author[1:i-1], 1, 2) == substring(citations$author[i], 1, 2)) & (citations$year[i] != "") & (citations$author[i] != "Anonymous "))
  #Is the whole vector false?
  if (!any(match, na.rm = TRUE)){
    next
  }
  #If previous records had any true do another test
  else if (any(match, na.rm = TRUE)){
    #clear old values
    duplengthratio <- 0
    ident <- 0
    lengthi <- 0
    compare <- 0
    x <- 1
    #Turning match into a vector with numbers instead of logicals
    compare <- which (match == TRUE)
    #How many abstracts in the author-date identical rows are identical?
    for (c  in compare){
      lastvalid <- max(which(Abstracts[i, ] != ""))
      lastvalidc <- max(which(Abstracts[c, ] != ""))
      identc <- length(intersect(Abstracts[i, 1:lastvalid], Abstracts[c, 1:lastvalidc])) - 1
      identi <- length(intersect(Abstracts[c, 1:lastvalidc], Abstracts[i, 1:lastvalid])) - 1
      #subtracting a very small number so that if the length is 0 it becomes a negative number, and so that it will never be undefined.
      lengthi <- lastvalid - 1.000001
      lengthc <- lastvalidc - 1.000001
      duplengthratio[x] <- identi/lengthi*identc/lengthc 
      x <- x + 1
    }
    if (max(duplengthratio) > criticalval){
      #Add this to a list of ovservations for us to remove later
      removelist[r] <- i
      r <- r + 1
    }
  }

}

#removing all duplicates from the database
export <- citations[-removelist, ]
rejected <- citations[removelist, ]
total <- length(export$author)

#Making a column with the annotated in the format I want
export$AuTiYr <- paste(1:total, ": ", export$author, ", ", export$title, ", ", export$year, sep = "")

#writing the csv for later use
write.csv(export, file = "/Users/zacharyclement/Desktop/export.csv", quote = TRUE, row.names = TRUE, col.names = TRUE)
write.csv(rejected, file = "/Users/zacharyclement/Desktop/Burlingame Team/Alee Screening/rejected.csv", quote = TRUE, row.names = TRUE)

coders <- c("AW", "AP", "BB", "CB", "JN", "MH", "TP", "TP", "SW", "ZC")

narticles <- length(export$author)
breakpoints <- round(c(0, c(1:screeners)*narticles/screeners))
screeners <- length(coders)
codercol <- 0
for (i in 1:screeners){
  codercol[(breakpoints[i]+1):breakpoints[i+1]] <- coders[i]
}


#Writing the CSV which will have the column
write.csv(cbind(codercol, export$AuTiYr), file = "/Users/zacharyclement/Desktop/AuthorsCoders.csv", quote = TRUE, row.names = FALSE)

#Attaching numbers to the annotated abstracts
export$numbered <- paste(1:total, export$annotated, sep = ". ")

#Setting the number of screeners we will have
screeners <- length(coders)
narticles <- length(export$author)
breakpoints <- round(c(0, c(1:screeners)*narticles/screeners))

#writing .txt files with the annotated abstracts divided into the number of screeners
for (i in 1:screeners){
  write.table(export$numbered[(breakpoints[i]+1):breakpoints[i+1]], file = paste("/Users/zacharyclement/Desktop/Annotated", coders[i], ".txt", sep = ""), sep = " ", quote = FALSE, row.names = FALSE, col.names = FALSE)
}




#Generating a distribution for the rejected studies
rejectedstat <- 0
r <- 1
for (i in rejectedtable[ ,1]){
  match <- ((citations$year[1:i-1] == citations$year[i]) & (substring(citations$author[1:i-1], 1, 2) == substring(citations$author[i], 1, 2)) & (citations$year[i] != "") & (citations$author[i] != "Anonymous "))
    #clear old values
    duplengthratio <- 0
    ident <- 0
    lengthi <- 0
    compare <- 0
    x <- 1
    #Turning match into a vector with numbers instead of logicals
    compare <- which (match == TRUE)
    #How many abstracts in the author-date identical rows are identical?
    for (c  in compare){
      lastvalid <- max(which(Abstracts[i, ] != ""))
      lastvalidc <- max(which(Abstracts[c, ] != ""))
      ident <- min(length(intersect(Abstracts[i, 1:lastvalid], Abstracts[c, 1:lastvalidc])) - 1, length(intersect(Abstracts[c, 1:lastvalidc], Abstracts[i, 1:lastvalid])) - 1)
      #subtracting a very small number so that if the length is 0 it becomes a negative number, and so that it will never be undefined.
      lengthi <- lastvalid - 1.000001
      lengthc <- lastvalidc - 1.000001
      duplengthratio[x] <- ident^2/lengthi/lengthc 
      x <- x + 1
    }
    rejectedstat[r] <- max(duplengthratio)
    r <- r + 1
  }
hist(rejectedstat)


#Here is a test of our probability that we will falsely reject a study

#Generating a population with all of the records that contain abstracts
population <- which(Abstracts[ , 2] != "")
#Choosing a random sample that has an abstract
i <- sample(population, 1)
i

#running the code from above, but instead of comparing to studies with the same date/name, we're just doing random ones
compare <- sample(population, 300)
duplengthratio <- 0
ident <- 0
lengthi <- 0
x <- 1
for (c  in compare){
  lastvalid <- max(which(Abstracts[i, ] != ""))
  lastvalidc <- max(which(Abstracts[c, ] != ""))
  identc <- length(intersect(Abstracts[i, 1:lastvalid], Abstracts[c, 1:lastvalidc])) - 1
  identi <- length(intersect(Abstracts[c, 1:lastvalidc], Abstracts[i, 1:lastvalid])) - 1
  #subtracting a very small number so that if the length is 0 it becomes a negative number, and so that it will never be undefined.
  lengthi <- lastvalid - 1.000001
  lengthc <- lastvalidc - 1.000001
  duplengthratio[x] <- identi/lengthi*identc/lengthc 
  x <- x + 1
}
#Here is a boxplot. If one value is really high we randomly selected a study and a duplicate, but that's not likely.
boxplot(duplengthratio)
hist(duplengthratio, xlab = "Proportion of Identical Words to Abstract Length", main = "Histogram of Values for Randomly Selected Studies")
#Here is the maximum value for this random distribution
max(duplengthratio)
#Here is the probability of falsely rejecting a study under various critical values
#The critical value you used
1- pnorm(criticalval, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.6
1-pnorm(.6, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.5
1-pnorm(.5, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.45
1-pnorm(.45, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.4
1-pnorm(.4, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.3
1-pnorm(.3, mean = mean(duplengthratio), sd = sd(duplengthratio))
#.2
1-pnorm(.2, mean = mean(duplengthratio), sd = sd(duplengthratio))






#Everything below here is junk

i <- 41
c <- 42

lastvalid <- max(which(Abstracts[i, ] != ""))
lastvalidc <- max(which(Abstracts[c, ] != ""))
ident <- min(length(intersect(Abstracts[i, 1:lastvalid], Abstracts[c, 1:lastvalidc])) - 1, length(intersect(Abstracts[c, 1:lastvalidc], Abstracts[i, 1:lastvalid])) - 1)
#subtracting a very small number so that if the length is 0 it becomes a negative number, and so that it will never be undefined.
lengthi <- lastvalid - 1.000001
lengthc <- lastvalidc - 1.000001
ident^2/lengthi/lengthc 



#How many rows are there?
n <- length(Abstracts$V1)
#How many columns are there?
w <- length(Abstracts[a,])
#clearing old data
duplengthratio <- matrix(nrow = n, ncol = n)
lengthratio <- 0
dupratio <- 0

for (b in 1:n){
  for (a in 1:n){
    #How many in a row are identical?
    ident <- length(intersect(Abstracts[a,], Abstracts[b,])) 
    #How many in a row are NA?
    Blank <- min(sum(Abstracts[a,] == ""), sum(Abstracts[b,] == ""))
    #How many non-blank values are identical?
    dupratio[a] <- (ident-Blank)/(w-Blank)
    #Are the articles the same length?
    lengthratio[a] <- (w-max(sum(Abstracts[a,] == ""), sum(Abstracts[b,] == "")))/ (w-min(sum(Abstracts[a,] == ""), sum(Abstracts[b,] == "")))
  } 
  duplengthratio[, b] <- lengthratio*dupratio 
}

for (a  in 1:n){
  duplengthvector[a] <- sum(duplengthratio[1:n, a] > .6)
}

a <- 10
boxplot(duplengthratio[a, c(1:(a-1), (a+1):n)])
max(duplengthratio[a, c(1:(a-1), (a+1):n)])


for (i in 1:length(citations$V3)){
  if (citations$v3[i] == ""){
    citations$v3[i] <- citations$V2[i]
  }
}


i <- 43
length(intersect(Abstracts[i, (Abstracts[i, ] != "")], Abstracts[match, ][1:sum(match), ]))  #115
length(union(Abstracts[i, (Abstracts[i, ] != "")], Abstracts[match, ][1:sum(match), ]))    #138
length(setdiff(Abstracts[i, (Abstracts[i, ] != "")], Abstracts[match, ][1:sum(match), ])) #40

