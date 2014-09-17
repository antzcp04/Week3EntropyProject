#Create a function entropy() that takes a vector ??????? as input and returns a single numeric value that is the
# entropy ????(???????) of the vector
fileName <- "C:/Anthony/School_CUNY/IS 607/Week3/entropy-test-file.csv"
entropy <- function(fileName)  {
  # Read FileName
  csv.data = read.csv(fileName)
  
  # piTotalCount <- length(csv.data)
  piTotalCount <- 1000
  # Get Frequency of Element to be used for Entropy
  v <- as.data.frame(table(csv.data$answer))[2]
  
  
  # Check # of PI
  ed <- 0 
  for (i in 1:length(unique(csv.data$answer)) ) {
    
     ed <- ed +  v$Freq[i] / piTotalCount * log2( v$Freq[i] / piTotalCount )
    
  }
  # Satisfies Log2(n)
  edFinal  <- ed*-1
  print(edFinal)
}

#2 Create a function infogain() that takes two vectors - the target ??????? and the attribute ??????? 
# with which to partition the data - and returns the information gain ???????????????,??????????? for the attribute:



infogain <- function(fileName)  {
  # Read FileName
  csv.data = read.csv(fileName)
  
  # piTotalCount <- length(csv.data)
  piTotalCount <- 1000
  # Get Frequency of Element to be used for Entropy Attribute 1
  vAttr1 <- as.data.frame(table(csv.data$attr1))[2]
  # Check # of PI
  edAttr1 <- 0 
  for (i in 1:length(unique(csv.data$attr1)) ) {
    edAttr1 <- edAttr1 +  vAttr1$Freq[i] / piTotalCount * log2( vAttr1$Freq[i] / piTotalCount )
  }
  # Satisfies Log2(n)
  edAttr1 <- edAttr1*-1 
  ed <- entropy(fileName)
  edAttr1Final <-   ed - edAttr1 
  #print(ed)
  #print(edAttr1 )
  print(edAttr1Final)

  
  # Get Frequency of Element to be used for Entropy Attribute 2
  vAttr2 <- as.data.frame(table(csv.data$attr2))[2]
  # Check # of PI
  edAttr2 <- 0 
  for (i in 1:length(unique(csv.data$attr2)) ) {
    edAttr2 <- edAttr2 +  vAttr2$Freq[i] / piTotalCount * log2( vAttr2$Freq[i] / piTotalCount )
  }
  # Satisfies Log2(n)
  edAttr2 <- edAttr2*-1 
  ed <- entropy(fileName)
  edAttr2Final <-   ed - edAttr2 
  #print(ed)
  #print(edAttr1 )
  print(edAttr2Final)
  
  # Get Frequency of Element to be used for Entropy Attribute 3
  vAttr3 <- as.data.frame(table(csv.data$attr3))[2]
  # Check # of PI
  edAttr3 <- 0 
  for (i in 1:length(unique(csv.data$attr2)) ) {
    edAttr3 <- edAttr3 +  vAttr3$Freq[i] / piTotalCount * log2( vAttr3$Freq[i] / piTotalCount )
  }
  # Satisfies Log2(n)
  edAttr3 <- edAttr3*-1 
  ed <- entropy(fileName)
  edAttr3Final <-   ed - edAttr3 
  #print(ed)
  #print(edAttr1 )
  print(edAttr3Final)
}



#3. Create a function decide() that takes a data frame - the target ??????? and the collection of candidate attributes ????
#??????? with which to partition the data - and the number of the column that is the target and returns a list containin
#g two items: the identity (by column number) of the attribute that maximizes the information gain and a vector of 
#the information gains for each of the candidate attributes.


decide <- function(fileName)  {
  
}

