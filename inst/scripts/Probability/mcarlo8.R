## Matching card problem (from the book 
# Introduction to Probability, Blitzstein and Hwang, page 31  )
## "Consider a well-shuffled deck
#of n cards, labeled 1 through n. You flip over the cards one by one, saying the
#numbers 1 through n as you do so. You win the game if, at some point, the number
#you say aloud is the same as the number on the card being flipped over (for example,
# if the 7th card in the deck has the label 7). What is the probability of winning?"


R<-10^5 ## number of Monte Carlo trials
n <- 20 ## number of cards
r <- replicate(R,sum(sample(n)==(1:n)))
phat=sum(r>=1)/R ## frequency of matching event
cat("phat=",phat, "; 1-1/e=", 1-1/exp(1),"\n")