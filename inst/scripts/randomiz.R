#>YAL068C	6319249 unspliced OF sequence, from start to stop, size 363
seq<-"ATGGTCAAATTAACTTCAATCGCCGCTGGTGTCGCTGCCATCGCTGCTACTGCTTCTGCAACCACCACTCTAGCTCAATCTGACGAAAGAGTCAACTTGGTGGAATTGGGTGTCTACGTCTCTGATATCAGAGCTCACTTAGCCCAATACTACATGTTCCAAGCCGCCCACCCAACTGAAACCTACCCAGTCGAAGTTGCTGAAGCCGTTTTCAACTACGGTGACTTCACCACCATGTTGACCGGTATTGCTCCAGACCAAGTGACCAGAATGATCACCGGTGTTCCAAGTGGTACTCCAGCAGATTAAAGCCAGCCATCTCCAGTGCTCTAAGTCCAAGGACGGTATCTACACTATCGCAAACTAAG"




count.motif<-function(seq,mot){
 N<-nchar(seq)
  M<-nchar(mot)
  cnt<-0
  for (i in 1:N){
    cnt<-cnt+length(grep(mot,substr(seq,i,i+M-1)))
  }
  cnt
}

R<-200
l<-unlist(strsplit(seq,split=""))
mot<-"AAG"
count<-count.motif(seq,mot)
print(paste ("Motif ", mot, " encountered ", count," times"))
count.perm<-array(0,c(R,1))
for (r in 1:R){
  permut.l<-sample(l)
  count.perm[r]<-count.motif(paste(permut.l,collapse=""),mot)
}

p<- sum(count.perm>=count)/R

if (p<0.05){
print(paste ("Motif ", mot, " significantly represented"))} else
{
print(paste("Motif ", mot, " NOT significantly represented"))
}


