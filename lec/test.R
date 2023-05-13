student<-data.frame(ID=c(11,12,13),Name=c("Devin","Edward","Wenli"),Gender=c("M","M","F"),Birthdate=c("1984-12-29","1983-5-6","1986-8-8"))
sub = subset(student, student$Name %in% c("M"))
sub

