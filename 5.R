data("mtcars")
sapply(mtcars, class)
nrow(mtcars)
ncol(mtcars)
cntm = 0
cnta = 0
for(i in 0:nrow(mtcars))
  ifelse(mtcars[i,9] == 1, cntm <- cntm + 1, cnta <- cnta +1)
if(cnta>cntm)
  print("Automatic")
if(cntm>cnta) 
  print("manual")
plot(mtcars$hp, mtcars$wt)

newmtc = data.frame(as.integer(mtcars$cyl), as.integer(mtcars$vs), as.integer(mtcars$am))
newmtc
count =0
for(i in 0:nrow(mtcars))
  ifelse(mtcars[i,2]<5, count <- count+1, "")
    
print(count)