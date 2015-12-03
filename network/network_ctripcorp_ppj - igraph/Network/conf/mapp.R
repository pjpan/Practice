
###
mapp <- function(x, range = c(0,1), from.range=NA) {
  if(any(is.na(from.range))) from.range <- range(x, na.rm=TRUE)
  
  ## check if all values are the same
  if(!diff(from.range)) return(
    matrix(mean(range), ncol=ncol(x), nrow=nrow(x), 
           dimnames = dimnames(x)))
  
  ## map to [0,1]
  x <- (x-from.range[1])
  x <- x/diff(from.range)
  ## handle single values
  if(diff(from.range) == 0) x <- 0 
  
  ## map from [0,1] to [range]
  if (range[1]>range[2]) x <- 1-x
  x <- x*(abs(diff(range))) + min(range)
  
  x[x<min(range) | x>max(range)] <- NA
  
  x
}


write.table(x=network%>%left_join(network,network, by = c("sourceName" = "targetName", "targetName" = "sourceName" ) ,copy =T)
           ,file='./data/merge.txt'
           ,row.names = F
           )

data <- read.table('./data/NewBI.txt'
                   , sep='\t'
                   , header = T
                   , stringsAsFactors = F
                   , encoding = "UTF8" 
                   )


#  生成随机颜色；
namedept <- distinct(data[,c('targetName','dep')])
colormap <- as.data.frame(cbind(dep=unique(data$dep),color=rainbow(length(unique(data$dep)))),stringsAsFactors = F)


#  data的

left_join(data,colormap,c("dep"="dep"))

View(namedept)













