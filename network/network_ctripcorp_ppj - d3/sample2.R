routes=read.table('.../routes.dat',sep=',')
ports=read.table('.../airports.dat',sep=',')

library(igraph)

# for each flight, get the country of the airport the plane took off from and landed at.
ports=ports[,c('V4','V5')]
names(ports)=c('country','airport')

routes=routes[,c('V3','V5')]
names(routes)=c('from','to')

m=merge(routes,ports,all.x=TRUE,by.x=c('from'),by.y=c('airport'))
names(m)[3]=c('from_c')
m=merge(m,ports,all.x=TRUE,by.x=c('to'),by.y=c('airport'))
names(m)[4]=c('to_c')

m$count=1
# create a unique country to country from/to route ID
m$id=paste(m$from_c,m$to_c,sep=',')

# see which routes are flown most frequently
a=aggregate(m$count,list(m$id),sum)
names(a)=c('id','flights')
a$fr=substr(a$id,1,regexpr(',',a$id)-1)
a$to=substr(a$id,regexpr(',',a$id)+1,100)
a=a[,2:4]

a$perc=(a$flights/sum(a$flights))*100

# create directed network graph
a=a[!(a[,2]==a[,3]),]
mat=as.matrix(a[,2:3])

g=graph.data.frame(mat, directed = T)

edges=get.edgelist(g)
deg=degree(g,directed=TRUE)
vv=V(g)

# use spinglass algo to detect community
set.seed(9)
sgc = spinglass.community(g)
V(g)$membership=sgc$membership
table(V(g)$membership)

V(g)[membership==1]$color = 'pink'
V(g)[membership==2]$color = 'darkblue'
V(g)[membership==3]$color = 'darkred'
V(g)[membership==4]$color = 'purple'
V(g)[membership==5]$color = 'darkgreen'

plot(g,
     main='Airline Routes Connecting Countries',
     vertex.size=5,
     edge.arrow.size=.1,
     edge.arrow.width=.1,
     vertex.label=ifelse(V(g)$name %in% c('Liberia','United States'),V(g)$name,''),
     vertex.label.color='black')
legend('bottomright',fill=c('darkgreen','darkblue', 'darkred', 'pink', 'purple'),
       c('Africa', 'Europe', 'Asia/Middle East', 'Kiribati, Marshall Islands, Nauru', 'Americas'),
       bty='n')

# plot degree distribution
dplot=degree.distribution(g,cumulative = TRUE)

plot(dplot,type='l',xlab='Degree',ylab='Frequency',main='Degree Distribution of Airline Network',lty=1)
lines((1:length(dplot))^(-.7),type='l',lty=2)
legend('topright',lty=c(1,2),c('Degree Distribution','Power Law with x^(-.7)'),bty='n')

# explore membership...regional patterns exist
cc=cbind(V(g)$name,V(g)$membership)
tt=cc[cc[,2]==5,]

# explort connection from Liberia to United States
m=mat[mat[,1]=='Liberia',]

t=mat[mat[,1] %in% m[,2],]
tt=t[t[,2]=='United States',]

# assess probabilities

lib=a[a$fr=='Liberia',]
lib$prob=lib$flights/sum(lib$flights)
# most probable route from liberia is Ghana

vec=c(tt[,1],'Liberia')
names(vec)=NULL

g2=graph.data.frame(mat[(mat[,1] %in% vec & mat[,2] == 'United States') | (mat[,1]=='Liberia'),], directed = T)
V(g2)$color=c('darkblue','darkgreen','darkgreen','darkgreen','darkgreen','purple','darkgreen','darkgreen')

plot(g2,
     main='Airline Connections from Liberia to the United States',
     vertex.size=5,
     edge.arrow.size=1,
     edge.arrow.width=.5,
     vertex.label.color='black')
legend('bottomright',fill=c('darkgreen','darkblue','purple'),
       c('Africa', 'Europe', 'Americas'),
       bty='n')

aa=a[a$fr %in% tt[,1],]
sum=aggregate(aa$flights,list(aa$fr),sum)

bb=a[a$fr %in% tt[,1] & a$to=='United States',]

fin=data.frame(bb$fr,sum$x,bb$flights,bb$flights/sum$x)

s=shortest.paths(g)
mean(s)