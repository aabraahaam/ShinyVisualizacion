i <- 'hola1'
df <- data.frame(x=rnorm(100),y=rpois(100,1),z=runif(100))
df <- data.frame()
g <- ggplot(df,aes(x=hola))+geom_histogram(bins=10)
h <- ggplot(df,aes(x=x))+geom_histogram(bins=10)
j <- ggplot(df,aes(x=y))+geom_histogram(bins=10)
ptlist <- list(g,h,j)
wtlist <- c(input$wt1,input$wt2,input$wt3)
# remove the null plots from ptlist and wtlist
to_delete <- !sapply(ptlist,is.null)
ptlist <- ptlist[to_delete] 
wtlist <- wtlist[to_delete]
if (length(ptlist)==0) return(NULL)

grid.arrange(grobs=plist)

df <-0

hombres <- c('x','y','z')
plist <-list()
n<-1
for (i in hombres){
  print(n)
  g <- ggplot(df,aes_string(i))+geom_histogram(bins=10)
  #df <- data.frame(df,i=rnorm(100))
  plist[[n]] <- g
  n <- n+1
}
grid.arrange(grobs=plist)


df[,1]<-NULL
colnames(df)<-hombres

df <- NULL
df %>% mutate(i = rnorm(100))
g
