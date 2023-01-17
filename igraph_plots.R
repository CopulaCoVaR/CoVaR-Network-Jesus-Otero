layout           =list(circle="circle", star="star")[2]
layout           =layout[[1]]
Title            ='Example plot'
edge.width.scale =15
node.size        =10
node.color       ='grey'
arrow.size       =2
node.label.size  =1

table_gephi               = matrix(data=c(0,0.5,0.7,0,0,0,
                                          0,0,0,0,0.3,0,
                                          0,0,0.7,0,0.3,
                                          0.3,0.5,0,0,0,0,
                                          0.5,0,0,0,0,0.3,
                                          0,0,0,0,0,0,0), nrow=6, ncol=6, byrow=TRUE)

melt1                      = melt(table_gephi)
melt                       = melt1 %>% filter( value != 0 ) #Solo para valores dif. de cero
colnames(melt)[3]          = "weight"
### calculate each percentile of the net pairwise connectedness values
### and choose only the top 10%
new.net  = melt[melt[,1] != melt[,2],] #Se quitan conexiones consigo mismo
### create igraph graph
network  = graph.data.frame(new.net,direct=T)
E(network)$weight = as.numeric(new.net$weight) #Ponderacion de los Edges 
### set graph nodes
#V(network)
### set edge colors
#Deficion de los cuantiles con base en lo parametros de entrada
#Definicion de los colores de los Edges segun los tres cuantiles indicados 
E(network)$color = ifelse(E(network)$weight== 0.5,"red",
                          ifelse(E(network)$weight == 0.3,"yellow",
                                 ifelse(E(network)$weight == 0.7,"black", 'black')))

### set node size
V(network)$size = node.size*degree(network) 
V(network)$color = rep(node.color, 6)
# Grafica de conectivad
if (is.null(layout) == TRUE) { #Estilo por defecto (teleranha)
  x11()
  plot(network, edge.width = ((E(network)$weight)*edge.width.scale),
       main = Title, vertex.label.cex = node.label.size, edge.arrow.size = arrow.size)
  
}else if (layout == 'circle'){ #Estilo poliedro
  x11()
  plot(network,layout=layout.circle(network),
       edge.width = ((E(network)$weight)*edge.width.scale),
       main = Title,vertex.label.cex = node.label.size,edge.arrow.size = arrow.size)
}


x11()
plot(network,layout=layout.star(network),
     edge.width = ((E(network)$weight)*edge.width.scale),
     main = Title,vertex.label.cex = node.label.size,edge.arrow.size = arrow.size)
