change_data = function(edges,nodes,var){
  id = nodes$id[nodes$label == var]
  node_id = sort(unique(
    c(edges$source[edges$target == id],
      edges$target[edges$source == id], id)))
  nodes_output = nodes[nodes$id %in% node_id,] 
  
  
  # edges_output = edges[edges$source %in% node_id | edges$target %in% node_id,]
  edges_output = edges[edges$source == id | edges$target == id,]
  dfJoin = data.frame(id=nodes_output$id,newid=0:(dim(nodes_output)[1]-1))
  edges_output = edges_output %>% left_join(dfJoin,by=c("source" = "id")) %>%
    left_join(dfJoin,by=c("target" = "id")) %>%
    select(target = newid.x, source = newid.y,weight)
  
  nodes_output$id = 0:(dim(nodes_output)[1]-1)
  
  return(list(nodes_output,edges_output))
}