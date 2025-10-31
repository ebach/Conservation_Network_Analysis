#Elizabeth Bach
#Network Analysis visualizations
#The Nature Conservancy
#October 2025

#load packages
library(tidyverse)
library(igraph)
library(tidygraph)
library(plyr)


    #read in data - quantify partnership: acres of stewardship shared, people support, equipment
    #approaching this as an undirected network, indicating reciprical relationships
    #are there any benefits to directional?
    #I have tracked down real numbers to represent these relationships to the best of my ability
    #included some subjective 1-10 scaled columns to capture non-quantitative relationship data
    #use Nachusa_Partner_Data.csv
    partnerships<-read.csv(file.choose(), header = T, na.strings = "NA")
    
    
    #Stewardship
    stewardship<-droplevels(subset(partnerships, partnerships$Group=="Stewardship"))
    
    #create nodes - frequency with which organizations appear on the list
    nodes<-ddply(stewardship, .(Partner), summarise, n=length(Partner))
    
    #create edges - connections to/from partners
    #Transform into co-occurance matrix first
    stewardship.cmx<-crossprod(table(stewardship[1:2]))
    diag(stewardship.cmx)<- 0
    stewardship.co<-as.data.frame(stewardship.cmx)
    
    edges<-stewardship.co %>%
      # add a new column 'from' with row names
      dplyr::mutate(from = rownames(.)) %>%
      # reshape the data from wide to long format using 'gather'
      tidyr::gather(to, n, BigSkyEnergy:USFWS) %>%
      # remove zero frequencies 
      dplyr::filter(n != 0)
    
    #Pros of the tidynetwork approach: 
    #uses frequency data to determine size of node (e.g. size of partner) - this could be an index value of multiple metrics like acres, equipment, buffalo
    #allows "bigger" or "more important" partner nodes to be larger in size - would account for multiple IDNR partnerships/properties/projects
    #list all the projects and partners involved, create frequency from this, how often does Nachusa share space - this may underrepresent deep/strong relationships focused on a single project?
    #edge value added separately, how many "edges" or times there are connections, this is the frequency of collaborations
    
    #generate graph
    #generate table for edges and nodes that can become graph object
    ig <- igraph::graph_from_data_frame(d=edges, vertices=nodes, directed = FALSE)
    
    #add labels
    tg <- tidygraph::as_tbl_graph(ig) %>% 
      tidygraph::activate(nodes) %>% 
      dplyr::mutate(label=name)
    
    #plot
    # set seed (so that the exact same network graph is created every time)
    set.seed(12345)
    
    # create a graph using the 'tg' data frame
    tg %>%
      ggraph::ggraph(layout = "kk") +
      
      # add arcs for edges with various aesthetics
      geom_edge_arc(colour = "black",
                    lineend = "round",
                    strength = .1,
                    aes(edge_width = edges$n,
                        alpha = edges$n)) +
      
      # add points for nodes with size based on log-transformed 'v.size' and color based on 'va$Family'
      geom_node_point(size = log(nodes$n) * 2, 
                      aes(color = nodes$type)) +
      
      # add text labels for nodes with various aesthetics
      geom_node_text(aes(label = name), 
                     repel = TRUE, 
                     point.padding = unit(0.2, "lines"), 
                     size = 2.5,
                     fontface= "bold",
                     colour = "gray10") +
      
      # adjust edge width and alpha scales
      scale_edge_width(range = c(0.1, 2.5)) +
      scale_edge_alpha(range = c(0.1, .3)) +
      
      # set graph background color to white
      theme_graph(background = "white") +
      
      # adjust legend position to the top
      theme(legend.position = element_blank()) +
      
      #labs(caption = "Figure 2: Nachusa Grasslands stewardship partnerships and influence.")+
      
      # remove edge width and alpha guides from the legend
      guides(edge_width = FALSE,
             edge_alpha = FALSE)