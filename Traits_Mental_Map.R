#########  Creation of an interactive mental map of aquatic functional traits as described in the infified topology proposed by Martini et al. (2019)

# Emile Faure
# Last update 04/02/2020

#### Packages and data ####

# Package call
library(networkD3)
library(igraph)
library(magrittr)

# Data importation
Vertex = read.table("/PATH/TO/Traits_vertex.txt", header = T, sep = "\t")
Edges = read.table("/PATH/TO/Traits_edges.txt", header = T, sep = "\t")

#### Network creation ####

#Graph creation (igraph object)
g <- graph_from_data_frame(Edges, directed = T, vertices = Vertex)
plot(g)

#Switch to D3 object
gD3 <- igraph_to_networkD3(g, group = Vertex$Attribute)

# Add a size column in the nodes attributes based on node type
gD3$nodes[,3] = gD3$nodes[,2]
names(gD3$nodes)[3] = "size"
gD3$nodes[,3] = gsub("Trait", "2", gD3$nodes[,3])
gD3$nodes[,3] = gsub("Ecological function", "30", gD3$nodes[,3])
gD3$nodes[,3] = gsub("2 type", "30", gD3$nodes[,3])

gD3$nodes$size = as.numeric(gD3$nodes$size)

# Choose a color scale
ColourScale <- 'd3.scaleOrdinal()
            .domain(["Ecological function","Trait","Trait type"])
.range(["steelblue","#666666","#FF6633"]);'

# And create the network
net <- forceNetwork(Links = gD3$links, Nodes = gD3$nodes, Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', zoom = T, legend = T, linkDistance = 200, Nodesize = 'size', opacity = 1,
             opacityNoHover = 0.7, fontSize = 12,colourScale = JS(ColourScale), fontFamily = "Futura", charge = -600, linkColour = "#999999")

#### Hyperlinks set up ####

# We want each node to open a tab with a google scholar search corresponding to each trait
# We will create hyperlinks corresponding to searches for each node
NamesHyperlink = read.table("/PATH/TO/Traits_vertex_keywordsforhyperlink_nohierarchy.txt", header = F, sep = "\t") #Pre-written file containing search keywords for each trait
net$x$nodes$names_for_hyperlink = as.character(NamesHyperlink[,1])

net$x$nodes$names_for_hyperlink = gsub(" ", "+", net$x$nodes$names_for_hyperlink)

#We add these keywords to the google scholar search URL used to build Figure 1
net$x$nodes$hyperlink = paste0(
  'https://scholar.google.fr/scholar?as_vis=1&hl=fr&as_sdt=1,5&q=%28%22functional+trait%22+OR+%22trait-based%22%29+AND+%28%22aquatic%22+OR+%22marine%22+OR+%22ocean%22+OR+%22coastal%22+OR+%22deep-sea%22+OR+%22pelagic%22+OR+%22benthic%22+OR+%22freshwater%22+OR+%22lake%22+OR+%22river%22+OR+%22limnology%22%29
+AND+%28%22', net$x$nodes$names_for_hyperlink, '%22%29&btnG='
)
# The hyperlink including chemical compound is not satisfactory due to a parenthesis closing before the %22 automatically added, we manually refine it :
net$x$nodes$hyperlink[24]="https://scholar.google.fr/scholar?as_vis=1&hl=fr&as_sdt=1,5&q=%28%22functional+trait%22+OR+%22trait-based%22%29+AND+%28%22aquatic%22+OR+%22marine%22+OR+%22ocean%22+OR+%22coastal%22+OR+%22deep-sea%22+OR+%22pelagic%22+OR+%22benthic%22+OR+%22freshwater%22+OR+%22lake%22+OR+%22river%22+OR+%22limnology%22%29+AND+%28%22Chemical+compounds%22+OR+%22Signalling+molecules%22+OR+%22Signal+molecules%22%29+&btnG="

# Set the click action to open new tab using hyperlink :
net$x$options$clickAction = 'window.open(d.hyperlink)'

#### Saving an html version of the mental map ####

# Create and save the network :
htmlwidgets::prependContent(net,htmltools::tags$h1("Interactive mental map of aquatic functional traits as described in the unified topology proposed by Martini et al. (2019)")) %>% 
htmlwidgets::prependContent(htmltools::tags$h2("Hover your mouse over a node to highlight its connections. A simple click on a node will open a bibliography search about this trait in aquatic ecology studies. A maintained click on any node will allow you to move it around. Scroll to zoom in or out.")) %>% 
htmlwidgets::prependContent(htmltools::tags$h3("R code and detailed methods for this figure are available at https://github.com/severine13/Biblio-Functional-traits. Please cite Martini et al. (2019) for any public display of this map.")) %>% 
htmlwidgets::onRender(
  'function(el, x) { 
  d3.select("body").style("background-color", "white");
  d3.selectAll(".legend text").style("fill", "#144370");
  d3.select("h1").style("color", "#144370").style("font-family", "futura").style("font-size", "15px");
  d3.select("h2").style("color", "#144370").style("font-family", "futura").style("font-size", "10px");
  d3.select("h3").style("color", "#144370").style("font-family", "futura").style("font-size", "8px");
  }'
)  %>%
  saveNetwork(file = '/PATH/TO/Traits_Mental_map_ScholarLinks.html')


