
#Here is an example of adding an image to each node group in the SVG. There would still be many issues to work out, like appropriate sizing and positioning of the images, but this gives the basis for adding them...

library(networkD3)
library(htmlwidgets)

nodes <- data.frame(name = c("a", "b", "c", "d"))
links <- data.frame(source = c(0, 0, 1, 1),
                    target = c(2, 3, 2, 3),
                    value = c(1, 3, 2, 4)
)

sn <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name")
sn


js <- 
  '
    function(el) { 
      d3.select(el)
        .selectAll(".node")
        .append("image")
        .attr("a.png")
    }
  '

htmlwidgets::onRender(sn, js)




