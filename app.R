library("visNetwork")
library("shiny")

collections <- read.csv("./all.csv")
timeUTC <- readLines("./timeUTC.txt")

# Get edges.
edges_all <- subset(collections, collections$dependency != "")
edges_all <- edges_all[, c(1,2)]
colnames(edges_all) <- c("from", "to")

# Alphabetical list of ports for the dropdown.
ports <- sort(unique(collections$port))

# Look-up table: for each port name, which collection it comes from, homepage
# and description.
port_coll <- unique(collections[, c(2,3)])
port_coll <- setNames(port_coll$collection, port_coll$port)

port_url <- unique(collections[, c(2,4)])
port_url <- setNames(port_url$url, port_url$port)

port_desc <- unique(collections[, c(2,5)])
port_desc <- setNames(port_desc$description, port_desc$port)

# One colour per collection. Change these to whatever you like.
cols <- c("core"         = "#eeeeec",
          "opt"          = "#8ae234",
          "xorg"         = "#729fcf",
          "contrib"      = "#fce94f",
          "compat-32"    = "#ad7fa8",
          "kde-plasma-6" = "#e9b96e")

# The page layout: a dropdown to choose a port, and the network below it.
ui <- fluidPage(
  tags$style(".form-group { margin-bottom: 0; }
             body { font-family: Arial, sans-serif; }"),
  
  titlePanel(h2("GraphDeps: a graphical overview of CRUX ports dependencies", style = "font-size: 18px;"), windowTitle = "CRUX GraphDeps"),
  
  # Grey panel on top: controls go here.
  wellPanel(style = "width: 1366px; padding: 8px 15px; margin-top: 8px; margin-bottom: 8px; display: flex; align-items: center;",
            div(selectizeInput(inputId = "port", label = NULL,
                               choices = NULL, width = "300px",
                               options = list(placeholder = "Search ports"))),
            div(style = "margin-left: 20px;",
                radioButtons(inputId = "mode", label = NULL, inline = TRUE,
                             choices = c("Dependencies (recursive)" = "deps",
                                         "Dependents (direct)" = "rdeps"),
                             selected = "deps")),
            a(href = "https://crux.nu/", target = "_blank", style = "margin-left: auto;",
              img(src = "crux-logo.png", height = "69px"))
  ),
  div(style = "width: 1366px; min-height: 20px; margin-bottom: 8px; text-align: center;",
      uiOutput(outputId = "info")),
  
  # network frame, with the back button in its corner, up rihght
  div(style = "border: 1px solid #EAEAEA; width: 1366px; border-radius: 5px; overflow: hidden; position: relative;",
      div(style = "position: absolute; top: 8px; right: 8px; z-index: 10;",
          actionButton(inputId = "back", label = "Previous node", icon = icon("arrow-left"), disabled = TRUE)),
      visNetworkOutput(outputId = "net", width = "1366px", height = "768px")),
  
  p(style = "width: 1366px;",
    span(style = "float: right;",
         icon("clock"), paste0(" Last updated: ", timeUTC, " · "),
         a(href = "https://github.com/slackalaxy/crux-graphdeps", target = "_blank", icon("github"), "GitHub")),
    strong("Legend:", style = "margin-right: 8px;"),
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #ef2929;"), "selected",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #eeeeec; margin-left: 15px;"), "core",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #8ae234; margin-left: 15px;"), "opt",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #729fcf; margin-left: 15px;"), "xorg",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #fce94f; margin-left: 15px;"), "contrib",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #ad7fa8; margin-left: 15px;"), "compat-32",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #e9b96e; margin-left: 15px;"), "kde-plasma-6",
    span(style = "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 1px solid #888a85; background: #2e3436; margin-left: 15px;"), "N/A")
)

server <- function(input, output, session) {
  
  # maxOptions: how many names the dropdown can list at once (default 1000).
  # Set it to the number of ports, so scrolling reaches the end of the list.
  updateSelectizeInput(session, "port", choices = ports, selected = "", server = TRUE,
                       options = list(maxOptions = length(ports),
                                      placeholder = "Search ports"))
  
  # Info: name (collection), description/link.
  output$info <- renderUI({
    req(input$port)
    p(style = "margin: 0;",
      strong(input$port), paste0(" (", port_coll[input$port], "): "),
      a(href = unname(port_url[input$port]), target = "_blank",
        unname(port_desc[input$port]))
      )
  })
  
  # Ports visited so far, oldest first.
  visited <- reactiveVal(character(0))
  
  # Record each newly selected port, unless it's already the last one in the
  # list (which is what happens when the back button itself selects it).
  observeEvent(input$port, {
    req(input$port)
    v <- visited()
    if (length(v) == 0 || v[length(v)] != input$port) {
      visited(c(v, input$port))
    }
  })
  
  # Back: drop the current port from the list and select the one before it.
  observeEvent(input$back, {
    v <- visited()
    if (length(v) > 1) {
      v <- v[-length(v)]
      visited(v)
      updateSelectizeInput(session, "port", choices = ports, server = TRUE,
                           selected = v[length(v)],
                           options = list(maxOptions = length(ports),
                                          placeholder = "Search ports"))
    }
  })
  
  # Double-clicked node: make it the selected port in the dropdown.
  # Everything else (description, network) follows, as if picked by hand.
  observeEvent(input$dblclick_node, {
    # Skip names that aren't ports (the dark N/A nodes, e.g. wine).
    if (is.element(input$dblclick_node, ports)) {
      updateSelectizeInput(session, "port", choices = ports, server = TRUE,
                           selected = input$dblclick_node,
                           options = list(maxOptions = length(ports),
                                          placeholder = "Search ports"))
    }
  })
  
  # Dim the back button when there's nowhere to go back to.
  observeEvent(visited(), {
    updateActionButton(session, "back", disabled = length(visited()) <= 1)
  })
  
  # outputId = "net"
  output$net <- renderVisNetwork({
    
    # Until a port is chosen, show this message in the network area instead.
    validate(need(input$port, " Network will appear here. Double click on a node to switch to it."))
    
    keep <- input$port
    if (input$mode == "deps") {
      
      # Start from the chosen port and keep walking back along the arrows:
      # first its dependencies, then their dependencies, and so on,
      # until no new names turn up. Run until we reach a break
      repeat {
        # For each arrow: is the port it points to one of the ports in "keep"?
        # "keep" starts as just the selected port.
        
        hits <- is.element(edges_all$to, keep)
        #hits <- is.element(edges_all$from, keep)
        
        # Keep only those arrows.
        rows <- edges_all[hits, ]
        
        # Their "from" side: the dependencies of everything in keep.
        found <- rows$from
        #found <- rows$to
        
        # Drop the names we already have; what's left is new this round.
        new <- setdiff(found, keep)
        
        # stop if new is empty
        if (length(new) == 0) break
        
        # keep grows each time round the loop
        keep <- c(keep, new)
        #break
        }
      
      } else {
        # Dependents: one step forward only, the ports that list this one directly.
        hits <- is.element(edges_all$from, keep)
        rows <- edges_all[hits, ]
        found <- rows$to
        new <- setdiff(found, keep)
        keep <- c(keep, new)
        }
    
    # All arrows between the names collected above.
    #e <- edges_all[is.element(edges_all$to, keep),]
    e <- edges_all[is.element(edges_all$from, keep) & is.element(edges_all$to, keep), ]
    ids <- keep
    n <- data.frame(id = ids, label = ids)
    
    # Colour each node by its collection.
    n$color <- unname(cols[port_coll[n$id]])
    
    # A dependency that isn't a port in any collection (e.g. a typo in a
    # Pkgfile, or a port that was removed) has no collection: make it dark.
    n$color[is.na(n$color)] <- "#2e3436"
    
    # The chosen port overrides its collection colour, so it stands out.
    n$color[n$id == input$port] <- "#ef2929"
    
    # In the dependents view, colour each arrow like the port it points to,
    # instead of the default (the colour of the port it starts from).
    if (input$mode == "rdeps") {
      node_col <- setNames(n$color, n$id)
      e$color <- unname(node_col[e$to])
      
      }
    
    visNetwork(n, e) |>
      visNodes(font = list(size = 20, face = "Arial"), shadow = FALSE) |>
      visEdges(arrows = "to") |>
      visPhysics(solver = "forceAtlas2Based") |>
      
      # When a node is double-clicked, send its name to R as input$dblclick_node.
      # params.nodes is the list of nodes under the mouse (empty if you
      # double-click on empty space, so we check that first).
      visEvents(doubleClick = "function(params) {
        if (params.nodes.length > 0) {
          Shiny.setInputValue('dblclick_node', params.nodes[0], {priority: 'event'});
        }
      }")
  })
}

shinyApp(ui, server)
