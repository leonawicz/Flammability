# @knitr ui
headerPanel_2(
	HTML(
		'
		<div id="stats_header">
		Alfresco FRP
		<a href="http://snap.uaf.edu" target="_blank">
		<img id="stats_logo" align="right" alt="SNAP Logo" src="./img/SNAP_acronym_100px.png" />
		</a>
		</div>'
	), h3, "Alfresco Fire Return Period"
)

tabPanelAbout <- source("about.R",local=T)$value
headerPanel_2 <- function(title, h, windowTitle=title) {    
  tagList(
    tags$head(tags$title(windowTitle)),
      h(title)
    )
}

shinyUI(fluidPage(
	source("header.R",local=T)$value,
	fluidRow(
		source("sidebar.R",local=T)$value,
		column(8,
			tabsetPanel(
				tabPanel("RAB ~ Time",
					plotOutput("RAB_tsplot",height="auto"),
					br(), value="rab_ts"),
				tabPanel("CRAB ~ Time",
					plotOutput("CRAB_tsplot",height="auto"),
					br(), value="crab_ts"),
				tabPanel("FRP ~ Buffer Radius",
					plotOutput("FRP_bufferplot",height="auto"),
					br(), value="frp_buffer"),
				tabPanel("FRI Boxplots",
					plotOutput("FRI_boxplot",height="auto"),
					br(), value="fri_boxplot"),
				tabPanelAbout(),
				id="tsp",
				type="pill",
				selected="rab_ts"
			)
		)

	)
))
