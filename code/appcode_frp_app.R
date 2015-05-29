# @knitr app
source("reactives.R",local=T) # Source reactive expressions and other code
source("plotFunctions.R",local=T) # source plotting functions

rv <- reactiveValues()

updateDataFun <- function(file){
	if(!is.null(file)){
		vars <- load(file=file, envir=.GlobalEnv)
		for(v in vars) rv[[v]] <- get(v, .GlobalEnv)
	}
}

updateData <- reactive({ updateDataFun(file=input$workspace) })

observe({ updateData() })

# Primary plot outputs reactive expressions
# Plot RAB ~ time
doPlot_RABbyTime <- function(){
	print(names(rv))
	if(!(is.null(subjects()) | is.null(groups()) | is.null(input$buffersize) | is.null(input$facetcols))){
	plotRABbyTime(data=rv$rab.dat, buffersize=input$buffersize, subject=subjects(), grp=groups(),
		year.range=input$yearsrab, cumulative=F,
		colpal=cbpalette, fontsize=16, leg.pos="top",
		facet.by=facetBy(), facet.cols=input$facetcols)
	} else NULL
}

# Plot CRAB ~ time
doPlot_CRABbyTime <- function(){
	if(!(is.null(subjects()) | is.null(groups()) | is.null(input$buffersize) | is.null(input$facetcols))){
		plotRABbyTime(data=rv$rab.dat, buffersize=input$buffersize, subject=subjects(), grp=groups(),
			year.range=input$yearscrab, cumulative=T,
			colpal=cbpalette, fontsize=16, leg.pos="top",
			facet.by=facetBy(), facet.cols=input$facetcols)
	} else NULL
}

# Plot FRP ~ buffer radius
doPlot_FRPbyBuffer <- function(){
	if(!(is.null(subjects()) | is.null(groups()) | is.null(input$minbuffersize) | is.null(input$facetcols))){
		plotFRPbyBuffer(data=rv$frp.dat, min.buffer=input$minbuffersize, subject=subjects(), grp=groups(),
			colpal=cbpalette, fontsize=16, leg.pos="top", maintitle=main.frp, xlb=xlb.frp, ylb=ylb.frp,
			facet.by=facetBy(), facet.cols=input$facetcols)
	} else NULL
}

# Plot FRI boxplots
doPlot_FRIboxplot <- function(){
	if(!(is.null(input$boxplot_X) | is.null(input$boxplot_points) | is.null(input$points_alpha) | is.null(Boxplot_groups()) | is.null(input$boxplot_log) | is.null(input$boxplot_facetcols))){
		plotFRIboxplot(d=rv$fri.dat, x=input$boxplot_X, y="FRI", grp=Boxplot_groups(),
			colpal=cbpalette, Log=input$boxplot_log, show.points=input$boxplot_points, pts.alpha=input$points_alpha, fontsize=16, leg.pos="top",
			facet.by=Boxplot_facetBy(), facet.cols=input$boxplot_facetcols)
	} else NULL
}

# Primary plot reactive outputs
output$RAB_tsplot <- renderPlot({ doPlot_RABbyTime() }, height=800, width=1000)

output$CRAB_tsplot <- renderPlot({ doPlot_CRABbyTime() }, height=800, width=1000)

output$FRP_bufferplot <- renderPlot({ doPlot_FRPbyBuffer() }, height=800, width=1000)

output$FRI_boxplot <- renderPlot({ doPlot_FRIboxplot() }, height=800, width=1000)

# PDF download buttons for each plot
output$dl_RAB_tsplotPDF <- downloadHandler(
	filename='RABvsTime.pdf',
	content=function(file){	pdf(file=file, width=10, height=8, pointsize=8); doPlot_RABbyTime(); dev.off() }
)

output$dl_CRAB_tsplotPDF <- downloadHandler(
	filename='CRABvsTime.pdf',
	content=function(file){	pdf(file=file, width=10, height=8, pointsize=8); doPlot_CRABbyTime(); dev.off() }
)

output$dl_FRP_bufferplotPDF <- downloadHandler(
	filename='FRPvsBufferRadius.pdf',
	content=function(file){	pdf(file=file, width=10, height=8, pointsize=8); doPlot_FRPbyBuffer(); dev.off() }
)

output$dl_FRI_boxplotPDF <- downloadHandler(
	filename='FRIboxplots.pdf',
	content=function(file){	pdf(file=file, width=10, height=8, pointsize=8); doPlot_FRIboxplot(); dev.off() }
)
