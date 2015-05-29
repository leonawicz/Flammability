# @knitr reactives
facetBy <- reactive({
	x <- NULL
	if(!is.null(input$facetby)){
		x <- input$facetby
		if(x=="") x <- NULL
	}
	x
})

subjects <- reactive({
	if(!length(input$interact)) x <- NULL else x <- sprintf("interaction(%s)", paste0(input$interact, collapse = ", "))
	x
})

groups <- reactive({
	if(!length(input$grp)) x <- NULL else x <- input$grp
	x
})

Boxplot_facetBy <- reactive({
	x <- NULL
	if(!is.null(input$boxplot_facetby)){
		x <- input$boxplot_facetby
		if(x=="") x <- NULL
	}
	x
})

Boxplot_subjects <- reactive({
	if(!length(input$boxplot_interact)) x <- NULL else x <- sprintf("interaction(%s)", paste0(input$boxplot_interact, collapse = ", "))
	x
})

Boxplot_groups <- reactive({
	if(!length(input$boxplot_grp)) x <- NULL else x <- input$boxplot_grp
	x
})
