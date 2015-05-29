# @knitr sidebar
column(4,
	conditionalPanel(condition="input.tsp!=='about'",
		wellPanel(
			h5("Select data"),
			selectInput("workspace", "Alfresco run:", choices=wsfiles, selected=wsfiles[1], width="100%")
		)
	),
	conditionalPanel(condition="input.tsp!=='fri_boxplot' && input.tsp!=='about'",
		wellPanel(
			h5("Define subjects, groups, and panels"),
			fluidRow(
				column(6, selectInput("interact", "Subject (factor interaction):", choices=c("Replicate", "Location"), selected=c("Replicate", "Location"), multiple=T, width="100%")),
				column(6, selectInput("grp", "Group by:", choices=c("", "Location"), selected="", width="100%"))
			),
			fluidRow(
				column(6, selectInput("facetby", "Facet by:", choices=c("", "Location"), selected="", width="100%")),
				column(6, selectInput("facetcols", "Columns if faceting:", choices=1:4, selected=1, width="100%"))
			)
		)
	),
	conditionalPanel(condition="input.tsp==='rab_ts' || input.tsp==='crab_ts'",
		wellPanel(
			fluidRow(
				column(9, h5("Time series options")),
				column(3,
					conditionalPanel(condition="input.tsp==='rab_ts'",
						downloadButton("dl_RAB_tsplotPDF","Get Plot")
					),
					conditionalPanel(condition="input.tsp==='crab_ts'",
						downloadButton("dl_CRAB_tsplotPDF","Get Plot")
					)
				)
			),
			fluidRow(
				column(6, selectInput("buffersize", "Buffer radius (km):", choices=buffersize, selected=buffersize[6], width="100%"))#,

			),
			fluidRow(
				conditionalPanel(condition="input.tsp==='rab_ts'", sliderInput("yearsrab", "Years", mod.years.range[1], mod.years.range[2], c(max(mod.years.range[1], 1901), mod.years.range[2]), step=1, format="#", width="100%")),
				conditionalPanel(condition="input.tsp==='crab_ts'", sliderInput("yearscrab", "Years", obs.years.range[1], obs.years.range[2], obs.years.range, step=1, format="#", width="100%"))
			)
		)
	),
	conditionalPanel(condition="input.tsp==='frp_buffer'",
		wellPanel(
			fluidRow(
				column(9, h5("FRP options")),
				column(3, downloadButton("dl_FRP_bufferplotPDF","Get Plot"))
			),
			fluidRow(
				column(6, selectInput("minbuffersize", "Min. buffer radius (km):", choices=buffersize[-length(buffersize)], selected=buffersize[6], width="100%"))#,
				
			)
		)
	),
	conditionalPanel(condition="input.tsp==='fri_boxplot'",
		wellPanel(
			fluidRow(
				column(9, h5("Boxplot options")),
				column(3, downloadButton("dl_FRI_boxplotPDF","Get Plot"))
			),
			fluidRow(
				column(6, selectInput("boxplot_X", "X-axis:", choices=c("Source", "Replicate", "Buffer_km", "Location"), selected="Source", width="100%")),
				column(6, selectInput("boxplot_grp", "Group by:", choices=c("", "Source", "Buffer_km", "Location"), selected="", width="100%"))
			),
			fluidRow(
				column(6, selectInput("boxplot_facetby", "Facet by:", choices=c("", "Source", "Buffer_km", "Location"), selected="", width="100%")),
				column(6, selectInput("boxplot_facetcols", "Columns if faceting:", choices=1:4, selected=1, width="100%"))
			),
			fluidRow(
				column(6, selectInput("points_alpha", "Point transparency:", choices=seq(0.1, 1, by=0.1), selected="0.1", width="100%")),
				column(6, checkboxInput("boxplot_points", "Show points", FALSE))
			),
			fluidRow(
				column(6, checkboxInput("boxplot_log", "Log scale", FALSE))#,
			)#,
			#fluidRow(
			#	sliderInput("yearsfri", "Years", mod.years.range[1], mod.years.range[2], c(max(mod.years.range[1], 1901), mod.years.range[2]), step=1, format="#", width="100%")
			#)
		)
	)
)
