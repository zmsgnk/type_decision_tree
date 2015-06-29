library(dplyr)
library(shiny)
library(shinydashboard)
library(rpart)
library(rpart.plot)
library(partykit)
require(markdown)


#################################################################
#
# UI
#
#################################################################
header <- dashboardHeader(title = "好みのタイプ診断")

sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("App", tabName = "app"),
	  menuItem("Source code for app", icon = icon("github"),
   	   href = "https://github.com/zmsgnk/type_decision_tree"
   	),
    tags$li(
      a(href = "http://twitter.com/intent/tweet?text=女の子の好みを診断するアプリ&url=https://zmsgnk.shinyapps.io/type_decision_tree",
        target = "_blank",
        icon("twitter"),
        onClick = "window.open(encodeURI(decodeURI(this.href)),
          'tweetwindow',
          'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
          ); return false;",
        span('Tweet'),
        tags$small(
          class = paste0("badge pull-right bg-", 'light-blue'),
          'Share'
        )
      )
    ),
    tags$li(
      a( href = "http://www.facebook.com/sharer.php?u=https://zmsgnk.shinyapps.io/type_decision_tree&t=女の子の好みを診断するアプリ",
        target = "_blank",
        icon("facebook"),
        span('Facebook'),
        tags$small(
          class = paste0("badge pull-right bg-", 'light-blue'),
          'Share'
        )
      )
    )
	)
)

app_tab <- tabItem(tabName = "app",
	fluidRow(
		column(width = 1),
		column(width = 10, 
			conditionalPanel("input.do > 0",
				box(width = NULL, title = "結果", solidHeader = TRUE, status = "warning", collapsible = TRUE,
					plotOutput("result")
				)
			)
		), 
		column(width = 1)
	),

	fluidRow(
		column(width = 1),
		column(width = 4,
			box(width = NULL, solidHeader = TRUE, status = "warning",
		    tags$div(class = "my-box-body", 
		      tags$button(id = "like", type = "button", class = "btn btn-success btn-lg btn-half-block action-button", list(label = shiny::tagList(tags$strong("Good "), icon("thumbs-o-up")))), 
		      tags$button(id = "dislike", type = "button", class = "btn btn-danger btn-lg btn-half-block action-button", list(label = shiny::tagList(tags$strong("Bad "), icon("thumbs-o-down"))))
		    ),
			  htmlOutput("progress"),
			  uiOutput("do_button"),
			  hr(),
				htmlOutput("image")
			)
		),
		column(width = 6, 
			box(width = NULL, status = "warning", solidHeader = TRUE, 
				htmlOutput("profile")
			), 
			box(width = NULL, status = "warning", solidHeader = TRUE,
				includeMarkdown("www/about.md")
			)
		), 
		column(width = 1)
	)
)


body <- dashboardBody(
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')
  ),  
	tabItems(
		app_tab
	)
)


ui <- dashboardPage(header, sidebar, body)


#################################################################
#
# Server
#
#################################################################
server <- function(input, output, session) {
	data <- read.csv("data/sample_data.csv", fileEncoding = "sjis")
	data_model <- data.frame(class = NA, select(data, age:hip))
	colnames(data_model) <- c("class", "age", "height", "weight", "bust", "waist", "hip")

	pager <- reactiveValues(p = 1)

  observeEvent(input$like, {
  	data_model[pager$p, "class"] <<- "Good"
  	pager$p <- pager$p + 1  	
  })

  observeEvent(input$dislike, {
  	data_model[pager$p, "class"] <<- "Bad"
		pager$p <- pager$p + 1  	  	
  })

  output$progress <- renderUI({
  	tags$p(" ", paste(nrow(data), "人中 ", pager$p, "人目"))
  })

  output$do_button <- renderUI({
  	if (pager$p >= 20) {
  		tags$button(id = "do", type = "button", class = "btn btn-primary btn-lg btn-block action-button", list(label = tags$strong("診断する")))
  	} else {
		  helpText("少なくとも20人ほどのデータが必要です。")
  	}
  })

	output$image <- renderUI({
		if (pager$p > nrow(data)) {
			pager$p <- nrow(data)
		}	else if (pager$p == 0) {
			pager$p <- nrow(data)
		}
		src <- paste0("img/", pager$p, ".jpg")
		tags$img(src = src, style = "width:100%;height:500;" )
	})

	output$profile <- renderUI({
		shiny::tagList(
			tags$h3("プロフィール"),
			tags$table(class = "table", 
				tags$tr(
					tags$td("年齢: "),
					tags$td(paste0(data[pager$p, "age"], "歳"))
				),	
				tags$tr(
					tags$td("身長: "),
					tags$td(paste0(data[pager$p, "tall"], "cm"))
				),
				tags$tr(
					tags$td("体重: "),
					tags$td(paste0(data[pager$p, "weight"], "kg"))
				),
				tags$tr(
					tags$td("スリーサイズ: "),
					tags$td(paste(data[pager$p, "bust"], data[pager$p, "waist"], data[pager$p, "hip"], sep = "-"))
				)
			)
		)
	})

	extractNodeInfo <- function(fit) {
	  splits <- fit$splits %>%
	    as.data.frame %>%
	    filter(count > 0) %>%
	    group_by(count) %>% 
	    slice(1) %>%
	    as.data.frame %>%
	    arrange(desc(count))
	  
	  frame <- fit$frame
	  frame <- frame[frame$var != "<leaf>", ]
	  
	  node_info <- merge(frame, splits, by.x = "n", by.y = "count")
	  node_info[order(node_info$n, decreasing = TRUE), ]
	}
	
	observeEvent(input$do, {
		data_model <- na.omit(data_model)
		data_model$class <- as.factor(data_model$class)
		fit <- rpart(class ~., data = data_model, method = "class")
		print(fit)		
		node_info <- extractNodeInfo(fit)
		print(node_info)
		output$result <- renderPlot({
		  plot(as.party(fit))
		})			
	})

}


shinyApp(ui, server)