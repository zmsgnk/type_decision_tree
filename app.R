library(dplyr)
library(shiny)
library(shinydashboard)
library(rpart)
library(rpart.plot)


#################################################################
#
# UI
#
#################################################################

header <- dashboardHeader(title = "好みのタイプ診断")

sidebar <- dashboardSidebar(
	# disable = TRUE
	sidebarMenu(
		menuItem("App", tabName = "app"),
	  # menuItem("Source code for app", icon = icon("github"),
   	#    href = "http://github.com/ksmzn/ShinyDistributionsApp"
	  #  ),
    # tags$li(
    #   a(href = "http://twitter.com/intent/tweet?text=女の子の好みを診断するアプリ&url=http://statdist.ksmzn.com/&via=ksmzn&hashtags=rshiny",
    #     target = "_blank",
    #     icon("twitter"),
    #     onClick = "window.open(encodeURI(decodeURI(this.href)),
    #       'tweetwindow',
    #       'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
    #       ); return false;",
    #     span('Tweet'),
    #     tags$small(
    #       class = paste0("badge pull-right bg-", 'light-blue'),
    #       'Share'
    #     )
    #   )
    # ),
    # tags$li(
    #   a( href = "http://www.facebook.com/sharer.php?u=http://statdist.ksmzn.com/&t=いろいろな確率分布のパラメータをいじくるアプリ",
    #     target = "_blank",
    #     icon("facebook"),
    #     span('Facebook'),
    #     tags$small(
    #       class = paste0("badge pull-right bg-", 'light-blue'),
    #       'Share'
    #     )
    #   )
    # )
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
 				actionButton("like", shiny::tagList("Good ", icon("thumbs-o-up"))),
			  actionButton("dislike", shiny::tagList("Bad ", icon("thumbs-o-down"))),
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
	tabItems(
		app_tab
	)
)


ui <- dashboardPage(header, sidebar, body, skin = "yellow")


#################################################################
#
# Server
#
#################################################################
server <- function(input, output, session) {
	data <- read.csv("data/sample_data.csv", fileEncoding = "sjis")
	data_model <- data.frame(class = NA, select(data, age:hip))
	colnames(data_model) <- c("class", "年齢", "身長", "体重", "バスト", "ウエスト", "ヒップ")

	pager <- reactiveValues(p = 1)
  class <- reactiveValues(class = 0)

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
  		tags$button(id = "do", type = "button", class = "btn btn-primary btn-lg btn-block action-button", list(label = "診断する"))
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
				# tags$tr(
				# 	tags$td("名前: "),
				# 	tags$td(data[pager$p, "name"])
				# ),
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

	observeEvent(input$do, {
		data_model <- na.omit(data_model)
		data_model$class <- as.factor(data_model$class)
		fit <- rpart(class ~., data = data_model, method = "class")
		output$result <- renderPlot({
			rpart.plot(fit, type = 1, extra = 1, under = TRUE)
		})			
	})

}


shinyApp(ui, server)