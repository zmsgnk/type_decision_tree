library(dplyr)
library(shiny)
library(shinydashboard)
library(rpart)
library(rpart.plot)
library(partykit)
require(markdown)
library(stringr)
library(foreach)
library(tidyr)
library(iterators)


#################################################################
#
# UI
#
#################################################################
header <- dashboardHeader(title = tags$a("好みのタイプ診断", href = "https://zmsgnk.shinyapps.io/type_decision_tree"))

sidebar <- dashboardSidebar(
	sidebarMenu(
		menuItem("App", tabName = "app"),
	  menuItem("Source code for app", icon = icon("github"),
   	   href = "https://github.com/zmsgnk/type_decision_tree"
   	),
		menuItem(uiOutput("twitter")),
		menuItem(uiOutput("facebook"))
	)
)

app_tab <- tabItem(tabName = "app",
	fluidRow(
		column(width = 1),
		column(width = 10, 
			conditionalPanel("input.do > 0",
			  box(width = NULL, title = "診断結果", solidHeader = TRUE, status = "warning", collapsible = TRUE,
			    uiOutput("result_text")
			  ), 
				box(width = NULL, title = "グラフ", solidHeader = TRUE, status = "warning", collapsible = TRUE,
					plotOutput("result_plot")
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


ui <- dashboardPage(header, sidebar, body, title = "女性の好みを診断")


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

	makeTree <- function(data) {
	  rpart(class ~., data = data, method = "class")
	}
	
	tryMakeTree <- function(data) {
	  e <- try(makeTree(data), silent = FALSE)
	  if (class(e) == "try-error") {
	    return(NULL)
	  } else {
	    return(e)
	  }
	}

	parseNodePath <- function(path) {
	  if (str_detect(path, "< ")) {
	    path <- c(str_split(path, "< ")[[1]], "lt")
	  } else if (str_detect(path, ">=")) {
	    path <- c(str_split(path, ">=")[[1]], "gte")
	  }
	  result_text <- function() {
	    if (path[1] == "age" & path[3] == "lt") {
	      c("年齢が", "若い")
	    } else if (path[1] == "age" & path[3] == "gte") {
	      c("年齢が", "大人っぽい")
	    } else if (path[1] == "height" & path[3] == "lt") {
	      c("身長が", "背の低い")
	    } else if (path[1] == "height" & path[3] == "gte") {
	      c("身長が", "背の高い")
	    } else if (path[1] == "weight" & path[3] == "lt") {
	      c("体重が", "体重の軽い")
	    } else if (path[1] == "weight" & path[3] == "gte") {
	      c("体重が", "体重の重い")
	    } else if (path[1] == "waist" & path[3] == "lt") {
	      c("ウエストが", "ウエストの細い")
	    } else if (path[1] == "waist" & path[3] == "gte") {
	      c("ウエストが", "ウエストの太い")
	    } else if (path[1] == "hip" & path[3] == "lt") {
	      c("おしりが", "おしりが小さい")
	    } else if (path[1] == "hip" & path[3] == "gte") {
	      c("おしりが", "おしりが大きい")
	    } else if (path[1] == "bust" & path[3] == "lt") {
	      c("バストが", "バストが小さい")
	    } else if (path[1] == "bust" & path[3] == "gte") {
	      c("バストが", "バストが大きい")
	    }
	  }
	  result_text()
	}
	
	extractNodePath <- function(fit) {
	  ## Goodになる確率が一番高いnodeを抽出する
	  frame <- fit$frame
	  good_prob <- as.data.frame(frame$yval2)$V5
	  i <- which(good_prob == max(good_prob))
	  node <- as.integer(rownames(frame)[i])
	  ## パスを抜き出す
	  path <- path.rpart(fit, node)[[1]]
	  foreach(p = path, .combine = rbind) %do% {
	    parseNodePath(p)
	  }
	}
	
	observeEvent(input$do, {
		data_model <- na.omit(data_model)
		data_model$class <- as.factor(data_model$class)
		print(head(data_model))
	  fit <- tryMakeTree(data_model)
    print(fit)
    
    result_text <- NULL
    if (!is.null(fit)) {
      result_text <- extractNodePath(fit) %>% as.data.frame
      names(result_text) <- c("part", "text")
      result_text <- result_text %>%
        mutate(part = as.character(part), 
               text = as.character(text))
      
      num_part <- result_text %>% count(part) %>% filter(n > 1)
      foreach(i = icount(nrow(num_part))) %do% {
        multi_part <- num_part[i, "part"] %>% as.character
        result_text <- result_text %>%
          mutate(text = ifelse(part == multi_part, paste0(multi_part, "ほどほどな"), text))
      }
      result_text <- paste0(unique(result_text$text), collapse = "、")
      result_text <- paste0(result_text, "女性が好きなようです。")		    
    } else if (unique(data_model$class) == "Bad") {
      result_text <- "もしかして男性が好きなのですか？。"
    } else {
      result_text <- "女性が好きなようです。"
    }
        
		output$result_plot <- renderPlot({
		  validate({
		    need(!is.null(fit$splits), "正常に計算が終了出来ませんでした。「Good」と「Bad」の数がだいたい同じくらいになるのが好ましいです。")
		  })
		  plot(as.party(fit))
		})
		
		output$result_text <- renderUI({
		  tags$h2(result_text)
		})
		
		output$twitter <- renderUI({
		  tags$li(
		    a(href = sprintf("http://twitter.com/intent/tweet?text=【女性の好み診断】%%0a%%0aあなたは、「%s」%%0a%%0a&url=https://zmsgnk.shinyapps.io/type_decision_tree", result_text),
		      target = "_blank",
		      icon("twitter"),
		      onClick = "window.open(encodeURI(decodeURI(this.href)),
	          'tweetwindow',
	          'width=550, height=450, personalbar=0, toolbar=0, scrollbars=1, resizable=1'
	          ); return false;",
		      span('結果をシェア')
		    )
		  )
		})
		
		output$facebook <- renderUI({
		  tags$li(
		    a(href = sprintf("http://www.facebook.com/dialog/feed?app_id=392319910975259&redirect_uri=https://zmsgnk.shinyapps.io/type_decision_tree&link=https://zmsgnk.shinyapps.io/type_decision_tree&description=女性の好みをデータから診断！！&name=「%s」&picture=http://bds.datumstudio.jp/wp-content/uploads/koukoku3.jpg", result_text), 
		    	target = "_blank",
		    	icon("facebook"),
		    	span("結果をシェア")
		    )
		  )
		})
	})
	
}


shinyApp(ui, server)