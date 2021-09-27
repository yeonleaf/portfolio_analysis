
library(shiny)
library(quantmod)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)

ticker_data <- read_excel("C:/Rstudy/analysis_portfolio_practice/ticker_code_data.xls")
ticker_data$compile <- paste0(ticker_data$ticker,"(",ticker_data$company,")")
head(ticker_data)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Portfolio Analysis - Efficient Frontier & CAL"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("stock1","stock1",choices=NULL,selected=NULL),
            selectizeInput("stock2","stock2",choices=NULL,selected=NULL),
            # textInput("stock1", label = h3("Stock 1"), 
            #           value = "Enter 6 number ticker code"),
            # 
            # textInput("stock2", label = h3("Stock 2"), 
            #           value = "Enter 6 number ticker code"),
            
            numericInput("money", label = h3("How much money do you want to invest in?"), value = 1),
            
            hr(),
            fluidRow(column(3, verbatimTextOutput("value"))),
            
            actionButton("action1", "Go!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("Plotdata"),
           tableOutput("resultTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    updateSelectizeInput(session, 'stock1',
                         choices = ticker_data$compile,
                         server = TRUE
    )
    updateSelectizeInput(session, 'stock2',
                         choices = ticker_data$compile,
                         server = TRUE
    )
    
    tangent_stock1 <- 0
    tangent_stock2 <- 0
    tangent_Port_ER <- 0
    tangent_Port_SD <- 0
    
    observe({

        ## 데이터 불러오기
        if(input$action1 == 0) return()
        input$action1
        stock1df <- isolate({
            getSymbols(paste0(substr(input$stock1,1,6),".","KS"),
                               from='2019-01-01', to='2019-12-30', auto.assign=F)
            })
        stock1df <- unlist(stock1df) %>% data.frame()
        stock2df <- isolate({
            getSymbols(paste0(substr(input$stock2,1,6),".","KS"),
                       from='2019-01-01', to='2019-12-30', auto.assign=F)
            })
        stock2df <- unlist(stock2df) %>% data.frame()

        koreadf <- isolate({
            getSymbols('148070.KS',
                                from = '2019-01-01', to='2019-12-30', auto.assign=F)
        })
        koreadf <- unlist(koreadf) %>% data.frame()
        #View(stock2df)

        ## 일별 수익률 계산
        Daily_R1 <- c(0)
        for (i in 2:dim(stock1df)[1]) {
            rate <- round(((stock1df[i,4]/stock1df[i-1,4])-1)*100,2)
            Daily_R1 <- c(Daily_R1,rate)
        }
        DRstock1 <- Daily_R1 %>% data.frame()
        DRstock1 <- rename(DRstock1,"Daily_Return_stock1"=".")
        stock1df <- cbind(stock1df,DRstock1)
        #View(stock1df)

        Daily_R2 <- c(0)
        for (i in 2:dim(stock2df)[1]) {
            rate <- round(((stock2df[i,4]/stock2df[i-1,4])-1)*100,2)
            Daily_R2 <- c(Daily_R2,rate)
        }
        DRstock2 <- Daily_R2 %>% data.frame()
        DRstock2 <- rename(DRstock2,"Daily_Return_stock2"=".")
        stock2df <- cbind(stock2df,DRstock2)
        #View(stock2df)

        Daily_R_KOREA <- c(0)
        for (i in 2:dim(koreadf)[1]) {
            rate <- round(((koreadf[i,4]/koreadf[i-1,4])-1)*100,2)
            Daily_R_KOREA <- c(Daily_R_KOREA,rate)
        }
        DR_KOREA <- Daily_R_KOREA %>% data.frame()
        DR_KOREA <- rename(DR_KOREA,"Daily_Return_KOREA"=".")
        koreadf <- cbind(koreadf,DR_KOREA)
        #View(koreadf)

        # weight
        weight <- data.frame(w_stock1=seq(0,1,by=0.005),w_stock2=seq(1,0,by=-0.005))

        # elements
        elements <- data.frame(Er_stock1=0,Er_stock2=0,Er_KOREA=0,sd_stock1=0,sd_stock2=0,stocks_cor=0)

        elements$Er_stock1 <- stock1df %>% filter(!is.na(Daily_Return_stock1)) %>%
            summarise(Er_stock1=round(mean(Daily_Return_stock1),4)) # Er_Naver
        elements$Er_stock2 <- stock2df %>% filter(!is.na(Daily_Return_stock2)) %>%
            summarise(Er_stock2=round(mean(Daily_Return_stock2),4)) # Er_SDI
        elements$Er_KOREA <- koreadf %>% filter(!is.na(Daily_Return_KOREA)) %>%
            summarise(Er_KOREA=round(mean(Daily_Return_KOREA),4)) # Er_KOREA
        elements$sd_stock1 <- stock1df %>% filter(!is.na(Daily_Return_stock1)) %>%
            summarise(sd_stock1=round(sd(Daily_Return_stock1),4)) # sd_Naver
        elements$sd_stock2 <- stock2df %>% filter(!is.na(Daily_Return_stock2)) %>%
            summarise(sd_stock2=round(sd(Daily_Return_stock2),4)) # sd_SDI

        filteredStock1 <- stock1df %>% filter(!is.na(Daily_Return_stock1)) %>%
            select(Daily_Return_stock1)
        filteredStock2 <- stock2df %>% filter(!is.na(Daily_Return_stock2)) %>%
            select(Daily_Return_stock2)
        elements$stocks_cor <- as.numeric(cor(filteredStock1,filteredStock2)) # NS_cor
        #View(elements)

        ## 포트폴리오의 ER_P와 SD_P 구하기

        Er_stock1 <- as.numeric(elements$Er_stock1)
        Er_stock2 <- as.numeric(elements$Er_stock2)
        Er_KOREA <- as.numeric(elements$Er_KOREA)
        SD_stock1 <- as.numeric(elements$sd_stock1)
        SD_stock2 <- as.numeric(elements$sd_stock2)
        stocks_COR <- as.numeric(elements$stocks_cor)

        portfolio <- weight %>% mutate(ER_P=(weight$w_stock1*Er_stock1)+
                                           (weight$w_stock2*Er_stock2)) %>%
            mutate(SD_P=sqrt((weight$w_stock1^2*SD_stock1^2) +
                                 (weight$w_stock2^2*SD_stock2^2) +
                                 (2*weight$w_stock1*weight$w_stock2*
                                      SD_stock1*SD_stock2*
                                      stocks_COR)))
        #View(portfolio)

        ## Tangent weight 구하기
        bstock1 <- Er_stock1-Er_KOREA
        bstock2 <- Er_stock2-Er_KOREA
        c <- stocks_COR*SD_stock1*SD_stock2

        tangent_stock1 <- (bstock1*SD_stock2^2 - bstock2*c)/
            (bstock1*SD_stock2^2 + bstock2*SD_stock1^2-(bstock1-bstock2)*c)
        tangent_stock2 <- 1-tangent_stock1
        tangent_Port_ER <- as.numeric(tangent_stock1*Er_stock1 + tangent_stock2*Er_stock2)
        tangent_Port_SD <- as.numeric(sqrt((tangent_stock1^2*SD_stock1^2)+
                                               (tangent_stock2^2*SD_stock2^2) +
                                               (2*tangent_stock1*tangent_stock2*SD_stock1*SD_stock2*stocks_COR)))

        slope1 <- (tangent_Port_ER-Er_KOREA)/tangent_Port_SD

        ## 그래프 그리기
        output$Plotdata <- renderPlotly({
            z1 <- rnorm(100)
            graph <- ggplot(data=portfolio,aes(x=SD_P,y=ER_P)) + geom_point(colour="coral") +
                geom_abline(intercept=Er_KOREA, slope=slope1) +
                geom_point(aes(x=tangent_Port_SD,y=tangent_Port_ER),colour="RED") + theme_bw() +
                labs(x="risk",y="return")
            
            graph <- graph + ggtitle(paste("Analysis Complete! - ",input$stock1,"/",input$stock2)) +
                theme(plot.title=element_text(vjust=-5.5))
            
            return(ggplotly(graph))
        })
        
        ## resultdf 표 만들기
        output$resultTable <- renderTable({
            if(input$action1 == 0) return()
            input$action1

            resultdf <- data.frame(weight_stock1=0, weight_stock2=0, 
                                   Investment_stock1=0, Investment_stock2=0)
            resultdf$weight_stock1 <- round(as.numeric(tangent_stock1),2)
            resultdf$weight_stock2 <- round(as.numeric(tangent_stock2),2)
            resultdf$Portfolio_return <- as.numeric(tangent_Port_ER)
            resultdf$Portfolio_return <- as.numeric(tangent_Port_SD)
            InvestedMoney <- isolate(input$money)
            resultdf$Investment_stock1 <- paste0(round(InvestedMoney * round(tangent_stock1,2)),"원")
            resultdf$Investment_stock2 <- paste0(round(InvestedMoney * round(tangent_stock2,2)),"원")
            
            return(resultdf)
        })

    })

}

# Run the application 
shinyApp(ui = ui, server = server)
