
BD = read.csv("D:/FCU/全德堂實習/0319-0320/data/BeforeData.csv")
AD = read.csv("D:/FCU/全德堂實習/0319-0320/data/AfterData.csv")
SS = read.csv("D:/FCU/全德堂實習/0319-0320/data/SleepScore.csv")

#########################前處理##############################
a = which(is.na(BD[,2])) #10位無後測者
BD = BD[-a,] ;AD = AD[-a,]
BD[,14] = SS[,3] ; AD[,14] = SS[,4]
BD[,1] = 1:90 ; BD[,34] = as.character(BD[,34]) ; colnames(BD)[1] = "編號"
AD[,1] = 1:90 ; AD[,34] = as.character(AD[,34]) ; colnames(AD)[1] = "編號"

#1.天母(n=49) ;2.小南門(n=41)
BD1 = BD[which(BD[,3]==1),] ;BD2 = BD[which(BD[,3]==2),]
AD1 = AD[which(AD[,3]==1),] ;AD2 = AD[which(AD[,3]==2),]

#########################ui開始##############################
library(shiny)

navbarPage(title = h4(strong('中醫預防醫學健康管理模式發展評估計畫',style = "font-family:Microsoft JhengHei")),style = "font-family:Microsoft JhengHei",
    tabPanel(h5('前測',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
             fluidRow(
             column(2,checkboxGroupInput("BD_vars", "選取欄位",
                                         c("基本資料","基本生理參數","問卷量表","HRV","五力","陽虛","陰虛","痰瘀"), selected = "基本資料")
             ),
             column(9,DT::dataTableOutput("BD")))),
    
    tabPanel(h5('後測',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
             fluidRow(
               column(2,checkboxGroupInput("AD_vars", "選取欄位",
                                           c("基本資料","基本生理參數","問卷量表","HRV","五力","陽虛","陰虛","痰瘀"), selected = "基本資料")
               ),
               column(9,DT::dataTableOutput("AD")))),
    tabPanel(h5('雷達圖',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
               column(1),
               column(1, br(),br(),br(),br(),br(),br(),
                      textInput("Num"," 編號1~90", 1), align = "center"),
               column(5,plotOutput("P01")),column(5,plotOutput("P02")) ,
               column(2),
       column(5,plotOutput("P03")),column(5,plotOutput("P04"))
     
     ),
    tabPanel(h5('比較',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
             fluidRow(
               column(1),
               column(2,
                      radioButtons(inputId = "BorA",
                                  label = "資料",
                                  choices = c("前測","後測"),
                                  selected = "前測"),
                      radioButtons(inputId = "TYPE",
                                  label = "分類",
                                  choices = c("性別","收案地點"),
                                  selected = "性別"),
             selectInput(inputId = "VAR",
                         label = "選取大項",
                         choices = c("基本資料","基本生理參數","問卷量表","HRV","五力","陽虛","陰虛","痰瘀"),
                         selected = "基本資料"),
             conditionalPanel(
               condition = "input.VAR == '基本資料'",
                              selectInput(inputId = "vvrr1",
                                          label = "選取變數",
                                          choices = colnames(BD)[4:7],
                                          selected = colnames(BD)[4])
                              ),
             conditionalPanel(condition = "input.VAR == '基本生理參數'",
             selectInput(inputId = "vvrr2",
                         label = "選取變數",
                         choices = colnames(BD)[8:13],
                         selected = colnames(BD)[8]) ),
             conditionalPanel(condition = "input.VAR == '問卷量表'",
             selectInput(inputId = "vvrr3",
                         label = "選取變數",
                         choices = colnames(BD)[14:17],
                         selected = colnames(BD)[14])),
             conditionalPanel(condition = "input.VAR == 'HRV'",
             selectInput(inputId = "vvrr4",
                         label = "選取變數",
                         choices = colnames(BD)[18:28],
                         selected = colnames(BD)[18])),
             conditionalPanel(condition = "input.VAR == '五力'",
             selectInput(inputId = "vvrr5",
                         label = "選取變數",
                         choices = colnames(BD)[29:33],
                         selected = colnames(BD)[29])),
             conditionalPanel(condition = "input.VAR == '陽虛'",
             selectInput(inputId = "vvrr6",
                         label = "選取變數",
                         choices = colnames(BD)[35:40],
                         selected = colnames(BD)[35])),
             conditionalPanel(condition = "input.VAR == '陰虛'",
             selectInput(inputId = "vvrr7",
                         label = "選取變數",
                         choices = colnames(BD)[42:47],
                         selected = colnames(BD)[42])),
             conditionalPanel(condition = "input.VAR == '痰瘀'",
             selectInput(inputId = "vvrr8",
                         label = "選取變數",
                         choices = colnames(BD)[49:53],
                         selected = colnames(BD)[49])),
             verbatimTextOutput("txtout"),
             verbatimTextOutput("diffout")),
             column(8,plotOutput("P05"))
    ),
    fluidRow(
      column(3),
      column(8,plotOutput("P06"))
    ),
    fluidRow(
      column(1),
      column(10,tableOutput("P07"))
    )
),
tabPanel(h5('迴歸分析',style = "font-family:Microsoft JhengHei"),style = "font-family:Microsoft JhengHei",
         fluidRow(column(1),
                  column(2,radioButtons(inputId = "BorA2",
                                        label = "資料",
                                        choices = c("前測","後測"),
                                        selected = "前測"),
                         selectInput(inputId = "y",
                                     label = "體質",
                                     choices = c("陽虛體質","陰虛體質","痰瘀體質"),
                                     selected = "陽虛體質"),
                         conditionalPanel(condition = "input.y == '陽虛體質'",
                           selectInput(inputId = "y1",
                                       label = "依變數",
                                       choices = c("陽虛得分",colnames(BD)[35:39]),
                                       selected = "陽虛得分")),
                         conditionalPanel(condition = "input.y == '陰虛體質'",
                                          selectInput(inputId = "y2",
                                                      label = "依變數",
                                                      choices = c("陰虛得分",colnames(BD)[42:46]),
                                                      selected = "陰虛得分")),
                         conditionalPanel(condition = "input.y == '痰瘀體質'",
                                          selectInput(inputId = "y3",
                                                      label = "依變數",
                                                      choices = c("痰瘀得分",colnames(BD)[49:52]),
                                                      selected = "痰瘀得分"))),
                         column(8,verbatimTextOutput("P08")
                         ))
           
         )
)
