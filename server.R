
library(shiny)
library(MASS)
library(fmsb)
library(ggplot2)
library(scales)
options(scipen=999)

shinyServer(function(input, output) {

  output$BD <- DT::renderDataTable(DT::datatable({
    BD[,c(1:3,
          if("基本資料" %in% input$BD_vars){4:7},
          if("基本生理參數" %in% input$BD_vars){8:13},
          if("問卷量表" %in% input$BD_vars){14:17},
          if("HRV" %in% input$BD_vars){18:28},
          if("五力" %in% input$BD_vars){29:33},
          if("陽虛" %in% input$BD_vars){35:41},
          if("陰虛" %in% input$BD_vars){42:48},
          if("痰瘀" %in% input$BD_vars){49:54}
    )]
  }))
  output$AD <- DT::renderDataTable(DT::datatable({
    AD[,c(1:3,
           if("基本資料" %in% input$AD_vars){4:7},
           if("基本生理參數" %in% input$AD_vars){8:13},
           if("問卷量表" %in% input$AD_vars){14:17},
           if("HRV" %in% input$AD_vars){18:28},
           if("五力" %in% input$AD_vars){29:33},
           if("陽虛" %in% input$AD_vars){35:41},
           if("陰虛" %in% input$AD_vars){42:48},
           if("痰瘀" %in% input$AD_vars){49:54}
    )]
  }))
  output$P01 <- renderPlot({
    w = c() ; e = c()
    for(i in 35:39){ w[i-34] = BD[which(input$Num == BD[,1]),i] }
    for(i in 35:39){ e[i-34] = AD[which(input$Num == AD[,1]),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陽虛" , "胸腔陽虛" , "四肢陽虛" , "腹腔陽虛" , "體表陽虛")
    rownames(data) = c("前測","後測")
    data = rbind(rep(15,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    yang = radarchart( data  , axistype=1,seg=5 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,15,3), cglwd=0.8, vlcex=0.8,title  = "陽虛") 
    legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  })
  output$P02 <- renderPlot({
    w = c() ; e = c()
    for(i in 42:46){ w[i-41] = BD[which(input$Num == BD[,1]),i] }
    for(i in 42:46){ e[i-41] = AD[which(input$Num == AD[,1]),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("頭部陰虛" , "四肢陰虛" , "腸胃道陰虛" , "體表陰虛" , "腹腔陰虛")
    rownames(data) = c("前測","後測")
    data = rbind(rep(20,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    yang = radarchart( data  , axistype=1 ,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8, vlcex=0.8,title  = "陰虛") 
   legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  })
  output$P03 <- renderPlot({
    w = c() ; e = c()
    for(i in 49:52){ w[i-48] = BD[which(input$Num == BD[,1]),i] }
    for(i in 49:52){ e[i-48] = AD[which(input$Num == AD[,1]),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("軀幹痰瘀" , "體表痰瘀" , "頭部痰瘀" , "腸胃道痰瘀")
    rownames(data) = c("前測","後測")
    data = rbind(rep(20,4) , rep(0,4) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    yang = radarchart( data  , axistype=1,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8, vlcex=0.8,title  = "痰淤") 
    legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  })
  output$P04 <- renderPlot({
    w = c() ; e = c()
    for(i in 29:33){ w[i-28] = BD[which(input$Num == BD[,1]),i] }
    for(i in 29:33){ e[i-28] = AD[which(input$Num == AD[,1]),i] }
    data = as.data.frame(rbind(w,e))
    colnames(data) = c("heart" , "health" , "sex" , "vital", "fight")
    rownames(data) = c("前測","後測")
    data = rbind(rep(100,5) , rep(0,5) , data)
    colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
    yang = radarchart( data  , axistype=1 ,seg=5,pcol = colors_border , pfcol=colors_in , plwd=4 , plty=1,
                       cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.8, vlcex=0.8,title  = "五力") 
    legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  })
  output$P05 <- renderPlot({
    if(input$BorA == "前測"){
      if(input$VAR == "基本資料"){
        qplot(BD[,which(colnames(BD) == input$vvrr1)], data = BD, geom = "bar",
              fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
          labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
               x = colnames(BD)[which(colnames(BD) == input$vvrr1)], y = "人數") +
          scale_y_continuous(breaks= 1:100)
      } else {
        if(input$VAR == "基本生理參數"){
          qplot(BD[,which(colnames(BD) == input$vvrr2)], data = BD, geom = "histogram",
                fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
            labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                 x = colnames(BD)[which(colnames(BD) == input$vvrr2)], y = "人數") +
            scale_y_continuous(breaks= 1:100)
        } else {
          if(input$VAR == "問卷量表"){
            qplot(BD[,which(colnames(BD) == input$vvrr3)], data = BD, geom = "histogram",
                  fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
              labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                   x = colnames(BD)[which(colnames(BD) == input$vvrr3)], y = "人數") +
              scale_y_continuous(breaks= 1:100)
          } else {
            if(input$VAR == "HRV"){
              qplot(BD[,which(colnames(BD) == input$vvrr4)], data = BD, geom = "histogram",
                    fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
                labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                     x = colnames(BD)[which(colnames(BD) == input$vvrr4)], y = "人數") +
                scale_y_continuous(breaks= pretty_breaks())
            } else {
              if(input$VAR == "五力"){
                qplot(BD[,which(colnames(BD) == input$vvrr5)], data = BD, geom = "histogram",
                      fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
                  labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                       x = colnames(BD)[which(colnames(BD) == input$vvrr5)], y = "人數") +
                  scale_y_continuous(breaks= 1:100)
              } else {
                if(input$VAR == "陽虛"){
                  qplot(BD[,which(colnames(BD) == input$vvrr6)], data = BD, geom = "bar",
                        fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
                    labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                         x = colnames(BD)[which(colnames(BD) == input$vvrr6)], y = "人數") +
                    scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                } else {
                  if(input$VAR == "陰虛"){
                    qplot(BD[,which(colnames(BD) == input$vvrr7)], data = BD, geom = "bar",
                          fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
                      labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                           x = colnames(BD)[which(colnames(BD) == input$vvrr7)], y = "人數") +
                      scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                  } else {
                    qplot(BD[,which(colnames(BD) == input$vvrr8)], data = BD, geom = "bar",
                          fill = as.character(BD[,which(colnames(BD)==input$TYPE)]))+
                      labs(fill = colnames(BD)[which(colnames(BD)==input$TYPE)],
                           x = colnames(BD)[which(colnames(BD) == input$vvrr8)], y = "人數") +
                      scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                  }
                }
              }
            }
          }
        }
      }
    } else {
      if(input$VAR == "基本資料"){
        qplot(AD[,which(colnames(AD) == input$vvrr1)], data = AD, geom = "bar",
              fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
          labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
               x = colnames(AD)[which(colnames(AD) == input$vvrr1)], y = "人數") +
          scale_y_continuous(breaks= 1:100)
      } else {
        if(input$VAR == "基本生理參數"){
          qplot(AD[,which(colnames(AD) == input$vvrr2)], data = AD, geom = "histogram",
                fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
            labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                 x = colnames(AD)[which(colnames(AD) == input$vvrr2)], y = "人數") +
            scale_y_continuous(breaks= 1:100)
        } else {
          if(input$VAR == "問卷量表"){
            qplot(AD[,which(colnames(AD) == input$vvrr3)], data = AD, geom = "histogram",
                  fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
              labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                   x = colnames(AD)[which(colnames(AD) == input$vvrr3)], y = "人數") +
              scale_y_continuous(breaks= 1:100)
          } else {
            if(input$VAR == "HRV"){
              qplot(AD[,which(colnames(AD) == input$vvrr4)], data = AD, geom = "histogram",
                    fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
                labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                     x = colnames(AD)[which(colnames(AD) == input$vvrr4)], y = "人數") +
                scale_y_continuous(breaks= pretty_breaks())
            } else {
              if(input$VAR == "五力"){
                qplot(AD[,which(colnames(AD) == input$vvrr5)], data = AD, geom = "histogram",
                      fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
                  labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                       x = colnames(AD)[which(colnames(AD) == input$vvrr5)], y = "人數") +
                  scale_y_continuous(breaks= 1:100)
              } else {
                if(input$VAR == "陽虛"){
                  qplot(AD[,which(colnames(AD) == input$vvrr6)], data = AD, geom = "bar",
                        fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
                    labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                         x = colnames(AD)[which(colnames(AD) == input$vvrr6)], y = "人數") +
                    scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                } else {
                  if(input$VAR == "陰虛"){
                    qplot(AD[,which(colnames(AD) == input$vvrr7)], data = AD, geom = "bar",
                          fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
                      labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                           x = colnames(AD)[which(colnames(AD) == input$vvrr7)], y = "人數") +
                      scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                  } else {
                    qplot(AD[,which(colnames(AD) == input$vvrr8)], data = AD, geom = "bar",
                          fill = as.character(AD[,which(colnames(AD)==input$TYPE)]))+
                      labs(fill = colnames(AD)[which(colnames(AD)==input$TYPE)],
                           x = colnames(AD)[which(colnames(AD) == input$vvrr8)], y = "人數") +
                      scale_y_continuous(breaks= seq(0, 80, length.out = 41))
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  output$P06 <- renderPlot({
    if(input$BorA == "前測"){
      if(input$VAR == "基本資料"){
        ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr1)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                       shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
          geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                     stat = "count" ,size = 3, show.legend = FALSE) +
          labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
               x = colnames(BD)[which(colnames(BD) == input$vvrr1)], y = "人數") +
          scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                        position="identity", stat = "count" ,size = 1)
      } else {
        if(input$VAR == "基本生理參數"){
          ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr2)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                         shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
            geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                       stat = "count" ,size = 3, show.legend = FALSE) +
            labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                 x = colnames(BD)[which(colnames(BD) == input$vvrr2)], y = "人數") +
            scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                          position="identity", stat = "count" ,size = 1)
        } else {
          if(input$VAR == "問卷量表"){
            ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr3)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                           shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
              geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                         stat = "count" ,size = 3, show.legend = FALSE) +
              labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                   x = colnames(BD)[which(colnames(BD) == input$vvrr3)], y = "人數") +
              scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                            position="identity", stat = "count" ,size = 1)
          } else {
            if(input$VAR == "HRV"){
              ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr4)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                             shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
                geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                           stat = "count" ,size = 3, show.legend = FALSE) +
                labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                     x = colnames(BD)[which(colnames(BD) == input$vvrr4)], y = "人數") +
                scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                              position="identity", stat = "count" ,size = 1)
            } else {
              if(input$VAR == "五力"){
                ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr5)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                               shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
                  geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                             stat = "count" ,size = 3, show.legend = FALSE) +
                  labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                       x = colnames(BD)[which(colnames(BD) == input$vvrr5)], y = "人數") +
                  scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                                position="identity", stat = "count" ,size = 1)
              } else {
                if(input$VAR == "陽虛"){
                  ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr6)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                                 shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
                    geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])), position="identity",
                               stat = "count" ,size = 3, show.legend = FALSE) +
                    labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                         x = colnames(BD)[which(colnames(BD) == input$vvrr6)], y = "人數") +
                    scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                                  position="identity", stat = "count" ,size = 1)
                } else {
                  if(input$VAR == "陰虛"){
                    ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr7)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                                   shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
                      geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                 position="identity", stat = "count" ,size = 3, show.legend = FALSE) +
                      labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                           x = colnames(BD)[which(colnames(BD) == input$vvrr7)], y = "人數") +
                      scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                                    position="identity", stat = "count" ,size = 1)
                  } else {
                    ggplot(BD, aes(BD[,which(colnames(BD) == input$vvrr8)], group = as.character(BD[,which(colnames(BD)==input$TYPE)]),
                                   shape = as.character(BD[,which(colnames(BD)==input$TYPE)]))) +
                      geom_point(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                 position="identity", stat = "count" ,size = 3, show.legend = FALSE) +
                      labs(colour = colnames(BD)[which(colnames(BD)==input$TYPE)],
                           x = colnames(BD)[which(colnames(BD) == input$vvrr8)], y = "人數") +
                      scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(BD[,which(colnames(BD)==input$TYPE)])),
                                                                    position="identity", stat = "count" ,size = 1)
                  }
                }
              }
            }
          }
        }
      }
    } else {
      if(input$VAR == "基本資料"){
        ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr1)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                       shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
          geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                     stat = "count" ,size = 3, show.legend = FALSE) +
          labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
               x = colnames(AD)[which(colnames(AD) == input$vvrr1)], y = "人數") +
          scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                        position="identity", stat = "count" ,size = 1)
      } else {
        if(input$VAR == "基本生理參數"){
          ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr2)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                         shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
            geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                       stat = "count" ,size = 3, show.legend = FALSE) +
            labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                 x = colnames(AD)[which(colnames(AD) == input$vvrr2)], y = "人數") +
            scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                          position="identity", stat = "count" ,size = 1)
        } else {
          if(input$VAR == "問卷量表"){
            ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr3)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                           shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
              geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                         stat = "count" ,size = 3, show.legend = FALSE) +
              labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                   x = colnames(AD)[which(colnames(AD) == input$vvrr3)], y = "人數") +
              scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                            position="identity", stat = "count" ,size = 1)
          } else {
            if(input$VAR == "HRV"){
              ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr4)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                             shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
                geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                           stat = "count" ,size = 3, show.legend = FALSE) +
                labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                     x = colnames(AD)[which(colnames(AD) == input$vvrr4)], y = "人數") +
                scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                              position="identity", stat = "count" ,size = 1)
            } else {
              if(input$VAR == "五力"){
                ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr5)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                               shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
                  geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                             stat = "count" ,size = 3, show.legend = FALSE) +
                  labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                       x = colnames(AD)[which(colnames(AD) == input$vvrr5)], y = "人數") +
                  scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                                position="identity", stat = "count" ,size = 1)
              } else {
                if(input$VAR == "陽虛"){
                  ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr6)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                                 shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
                    geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])), position="identity",
                               stat = "count" ,size = 3, show.legend = FALSE) +
                    labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                         x = colnames(AD)[which(colnames(AD) == input$vvrr6)], y = "人數") +
                    scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                                  position="identity", stat = "count" ,size = 1)
                } else {
                  if(input$VAR == "陰虛"){
                    ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr7)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                                   shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
                      geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                 position="identity", stat = "count" ,size = 3, show.legend = FALSE) +
                      labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                           x = colnames(AD)[which(colnames(AD) == input$vvrr7)], y = "人數") +
                      scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                                    position="identity", stat = "count" ,size = 1)
                  } else {
                    ggplot(AD, aes(AD[,which(colnames(AD) == input$vvrr8)], group = as.character(AD[,which(colnames(AD)==input$TYPE)]),
                                   shape = as.character(AD[,which(colnames(AD)==input$TYPE)]))) +
                      geom_point(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                 position="identity", stat = "count" ,size = 3, show.legend = FALSE) +
                      labs(colour = colnames(AD)[which(colnames(AD)==input$TYPE)],
                           x = colnames(AD)[which(colnames(AD) == input$vvrr8)], y = "人數") +
                      scale_y_continuous(breaks= 1:100) + geom_line(aes(color = as.character(AD[,which(colnames(AD)==input$TYPE)])),
                                                                    position="identity", stat = "count" ,size = 1)
                  }
                }
              }
            }
          }
        }
      }
      
    }
  })
  output$P07 <- renderTable({
    if(input$BorA == "前測"){
      if(input$VAR == "基本資料"){
        table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr1)]) 
      } else {
        if(input$VAR == "基本生理參數"){
          table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr2)]) 
        } else {
          if(input$VAR == "問卷量表"){
            table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr3)]) 
          } else {
            if(input$VAR == "HRV"){
              table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr4)]) 
            } else {
              if(input$VAR == "五力"){
                table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr5)])
              } else {
                if(input$VAR == "陽虛"){
                  table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr6)])
                } else {
                  if(input$VAR == "陰虛"){
                    table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr7)])
                  } else {
                    table(BD[,which(colnames(BD)==input$TYPE)],BD[,which(colnames(BD) == input$vvrr8)])
                  }
                }
              }
            }
          }
        }
      }
    } else {
      
      if(input$VAR == "基本資料"){
        table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr1)]) 
      } else {
        if(input$VAR == "基本生理參數"){
          table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr2)]) 
        } else {
          if(input$VAR == "問卷量表"){
            table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr3)]) 
          } else {
            if(input$VAR == "HRV"){
              table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr4)]) 
            } else {
              if(input$VAR == "五力"){
                table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr5)])
              } else {
                if(input$VAR == "陽虛"){
                  table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr6)])
                } else {
                  if(input$VAR == "陰虛"){
                    table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr7)])
                  } else {
                    table(AD[,which(colnames(AD)==input$TYPE)],AD[,which(colnames(AD) == input$vvrr8)])
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  output$txtout <- renderPrint({
    if(input$BorA == "前測"){
      if(input$VAR == "基本資料"){
        data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr1)],
               BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr1)])$statistic[[1]],4),
               "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr1)],
                      BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr1)])$p.value[[1]],4))
      } else {
        if(input$VAR == "基本生理參數"){
          data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr2)],
                                                  BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr2)])$statistic[[1]],4),
                     "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr2)],
                                              BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr2)])$p.value[[1]],4))
        } else {
          if(input$VAR == "問卷量表"){
            data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr3)],
                                                    BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr3)])$statistic[[1]],4),
                       "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr3)],
                                                BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr3)])$p.value[[1]],4))
          } else {
            if(input$VAR == "HRV"){
              data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr4)],
                                                      BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr4)])$statistic[[1]],4),
                         "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr4)],
                                                  BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr4)])$p.value[[1]],4))
            } else {
              if(input$VAR == "五力"){
                data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr5)],
                                                        BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr5)])$statistic[[1]],4),
                           "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr5)],
                                                    BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr5)])$p.value[[1]],4))
              } else {
                if(input$VAR == "陽虛"){
                  data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr6)],
                                                          BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr6)])$statistic[[1]],4),
                             "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr6)],
                                                      BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr6)])$p.value[[1]],4))
                } else {
                  if(input$VAR == "陰虛"){
                    data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr7)],
                                                            BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr7)])$statistic[[1]],4),
                               "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr7)],
                                                        BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr7)])$p.value[[1]],4))
                  } else {
                    data.frame("T.statistic" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr8)],
                                                            BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr8)])$statistic[[1]],4),
                               "p.value" = round(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr8)],
                                                        BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr8)])$p.value[[1]],4))
                  }
                }
              }
            }
          }
        }
      }
    } else {
      if(input$VAR == "基本資料"){
        data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr1)],
                                                AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr1)])$statistic[[1]],4),
                   "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr1)],
                                            AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr1)])$p.value[[1]],4))
      } else {
        if(input$VAR == "基本生理參數"){
          data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr2)],
                                                  AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr2)])$statistic[[1]],4),
                     "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr2)],
                                              AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr2)])$p.value[[1]],4))
        } else {
          if(input$VAR == "問卷量表"){
            data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr3)],
                                                    AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr3)])$statistic[[1]],4),
                       "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr3)],
                                                AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr3)])$p.value[[1]],4))
          } else {
            if(input$VAR == "HRV"){
              data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr4)],
                                                      AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr4)])$statistic[[1]],4),
                         "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr4)],
                                                  AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr4)])$p.value[[1]],4))
            } else {
              if(input$VAR == "五力"){
                data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr5)],
                                                        AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr5)])$statistic[[1]],4),
                           "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr5)],
                                                    AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr5)])$p.value[[1]],4))
              } else {
                if(input$VAR == "陽虛"){
                  data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr6)],
                                                          AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr6)])$statistic[[1]],4),
                             "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr6)],
                                                      AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr6)])$p.value[[1]],4))
                } else {
                  if(input$VAR == "陰虛"){
                    data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr7)],
                                                            AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr7)])$statistic[[1]],4),
                               "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr7)],
                                                        AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr7)])$p.value[[1]],4))
                  } else {
                    data.frame("T.statistic" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr8)],
                                                            AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr8)])$statistic[[1]],4),
                               "p.value" = round(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr8)],
                                                        AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr8)])$p.value[[1]],4))
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  output$diffout <- renderPrint({
    if(input$BorA == "前測"){
      if(input$VAR == "基本資料"){
        if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr1)],
                  BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr1)])$p.value[[1]] <= 0.05){
          paste0(input$TYPE,"在",input$vvrr1,"上有顯著差異")
        }
      } else {
        if(input$VAR == "基本生理參數"){
          if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr2)],
                    BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr2)])$p.value[[1]] <= 0.05){
            paste0(input$TYPE,"在",input$vvrr2,"上有顯著差異")
          }
        } else {
          if(input$VAR == "問卷量表"){
            if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr3)],
                      BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr3)])$p.value[[1]] <= 0.05){
              paste0(input$TYPE,"在",input$vvrr3,"上有顯著差異")
            }
          } else {
            if(input$VAR == "HRV"){
              if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr4)],
                        BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr4)])$p.value[[1]] <= 0.05){
                paste0(input$TYPE,"在",input$vvrr4,"上有顯著差異")
              }
            } else {
              if(input$VAR == "五力"){
                if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr5)],
                          BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr5)])$p.value[[1]] <= 0.05){
                  paste0(input$TYPE,"在",input$vvrr5,"上有顯著差異")
                }
              } else {
                if(input$VAR == "陽虛"){
                  if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr6)],
                            BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr6)])$p.value[[1]] <= 0.05){
                    paste0(input$TYPE,"在",input$vvrr6,"上有顯著差異")
                  }
                } else {
                  if(input$VAR == "陰虛"){
                    if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr7)],
                              BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr7)])$p.value[[1]] <= 0.05){
                      paste0(input$TYPE,"在",input$vvrr7,"上有顯著差異")
                    }
                  } else {
                    if(t.test(BD[which(BD[,which(colnames(BD)==input$TYPE)]==1),which(colnames(BD) == input$vvrr8)],
                              BD[which(BD[,which(colnames(BD)==input$TYPE)]==2),which(colnames(BD) == input$vvrr8)])$p.value[[1]] <= 0.05){
                      paste0(input$TYPE,"在",input$vvrr8,"上有顯著差異")
                    }
                  }
                }
              }
            }
          }
        }
      }
    } else {
      if(input$VAR == "基本資料"){
        if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr1)],
                  AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr1)])$p.value[[1]] <= 0.05){
          paste0(input$TYPE,"在",input$vvrr1,"上有顯著差異")
        }
      } else {
        if(input$VAR == "基本生理參數"){
          if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr2)],
                    AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr2)])$p.value[[1]] <= 0.05){
            paste0(input$TYPE,"在",input$vvrr2,"上有顯著差異")
          }
        } else {
          if(input$VAR == "問卷量表"){
            if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr3)],
                      AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr3)])$p.value[[1]] <= 0.05){
              paste0(input$TYPE,"在",input$vvrr3,"上有顯著差異")
            }
          } else {
            if(input$VAR == "HRV"){
              if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr4)],
                        AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr4)])$p.value[[1]] <= 0.05){
                paste0(input$TYPE,"在",input$vvrr4,"上有顯著差異")
              }
            } else {
              if(input$VAR == "五力"){
                if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr5)],
                          AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr5)])$p.value[[1]] <= 0.05){
                  paste0(input$TYPE,"在",input$vvrr5,"上有顯著差異")
                }
              } else {
                if(input$VAR == "陽虛"){
                  if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr6)],
                            AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr6)])$p.value[[1]] <= 0.05){
                    paste0(input$TYPE,"在",input$vvrr6,"上有顯著差異")
                  }
                } else {
                  if(input$VAR == "陰虛"){
                    if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr7)],
                              AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr7)])$p.value[[1]] <= 0.05){
                      paste0(input$TYPE,"在",input$vvrr7,"上有顯著差異")
                    }
                  } else {
                    if(t.test(AD[which(AD[,which(colnames(AD)==input$TYPE)]==1),which(colnames(AD) == input$vvrr8)],
                              AD[which(AD[,which(colnames(AD)==input$TYPE)]==2),which(colnames(AD) == input$vvrr8)])$p.value[[1]] <= 0.05){
                      paste0(input$TYPE,"在",input$vvrr8,"上有顯著差異")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  output$P08 <- renderPrint({
    if(input$BorA2 == "前測"){
      if(input$y == "陽虛體質"){
        BDv = data.frame(BD[,which(colnames(BD) == input$y1)],BD[,14:17])
        colnames(BDv) = c('y','x1','x2','x3','x4')
        fit <- lm(y ~ x1 * x2 * x3 * x4 , data = BDv)
        summary(stepAIC(fit, direction="both"))
      } else {
        if(input$y == "陰虛體質"){
          BDv = data.frame(BD[,which(colnames(BD) == input$y2)],BD[,14:17])
          colnames(BDv) = c('y','x1','x2','x3','x4')
          fit <- lm(y ~ x1 * x2 * x3 * x4 , data = BDv)
          summary(stepAIC(fit, direction="both"))
        } else {
          BDv = data.frame(BD[,which(colnames(BD) == input$y3)],BD[,14:17])
          colnames(BDv) = c('y','x1','x2','x3','x4')
          fit <- lm(y ~ x1 * x2 * x3 * x4 , data = BDv)
          summary(stepAIC(fit, direction="both"))
        }
      }
    } else {
      if(input$y == "陽虛體質"){
        ADv = data.frame(AD[,which(colnames(AD) == input$y1)],AD[,14:17])
        colnames(ADv) = c('y','x1','x2','x3','x4')
        fit <- lm(y ~ x1 * x2 * x3 * x4 , data = ADv)
        summary(stepAIC(fit, direction="both"))
      } else {
        if(input$y == "陰虛體質"){
          ADv = data.frame(AD[,which(colnames(AD) == input$y2)],AD[,14:17])
          colnames(ADv) = c('y','x1','x2','x3','x4')
          fit <- lm(y ~ x1 * x2 * x3 * x4 , data = ADv)
          summary(stepAIC(fit, direction="both"))
        } else {
          ADv = data.frame(AD[,which(colnames(AD) == input$y3)],AD[,14:17])
          colnames(ADv) = c('y','x1','x2','x3','x4')
          fit <- lm(y ~ x1 * x2 * x3 * x4 , data = ADv)
          summary(stepAIC(fit, direction="both"))
        }
      }
    }
  })
})
