##############################################################
#         Install missing packages and load library          #
##############################################################
if(require("DT")==FALSE){install.packages("DT")}
library(DT)
if(require("ggplot2")==FALSE){install.packages("ggplot2")}
library(ggplot2)
if(require("Amelia")==FALSE){install.packages("Amelia")}
library(Amelia)
if(require("arules")==FALSE){install.packages("arules")}
library(arules)
if(require("arulesViz")==FALSE){install.packages("arulesViz")}
library(arulesViz)
##############################################################

#Read dataset as transaction
tran1 <- read.transactions("files/1000-out1.csv", format ="basket",  sep = ",", rm.duplicates = TRUE, cols=1)

#create vector for item names according item ID
myLabels <- c("Chocolate Cake","Lemon Cake", "Almond Tart", "Apple Pie","Apple Tart",
              "Apricot Tart", "Berry Tart", "Blackberry Tart", "Blueberry Tart","Chocolate Tart", 
              "Cherry Tart", "Lemon Tart","Casino Cake","Pecan Tart", "Ganache Cookie",
              "Gongolais Cookie", "Raspberry Cookie", "Lemon Cookie","Chocolate Meringue","Vanilla Meringue",
              "Marzipan Cookie", "Tuile Cookie", "Walnut Cookie","Opera Cake","Almond Croissant", 
              "Apple Croissant", "Apricot Croissant", "Cheese Croissant", "Chocolate Croissant","Apricot Danish",
              "Apple Danish","Almond Twist","Almond Bear Claw", "Blueberry Danish",  "Strawberry Cake",
              "Lemon Lemonade", "Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water",
              "Hot Coffee","Chocolate Coffee","Vanilla Frappuccino", "Cherry Soda", "Single Espresso",
              "Truffle Cake", "Chocolate Eclair", "Coffee Eclair", "Vanilla Eclair", "Napoleon Cake")

#label item name to tran1
itemInfo(tran1) <- data.frame(labels = myLabels)

#convert transaction data as dataframe
itemdata = as(tran1, "data.frame")

#exchange itemdata column position
itemdata <- itemdata[c("transactionID","items")]


#create item references list
itemslist <- matrix(c(0:9,"Chocolate Cake","Lemon Cake", "Casino Cake","Opera Cake","Strawberry Cake","Truffle Cake", "Chocolate Eclair", "Coffee Eclair", "Vanilla Eclair", "Napoleon Cake",
                      10:19,"Almond Tart", "Apple Pie","Apple Tart","Apricot Tart", "Berry Tart", "Blackberry Tart", "Blueberry Tart","Chocolate Tart","Cherry Tart", "Lemon Tart",
                      20:29,"Pecan Tart", "Ganache Cookie","Gongolais Cookie", "Raspberry Cookie", "Lemon Cookie","Chocolate Meringue","Vanilla Meringue","Marzipan Cookie", "Tuile Cookie", "Walnut Cookie",
                      30:39,"Almond Croissant","Apple Croissant", "Apricot Croissant", "Cheese Croissant", "Chocolate Croissant","Apricot Danish","Apple Danish","Almond Twist","Almond Bear Claw", "Blueberry Danish",  
                      40:49,"Lemonade", "Raspberry Lemonade","Orange Juice","Green Tea","Bottled Water","Hot Coffee","Chocolate Coffee","Vanilla Frappuccino", "Cherry Soda", "Single Espresso"), ncol = 10)
colnames(itemslist) <- c("Item ID","Item Name","Item ID","Item Name","Item ID","Item Name","Item ID","Item Name","Item ID","Item Name")

#find association rule with default setting
tran1rules0 <- apriori(tran1) 


#Start Server Setting
server <- function(input, output,session) {
  
  #Render Table
  #Item List
  output$itemsTable <- renderTable(
    itemslist,
    width = "auto",
    align = "c"
  )
  
  #Item Dataset
  output$itemsDataset <- renderDataTable(
    itemdata
  )
  
  #Item Frequence
  output$feqPlot <- renderPlot({
    itemFrequencyPlot(tran1, 
      topN = input$feqTopN, 
      support = input$feqSupport
    )
  })
  output$feq_plot_clickInfo <- renderText({
    paste0(input$feq_plot_click$y)
  })
  
  #Get Interesting Rules
  output$rInterestRules <- renderDataTable({
    tran1rules <- apriori(
      tran1,
      parameter = list(
        sup = 0.03, 
        conf = 0.9, 
        minlen=2,
        target="rules")
    )
    subset.matrix <- is.subset(tran1rules, tran1rules)
    subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >=1
    rules.pruned <- tran1rules[!redundant]
    
    if(input$rbtnRules == "default"){
      tran1rules0 = as((tran1rules0),"data.frame")
      tran1rules0
    }
    else if(input$rbtnRules == "wredundant"){
      tran1rules = as((tran1rules),"data.frame")
      tran1rules["rules"]
    }
    else if(input$rbtnRules == "woredundant"){
      rules.pruned = as((rules.pruned),"data.frame")
      rules.pruned["rules"]
    }
  })
  
  #Rules
  observe({
    if(input$rbtnRules == "default") {
      session$sendCustomMessage(type="jsCode",
                                list(code= "
                                     $('#rConf').prop('disabled',true);
                                     $('#rSupport').prop('disabled',true);
                                     $('#rMinLen').prop('disabled',true);
                                "))
    } else {
      session$sendCustomMessage(type="jsCode",
                                list(code= "
                                     $('#rConf').prop('disabled',false);
                                     $('#rSupport').prop('disabled',false);
                                     $('#rMinLen').prop('disabled',false);
                                "))
    }
  })
  #Get Rules View
  output$rRules <- renderDataTable({
    tran1rules <- apriori(
      tran1,
      parameter = list(
        sup = input$rSupport, 
        conf = input$rConf, 
        minlen=input$rMinLen,
        target="rules")
    )
    subset.matrix <- is.subset(tran1rules, tran1rules)
    subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >=1
    rules.pruned <- tran1rules[!redundant]
    
    if(input$rbtnRules == "default"){
      tran1rules0 = as((tran1rules0),"data.frame")
      tran1rules0
    }
    else if(input$rbtnRules == "wredundant"){
      tran1rules = as((tran1rules),"data.frame")
      tran1rules["rules"]
    }
    else if(input$rbtnRules == "woredundant"){
      rules.pruned = as((rules.pruned),"data.frame")
      rules.pruned["rules"]
    }
  })
  
  #Get Rules Summary
  output$rSummary <- renderPrint({
    #Setting Rules
    tran1rules <- apriori(
      tran1,
      parameter = list(
        sup = input$rSupport, 
        conf = input$rConf, 
        minlen=input$rMinLen,
        target="rules")
    )
    subset.matrix <- is.subset(tran1rules, tran1rules)
    subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >=1
    rules.pruned <- tran1rules[!redundant]
    
    if(input$rbtnRules == "default"){
      summary(tran1rules0)
    }
    else if(input$rbtnRules == "wredundant"){
      summary(tran1rules)
    }
    else if(input$rbtnRules == "woredundant"){
      summary(rules.pruned)
    }
  })
  
  #Get Rules Inspect
  output$rInspect <- renderPrint({
    #Setting Rules
    tran1rules <- apriori(
      tran1,
      parameter = list(
        sup = input$rSupport, 
        conf = input$rConf, 
        minlen=input$rMinLen,
        target="rules")
    )
    subset.matrix <- is.subset(tran1rules, tran1rules)
    subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >=1
    rules.pruned <- tran1rules[!redundant]
    
    if(input$rbtnRules == "default"){
      inspect(tran1rules0)
    }
    else if(input$rbtnRules == "wredundant"){
      if(input$rbtnInspectType == "default"){
        inspect(tran1rules)
      }
      else{ 
        inspect(sort(tran1rules, by=input$rbtnInspectType, decreasing= input$rbtnInspectOrder))
      }
    }
    else if(input$rbtnRules == "woredundant"){
      if(input$rbtnInspectType == "default"){
        inspect(rules.pruned)
      }
      else{
        inspect(sort(rules.pruned, by=input$rbtnInspectType, decreasing= input$rbtnInspectOrder))
      }
    }
  })
  
  #Get Rules Plot
  output$rulesPlot <- renderPlot({
    
    #Setting Rules
    tran1rules <- apriori(
      tran1,
      parameter = list(
        sup = input$rSupport, 
        conf = input$rConf, 
        minlen=input$rMinLen,
        target="rules")
    )
    subset.matrix <- is.subset(tran1rules, tran1rules)
    subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
    redundant <- colSums(subset.matrix, na.rm=T) >=1
    rules.pruned <- tran1rules[!redundant]
    
    if(input$rbtnRules == "default"){
      plot(tran1rules0)
    }
    else if(input$rbtnRules == "wredundant"){
      if(input$rbtnPlotsType == "default"){
        if(input$rbtnPlotsControl == "default"){
          plot(tran1rules)
        }
        else if(input$rbtnPlotsControl == "k"){
          plot(tran1rules, control = list(k = 5))
        }
        else if(input$rbtnPlotsControl == "type"){
          plot(tran1rules, control=list(type="items"))
        }
        else if(input$rbtnPlotsControl == "alpha"){
          plot(tran1rules, control=list(alpha=.5, reorder=TRUE))
        }
      }
      else{
        if(input$rbtnPlotsControl == "default"){
          plot(tran1rules, method = input$rbtnPlotsType)
        }
        else if(input$rbtnPlotsControl == "k"){
          plot(tran1rules, method = input$rbtnPlotsType, control = list(k = 5))
        }
        else if(input$rbtnPlotsControl == "type"){
          plot(tran1rules, method=input$rbtnPlotsType, control=list(type="items"))
        }
        else if(input$rbtnPlotsControl == "alpha"){
          plot(tran1rules, method=input$rbtnPlotsType,  control=list(alpha=.5, reorder=TRUE))
        }
      }
        
    }
    else if(input$rbtnRules == "woredundant"){
      if(input$rbtnPlotsType == "default"){
        if(input$rbtnPlotsControl == "default"){
          plot(rules.pruned)
        }
        else if(input$rbtnPlotsControl == "k"){
          plot(rules.pruned, control = list(k = 5))
        }
        else if(input$rbtnPlotsControl == "type"){
          plot(rules.pruned, control=list(type="items"))
        }
        else if(input$rbtnPlotsControl == "alpha"){
          plot(rules.pruned, control=list(alpha=.5, reorder=TRUE))
        }
      }
      else{
        if(input$rbtnPlotsControl == "default"){
          plot(rules.pruned, method = input$rbtnPlotsType)
        }
        else if(input$rbtnPlotsControl == "k"){
          plot(rules.pruned, method = input$rbtnPlotsType, control = list(k = 5))
        }
        else if(input$rbtnPlotsControl == "type"){
          plot(rules.pruned, method=input$rbtnPlotsType, control=list(type="items"))
        }
        else if(input$rbtnPlotsControl == "alpha"){
          plot(rules.pruned, method=input$rbtnPlotsType,  control=list(alpha=.5, reorder=TRUE))
        }
      }
      
    }
  })
  
  #About Us
  output$Ben <- renderImage({
      return(list(
        src = "files/Ben.jpg",
        filetype = "image/jpeg",
        alt = "This is a Benjamin"
      ))
  }, deleteFile = FALSE)
  output$Mah <- renderImage({
    return(list(
      src = "files/Mah.jpg",
      filetype = "image/jpeg",
      alt = "This is a Siew Chin"
    ))
  }, deleteFile = FALSE)
  output$Ronald <- renderImage({
    return(list(
      src = "files/Ronald.jpg",
      filetype = "image/jpeg",
      alt = "This is a Soon Kit"
    ))
  }, deleteFile = FALSE)
  output$CR <- renderImage({
    return(list(
      src = "files/CR.jpg",
      filetype = "image/jpeg",
      alt = "This is a Chong Raen"
    ))
  }, deleteFile = FALSE)
  
}

