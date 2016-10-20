#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(openxlsx)
library(RColorBrewer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    col = brewer.pal(4, "Set2")
    
    data = reactive({
        infile = input$file
        
        if (is.null(infile)) {
            return(NULL)
        }
        r = read.xlsx(infile$datapath, sheet=1, colNames=T)
        r = r[,c(3,5:8)]
        print(infile$name)
        r
    })
    
    results = reactive({
        if (is.null(data())) {
            return(NULL)
        }
        res = data.frame(StartTime.min=as.numeric(input$start),
                         M1=0, B1=0, SSR1=0,
                         M2=0, B2=0, SSR2=0,
                         M3=0, B3=0, SS3=0,
                         M4=0, B4=0, SS4=0)
        
        se = seq(as.numeric(input$start),
                 max(data()[,1]),
                 (as.numeric(input$step) + as.numeric(input$buff))*60)
        
        for (i in 1:(length(se)-1)) {
            l = which(data()[,1] >= se[i] & data()[,1] < se[i] + as.numeric(input$step)*60)
            
            dat = data()[l, 2]
            t = data()[l, 1]
            fit = lm( as.numeric(dat) ~ as.numeric(t) )
            
            dat = data()[l, 3]
            t = data()[l, 1]
            fit2 = lm( as.numeric(dat) ~ as.numeric(t) )
            
            dat = data()[l, 4]
            t = data()[l, 1]
            fit3 = lm( as.numeric(dat) ~ as.numeric(t) )
            
            dat = data()[l, 5]
            t = data()[l, 1]
            fit4 = lm( as.numeric(dat) ~ as.numeric(t) )
            
            res = rbind(res, c(se[i]/60, coefficients(fit)[[2]], coefficients(fit)[[1]], sum(fit$residuals^2),
                               coefficients(fit2)[[2]], coefficients(fit2)[[1]], sum(fit2$residuals^2),
                               coefficients(fit3)[[2]], coefficients(fit3)[[1]], sum(fit3$residuals^2),
                               coefficients(fit4)[[2]], coefficients(fit4)[[1]], sum(fit4$residuals^2))
            )
        }
        res[-1,]
    })
    
    output$preview2 = renderDataTable({
        
        if (is.null(results())) {
            return(data.frame(StartTime=NA, m1=NA, b1=NA, R=NA))
        }
        results()
    })
    
    output$pre = renderPlot({
        d = data()
        
        if(is.null(d)) {
            return( {plot(NULL,NULL,xlim=c(0,1), ylim=c(0,1))
                text(0.5,0.5,"No Data Loaded", cex=3)} )
        }
        
        plot(d[,1]/60, d[,2], type='l', xlab="Time (min)", ylab="Signal", lwd=3)
        lines(d[,1]/60, d[,3], col=col[1], lwd=3)
        lines(d[,1]/60, d[,4], col=col[2], lwd=3)
        lines(d[,1]/60, d[,5], col=col[3], lwd=3)
        legend(0, max(d[,2]), c("Ch1", "Ch2", "Ch3", "Ch4"), col=col[1:4], lty=1, lwd=3)
    })
    
    output$pre2 = renderPlot({
        r = results()
        
        if(is.null(r)) {
            return( {plot(NULL,NULL,xlim=c(0,1), ylim=c(0,1))
                text(0.5,0.5,"No Data Loaded", cex=3)} )
        }
        if (input$cSelect == "Ch1") {
            d = data()[,2]
            m = r[,2]
            b = r[,3]
            color = col[1]
        }
        if (input$cSelect == "Ch2") {
            d = data()[,3]
            m = r[,5]
            b = r[,6]
            color = col[2]
        }
        if (input$cSelect == "Ch3") {
            d = data()[,4]
            m = r[,8]
            b = r[,9]
            color = col[3]
        }
        if (input$cSelect == "Ch4") {
            d = data()[,5]
            m = r[,11]
            b = r[,12]
            color = col[4]
        }
        
        plot(data()[,1]/60, d, type='l', col="#000000",
             xlab="Time (min)", ylab="Signal", main=paste(input$cSelect, "Results"), lwd=3)
        for (i in 1:nrow(r)) {
            x = c((60*r[i,1]):(60*r[i,1]+as.numeric(input$step)*60))
            lines( x/60, m[i]*(x)+b[i], col=color, lwd=2)
        }
        legend(0, max(d), c(input$cSelect, paste("Fits for", input$cSelect)),
               col=c("#000000", color), lty=c(1,1), lwd=3)
        
    })
    
    output$preview = renderDataTable({ 
        data()[1:10,]
    })
    
    output$go = downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv")
        },
        content = function(con) {
            write.csv(results(), con)
        }
    )
    
    output$saveIMG = downloadHandler({
        filename = function() {
            paste("plots-", Sys.Date(), ".pdf")
        }
        content <- function(filename) {
            pdf(filename)
            output$pre()
            output$pre2()
            dev.off()
        }
    }
    )
    
})
