library(shiny)
library(readxl)
library(drc)
library(multcomp)
library(xtable)


server = function(input, output, session){
  
  #-------------- read data--------------#
  myData = reactive({
    
    if(is.null(input$file1))
      return()
    
    inFile = input$file1
    if (is.null(inFile)) return(NULL)
    data1 = read_xlsx(inFile$datapath)
    names(data1) = c("Concentration","M","M","M","P","P","P",
                    "M+P","M+P","M+P","DMSO1","DMSO2","DMSO3")
    data1$DMSO_average = rowMeans(data1[,c("DMSO1","DMSO2","DMSO3")])
    data1
  })
  
  #-------------- Model&Plot --------------#
  myPlot = reactive({
    
    if(is.null(myData()))
      return()
    
    plate = myData()[,c(1:10,14)]
    plate = as.data.frame(plate)
    
    p = c()
    for (i in 1:nrow(plate)){
      for(j in 1:(ncol(plate)-2)){
        p = rbind(p,c(
          concentration = plate[i,1], 
          drug = names(plate)[j+1], 
          cell_count = plate[i,j+1],
          viability = plate[i,j+1]/plate[i,ncol(plate)]))
      }
    }
    p1 = as.data.frame(p)
    
    dr_name = c("M","P","M+P")
    p1$drug = factor(p1$drug,levels = dr_name,labels = dr_name)
    p1$concentration = as.numeric(as.character(p1$concentration))
    p1$viability = as.numeric(as.character(p1$viability))
    # Fitting a curve for both drugs sharing same upper bound
    p1_model1 = drm(viability ~ concentration, drug,data = p1, 
                    fct = LL.4(names = c("h","lower","upper","IC50")),
                    pmodels = list(~drug-1,~drug-1,~drug-1,~drug-1))
    
    summary = data.frame(drug = dr_name,
                         IC50 = p1_model1$coefficients[paste0("IC50:drug",dr_name)],
                         upper = p1_model1$coefficients[paste0("upper:drug",dr_name)],
                         lower = p1_model1$coefficients[paste0("lower:drug",dr_name)])
    summary$halfway = (summary$upper+summary$lower)/2
    rownames(summary)=summary$drug
    
    # Setup contrasts for multi-comparison:1)M vs M+P 2)P vs M+P
    K = rbind(c(0,0,0,0,0,0,0,0,0,1,0,-1),
              c(0,0,0,0,0,0,0,0,0,0,1,-1))
    rownames(K) = c('M vs M+P','P vs M+P')
    comparer = summary(glht(p1_model1, linfct = K, alternative="greater"))
    pv = round(comparer$test$pvalues,4)[1:2]
    names(pv) = c('M vs M+P','P vs M+P')
    
    # plot curves
    IC50print = paste0(summary$drug,"=",round(summary$IC50,3))
    IC50print = paste(IC50print,collapse=", ")
    IC50print = paste0("IC50: ",IC50print)
    
    pv_print = paste0("M vs M+P: p = ",pv[1],"\nP vs M+P: p = ",pv[2])
    
    result = list(p1_model1 = p1_model1, dr_name = dr_name,
                  IC50print = IC50print, pv_print = pv_print,
                  summary = summary)
    result
    
  })
  
  
  #---------- render to output ----------#
  
  output$originalTable = renderTable({
    myData()
  }, digits = 5,striped = T,bordered = T,
  caption = "<b>Uploaded data: </b>",caption.placement = getOption("xtable.caption.placement", "top"))

  
  output$plot = renderPlot({
    
    if(is.null(myPlot()))
      return()
    
    plt = myPlot()
    p1_model1 = plt$p1_model1
    dr_name = plt$dr_name
    IC50print = plt$IC50print
    pv_print = plt$pv_print
    summary = plt$summary
    plot(p1_model1,ylab="Relative viability",level = dr_name,col = c("red","blue","black"),lwd = 3,
               xlab = paste0("concentration (uM)\n",IC50print),main="Dose-response curve") +
      text(0.0001,0.4,pv_print) + 
      lines(x=rep(summary[dr_name[1],'IC50'],50),y=seq(0,summary[dr_name[1],'halfway'],length.out = 50),lty=1,lwd=3,col="red") + 
      lines(x=rep(summary[dr_name[2],'IC50'],50),y=seq(0,summary[dr_name[2],'halfway'],length.out = 50),lty=2,lwd=3,col="blue") + 
      lines(x=rep(summary[dr_name[3],'IC50'],50),y=seq(0,summary[dr_name[3],'halfway'],length.out = 50),lty=3,lwd=3,col="black")
    
  })
    
  output$down <- downloadHandler(
    filename =  "Dose response curve & IC50.pdf",
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      pdf(file, width = 6, height = 5) # open the pdf device
      plt = myPlot()
      p1_model1 = plt$p1_model1
      dr_name = plt$dr_name
      IC50print = plt$IC50print
      pv_print = plt$pv_print
      summary = plt$summary
      plot(p1_model1,ylab="Relative viability",level = dr_name,col = c("red","blue","black"),lwd = 3,
           xlab = paste0("concentration (uM)\n",IC50print),main="Dose-response curve") + ylim(c(0,1.2)) + 
        text(0.0001,0.6,pv_print) + 
        lines(x=rep(summary[dr_name[1],'IC50'],50),y=seq(0,summary[dr_name[1],'halfway'],length.out = 50),lty=1,lwd=3,col="red") + 
        lines(x=rep(summary[dr_name[2],'IC50'],50),y=seq(0,summary[dr_name[2],'halfway'],length.out = 50),lty=2,lwd=3,col="blue") + 
        lines(x=rep(summary[dr_name[3],'IC50'],50),y=seq(0,summary[dr_name[3],'halfway'],length.out = 50),lty=3,lwd=3,col="black")
      
      dev.off()  # turn the device off
      
    } 
  )

  
}




