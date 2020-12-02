#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Author: Nick Keur

library(shiny)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(rstatix)
library(tidyverse)
library(plotly)
library(kml3d)
library(gplots)
library(ComplexHeatmap)
library(rsconnect)
library(circlize)

#Only for local use
setwd("~/R-cytokine-analysis/Cytokine-analysis12/")

# Read filtered and processed data
data.cc <- read.table(file = 'data/cytokine_2008-2019_sex_filtered3.csv', header = T, 
                      colClasses = c("Year" = "factor", "ID_bosw"="factor"))

    
data <- data.cc
                
    ##########################################
### Following code is used for the GUI ###
##########################################

shinyApp(
    ui = tagList(
        navbarPage(
            #theme = "cerulean",  # <--- To use a theme, uncomment this
            "Cytokine Analysis",
            tabPanel("Table",
                     sidebarPanel(width = 3,
                        conditionalPanel(condition="input.conditionedPanels == 1",
                            selectInput(inputId = "Medium_nav1",
                                        label = "Medium:",
                                        choices = c("All",unique(as.character(data$Medium)))),
                            selectInput(inputId = "Year_nav1",
                                        label = "Year:",
                                        choices = c("All",unique(as.character(data$Year)))),
                            selectInput(inputId = "Cytokine_nav1",
                                        label = "Cytokine:",
                                        choices = c("All",unique(as.character(data$Cytokine)))),
                            selectInput(inputId = "ID_bosw_nav1",
                                        label = "ID:",
                                        choices = c("All",unique(as.character(data$ID_bosw)))))
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Table:", value=1,
                                      h4("Table"),
                                      DT::dataTableOutput("table")),
                             #tabPanel("Summary", value=2,
                              #        verbatimTextOutput("summary_txt")),
                             id = "conditionedPanels")
                     )
            ),
            
            tabPanel("Distribution Analysis", 
                     sidebarPanel(width = 3,
                                  
                conditionalPanel(condition="input.conditionedPanels2==3",
                    selectInput(inputId = "Medium_nav2",
                                label = "Medium:",
                                selected = "B.B.",
                                choices = c("All",unique(as.character(data$Medium)))),
                    selectInput(inputId = "Year_nav2",
                                label = "Year:",
                                selected = "2008",
                                choices = c("All",unique(as.character(data$Year)))),
                    # selectInput(inputId = "dataType_nav2",
                    #             label = "Data Type:",
                    #             choices = c("Raw","Standarised_by_RPMI")),
                    selectInput(inputId = "dataTrans_nav2",
                                label = "Transformation:",
                                choices = c("None","Log2","Log10","Inv_Rank"))),
                
                conditionalPanel(condition="input.conditionedPanels2==4",
                    selectInput(inputId = "Year2_nav2",
                                label = "Year:",
                                selected = "2008",
                                choices = c("All",unique(as.character(data$Year)))),
                    selectInput(inputId = "dataTrans2_nav2",
                                label = "Transformation:",
                                choices = c("None","Log2","Log10","Inv_Rank")))
                ),
                
                mainPanel(
                    tabsetPanel(
                        tabPanel("Histogram", value=3,
                                 plotOutput('histPlot',height="800")),
                        tabPanel("Density plot", value=3,
                                 plotOutput('densityPlot', height="800")),
                        tabPanel("QQplot", value = 3,
                                 plotOutput('qqPlot', height="800")),
                        tabPanel("Normality matrix", value=4,
                                 plotOutput('normalMatrixPlot', height="800")),
                        id = "conditionedPanels2")),
            ),
            
            tabPanel("Variation Analysis", 
                     sidebarPanel(
                         conditionalPanel(condition="input.conditionedPanels3==3",
                         selectInput(inputId = "Medium_nav5",
                                     label = "Medium:",
                                     selected = "B.B.",
                                     choices = c("All",unique(as.character(data$Medium)))),
                         selectInput(inputId = "Cytokine_nav5",
                                     label = "Cytokine:",
                                     choices = c("All",unique(as.character(data$Cytokine)))),
                         selectInput(inputId = "dataType_nav5",
                                     label = "Data Type:",
                                     choices = c("Raw","Standarised_by_RPMI")),
                         selectInput(inputId = "dataTrans_nav5",
                                     label = "Transformation:",
                                     choices = c("None","Log2","Log10","Inv_Rank"))),
                         
                         conditionalPanel(condition="input.conditionedPanels3==4",
                         sliderInput(inputId = 'bins',
                                     label = 'Sample Size', 
                                     min=1,
                                     max=11,
                                     value=3),
                         selectInput(inputId = "Medium_nav3",
                                     label = "Medium:",
                                     selected = "B.B.",
                                     choices = c("All",unique(as.character(data$Medium)))),
                         selectInput(inputId = "Cytokine_nav3",
                                     label = "Cytokine:",
                                     choices = c("All",unique(as.character(data$Cytokine)))),
                         selectInput(inputId = "dataType_nav3",
                                     label = "Data Type:",
                                     choices = c("Raw","Standarised_by_RPMI")),
                         selectInput(inputId = "dataTrans_nav3",
                                     label = "Transformation:",
                                     choices = c("None","Log2","Log10")),
                         selectInput(inputId = "dataSort_nav3",
                                     label = "Sorting:",
                                     choices = c("Unsorted","Median","IQR")))
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Variation by year", value=3,
                                      plotOutput('boxxPlot1', height="800")),
                             tabPanel("Individual variation", value=4,
                                      plotOutput('boxxPlot2', height = "800")),
                             id = "conditionedPanels3")),
                     ),
            
            tabPanel("Correlation analysis", 
                     sidebarPanel(
                         selectInput(inputId = "Year_nav4",
                                     label = "Year:",
                                     choices = c(unique(as.character(data$Year)))),
                         checkboxInput(inputId = "cluster_nav4",
                                     label = "Perform clustering",
                                     value = FALSE)),
                     mainPanel(tabsetPanel(
                         tabPanel("Correlation Plot",
                                      plotOutput('corrPlot', height = "800")),
                         tabPanel("Cluster Correlation Plot", 
                                      plotOutput('clustercorrPlot', height="800")))
                     ),
            ),
            
            tabPanel("Trajectory Analysis", 
                     sidebarPanel(
                         selectInput(inputId = "Medium_traj",
                                     label = "Medium:",
                                     selected = "B.B.",
                                     choices = c("All",unique(as.character(data$Medium)))),
                         selectInput(inputId = "Traj_nav", 
                                     label = "Year:",
                                     selected = c(unique(as.character(data$Year))),
                                     choices = c(unique(as.character(data$Year))),
                                     multiple=TRUE),
                         sliderInput(inputId = 'Bins_traj',
                                     label = 'Minimum Occurences:', 
                                     min=1,
                                     max=11,
                                     value=3),
                         selectInput(inputId = "dataTrans_traj",
                                     label = "Transformation:",
                                     choices = c("None","Log2"))),
                     
                     mainPanel(tabsetPanel(
                         tabPanel("Bar Plot:",
                                  h4("Bar Plot"),
                                  plotOutput('barPlot', height = "800")),
                         tabPanel("Line Plot", 
                                  h4("Bar Plot"),
                                  plotOutput('linePlot', height = "800")),
                         tabPanel("Kmeans cluster", 
                                  h4("Cluster Plot"),
                                  plotOutput('clusterPlot', height = "800")))
                     ),
            ),
            
            tabPanel("Trend Analysis", 
                     sidebarPanel(
                         selectInput(inputId = "medium_trend1",
                                     label = "Medium:",
                                     selected = c("B.B.","LPS"),
                                     choices = c("CA","LPS","B.B.","MTB")),
                         selectInput(inputId = "cytokine_trend1",
                                     label = "Cytokine:",
                                     selected = c(unique(as.character(data$Cytokine))),
                                     choices = c(unique(as.character(data$Cytokine))),
                                     multiple=TRUE),
                         selectInput(inputId = "year_trend1",
                                     label = "Year:",
                                     selected = c(2016,2017,2018),
                                     choices = c(unique(as.character(data$Year))),
                                     multiple=TRUE),
                     selectInput(inputId = "dataTrans_trend1",
                                 label = "Transformation:",
                                 choices = c("None","Log2"))),
                     
                     mainPanel(tabsetPanel(
                         tabPanel("Yearly trend:",
                                  h4("Bar Plot"),
                                  plotOutput('trendPlot1', height = "800")),
                         tabPanel("Cytokine trend:", 
                                  h4("Bar Plot"),
                                  plotOutput('trendPlot2', height = "800")),
                         tabPanel("Individual trend",
                                  h4("Cluster Plot"),
                                  plotOutput('trendPlot3', height = "800")))
                     ),
            )
            
            # tabPanel("cQTL Analysis", 
            #          sidebarPanel(
            #              selectInput(inputId = "year_qtl1",
            #                          label = "Year:",
            #                          selected = c(2016),
            #                          choices = c(unique(as.character(data$Year))),
            #                          multiple=FALSE)),
            #          
            #          mainPanel(tabsetPanel(
            #              tabPanel("Table:",
            #                       plotOutput("qtlBoxPlot", height = "400")),
            #                       DT::dataTableOutput("qtltable"))
            #          ),
            # )
            
            )
    ),
    
##########################################
### Following code is used to plot data###
##########################################

    function(input, output) {
         # Output is an table
        output$table <- DT::renderDataTable(DT::datatable({ 
            data <- data.cc
            # Only select the following colum for the table (Long format)
            data <- data[,c("ID_bosw","GenomeID","Birthdate","Age","Medium","Cytokine","Year","Value"),]
            
            # Filter data based on selections
            if (input$Medium_nav1 != "All") {
                data <- data[data$Medium == input$Medium_nav1,]
            }
            if (input$Cytokine_nav1 != "All") {
                data <- data[data$Cytokine == input$Cytokine_nav1,]
            }
            if (input$Year_nav1 != "All") {
                data <- data[data$Year == input$Year_nav1,]
            }
            if (input$ID_bosw_nav1 != "All") {
                data <- data[data$ID_bosw == input$ID_bosw_nav1,]
            }
            data
            
        },options = list(pageLength = 250)))
        
########################################
# QTL analysis is currently disabled ###
########################################
        
        output$qtltable <- DT::renderDataTable(DT::datatable({ 
            data_qtl_hits <- read.table("data/qtl/result_2016.txt", header = 1,sep = "\t")
            if (input$year_qtl1 == "2016") {
                data_qtl_hits <- data_qtl_hits %>% separate(gene, c("Medium","Cytokine"), sep = "__")
                }
            data_qtl_hits
        },options = list(pageLength = 10, dom = 'tp'),selection = list(selection = 'single',selected = 1)))
        
        output$qtlBoxPlot <- renderPlot({
            data2 <- read.table("data/qtl/data_vcfname_16-17-18.txt",header = 1,check.names = F, sep = "\t")
            qtl_data<- read.table("data/qtl/qtlsub_log2_2016_IL8.txt",header = 1,check.names = F, sep = "\t")
            
            tqtldata <- as.data.frame(t(qtl_data))
            tqtldata <- tibble::rownames_to_column(tqtldata, "VCF_name")
            
            tt2 <- full_join(data2, tqtldata, by = c("VCF_name"), copy = FALSE)
            s <- input$qtltable_rows_selected
            
            qtl_snp<- data_qtl_hits[s,"SNP"]
            qtl_med<- data_qtl_hits[s,"Medium"]
            qtl_cyt<- data_qtl_hits[s,"Cytokine"]
            
            if (input$year_qtl1 == "2016") {
                tt2 <- tt2 %>% 
                    filter(Year %in% input$year_qtl1) %>%
                    filter(Medium == qtl_med) %>%
                    filter(Cytokine == qtl_cyt)
            }

            ggplot(subset(tt2, Year == input$year_qtl1), aes_string(x = tt2[[qtl_snp]], y = (log2(tt2$Value)))) +
                geom_point(aes(colour=Gender)) +
                geom_smooth(method='lm', formula= y~x) +
                xlab("Genotype") + ylab("Log2(Value)")
        })
########################################
########## Box plot ####################
########################################
        
        output$boxxPlot1 <- renderPlot({
            #Create fresh subset
            data <- data.cc
            
            # Filter data based on selections
            if (input$Medium_nav5 != "All") {
                data <- data[data$Medium == input$Medium_nav5,]
            }
            if (input$Cytokine_nav5 != "All") {
                data <- data[data$Cytokine == input$Cytokine_nav5,]
            }
            if (input$dataTrans_nav5 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_nav5 == "Log10") {
                data$Value <- log10(data$Value)
            }
            
            if (input$dataType_nav5 == "Standarised_by_RPMI"){
                data$Value <- data$Div_med
            }
            
            # Visualize the subset using the filters
            ggplot(data, aes(x=Year, y=Value, fill=Cytokine)) + 
                geom_boxplot() + facet_wrap(~Cytokine, scale="free",ncol = 2) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        )
        
########################################
########## Box plot ####################
########################################
        output$boxxPlot2 <- renderPlot({
            
            # Filter data based on selections
            if (input$Medium_nav3 != "All") {
                data <- data[data$Medium == input$Medium_nav3,]
            }
            if (input$Cytokine_nav3 != "All") {
                data <- data[data$Cytokine == input$Cytokine_nav3,]
            }
            # if (input$dataType_nav3 == "Raw"){
            #     data
            # }
            # if (input$dataType_nav3 == "Standarised_by_RPMI"){
            #     data$Value <- data$Div_med
            # }
            # 
            if (input$dataTrans_nav3 == "None") {
                data
            }
            if (input$dataTrans_nav3 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_nav3 == "Log10") {
                data$Value <- log10(data$Value)
            }
            data <- data %>%
                group_by(ID_bosw) %>%
                filter(length(unique(Year)) >= input$bins )
            
            # Visualize the unsorted boxplot
            if (input$dataSort_nav3 == "Unsorted"){
            p <- ggplot(data, aes(x=ID_bosw, y=Value, fill=Gender)) + 
                geom_boxplot() +
                facet_wrap(~Cytokine, scale="free", ncol = 2) +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
                print(p)
            }
            # Sort the boxplot based on Median
            if (input$dataSort_nav3 == "Median"){
                data <- data %>%
                    as.tibble()
            p <- ggplot(data, aes(reorder(ID_bosw, Value, median, na.rm=TRUE), Value, fill=Gender)) + 
                    geom_boxplot() +
                    facet_wrap(~Cytokine, scale="free", ncol = 2) +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1))
            print(p)
            }
            # Sort the boxplot based on IQR
            if (input$dataSort_nav3 == "IQR"){
            data <- data %>%
                    as.tibble()
            p <- ggplot(data, aes(reorder(ID_bosw, Value, IQR), Value, fill=Gender)) + 
            geom_boxplot()
            print(p)
            }

        })
########################################
####### Correlation plot ###############
########################################
        output$corrPlot <- renderPlot({
            # Select fresh data set
            data <- data.cc
            #Filter based on Year
            data <- data[data$Year == input$Year_nav4,]
            
            #Transform data to wide format
            data$combined <- paste(data$Medium,"__",data$Cytokine)
            data$combined <- gsub(" ", "", data$combined)
            data <- data[,c("ID_bosw","combined","Value")]
            data_spread <- spread(data, combined, Value)
            
            # Remove RPMI (controls) from the data
            data_spread <- data_spread %>% select(-contains(c("RPMI")))
            
            # Perform spear correlation analysis and plot results.
            res2<-rcorr(as.matrix(data_spread[,2:ncol(data_spread)]), type="spearman")
            corrplot(res2$r, type="full",hclust.method = ,
                     p.mat = res2$P, sig.level = 1e-6, insig = "blank",tl.cex = 1, tl.col = 'Black')
        })

########################################
###Clustered Correlation plot ##########
########################################
        output$clustercorrPlot <- renderPlot({
            # Select fresh data set
            data <- data.cc
            #Filter based on Year
            data <- data[data$Year == input$Year_nav4,]
            
            # Transform data to wide format
            data$combined <- paste(data$Medium,"__",data$Cytokine)
            data$combined <- gsub(" ", "", data$combined)
            data <- data[,c("ID_bosw","combined","Value")]
            data_spread <- spread(data, combined, Value)
            
            # Remove RPMI (controls) from the data
            data_spread <- data_spread %>% select(-contains(c("RPMI")))
            
            # Perform spear correlation analysis and plot results.
            res2<-rcorr(as.matrix(data_spread[,2:ncol(data_spread)]), type="spearman")
            
            #Setup color scheme usind in the plot
            col_fun = colorRamp2(c(-1,0, 1), c("blue","white", "red"))
            col <- colorRampPalette(c("blue", "white", "red"))(20)
                

            # Cluster the data if selected 
            # else unclustered data
            if (input$cluster_nav4 == TRUE){
                Heatmap(res2$r, name = " ", col = col_fun,na_col = 'black',cluster_rows = T,
                        cluster_columns = T,
                        rect_gp = gpar(col = "white", lwd = 2),
                        column_dend_height = unit(4, "cm"),
                        row_dend_width = unit(4, "cm"),
                        layer_fun = function(j, i, x, y, width, height, fill) {
                            grid.text(sprintf("%.1f", pindex(res2$r, i, j)), x, y, gp = gpar(fontsize = 10))
                        })
            }
            else{
            Heatmap(res2$r, name = " ", col = col_fun,na_col = 'black',cluster_rows = F,
                    cluster_columns = F,
                    rect_gp = gpar(col = "white", lwd = 2),
                    column_dend_height = unit(4, "cm"),
                    row_dend_width = unit(4, "cm"),
                    layer_fun = function(j, i, x, y, width, height, fill) {
                        grid.text(sprintf("%.1f", pindex(res2$r, i, j)), x, y, gp = gpar(fontsize = 10))
                    })
            }
        })
        
        
        ########################################
        ############# Histogram ###############
        ########################################
        output$histPlot <- renderPlot({
            # Select fresh data 
            data <- data.cc
            
            # Remove RPMI from data
            # data <- data%>% group_by(Year) %>%
            #     filter(!Medium %in% c("RPMI"))
            
            # Create subset based on selections made
            if (input$Year_nav2 != "All") {
                data <- data[data$Year == input$Year_nav2,]}
            if (input$Medium_nav2 != "All") {
                data <- data[data$Medium == input$Medium_nav2,]}

            # 
            # if (input$dataType_nav2 == "Raw"){
            #     data}
            # if (input$dataType_nav2 == "Standarised_by_RPMI"){
            #     data$Value <- data$Div_med}
            
            
            if (input$dataTrans_nav2 == "None"){
                data}
            if (input$dataTrans_nav2 == "Log2"){
                data$Value <- log2(data$Value)}
            if (input$dataTrans_nav2 == "Log10"){
                data$Value <- log10(data$Value)}
            if (input$dataTrans_nav2 == "Inv_Rank"){
                data$Value <- data$rnk}
            
            if (input$Medium_nav2 == "All" & input$Year_nav2 == "All") {
                ggplot(data=subset(data, !is.na(Value)), aes(x=Value)) +  
                    geom_histogram(stat="bin") + 
                    facet_wrap(Year~Cytokine*Medium, scale="free", ncol=2) +
                    theme(plot.subtitle = element_text(hjust=0.5,size=8))
            }
            else if (input$Medium_nav2 != "All" & input$Year_nav2 == "All") {
                ggplot(data=subset(data, !is.na(Value)), aes(x=Value)) +  
                    geom_histogram(stat="bin") + 
                    #geom_density(fill="grey") +
                    facet_wrap(Year~Cytokine, scale="free", ncol=2) +
                    theme(plot.subtitle = element_text(hjust=0.5,size=8))
                }
            else if (input$Medium_nav2 == "All" & input$Year_nav2 != "All") {
                ggplot(data=subset(data, !is.na(Value)), aes(x=Value)) +  
                    geom_histogram(stat="bin") + 
                    #geom_density(fill="grey") +
                    facet_wrap(Medium~Cytokine, scale="free", ncol=2) +
                    theme(plot.subtitle = element_text(hjust=0.5,size=8))}
            else{
                ggplot(data=subset(data, !is.na(Value)), aes(x=Value)) +  
                    geom_histogram(stat="bin") + 
                    #geom_density(fill="grey") +
                    facet_wrap(~Cytokine, scale="free", ncol=2) +
                    theme(plot.subtitle = element_text(hjust=0.5,size=8))
                
            }
            })
        
########################################
####### Normality matrix ###############
########################################
        output$normalMatrixPlot <- renderPlot({
            # Select fresh subset
            data <- data.cc

            # Create subset based on selection made
            if (input$Year2_nav2 != "All") {
                data <- data[data$Year == input$Year2_nav2,]}
            
            if (input$dataTrans2_nav2 == "None"){
                data}
            if (input$dataTrans2_nav2 == "Log2"){
                data$Value <- log2(data$Value)}
            if (input$dataTrans2_nav2 == "Log10"){
                data$Value <- log10(data$Value)}
            if (input$dataTrans2_nav2 == "Inv_Rank"){
                data$Value <- data$rnk}
            
            # Remove RPMI from data
            data <- data%>% group_by(Year) %>%
                filter(!Medium %in% c("RPMI"))
            
            # Create subset using Medium Cytokine and year
            # Initialize function for shapiro test
            data_nest <- group_by(data, Medium, Cytokine, Year) %>% nest()
            norm_fun <- function(df) shapiro.test(df$Value) %>% tidy()
            data_nest <- mutate(data_nest, model = map(data, norm_fun))
            
            # Create dataframe with shapiro results
            corr_pr <- select(data_nest, -data) %>% unnest()
            corr_pr <- mutate(corr_pr, sig = ifelse(p.value >0.05, "Sig.", "Non Sig."))
            
            # Create matrix plot using the shapiro values
            # Coloring is based on sig lvl
                ggplot()+
                    geom_tile(data = corr_pr,
                              aes(Cytokine, Medium, fill = p.value),
                              size = 1,
                              colour = "grey",
                              fill = "yellow")+
                    geom_tile(data = filter(corr_pr, sig == "Sig."),
                              aes(Cytokine, Medium),
                              size = 1,
                              colour = "grey",
                              fill = "blue")+
                    labs(x = "", y = "", fill = "", p.value = "")+
                    theme_minimal()+
                    theme(panel.grid.major = element_blank(),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          axis.ticks = element_blank())
        })
        
########################################
####### Density plot ###################
########################################
        output$densityPlot <- renderPlot({
            # Select fresh data  
            data <- data.cc
            
            # #Remove PRMI Values
            # data <- data%>% group_by(Year) %>%
            #     filter(!Medium %in% c("RPMI"))
            
            # Filter data bassed on user selection
            if (input$Year_nav2 != "All") {
                data <- data[data$Year == input$Year_nav2,]
            }
            if (input$Medium_nav2 != "All") {
                data <- data[data$Medium == input$Medium_nav2,]
            }
            if (input$dataTrans_nav2 == "None") {
                data
            }
            if (input$dataTrans_nav2 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_nav2 == "Log10") {
                data$Value <- log10(data$Value)
            }
            if (input$dataTrans_nav2 == "Inv_Rank") {
                data <- data %>% 
                    group_by(Medium, Cytokine, Year) %>%
                    mutate(rnk = qnorm((rank(Value,na.last="keep")-0.5)/sum(!is.na(Value))))
                data<- dplyr::filter(data, !grepl("RPMI",Medium,ignore.case = T))
                data$Value <- data$rnk    
            }
            # Visualize the created subset
            ggplot(data=subset(data, !is.na(Value)), aes(x=Value)) +  
                geom_density(fill="grey") +
                facet_wrap(~Cytokine*Medium, scale="free", ncol = 2) +
                theme(plot.subtitle = element_text(hjust=0.5,size=8))
        })
        
        output$trendPlot1 <- renderPlot({
            # Select fresh data
            data <- data.cc
            
            #Remove RPMI
            data <- data%>% group_by(Year) %>%
                filter(!Medium %in% c("RPMI"))
            
            # Filter data based onn user selection
            if (input$year_trend1 != "All") {
                data <- data[data$Year == input$year_trend1,]
                
            }
            if (input$medium_trend1 != "All") {
                data <- data[data$Medium == input$medium_trend1,]
            }
            
            if (input$cytokine_trend1 != "All") {
                data <- data[data$Cytokine %in% input$cytokine_trend1,]
            }
            
            if (input$dataTrans_trend1 == "None") {
                data
            }
            if (input$dataTrans_trend1 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_trend1 == "Log10") {
                data$Value <- log10(data$Value)
            }
            if (input$dataTrans_trend1 == "Inv_Rank") {
                data <- data %>% 
                    group_by(Medium, Cytokine, Year) %>%
                    mutate(rnk = qnorm((rank(Value,na.last="keep")-3/8)/sum(!is.na(Value))))
                data<- dplyr::filter(data, !grepl("RPMI",Medium,ignore.case = T))
                data$Value <- data$rnk    
            }
            # Visualize data as line plot
            ggplot(data, aes(x = ID_bosw, y = (Value), color = Year, group=Year, na.remove=TRUE)) +
                geom_line(aes(group = Year), alpha = 1) +
                facet_wrap(~Medium*Cytokine,ncol = 1, scale="free") +
                theme(legend.position = c(0.8, 0.2))
        })
        
########################################
####### Trend plot #####################
########################################
        output$trendPlot2 <- renderPlot({
            data <- data.cc
                        
            data <- data%>% group_by(Year) %>%
                filter(!Medium %in% c("RPMI"))
            
            if (input$year_trend1 != "All") {
                data <- data[data$Year == input$year_trend1,]
            }
            if (input$medium_trend1 != "All") {
                data <- data[data$Medium == input$medium_trend1,]
            }
            if (input$cytokine_trend1 != "All") {
                data <- data[data$Cytokine %in% input$cytokine_trend1,]
            }
            
            if (input$dataTrans_trend1 == "None") {
                data
            }
            if (input$dataTrans_trend1 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_trend1 == "Log10") {
                data$Value <- log10(data$Value)
            }
            if (input$dataTrans_trend1 == "Inv_Rank") {
                data <- data %>% 
                    group_by(Medium, Cytokine, Year) %>%
                    mutate(rnk = qnorm((rank(Value,na.last="keep")-0.5)/sum(!is.na(Value))))
                data<- dplyr::filter(data, !grepl("RPMI",Medium,ignore.case = T))
                data$Value <- data$rnk    
            }
            ggplot(data, aes(x = ID_bosw, y = Value, color = Cytokine, group=Cytokine, na.remove=TRUE)) +
                geom_line(aes(group = Cytokine), alpha = 1) +
                facet_wrap(~Year,ncol = 1, scale="free") +
                theme(legend.position = c(0.8, 0.2))
        })
        
########################################
####### Trend plot ###################
########################################
        output$trendPlot3 <- renderPlot({
            # Create fresh sub dataset
            data <- data.cc
            # Create subset using selected input
            data <- data%>% group_by(Year) %>%
                filter(!Medium %in% c("RPMI"))
            
            if (input$year_trend1 != "All") {
                data <- data[data$Year %in% input$year_trend1,]
            }
            if (input$medium_trend1 != "All") {
                data <- data[data$Medium %in% input$medium_trend1,]
            }
            if (input$cytokine_trend1 != "All") {
                data <- data[data$Cytokine %in% input$cytokine_trend1,]
            }
            
            if (input$dataTrans_trend1 == "None") {
                data
            }
            if (input$dataTrans_trend1 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_trend1 == "Log10") {
                data$Value <- log10(data$Value)
            }
            # Inverse ranked normal transformation
            if (input$dataTrans_trend1 == "Inv_Rank") {
                data <- data %>% 
                    group_by(Medium, Cytokine, Year) %>%
                    mutate(rnk = qnorm((rank(Value,na.last="keep")-3/8)/sum(!is.na(Value))))
                data<- dplyr::filter(data, !grepl("RPMI",Medium,ignore.case = T))
                data$Value <- data$rnk    
            }
            # Visualize selected data
            ggplot(data, aes(x = ID_bosw, y = Value, fill = Gender,na.remove=TRUE)) +
                geom_boxplot() +
                facet_wrap(~Medium*Cytokine,ncol = 1, scale="free") +
                theme(legend.position = 'none')
        })
########################################
####### Q-Q plot ###################
########################################
        output$qqPlot <- renderPlot({
            # Select new subset
            data <- data.cc
            
            # Remove RPMI (Controls)
            # data <- data%>% group_by(Year) %>%
            #     filter(!Medium %in% c("RPMI"))
            
            # Select data based on input
            if (input$Year_nav2 != "All") {
                data <- data[data$Year == input$Year_nav2,]
            }
            if (input$Medium_nav2 != "All") {
                data <- data[data$Medium == input$Medium_nav2,]
            }
            if (input$dataTrans_nav2 == "None") {
                data
            }
            if (input$dataTrans_nav2 == "Log2") {
                data$Value <- log2(data$Value)
            }
            if (input$dataTrans_nav2 == "Log10") {
                data$Value <- log10(data$Value)
            }
            if (input$dataTrans_nav2 == "Inv_Rank") {
                data <- data %>% 
                    group_by(Medium, Cytokine, Year) %>%
                    mutate(rnk = qnorm((rank(Value,na.last="keep")-0.5)/sum(!is.na(Value))))
                data<- dplyr::filter(data, !grepl("RPMI",Medium,ignore.case = T))
                data$Value <- data$rnk    
            }
            
            #Visualize subset of data in qq plot
            ggplot(data=subset(data, !is.na(Value)), aes(sample=Value)) +  
                stat_qq() +
                stat_qq_line() +
                facet_wrap(~Cytokine*Medium, scale="free", ncol = 2) +
                theme(plot.subtitle = element_text(hjust=0.5,size=8))
        })
        
        # Summary is currently disabled
        output$summary_txt <- renderPrint({
            data <- data.cc
            summary(data[,])
        })        

########################################
####### Bar plot #######################
########################################
        output$barPlot <- renderPlot({
            # Create fresh subset 
            data <- data.cc 
            
            # Create subset using selected input
            if(input$Medium_traj != "All"){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Medium %in% input$Medium_traj)
            }
            
            if (!is.null(input$Traj_nav)){
                
                data <- data.cc %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Year %in% input$Traj_nav)
            }
            # Select minimum number of samples
            if (input$Bins_traj != 0){
                data <- data %>% 
                group_by(ID_bosw, Medium, Cytokine) %>%
                filter(length(unique(Year)) >= input$Bins_traj) 
            }
            # Created plot 
            g <- ggplot(data, aes(x=Year,fill=Cytokine))
            g + geom_bar(position = "stack") +
                facet_wrap(~Medium, scale="free", ncol=2) +
                geom_text(stat='count', aes(label=..count..) ,
                          position = position_stack(vjust = 0.5))
        })

########################################
####### Line plot ###################
########################################
        output$linePlot <- renderPlot({
            # Create fresh data
            data <- data.cc 
            
            # select data based on fliters
            if(input$Medium_traj != "All"){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Medium %in% input$Medium_traj)
            }
            if (!is.null(input$Traj_nav)){
                
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Year %in% input$Traj_nav)
            }
            
            if (input$Bins_traj != 0){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(length(unique(Year)) >= input$Bins_traj) 
            }
            
            ggplot(data, aes(x = Year, y = Value, color = ID_bosw, group = ID_bosw)) +
                geom_point() +
                geom_line(aes(group=ID_bosw)) +
                facet_wrap(~Cytokine,scale="free",ncol = 1) +
                theme(legend.position = "none")
        })
########################################
####### Cluster plot ###################
########################################
        output$clusterPlot <- renderPlot({
            # Create fresh data
            data <- data.cc 
            
            # select data based on fliters
            if(input$Medium_traj != "All"){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Medium %in% input$Medium_traj)
                }
            if(input$dataTrans_traj == "Log2"){
                data$Value <- log2(data$Value)
                }
            if (!is.null(input$Traj_nav)){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(Year %in% input$Traj_nav)
            }
            if (input$Bins_traj != 0){
                data <- data %>% 
                    group_by(ID_bosw, Medium, Cytokine) %>%
                    filter(length(unique(Year)) >= input$Bins_traj) 
            }
            
            data2 <- data %>% group_by(Year)
            
            # Transform data into wide format
            data2$combined <- paste(data2$Cytokine,"__",data2$Year)
            data2$combined <- gsub(" ", "", data2$combined)
            data2 <- data2[,c("ID_bosw","combined","Value")]
            pr_yr <- spread(data2, combined, Value)
            rownames(pr_yr) <- pr_yr$ID_bosw
            pr_yr <- as.data.frame(pr_yr)
            
            # Create cluster using kmeans trajectory clustering
            t1 <- (1*length(input$Traj_nav))
            t2 <- (2*length(input$Traj_nav))
            t3 <- (3*length(input$Traj_nav))
            
            # Perform clustering on the folowwing Cytokines
            ts <- cld3d(pr_yr, timeInData = list(IL1B = grep("IL.1B", colnames(pr_yr)),
                                                 IL6= grep("IL.6", colnames(pr_yr) ),
                                                 IL8 = grep("IL.8", colnames(pr_yr) )))
            
            ok <- kml3d(ts,nbClusters = 2)
            pr_yr$clusters <- getClusters(ts, 2)
            
            a1 <- as.factor(ts@idAll)
            a2 <- as.factor(getClusters(ts,2))
            aa<- cbind.data.frame(a1,a2)
            
            data.new <- data %>% inner_join(aa, by = c("ID_bosw" = "a1"))
            
            # create mean values 
            gd <- data.new %>% 
                group_by(Cytokine,a2, Year) %>% 
                summarise(Value = mean(Value))
            gd$ID_bosw <- 1
            gd$ID_bosw[gd$a2 == "B"] <- "2"
            gd <- na.omit(gd)
            
            # Visualize cluster plot with mean values for each Cytokine cluster
            ggplot(data.new, aes(x = Year, y = Value, color = a2, group=ID_bosw, na.remove=TRUE)) +
                geom_point(data = gd) +
                geom_line(aes(group = ID_bosw), alpha = .1) +
                geom_line(data = gd, size=2, alpha= .5) +
                facet_wrap(~Cytokine, ncol = 2, scale="free") +
                theme(legend.position = c(0.8, 0.2))
        })
    }
)


