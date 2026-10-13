# Restaurant Customer Satisfaction Analysis — Professional R Shiny Dashboard
# Run from this project folder using: shiny::runApp()

required_packages <- c("shiny", "shinydashboard", "tidyverse", "ggplot2", "DT", "scales", "plotly")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       "\nInstall them with:\ninstall.packages(c(",
       paste(sprintf('"%s"', missing_packages), collapse = ", "), "))")
}
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(scales)
library(plotly)

find_data_file <- function() {
  candidates <- c("data/restaurant_customer_satisfaction.csv",
                  "data/Restaurant_Customer_Satisfaction.csv",
                  "restaurant_customer_satisfaction.csv")
  found <- candidates[file.exists(candidates)]
  if (length(found)) found[[1]] else NULL
}
data_path <- find_data_file()
demo_mode <- is.null(data_path)

if (demo_mode) {
  set.seed(2026)
  n <- 650
  service <- sample(1:5, n, TRUE, c(.04,.08,.18,.38,.32))
  food <- sample(1:5, n, TRUE, c(.03,.07,.16,.38,.36))
  ambience <- sample(1:5, n, TRUE, c(.04,.08,.18,.38,.32))
  wait <- pmax(2, round(rnorm(n, 18, 9)))
  price <- round(runif(n, 12, 95), 2)
  visits <- sample(c("First visit", "Occasional", "Regular"), n, TRUE, c(.34,.42,.24))
  channel <- sample(c("Dine-in", "Takeaway", "Delivery"), n, TRUE, c(.57,.19,.24))
  satisfaction_score <- pmin(5, pmax(1, round(.28*service + .32*food + .22*ambience + .18*(6-pmin(wait,30)/6) + rnorm(n,0,.7))))
  satisfied <- ifelse(satisfaction_score >= 4, "Satisfied", "Needs improvement")
  restaurant_data <- tibble(
    ResponseID = sprintf("R%04d", seq_len(n)),
    CustomerAge = sample(18:75, n, TRUE),
    VisitType = visits,
    ServiceType = channel,
    FoodQuality = food,
    ServiceQuality = service,
    AmbienceRating = ambience,
    WaitingTimeMinutes = wait,
    SpendPerPerson = price,
    ValueForMoney = sample(1:5, n, TRUE, c(.04,.08,.2,.38,.3)),
    RecommendRestaurant = sample(c("Yes","No"), n, TRUE, c(.78,.22)),
    OverallSatisfaction = satisfaction_score,
    SatisfactionSegment = satisfied
  )
} else {
  restaurant_data <- read.csv(data_path, stringsAsFactors = FALSE, check.names = FALSE)
  names(restaurant_data) <- make.names(names(restaurant_data), unique = TRUE)
}

rename_if_found <- function(data, target, aliases) {
  normalize <- function(x) tolower(gsub("[._ -]", "", x))
  found <- names(data)[normalize(names(data)) %in% normalize(aliases)]
  if (!(target %in% names(data)) && length(found)) names(data)[match(found[1], names(data))] <- target
  data
}
restaurant_data <- rename_if_found(restaurant_data, "OverallSatisfaction", c("OverallSatisfaction","Satisfaction","SatisfactionScore","OverallRating","CustomerRating","Rating"))
restaurant_data <- rename_if_found(restaurant_data, "ServiceType", c("ServiceType","VisitType","DiningType","OrderType","ServiceMode"))
restaurant_data <- rename_if_found(restaurant_data, "FoodQuality", c("FoodQuality","FoodRating","FoodScore"))
restaurant_data <- rename_if_found(restaurant_data, "ServiceQuality", c("ServiceQuality","ServiceRating","StaffRating"))
restaurant_data <- rename_if_found(restaurant_data, "WaitingTimeMinutes", c("WaitingTimeMinutes","WaitingTime","WaitTime","WaitMinutes"))
restaurant_data <- rename_if_found(restaurant_data, "SpendPerPerson", c("SpendPerPerson","AmountSpent","BillAmount","AverageSpend","Spend"))
if (!"OverallSatisfaction" %in% names(restaurant_data)) restaurant_data$OverallSatisfaction <- NA_real_
restaurant_data$OverallSatisfaction <- suppressWarnings(as.numeric(as.character(restaurant_data$OverallSatisfaction)))
numeric_cols <- names(restaurant_data)[vapply(restaurant_data, is.numeric, logical(1))]
categorical_cols <- names(restaurant_data)[vapply(restaurant_data, function(x) is.character(x) || is.factor(x), logical(1))]
rating_col <- "OverallSatisfaction"
default_cat <- if ("ServiceType" %in% categorical_cols) "ServiceType" else if (length(categorical_cols)) categorical_cols[1] else NULL
default_num <- intersect(c("OverallSatisfaction","FoodQuality","ServiceQuality","AmbienceRating","WaitingTimeMinutes","SpendPerPerson"), numeric_cols)
if (!length(default_num)) default_num <- numeric_cols[1]
theme_set(theme_minimal(base_size = 13))
teal <- "#12B8A6"; blue <- "#4D7CFE"; coral <- "#F06C5B"; navy <- "#14213D"; muted <- "#718096"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Guest Experience", titleWidth = 285),
  dashboardSidebar(width = 285,
    sidebarMenu(
      menuItem("Executive Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Satisfaction Drivers", tabName = "drivers", icon = icon("star")),
      menuItem("Service Operations", tabName = "operations", icon = icon("utensils")),
      menuItem("Recommendations", tabName = "recommendations", icon = icon("thumbs-up")),
      menuItem("Customer Records", tabName = "records", icon = icon("table"))
    ),
    hr(),
    div(style = "padding: 0 15px;",
      h4("Dashboard filters", style = "color:white;"),
      uiOutput("service_filter"),
      uiOutput("segment_filter"),
      helpText("Filters apply to all dashboard pages.")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
      body, h1, h2, h3, h4, .main-sidebar, .main-header { font-family: 'Inter', sans-serif; }
      .content-wrapper, .right-side { background: #F4F7FB; }
      .main-header .navbar { background: #14213D !important; }
      .main-header .logo { background: #101A30 !important; font-weight: 800; }
      .main-sidebar { background: #14213D !important; }
      .sidebar-menu>li.active>a { border-left-color: #12B8A6 !important; background: #203354 !important; }
      .box { border-radius: 12px; border-top: 0; box-shadow: 0 5px 22px rgba(25,45,75,.06); }
      .small-box { border-radius: 12px; box-shadow: 0 5px 22px rgba(25,45,75,.08); }
      .small-box h3 { font-weight: 800; }
      .content { padding: 22px; }
      .section-title { font-weight: 800; color: #14213D; margin: 2px 0 4px; }
      .section-subtitle { color: #718096; margin-bottom: 18px; }
      .notice { padding: 12px 15px; background: #EAF8F6; border-left: 4px solid #12B8A6; border-radius: 7px; margin-bottom: 15px; color: #244; }
      table.dataTable thead th { background: #EEF2F8; color: #14213D; }
    "))),
    tabItems(
      tabItem(tabName="overview",
        h2(class="section-title","Executive Overview"),
        p(class="section-subtitle","A concise view of guest satisfaction, service quality, and dining experience."),
        uiOutput("data_notice"),
        fluidRow(valueBoxOutput("kpi_responses",width=3), valueBoxOutput("kpi_satisfaction",width=3),
                 valueBoxOutput("kpi_recommend",width=3), valueBoxOutput("kpi_wait",width=3)),
        fluidRow(box(title="Overall satisfaction distribution",status="primary",solidHeader=TRUE,width=5,plotOutput("satisfaction_donut",height=285)),
                 box(title="Average ratings by service type",status="primary",solidHeader=TRUE,width=7,plotOutput("service_ratings",height=285))),
        fluidRow(box(title="Food, service & ambience",status="primary",solidHeader=TRUE,width=6,plotOutput("quality_chart",height=275)),
                 box(title="Waiting time vs satisfaction",status="primary",solidHeader=TRUE,width=6,plotOutput("wait_chart",height=275)))
      ),
      tabItem(tabName="drivers",
        h2(class="section-title","Satisfaction Drivers"),
        p(class="section-subtitle","Explore which experience attributes move alongside overall customer ratings."),
        fluidRow(box(title="Select a rating metric",status="primary",solidHeader=TRUE,width=4,
          uiOutput("driver_metric_ui"), uiOutput("driver_group_ui")),
          box(title="Rating by customer segment",status="primary",solidHeader=TRUE,width=8,plotOutput("driver_chart",height=370))),
        fluidRow(box(title="Correlation between numeric attributes",status="primary",solidHeader=TRUE,width=12,plotOutput("correlation_chart",height=360)))
      ),
      tabItem(tabName="operations",
        h2(class="section-title","Service Operations"),
        p(class="section-subtitle","Compare service modes, waiting times, and spend patterns."),
        fluidRow(box(title="Service type mix",status="primary",solidHeader=TRUE,width=5,plotOutput("service_mix",height=320)),
                 box(title="Waiting time distribution",status="primary",solidHeader=TRUE,width=7,plotOutput("wait_distribution",height=320))),
        fluidRow(box(title="Spend per person by satisfaction",status="primary",solidHeader=TRUE,width=12,plotOutput("spend_chart",height=320)))
      ),
      tabItem(tabName="recommendations",
        h2(class="section-title","Recommendations"),
        p(class="section-subtitle","Use the current data to identify practical areas for service improvement."),
        fluidRow(valueBoxOutput("kpi_low",width=4),valueBoxOutput("kpi_highwait",width=4),valueBoxOutput("kpi_value",width=4)),
        fluidRow(box(title="Data-led action ideas",status="primary",solidHeader=TRUE,width=12,uiOutput("recommendation_text"))),
        fluidRow(box(title="Lowest-rated experience attributes",status="primary",solidHeader=TRUE,width=12,plotOutput("improvement_chart",height=300)))
      ),
      tabItem(tabName="records",
        h2(class="section-title","Customer Records"),
        p(class="section-subtitle","Search, sort, filter, and export the currently selected survey responses."),
        fluidRow(box(title="Restaurant satisfaction records",status="primary",solidHeader=TRUE,width=12,
          downloadButton("download_data","Download filtered CSV",class="btn-primary"),br(),br(),DTOutput("records_table")))
      )
    )
  )
)

server <- function(input, output, session) {
  output$service_filter <- renderUI({
    if (!"ServiceType" %in% names(restaurant_data)) return(NULL)
    selectInput("service_type","Service type",choices=c("All service types",sort(unique(as.character(restaurant_data$ServiceType)))),selected="All service types")
  })
  output$segment_filter <- renderUI({
    col <- if ("SatisfactionSegment" %in% names(restaurant_data)) "SatisfactionSegment" else if ("RecommendRestaurant" %in% names(restaurant_data)) "RecommendRestaurant" else NULL
    if (is.null(col)) return(NULL)
    selectInput("segment","Customer segment",choices=c("All segments",sort(unique(as.character(restaurant_data[[col]])))),selected="All segments")
  })
  filtered_data <- reactive({
    d <- restaurant_data
    if (!is.null(input$service_type) && input$service_type!="All service types" && "ServiceType" %in% names(d))
      d <- d[as.character(d$ServiceType)==input$service_type,,drop=FALSE]
    segcol <- if ("SatisfactionSegment" %in% names(d)) "SatisfactionSegment" else if ("RecommendRestaurant" %in% names(d)) "RecommendRestaurant" else NULL
    if (!is.null(segcol) && !is.null(input$segment) && input$segment!="All segments")
      d <- d[as.character(d[[segcol]])==input$segment,,drop=FALSE]
    d
  })
  output$data_notice <- renderUI({
    if (demo_mode) div(class="notice",strong("Sample data is active. "),"Add your real CSV as data/restaurant_customer_satisfaction.csv and restart the app.")
    else div(class="notice",strong("Dataset loaded: "),data_path," · ",comma(nrow(restaurant_data))," survey responses.")
  })
  output$kpi_responses <- renderValueBox(valueBox(comma(nrow(filtered_data())),"Survey responses",icon=icon("users"),color="aqua"))
  output$kpi_satisfaction <- renderValueBox({
    d<-filtered_data(); val<-if(nrow(d)&&any(is.finite(d$OverallSatisfaction))) mean(d$OverallSatisfaction,na.rm=TRUE) else NA_real_
    valueBox(if(is.finite(val)) paste0(round(val,2)," / 5") else "N/A","Average satisfaction",icon=icon("star"),color="green")
  })
  output$kpi_recommend <- renderValueBox({
    d<-filtered_data(); col<-if("RecommendRestaurant"%in%names(d)) "RecommendRestaurant" else NULL
    rate<-if(!is.null(col)&&nrow(d)) mean(tolower(as.character(d[[col]]))%in%c("yes","true","1"),na.rm=TRUE) else NA_real_
    valueBox(if(is.finite(rate)) percent(rate,accuracy=.1) else "N/A","Would recommend",icon=icon("thumbs-up"),color="purple")
  })
  output$kpi_wait <- renderValueBox({
    d<-filtered_data(); val<-if("WaitingTimeMinutes"%in%names(d)&&nrow(d)) mean(d$WaitingTimeMinutes,na.rm=TRUE) else NA_real_
    valueBox(if(is.finite(val)) paste0(round(val,1)," min") else "N/A","Average wait",icon=icon("clock"),color="yellow")
  })
  output$satisfaction_donut <- renderPlot({
    d<-filtered_data(); vals<-d$OverallSatisfaction[is.finite(d$OverallSatisfaction)]
    validate(need(length(vals)>0,"No numeric overall satisfaction data found."))
    counts<-as.data.frame(table(factor(vals,levels=sort(unique(vals))))); names(counts)<-c("Rating","Count")
    ggplot(counts,aes(x=2,y=Count,fill=Rating))+geom_col(width=.8,color="white")+coord_polar(theta="y")+xlim(.5,2.5)+
      scale_fill_brewer(palette="YlGnBu")+geom_text(aes(label=paste0(Rating,"\n",Count)),position=position_stack(vjust=.5),color="white",fontface="bold",size=3.5)+
      theme_void()+theme(legend.position="none")
  })
  output$service_ratings <- renderPlot({
    d<-filtered_data(); groupcol<-if("ServiceType"%in%names(d)) "ServiceType" else if(!is.null(default_cat)) default_cat else NULL
    validate(need(!is.null(groupcol),"No categorical service field found."))
    metrics<-intersect(c("FoodQuality","ServiceQuality","AmbienceRating","ValueForMoney","OverallSatisfaction"),names(d))
    metrics<-metrics[vapply(d[metrics],is.numeric,logical(1))]
    validate(need(length(metrics)>0,"No rating metrics available."))
    summary<-d%>%group_by(.data[[groupcol]])%>%summarise(across(all_of(metrics),~mean(.x,na.rm=TRUE)),.groups="drop")%>%
      pivot_longer(cols=all_of(metrics),names_to="Metric",values_to="Rating")
    ggplot(summary,aes(x=.data[[groupcol]],y=Rating,fill=Metric))+geom_col(position="dodge")+labs(x=NULL,y="Average rating",fill="Metric")+theme(axis.text.x=element_text(angle=20,hjust=1))
  })
  output$quality_chart <- renderPlot({
    d<-filtered_data(); cols<-intersect(c("FoodQuality","ServiceQuality","AmbienceRating","ValueForMoney"),names(d))
    cols<-cols[vapply(d[cols],is.numeric,logical(1))]
    validate(need(length(cols)>0,"No food/service/ambience rating columns found."))
    sums<-tibble(Attribute=cols,Average=vapply(cols,function(nm)mean(d[[nm]],na.rm=TRUE),numeric(1)))
    ggplot(sums,aes(x=reorder(Attribute,Average),y=Average,fill=Average))+geom_col(width=.7)+coord_flip()+
      scale_fill_gradient(low=coral,high=teal)+labs(x=NULL,y="Average score",title="Average attribute rating")+theme(legend.position="none")
  })
  output$wait_chart <- renderPlot({
    d<-filtered_data(); validate(need("WaitingTimeMinutes"%in%names(d)&&any(is.finite(d$OverallSatisfaction)),"Need wait time and overall satisfaction columns."))
    ggplot(d,aes(x=WaitingTimeMinutes,y=OverallSatisfaction))+geom_point(alpha=.35,color=blue)+geom_smooth(method="loess",se=TRUE,color=coral)+
      labs(x="Waiting time (minutes)",y="Overall satisfaction")
  })
  output$driver_metric_ui <- renderUI({
    opts<-numeric_cols
    selectInput("driver_metric","Numeric metric",choices=opts,selected=if("OverallSatisfaction"%in%opts)"OverallSatisfaction" else if(length(opts))opts[1] else NULL)
  })
  output$driver_group_ui <- renderUI({
    opts<-categorical_cols
    selectInput("driver_group","Group by",choices=opts,selected=default_cat)
  })
  output$driver_chart <- renderPlot({
    d<-filtered_data(); req(input$driver_metric,input$driver_group)
    d%>%group_by(.data[[input$driver_group]])%>%summarise(Value=mean(.data[[input$driver_metric]],na.rm=TRUE),N=n(),.groups="drop")%>%
      ggplot(aes(x=reorder(as.character(.data[[input$driver_group]]),Value),y=Value))+geom_col(fill=teal,width=.7)+coord_flip()+
      labs(x=input$driver_group,y=paste("Average",input$driver_metric))
  })
  output$correlation_chart <- renderPlot({
    d<-filtered_data(); cols<-names(d)[vapply(d,is.numeric,logical(1))]
    validate(need(length(cols)>=2,"At least two numeric columns are required."))
    cor_mat<-cor(d[,cols,drop=FALSE],use="pairwise.complete.obs")
    long<-as.data.frame(as.table(cor_mat)); names(long)<-c("Var1","Var2","Correlation")
    ggplot(long,aes(Var1,Var2,fill=Correlation))+geom_tile(color="white")+geom_text(aes(label=sprintf("%.2f",Correlation)),size=3)+
      scale_fill_gradient2(low=coral,mid="white",high=teal,midpoint=0,limits=c(-1,1))+labs(x=NULL,y=NULL,fill="Correlation")+
      theme(axis.text.x=element_text(angle=40,hjust=1))
  })
  output$service_mix <- renderPlot({
    d<-filtered_data(); col<-if("ServiceType"%in%names(d))"ServiceType" else default_cat
    validate(need(!is.null(col),"No service type/category field found."))
    d%>%count(.data[[col]])%>%ggplot(aes(x=reorder(as.character(.data[[col]]),n),y=n,fill=.data[[col]]))+
      geom_col(show.legend=FALSE,width=.7)+coord_flip()+scale_fill_manual(values=c(teal,blue,coral,navy,muted))+labs(x=NULL,y="Responses")
  })
  output$wait_distribution <- renderPlot({
    d<-filtered_data(); validate(need("WaitingTimeMinutes"%in%names(d),"No waiting-time column found."))
    ggplot(d,aes(x=WaitingTimeMinutes))+geom_histogram(bins=25,fill=blue,color="white")+
      geom_vline(xintercept=mean(d$WaitingTimeMinutes,na.rm=TRUE),color=coral,linewidth=1,linetype="dashed")+
      labs(x="Waiting time (minutes)",y="Responses",subtitle="Dashed line marks the average wait")
  })
  output$spend_chart <- renderPlot({
    d<-filtered_data(); validate(need("SpendPerPerson"%in%names(d)&&"OverallSatisfaction"%in%names(d),"Need spend and satisfaction fields."))
    ggplot(d,aes(x=factor(OverallSatisfaction),y=SpendPerPerson,fill=factor(OverallSatisfaction)))+
      geom_boxplot(alpha=.8,outlier.alpha=.2)+scale_fill_brewer(palette="YlGnBu")+labs(x="Overall satisfaction",y="Spend per person",fill="Rating")
  })
  output$kpi_low <- renderValueBox({
    d<-filtered_data(); val<-if("OverallSatisfaction"%in%names(d))mean(d$OverallSatisfaction<=2,na.rm=TRUE)else NA_real_
    valueBox(if(is.finite(val))percent(val,accuracy=.1)else"N/A","Low satisfaction (1–2)",icon=icon("triangle-exclamation"),color="red")
  })
  output$kpi_highwait <- renderValueBox({
    d<-filtered_data(); val<-if("WaitingTimeMinutes"%in%names(d))mean(d$WaitingTimeMinutes>=30,na.rm=TRUE)else NA_real_
    valueBox(if(is.finite(val))percent(val,accuracy=.1)else"N/A","Waits of 30+ minutes",icon=icon("clock"),color="yellow")
  })
  output$kpi_value <- renderValueBox({
    d<-filtered_data(); val<-if("ValueForMoney"%in%names(d))mean(d$ValueForMoney,na.rm=TRUE)else NA_real_
    valueBox(if(is.finite(val))paste0(round(val,2)," / 5")else"N/A","Value-for-money score",icon=icon("coins"),color="aqua")
  })
  output$recommendation_text <- renderUI({
    d<-filtered_data(); pieces<-c()
    if("WaitingTimeMinutes"%in%names(d)) {
      avg<-mean(d$WaitingTimeMinutes,na.rm=TRUE)
      pieces<-c(pieces,paste0("• **Reduce waiting friction:** current average wait is ",round(avg,1)," minutes. Review peak-hour staffing and table/queue handoffs if waits are high."))
    }
    rating_cols<-intersect(c("FoodQuality","ServiceQuality","AmbienceRating","ValueForMoney"),names(d))
    if(length(rating_cols)) {
      scores<-vapply(rating_cols,function(nm)mean(d[[nm]],na.rm=TRUE),numeric(1))
      lowest<-names(which.min(scores))
      pieces<-c(pieces,paste0("• **Focus on the lowest-rated attribute:** ",lowest," averages ",round(min(scores,na.rm=TRUE),2),". Validate this signal with customer comments or follow-up surveys."))
    }
    if("RecommendRestaurant"%in%names(d)) {
      rate<-mean(tolower(as.character(d$RecommendRestaurant))%in%c("yes","true","1"),na.rm=TRUE)
      pieces<-c(pieces,paste0("• **Grow advocacy:** ",percent(rate,accuracy=.1)," of responses indicate willingness to recommend. Consider inviting satisfied guests to share feedback."))
    }
    if(!length(pieces)) pieces<-"• Add rating, service type, and waiting-time fields to enable more specific recommendations."
    tags$div(style="font-size:16px;line-height:1.9;padding:8px 4px;",HTML(paste(pieces,collapse="<br><br>")),
      tags$p(style="color:#718096;font-size:12px;margin-top:16px;","These are descriptive signals from the current dataset, not proof of cause and effect."))
  })
  output$improvement_chart <- renderPlot({
    d<-filtered_data(); cols<-intersect(c("FoodQuality","ServiceQuality","AmbienceRating","ValueForMoney"),names(d))
    cols<-cols[vapply(d[cols],is.numeric,logical(1))]
    validate(need(length(cols)>0,"No quality rating columns found."))
    s<-tibble(Attribute=cols,Average=vapply(cols,function(nm)mean(d[[nm]],na.rm=TRUE),numeric(1)))
    ggplot(s,aes(x=reorder(Attribute,Average),y=Average,fill=Average))+geom_col(width=.7)+coord_flip()+
      scale_fill_gradient(low=coral,high=teal)+labs(x=NULL,y="Average score")+theme(legend.position="none")
  })
  output$records_table <- renderDT({
    datatable(filtered_data(),filter="top",options=list(pageLength=10,scrollX=TRUE,dom="Bfrtip",buttons=c("copy","csv","excel"),autoWidth=TRUE),
              extensions="Buttons",rownames=FALSE,class="stripe hover compact")
  })
  output$download_data <- downloadHandler(
    filename=function()paste0("restaurant_satisfaction_filtered_",Sys.Date(),".csv"),
    content=function(file)write.csv(filtered_data(),file,row.names=FALSE)
  )
}
shinyApp(ui,server)
