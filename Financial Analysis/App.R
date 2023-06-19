library(shiny)
library(scales)
library(glue)
library(shinycssloaders)
library(viridis)
library(gt)
library(gtExtras)
library(DT)

# shiny options for auto-reload
options(shiny.autoreload=TRUE)

# source helper functions
source("helpers/Helpers.R")
source("Constants.R")


ui <- navbarPage(
  header = (
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="styles.css")
    )
  ),
  collapsible = TRUE,
  title = "Finacial Analysis.",
  # start dashboard
  {
  tabPanel("Dashboard", icon=icon("gauge"),
           fluidRow(
             column(12, ValueBoxes(value=Thousand(total_loans$tot),
                                   value2 = Thousand(total_members),
                                   value3 = Thousand(total_paid$tot),
                                   value4 = Thousand(total_defaulted$tot)))
           ),
           fluidRow(style="margin-top:10px",
               column(3, 
                      div(style = "margin-left:30px", 
                          selectizeInput("year", "Year", choices = NULL))),
               column(3, radioButtons("filter", "EDIT PLOT?", choices = c("Yes","No"), selected = "No")),
               conditionalPanel(
                 condition = "input.filter == 'Yes'", 
                 column(3, textInput("xlab", "x-lab", placeholder = "xlab", value = "Month")),
                 column(3, textInput("ylab", "y-lab", placeholder = "ylab", value = "Total Loans"))
               )
           ),
           fluidRow(
             column(6, div(class="card", plotlyOutput("loan_reason_month") %>% withSpinner())),
             column(6, div(class="card", plotlyOutput("loan_year") %>% withSpinner()))
           ))
    },
  # end dashboard
  # start data
  {
  tabPanel("Data", icon = icon("table"),
           tabsetPanel(
             tabPanel("Members", 
                      div(class="members",
                          div(fluidRow(
                            column(2, selectizeInput("account", "Account No.", choices=NULL)),
                            column(2, selectizeInput("ID", "ID", choices=NULL,multiple=TRUE))
                          )
                        ),
                            div(class="member-table", 
                                dataTableOutput("members") %>% withSpinner())
                          )
                        ),     
             tabPanel("Accounts", 
                      div(class="members",
                          div(fluidRow(
                            column(2, selectizeInput("branch", "Branch", choices=NULL)),
                            column(2, selectizeInput("accountType", "Account Type", choices=NULL))
                          )),
                          div(class="member-table",
                              dataTableOutput("accounts") %>% withSpinner()
                              ))),
             tabPanel("Loans", 
                      div(class="members",
                          div(fluidRow(
                            column(3, selectizeInput("cols", "Columns", choices=NULL, multiple=TRUE)),
                            column(2, selectizeInput("default", "Loan Default", choices=NULL))
                          )),
                          div(class="member-table",
                              dataTableOutput("loans") %>% withSpinner())
                          )
                      ),
           )) 
  },
  # end data
  # human resource
  {
  tabPanel("Human Resource", icon = icon("book"),
           tabsetPanel(
             tabPanel("Clients",
                      fluidPage(style="margin-top:10px;",
                                fluidRow(
                                  column(2, 
                                         div(class="hr-card-filter",
                                             h4("Filters"),
                                             selectInput("col", "Attribute", choices = NULL),
                                             p("TOTAL CLIENTS OF THE ORGANIZATION BY DIFFERENT FACTORS")
                                         )),
                                  column(10, 
                                         div(class="hr-card-plot",
                                             plotlyOutput("tot_members") %>% withSpinner()
                                         ))
                                )
                      )),
             tabPanel("Accounts", 
                      fluidPage(style="margin-top:10px",
                                fluidRow(
                                  column(2,
                                         div(class="hr-card-filter", 
                                             h4("filters"),
                                             selectizeInput("account2", "Account", choices = NULL),
                                             selectizeInput("col2", "Attribute", choices = NULL))),
                                  column(10,
                                         div(class="hr-card-plot",
                                             plotlyOutput("tot_amount_by_accounts"))
                                    )
                                )
                      ))
           )
        )
  },
  # human resource
  # credit/lending
  {
  tabPanel("Credit/Lending", icon = icon("credit-card"), 
           tabsetPanel(
             tabPanel("Trends", 
                      fluidPage(style="margin:10px",
                        fluidRow(
                          column(6, div(class="hr-card-plot",
                                        plotlyOutput("tot_loan_by_default_branch") %>% withSpinner())),
                          column(6, div(class="hr-card-plot",
                                        plotlyOutput("tot_loan_by_reason_branch") %>% withSpinner()))
                        ),
                        fluidRow(style="margin-top:20px;margin-bottom:10px",
                          column(6, div(class="hr-card-plot",
                                        plotlyOutput("tot_loan_by_reason") %>% withSpinner())),
                          column(6, div(class="hr-card-plot",
                                        plotlyOutput("tot_loan_by_branch") %>% withSpinner()))
                        ),
                        fluidRow(style="margin-top:20px;margin-bottom:10px",
                          column(6, div(class="hr-card-plot",
                                        plotlyOutput("tot_loan_by_year_branch") %>% withSpinner()))
                        )
                      )),
             tabPanel("Summary Report", 
                      fluidPage(
                        fluidRow(style="margin-top:10px",
                          column(4, div(
                          style="border: 1px solid black;padding: 10px;
                          text-align: left;box-shadow: 5px 10px #888888;",
                          gt_output("members_branch") %>% withSpinner(type = 8))),
                          column(4, div(class="hr-card-plot",
                            gt_output("loans_branch") %>% withSpinner(type = 8))),
                          column(4, div(class="hr-card-plot", 
                                        gt_output("tot_loan_by_acc") %>% withSpinner(type=8)))
                          
                        ),
                        fluidRow(style="margin-top:20px;margin-bottom:20px",
                          column(4, div(class="hr-card-plot",
                                        gt_output("tot_loan_by_occ") %>% withSpinner(type = 8))),
                          column(4, div(class="hr-card-plot",
                                        gt_output("members_occ") %>% withSpinner(type = 8))),
                          column(4, div(class="hr-card-plot",
                                        gt_output("loan_default_by_branch") %>% withSpinner(type = 8)))
                        )
                      )),
             tabPanel("Report", 
                      fluidPage(style="margin-top:10px",
                        fluidRow(
                          column(10, uiOutput("report") %>% withSpinner())
                        )
                      ))
           ))
  },
  # credit/lending
)

server <- function(input, output, session) {
  #-------------------------- members -------------------------#
  {
  # update members name
  observeEvent(members,{
    updateSelectizeInput(session, "account", 
                         choices = members$account_no, server=TRUE, selected = "")
  })
  
  total_loans_by_year_month <- reactive({
    loans %>%
      mutate(year = year(date),
             month = month(date))
  })
  
  observeEvent(total_loans_by_year_month(), {
    updateSelectizeInput(session, "year", 
                         choices = total_loans_by_year_month()$year, server=TRUE,
                         selected = total_loans_by_year_month()$year[1])
  })
  
  # total loans by reason and month
  output$loan_reason_month <- renderPlotly({
    total_loans_by_year_month() %>%
      filter(year == input$year) %>%
      group_by(year, reason, month) %>%
      summarise(
        loans = sum(amount)
      ) %>%
      ggplot(aes(x=month, y=loans, fill=reason)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis(discrete = TRUE) + xlab(input$xlab) +
      ylab(input$ylab) + ggtitle(glue("Total Loans by month and reason - {input$year}")) +
      theme_light()
  })
  
  # total loans by year
  output$loan_year <- renderPlotly({
    total_loans_by_year_month() %>%
      group_by(year) %>%
      summarise(
        Loans = sum(amount)
      ) %>%
      ggplot(aes(x=year, y=Loans, fill=year)) +
      geom_bar(stat="identity") +
      scale_fill_viridis(discrete = FALSE) +
      theme_bw() +
      ylab("Total Loans") + xlab("Year") +
      ggtitle("Total Loans by Year") +
      theme(legend.position = "none")
  })
  
  # members by account
  members_by_account_no <- reactive({
    members %>%
      filter(account_no == input$account)
  })
  
  # members data
  output$members <- renderDataTable(options = list(searching=F, ordering=F),{
    if(input$account == ""){
      members
    } else {
      members_by_account_no()
    } 
  })

  # update members id
  observeEvent(members_by_account_no(),{
    updateSelectizeInput(session, "ID", 
                         choices = members_by_account_no()$ID, server=TRUE, 
                         selected = members_by_account_no()$ID)
  })
  
  # total members by branch
  output$members_branch <- render_gt({
    members %>%
      group_by(branch) %>%
      summarise(
        total = n()
      ) %>%
      mutate(percentage = 100*(total / sum(total))) %>%
      gt() %>%
      tab_header(
        title = md("**Total Members**"),
        subtitle = md("By **Location Branch**")
      ) %>%
      fmt_number(
        columns = c("total"),
        decimals = 2
      )  %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "lightgray")
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>%
      gt_theme_538() %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center",
        heading.background.color = "grey"
      )
  })
  
  # total members by occupation
  output$members_occ <- render_gt({
    members %>%
      group_by(occupation) %>%
      summarise(
        total = n()
      ) %>%
      mutate(percentage = 100*(total / sum(total))) %>%
      gt() %>%
      tab_header(
        title = md("**Total Members**"),
        subtitle = md("By **Occupation**")
      ) %>%
      fmt_number(
        columns = c("total"),
        decimals = 2
      )  %>%
      tab_style(
        style = list(
          cell_text(weight = "bold"),
          cell_fill(color = "lightgray")
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>%
      gt_theme_538() %>%
      tab_options(
        table.width = pct(100),
        heading.align = "center",
        heading.background.color = "grey"
      )
  })
}
  #------------------------- members data --------------------#
  
  ##------------------------accounts data ---------------------#
  {
    # accounts by branch
    accounts_by_branch <- reactive({
      accounts %>%
        filter(branch == input$branch)
    })
    # update branch
    observeEvent(accounts, {
      updateSelectizeInput(session, "branch", choices = accounts$branch, server=TRUE,
                           selected = "")
    })
    
    # update account type
    observeEvent(accounts_by_branch(), {
      updateSelectizeInput(session, "accountType", choices = accounts_by_branch()$account, 
                           server=TRUE, selected = "")
    })
    
    # accounts by branch and account type
    accounts_by_branch_accountType <- reactive({
      accounts %>%
        filter(branch == input$branch & account == input$accountType)
    })
    
  # the accounts table
    output$accounts <- renderDataTable(options = list(searching=F, ordering=F),{
      if(input$branch == ""){
        accounts
      } else if(input$branch != "" && input$accountType != ""){
        accounts_by_branch_accountType()
      } else {
        accounts 
      }
    })
  }
  ##------------------------accounts data ---------------------#
  
  ## -----------------------loans data------------------------#
  {
    # update columns
    observeEvent(loans, {
      updateSelectizeInput(session, "cols",  server=TRUE, 
                           choices = names(loans), selected = names(loans)[c(1:5,8,13)])
    })
    # loans by columns
    loans_by_cols <- reactive({
      validate(
        need(input$cols != "", "Select columns")
      )
      loans %>%
        select(input$cols)
    })
    # loans by default
    loans_by_default <- reactive({
      loans %>%
        select(input$cols) %>%
        filter(default == input$default)
    })
    # loans data 
    output$loans <- renderDataTable(
      options = list(searching=F, ordering=F, scrollX=T),{
        if(input$default != ""){
          loans_by_default()
        } else {
          loans %>%
            select(input$cols)
        }
      })
    # update default
    observeEvent(loans_by_cols(),{
      updateSelectizeInput(session, "default", 
                           choices = loans_by_cols()$default, server=TRUE, selected = "")
    })
    
    # total loans by branch
    # total members by branch
    output$loans_branch <- render_gt({
      loans %>%
        group_by(branch) %>%
        summarise(
          total = sum(amount)
        ) %>%
        mutate(percentage = 100*(total / sum(total))) %>%
        gt() %>%
        tab_header(
          title = md("**Total Loans**"),
          subtitle = md("By **Location Branch**")
        ) %>%
        fmt_number(
          columns = c("total"),
          decimals = 2
        )  %>%
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_fill(color = "lightgray")
          ),
          locations = cells_body(
            columns = everything(),
            rows = everything()
          )
        ) %>%
        gt_theme_538() %>%
        tab_options(
          table.width = pct(100),
          heading.align = "center",
          heading.background.color = "grey"
        )
    })
    
    # total loan by occupation
    output$tot_loan_by_occ <- render_gt({
      loans %>%
        dplyr::select(occupation, amount) %>%
        group_by(occupation) %>%
        summarise(
          loans = sum(amount)
        ) %>%
        mutate(percentage = 100*(loans / sum(loans))) %>%
        gt()  %>%
        tab_header(
          title = md("**Total Loans**"),
          subtitle = md("By **Occupation**")
        ) %>%
        fmt_number(
          columns = c("loans"),
          decimals = 2
        )  %>%
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_fill(color = "lightgray")
          ),
          locations = cells_body(
            columns = everything(),
            rows = everything()
          )
        ) %>%
        gt_theme_538() %>%
        tab_options(
          table.width = pct(100),
          heading.align = "center",
          heading.background.color = "grey"
        )
    })
    
   # total loan default by branch
    output$loan_default_by_branch <- render_gt({
      loans %>%
         group_by(default, branch) %>%
         summarise(tot_loan = sum(amount)) %>%
        mutate(percentage = 100*(tot_loan / sum(tot_loan))) %>%
        gt()  %>%
        tab_header(
          title = md("**Total Loan default**"),
          subtitle = md("By **Branch**")
        ) %>%
        fmt_number(
          columns = c("tot_loan"),
          decimals = 2
        )  %>%
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_fill(color = "lightgray")
          ),
          locations = cells_body(
            columns = everything(),
            rows = everything()
          )
        ) %>%
        gt_theme_538() %>%
        tab_options(
          table.width = pct(100),
          heading.align = "center",
          heading.background.color = "grey"
        )
    })
    
    # total loan default by account
    output$tot_loan_by_acc <- render_gt({
      loans %>%
        group_by(account) %>%
        summarise(tot_loan = sum(amount)) %>%
        mutate(percentage = 100*(tot_loan / sum(tot_loan))) %>%
        gt()  %>%
        tab_header(
          title = md("**Total Loan**"),
          subtitle = md("By **Account**")
        ) %>%
        fmt_number(
          columns = c("tot_loan"),
          decimals = 2
        )  %>%
        tab_style(
          style = list(
            cell_text(weight = "bold"),
            cell_fill(color = "lightgray")
          ),
          locations = cells_body(
            columns = everything(),
            rows = everything()
          )
        ) %>%
        gt_theme_538() %>%
        tab_options(
          table.width = pct(100),
          heading.align = "center",
          heading.background.color = "grey"
        )
    })
  }
  ## -----------------------loans data------------------------#
  
  # -------------- total members by gender, occupation, marital ---#
  {
    # update columns
    observeEvent(members, {
      updateSelectizeInput(session, "col", 
                           server=T, choices = names(members)[c(3,5:8,11)])
    })
    # total members
    output$tot_members <- renderPlotly({
      validate(
        need(input$col != "", "Please Select A column")
      )
      
     switch (input$col,
       gender = tot_members_by_gender,
       occupation = tot_members_by_occ,
       marital = tot_members_by_marital,
       education = tot_members_by_education,
       account = tot_members_by_account,
       branch = tot_members_by_branch,
       validate("Invalid column Selected")
     )
      
    })
    
  }
  # -------------- total members by gender, occupation, marital ---#
  
  #--------------- accounts -----------------------#
  {
  # update accounts
  observeEvent(accounts, {
    updateSelectizeInput(session, "account2", choices = accounts$account, server = TRUE)
  })
  
  # update attributes
  observeEvent(accounts, {
    updateSelectizeInput(session, "col2", choices = names(accounts)[c(3,5:7,11)], server = TRUE)
  })
  
  # plot
  output$tot_amount_by_accounts <- renderPlotly({
    validate(
      need(input$account2 != "", "Please select account"),
      need(input$col2 != "", "Please select attribute")
    )
    
    switch (input$col2,
      gender = tot_acc_amount_by_gender(input),
      occupation = tot_acc_amount_by_occupation(input),
      marital = tot_acc_amount_by_marital(input),
      education = tot_acc_amount_by_education(input),
      branch = tot_acc_amount_by_branch(input),
      validate("INVALID ATTRIBUTE SELECTED")
    )
  })
  }
  #--------------- accounts -----------------------#
  
  # ---------------- start trends -----------------------#
  {
    # total loan by default and branch
    output$tot_loan_by_default_branch <- renderPlotly({
      loans %>%
        dplyr::select(branch, default, amount) %>%
        group_by(branch, default) %>%
        summarise(
          Loan = sum(amount)
        ) %>%
        ggplot(aes(x=branch, y=Loan, group=1)) +
        geom_line() +
        geom_point() +
        theme_light() +
        xlab("Branch") + ggtitle("Total Loans by default and branch") +
        facet_wrap(~default)
    })
    
    # total loans by reason and branch
    output$tot_loan_by_reason_branch <- renderPlotly({
      loans %>%
        dplyr::select(reason, amount, branch) %>%
        group_by(reason, branch) %>%
        summarise(
          loans = sum(amount)
        ) %>%
        ggplot(aes(x=reason, y=loans, group=1)) +
        geom_line(aes(colour=branch)) +
        geom_point() +
        theme_light() +
        xlab("Purpose") + ylab("Total Loans") +
        ggtitle("Total Loans by Branch and Purpose") +
        theme(legend.position = "none") +
        facet_wrap(~branch)
    })
    
    # total loans by reason
    output$tot_loan_by_reason <- renderPlotly({
      loans %>%
        dplyr::select(reason, amount) %>%
        group_by(reason) %>%
        summarise(
          loans = sum(amount)
        ) %>%
        ggplot(aes(x=reason, y=loans, group=1)) +
        geom_line() +
        geom_point() +
        theme_light() +
        xlab("Reason") + ylab("Total Loans") +
        ggtitle("Total Loans by Reason")
    })
    
    # total loans by branch
    output$tot_loan_by_branch <- renderPlotly({
      loans %>%
        dplyr::select(branch, amount) %>%
        group_by(branch) %>%
        summarise(
          loans = sum(amount)
        ) %>%
        ggplot(aes(x=branch, y=loans, group=1)) +
        geom_line() +
        geom_point() +
        theme_light() +
        xlab("Branch") + ylab("Total Loans") +
        ggtitle("Total Loans by Branch")
    })
    
    # total loans by year and branch
    output$tot_loan_by_year_branch <- renderPlotly({
      loans %>%
        dplyr::mutate(year = year(date)) %>%
        dplyr::select(year, amount, branch) %>%
        group_by(year, branch) %>%
        summarise(
          loans = sum(amount)
        ) %>%
        ggplot(aes(x=year, y=loans,group=1)) +
        geom_point(aes(colour=branch, size=loans)) +
        geom_line(linetype=1) +
        xlab("Year") +
        ylab("Total Loan") +
        ggtitle("Total Loans by year and branch") +
        theme_light() +
        theme(legend.position = "none") +
        facet_wrap(~branch)
    })
  }
  # ---------------- end trends -----------------------#
  
  #------------------- start render pdf report --------#
  {
    output$report <- renderUI({
      tags$iframe(style="width:100%;height:100%",
                  src="F:/Software Data Solutions/R/Financial Analysis/shiny-1.pdf")
    })
  }
  #------------------- end render pdf report --------#
}

shinyApp(ui, server)