
# Dependencies ------------------------------------------------------------


library(magrittr)
library(shiny)
options(encoding = "UTF-8",
        warn = -1)


# Connect to database -----------------------------------------------


sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- DBI::dbConnect(RMySQL::MySQL(), dbname = "sys", user = 'admin', password = 'ArxEd01742!',host = 'arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com', port = 8209)
  # close db connection after function call exits
  on.exit(DBI::dbDisconnect(DB))
  # send Query to btain result set
  rs <- RMySQL::dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- RMySQL::fetch(rs, -1)
  # return the dataframe
  result
}

check_creds <- function(dbname, host, port, db_user, db_password) {
      function(user, password) {
        
        #Connect to DB
        con <- DBI::dbConnect(RMySQL::MySQL(), 
                              dbname = dbname, 
                              user = db_user, 
                              password = db_password,
                              host = host, 
                              port = port)
        
        on.exit(DBI::dbDisconnect(con))
        
        #Response gets the rows (if any) that correspond to the given user and password
        res <- RMySQL::fetch(RMySQL::dbSendQuery(con, glue::glue_sql("SELECT * 
                                                            FROM users 
                                                            WHERE user = {user} 
                                                            AND password = {password}
                                                            ", user = user, password = password, .con = con)))
        #If the rows do exist (more than zero rows) allowe access
        if (nrow(res) > 0) {
          list(result = TRUE, user_info = list(user = user, something = 123))
        } else {
          list(result = FALSE)
        }
      }
 }
 

# Data --------------------------------------------------------------------


 
 collaborative_names <- c('ACCEPT Education Collaborative',
                         'Assabet Valley Collaborative',
                         'Bi-County Collaborative',
                         'C.A.S.E. Concord Area SPED Collaborative',
                         'CAPS Education Collaborative',
                         'Cape Cod Collaborative',
                         'Central Massachusetts SPED Collaborative',
                         'Collaborative for Educational Services',
                         'Collaborative for Regional Educational Services (CREST)',
                         'Keystone Educational Collaborative',
                         'LABBB Collaborative',
                         'Lower Pioneer Valley Educational Collaborative',
                         'North River Collaborative',
                         'Northshore Education Consortium',
                         'Pilgrim Area Collaborative (PAC)',
                         'READS Collaborative',
                         'SEEM Collaborative',
                         'Shore Educational Collaborative',
                         'South Coast Educational Collaborative',
                         'South Shore Educational Collaborative',
                         'Southeastern Mass Educaational Collaborative (SMEC)',
                         'Southern Worcester County Educational Collaborative',
                         'The Education Cooperative',
                         'Valley Collaborative')

 short_collaborative_names <- c('ACCEPT',
                         'AVC',
                         'BICO',
                         'CASE',
                         'CAPS',
                         'CCC',
                         'CMC',
                         'CES',
                         'CREST',
                         'KEC',
                         'LABBB',
                         'LPVEC',
                         'NRC',
                         'NEC',
                         'PAC',
                         'READS',
                         'SEEM',
                         'SEC',
                         'SCEC',
                         'SSEC',
                         'SMEC',
                         'SWCEC',
                         'TEC',
                         'Valley Collaborative')
 
 collaborative_list <- purrr::set_names(collaborative_names, short_collaborative_names)
 
  gt_theme_538 <- function(data,...) {
    data %>%
        gt::opt_all_caps()  %>%
        gt::opt_table_font(
            font = list(gt::default_fonts())
        ) %>%
        gt::tab_style(
            style = gt::cell_borders(
                sides = "bottom", color = "black", weight = gt::px(3)
            ),
            locations = gt::cells_body(
                columns = gt::everything(),
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        gt::tab_options(
            column_labels.background.color = "white",
            table.border.top.width = gt::px(3),
            table.border.top.color = "white",
            table.border.bottom.color = "white",
            table.border.bottom.width = gt::px(3),
            table_body.border.bottom.color = "black",
            column_labels.border.top.width = gt::px(3),
            column_labels.border.top.color = "black",
            column_labels.border.lr.color = "grey",
            column_labels.vlines.style = "solid",
            table_body.vlines.style = "solid",
            table_body.vlines.color = "lightgrey",
            column_labels.border.bottom.width = gt::px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = gt::px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
  }
  
 
school_data <- read.csv('collaborative_data.csv')
sending_data <- readr::read_csv('sending_districts.csv')
sending_overview <- readRDS('sending_overview.rds')
districts_serving <- readr::read_csv('districts_serving.csv')

#Categories and options used for the plot feature
stat_categories <- c(
  'Basic Info',
  'Finances',
  'Tuitions',
  'Tuitions (Detailed)', 
  'Working Conditions'
)

statistic_names_full <- c(
  'Collaborative', #1
  'Director Base Salary',
  'Expenditures',
  'Districts Served',
  'Total Staff',
  'Total FTE',
  'Professional FTE',
  'Support FTE',
  'Profesionals in Union',
  'Non-Professionals in Union', #10
  'Total FTEs supporting PreK-22',
  'Gross Anual Payroll',
  'Avg. Staff Salary',
  'Avg. Member Tuition',
  'Avg. Non-Member Tuition',
  'Total Students',
  'Avg. Staff Salary per Student',
  'Member Per Diem',
  'Member School Year',
  'Member Summer', #20
  'Member Total Tuition',
  'Non-Member Per Diem',
  'Non-Member School Year',
  'Non-Member Summer',
  'Non-Member Total Tuition', 
  'Summer Days',
  'Member Per Diem PreK-22',
  'Member Per Diem Middle/High School',
  'Member Per Diem Elementary',
  'Member Per Diem High School',#30
  'Member Per Diem Post-Grad/Transition', 
  'Member Per Diem Elementary/Middle',
  'Member Per Diem Middle',
  'Member School Year PreK-22',
  'Member School Year Middle/High School',
  'Member School Year Elementary',
  'Member School Year High School',
  'Member School Year Post-Grad/Transition',
  'Member School Year Elementary/Middle',
  'Member School Year Middle', #40
  'Member Summer PreK-22',
  'Member Summer Middle/High School',
  'Member Summer Elementary',
  'Member Summer High School',
  'Member Summer Post-Grad/Transition',
  'Member Summer Elementary/Middle',
  'Member Summer Middle',
  'Member Total Tuition PreK-22',
  'Member Total Tuition Middle/High School',
  'Member Total Tuition Elementary',#50
  'Member Total Tuition High School', 
  'Member Total Tuition Post-Grad/Transition',
  'Member Total Tuition Elementary/Middle',
  'Member Total Tuition Middle',
  'Non-Member Per Diem PreK-22',
  'Non-Member Per Diem Middle/High School',
  'Non-Member Per Diem Elementary',
  'Non-Member Per Diem High School',
  'Non-Member Per Diem Post-Grad/Transition',
  'Non-Member Per Diem Elementary/Middle', #60
  'Non-Member Per Diem Middle',
  'Non-Member School Year PreK-22',
  'Non-Member School Year Middle/High School',
  'Non-Member School Year Elementary',
  'Non-Member School Year High School',
  'Non-Member School Year Post-Grad/Transition',
  'Non-Member School Year Elementary/Middle',
  'Non-Member School Year Middle',
  'Non-Member Summer PreK-22',
  'Non-Member Summer Middle/High School',#70
  'Non-Member Summer Elementary', 
  'Non-Member Summer High School',
  'Non-Member Summer Post-Grad/Transition',
  'Non-Member Summer Elementary/Middle',
  'Non-Member Summer Middle',
  'Non-Member Total Tuition PreK-22',
  'Non-Member Total Tuition Middle/High School',
  'Non-Member Total Tuition Elementary',
  'Non-Member Total Tuition High School',
  'Non-Member Total Tuition Post-Grad/Transition', #80
  'Non-Member Total Tuition Elementary/Middle',
  'Non-Member Total Tuition Middle',
  'Work Year',
  'Hours per Day',
  'Min. Hourly',
  'Max. Hourly',
  'Median Hourly',
  'Sick Days',
  'Personal Days',
  'Paid Holidays',#90
  'Vacation Days', 
  'Bachelors First',
  'Bachelors Top Step',
  'Masters First',
  'Masters Top Step',
  'Masters 15+ First',
  'Masters 15+ Top Step',
  'Masters 30+ First',
  'Masters 30+ Top Step',
  'CAGS First',#100
  'CAGS Top Step', 
  'Doctorate First',
  'Doctorate Top Step',
  'Bottom Right',
  'Additional Compensation',
  '# Years as Exec. Director',
  '# Years in Proffesion',
  'Sick Days Annually',
  'Sick Days Accumulate To',
  'Vacation Days Director', #110
  'Professional Dues Paid',
  'Travel Mileage',
  '% Medical Insurance Paid'

)

axis_options <- purrr::set_names(colnames(school_data), statistic_names_full)


overview_statistics <- c( #Full name for overview table stats
    "Total Enrollment",
    "Total Teacher FTE",
    "% Teachers Licensed",
    "Student:Teacher Ratio",
    "% Teachers Experienced",
    "% Teachers w/o Waiver",
    "% Teachers Teaching in Field",
    "Total Core Academic Classes",
    "% of Classes Taught by Highly Qualified Teachers",
    "Total Teacher Salaries",
    "Average Salary",
    "In-District Expenditure",
    "In-District Pupil FTE",
    "In-District Expenditure per Pupil",
    "Total Expenditure",
    "Total Pupil FTE",
    "Total Expenditure per Pupil"
)

# UI ----------------------------------------------------------------------

 
ui <- shinyMobile::f7Page(
    title = paste0("ArxEd | D³ Data-Driven Decisions"),
    options = list(dark = F),
    shinyjs::useShinyjs(),
    tags$script(src = "coll_script.js"),
    shinyMobile::f7SingleLayout(
        tags$head(
          tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png"),
          tags$link(rel = "stylesheet", type = "text/css", href = "coll_stylesheet.css"),
          tags$script(src = 'https://kit.fontawesome.com/8c5f048e9f.js',crossorigin="anonymous" )
          ),
        navbar = shinyMobile::f7Navbar( title = tags$div(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png',width='45px'),width='45px'),
                                                          tags$span(h3(paste0("  ArxEd | D³ Data Driven Decisions")), style = 'text-align:center;'), 
                                        tags$div(htmlOutput('logo_src'), class = 'top-district-logo')),
        tags$div(shinyMobile::f7Fab(
         inputId = 'pass',
         label = 'Change Password'
         ), class =  'pass-button'),
        tags$div(class = 'password-popup', shinyMobile::f7Popup(
         id = "popup1",
         title = tags$div(class = 'pass-title', h2("Change Password")),
         tags$div(
           shinyMobile::f7Text("newPass", "New Password", ""),
           shinyMobile::f7Text("newPassConf", "Confirm New Password", ""),
           br(),
           shinyMobile::f7Button('submit_pass', 'Submit', size = 'large')
           ),
         
      )),
      tags$div(class = 'select-card',
               shinyMobile::f7Shadow(
                   hover = T,
                   intensity = 16,
                   shinyMobile::f7Card(
                     tags$div(
                       id = 'menu-card',
                       tags$div(
                         style = 'display:flex',
                         tags$div(
                           tags$div(class = 'select-menu',
                                    htmlOutput('district_select')),
                           tags$div(
                             shinyMobile::f7Select(
                               inputId = 'year',
                               label = tags$span(class = 'input-label', h3('2. Select Year to Examine:')),
                               choices = c('2021-22'),
                               selected = '2021-22'
                             )
                           ),
                           class = 'inputs input-1'
                         ),
                         tags$div(
                           class = 'inputs',
                           shinyMobile::f7CheckboxGroup(
                             inputId = 'comp_cond',
                             label = h5('3. Choose comparison collaboratives:'),
                             choices = c('All Collaboratives', 'Region', 'My Selections')
                           )
                         ),
                       ),
                       tags$div(
                         selectizeInput(
                           "collaborative_comps",
                           label = tags$span(
                             class = 'input-label',
                             h3('5. View your comparison collaboratives and add/remove any manually:')
                           ),
                           choices = collaborative_names,
                           multiple = TRUE,
                           width = '100%'
                         )
                       )
                     ), 
                   footer = tagList(
                             tags$div(style= 'display:flex;', tags$div(
                                 shinyMobile::f7Button( #calls for DB replacing 
                                     "save",
                                     'Save "My Schools"',
                                     outline = FALSE,
                                     fill = TRUE,
                                     shadow = FALSE,
                                     rounded = FALSE,
                                     size = NULL
                                 ),
                                 class = 'card-button',
                             ),
                             tags$div(
                               tags$button(
                                 id=  'reset',
                                 type = 'button',
                                 class = 'button f7-action-button button-fill',
                                 onclick = 'uncheck("comp_cond")',
                                 'Reset'
                               ),class = 'card-button',
                             ),
                             tags$div(
                               tags$button(
                                 id=  'toggle_menu',
                                 type = 'button',
                                 class = 'button f7-action-button button-fill',
                                 'Toggle Menu Card'
                               ),class = 'card-button',
                             )
                             )
                         )
                   
                   )
               )), 
      shinyMobile::f7Tabs(
          animated = T,
            swipeable = F,
            id = "tabset",
            style = 'strong',
          shinyMobile::f7Tab(
               tabName = "Home",
                icon = shinyMobile::f7Icon("house"),
                active = TRUE,
               tags$div(
              shinyMobile::f7Button(
                inputId = 'down_home',
                label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Donwload as Image')) #fas fa classes corresponf to fontawesome
              ),
              class = "other-button card-button2"
            ),tags$div(style = 'display:flex;',
                 tags$div(class = 'side-panel',
                          style = 'padding:10px;
                                  margin-top:10px',
                   gt::gt_output('summary_table')
                 ),
                   htmlOutput('at_glance'),
                   tags$div(class = 'float-logo', htmlOutput('main_logo'))
                 ),
            tags$div(style = 'display:flex',
                 tags$div(class = 'side-panel',
                   shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       tags$div('Sending Districts', class = 'text1'),
                       tags$div(htmlOutput('comp_2'),style = 'margin-bottom:20px;'),
                       br(),
                       plotly::plotlyOutput('sending_cola', height = 'auto'),
                       plotly::plotlyOutput('sending_budget', height = 'auto'),
                       plotly::plotlyOutput('sending_enrollment', height = 'auto'),
                       plotly::plotlyOutput('sending_upper_left', height = 'auto'),
                       plotly::plotlyOutput('sending_lower_right', height = 'auto'),
                       plotly::plotlyOutput('sending_average_salary', height = 'auto')
                     )
                   )
                 ),
            tags$div(
              id = 'home-plots',style = 'width:75%;', 
              shinyMobile::f7Shadow(
                hover = T,
                intensity = 16,
                shinyMobile::f7Card(
                  tags$div(plotly::plotlyOutput('operational_budget', height = 'auto')),
                  tags$div(
                    style = 'display:flex;',
                    tags$div(class = 'grid-block',
                             plotly::plotlyOutput('gross_payroll', height = 'auto')),
                    tags$div(
                      class = 'grid-block',
                      plotly::plotlyOutput('average_salary', height = 'auto')
                    ),
                    tags$div(
                      class = 'grid-block',
                      plotly::plotlyOutput('total_enrollment', height = 'auto')
                    )
                  ),
                  tags$div(
                    style = 'display:flex;',
                    tags$div(
                      class = 'grid-block',
                      plotly::plotlyOutput('total_fte', height = 'auto')
                    ),
                    tags$div(
                      style = 'width:66%',
                      plotly::plotlyOutput('contract_plot', height = 'auto')
                      )
                    )
                  )
                )
              )
            )
          ), 
          shinyMobile::f7Tab(
               tabName = "Member Details",
               icon = shinyMobile::f7Icon("search"),
               active = F,
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(
                     style = 'display:flex;',
                     tags$div(
                       shinyMobile::f7Button(
                         inputId = 'down_overview',
                         label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Image')) #fas fa classes corresponf to fontawesome
                       ),
                       class = "card-button"
                     ),
                     tags$div(
                       shinyMobile::f7DownloadButton(#various download functionalities
                         outputId = 'down_overview_csv',
                         label = 'CSV'),
                       class = "card-button"
                     )
                   ),
                   tags$div( id = 'overview_table',
                              gt::gt_output('comparisons_table')
                             )
                  
                 )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Finances",
                icon = shinyMobile::f7Icon("money_dollar"),
                active = F,
               tags$div(
              shinyMobile::f7Button(
                inputId = 'down_finances',
                label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Donwload as Image')) #fas fa classes corresponf to fontawesome
              ),
              class = "other-button card-button2"
            ),
            tags$div(id = '#finance-plots',
                       )
          ),
          shinyMobile::f7Tab(
            tabName = "Staff",
            icon = shinyMobile::f7Icon("person_2"),
            active = F,
            tags$div(
              shinyMobile::f7Button(
                inputId = 'down_staff',
                label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Donwload as Image')) #fas fa classes corresponf to fontawesome
              ),
              class = "other-button card-button2"
            ),
            tags$div(id = '#staff-plots',
                     shinyMobile::f7Shadow(
                       hover = T,
                       intensity = 16,
                       shinyMobile::f7Card(
                                    plotly::plotlyOutput('staff_general')),
                         ),
                     shinyMobile::f7Shadow(
                       hover = T,
                       intensity = 16,
                       shinyMobile::f7Card(
                                    plotly::plotlyOutput('hourly_plot')),
                         ),
                     shinyMobile::f7Shadow(
                       hover = T,
                       intensity = 16,
                       shinyMobile::f7Card(
                                    plotly::plotlyOutput('time_off')),
                         )
                       )
          ),
          shinyMobile::f7Tab(
               tabName = "Students",
                icon = shinyMobile::f7Icon("person_3"),
                active = F
          ),
          shinyMobile::f7Tab(
               tabName = "Exec. Director",
                icon = shinyMobile::f7Icon("info"),
                active = F
          ),
          shinyMobile::f7Tab(
               tabName = "Tuition",
                icon = shinyMobile::f7Icon("folder"),
                active = F,
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(
                   style = 'display:flex;',
                   tags$div(class = 'quad-block',
                            plotly::plotlyOutput('per_diem_member')),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('year_member')
                   ),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('summer_member')
                     ),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('total_member')
                     )
                   )
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(
                   style = 'display:flex;',
                   tags$div(class = 'quad-block',
                            plotly::plotlyOutput('per_diem_non_member')),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('year_non_member')
                   ),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('summer_non_member')
                     ),
                   tags$div(
                     class = 'quad-block',
                     plotly::plotlyOutput('total_non_member')
                     )
                   )
                 )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Plots",
                icon = shinyMobile::f7Icon("chart_bar"),
                active = F,
               tags$div(
              shinyMobile::f7Button(
                inputId = 'down_plot',
                label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
              ),
              class = "other-button card-button2"
            ),
            tags$div(
              style = 'display:flex',
              tags$div(
                style = 'width:20%;',
                shinyMobile::f7Shadow(
                  hover = T,
                  intensity = 16,
                  shinyMobile::f7Card(
                    shinyMobile::f7Select(
                      'plot_type',
                      label = h2("Plot Type"),
                      choices = c("Bar", "Scatter", "Pie")
                    ),
                    uiOutput('y_cat_disp'),
                    uiOutput('y_var_disp'),
                    uiOutput('x_cat_disp'),
                    uiOutput('x_var_disp')
                  )
                )
              ),
              tags$div(
                style = 'width:80%;',
                id = 'plot_card',
                shinyMobile::f7Shadow(
                  hover = T,
                  intensity = 16,
                  shinyMobile::f7Card(plotly::plotlyOutput('userplot', height = 'auto'))
                )
                
              )
            )
          )
      )
    )
)
 
 ui <- shinymanager::secure_app(ui)
 
 
 server <- function(input, output, session) {
   
   #Info credential validation 

    res_auth <- shinymanager::secure_server(
    check_credentials = check_creds(
      dbname = "sys",
      host = "arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com",
      port = 8209,
      db_user = "admin",
      db_password = "ArxEd01742!"
    )
    )

  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })

  active_user <- reactive(stringr::str_remove_all(auth_output()$user, "[\r\n]"))
   
   
   query_district <- reactive({paste0("SELECT collaborative_name
                   FROM collaboratives
                   WHERE user =",
                 "'",active_user(),"';")})
   
   personal_collaborative_options <- reactive({
     if(active_user() %in% c('Mike', 'Brian', 'Santiago')){ #Admin access
       collaborative_names
     } else{
        sqlQuery(query_district()) %>% dplyr::pull(collaborative_name) #Restrict users to their own district
     }
   })
   
   output$district_select <- renderUI({ #Options module. All for admin, only one for the rest
     shinyMobile::f7Select(
       inputId = 'collaborative',
       label = tags$span(class = 'input-label', h3('1. My Collaborative')),
       choices = personal_collaborative_options(),
       selected = 'Bi-County Collaborative'
     )
   })
     
   
     collaborative <- reactive(input$collaborative)
     
     
     collaborative_logo <- reactive({ #access collaborative table to get name and logo associated with user
        query <- paste0("SELECT logo  
                          FROM collaboratives 
                          WHERE collaborative_name =", "'",  collaborative(),"';") 
        sqlQuery(query)%>% 
      as.character()
  })

  
  output$logo_src <- renderUI(
    tags$div(
      style = 'display:flex',
      tags$div(
        class = 'dis-name',
        collaborative()
      ),
      tags$div(
        tags$img(src = collaborative_logo(), height = '50px')
      )
    )
    
  )
  
  region <- reactive({
      query <- paste0("SELECT region  
                        FROM collaboratives 
                        WHERE collaborative_name =", "'",  collaborative(),"';") 
      sqlQuery(query)%>% 
      as.character()
  })
  
  #Depending on the filter selection (All, Region, Custom) output comparison vector
collaborative_comps_initial <- reactive({
      req(input$comp_cond)
      if(input$comp_cond[1] == 'All Collaboratives'){
            query <- paste0("SELECT collaborative_name  
                              FROM collaboratives;")
            sqlQuery(query) %>% 
              dplyr::filter(collaborative_name != collaborative()) %>% 
              dplyr::pull(collaborative_name)
      } else if(input$comp_cond[1] == 'Region'){
           query <- paste0("SELECT collaborative_name  
                            FROM collaboratives 
                            WHERE region =", "'",  region(),"';") 
        sqlQuery(query)%>% 
          dplyr::filter(collaborative_name != collaborative()) %>% 
              dplyr::pull(collaborative_name)
      } else if(input$comp_cond[1] == 'My Selections'){
        query <- paste0("SELECT school
                            FROM custom_collaborative
                            WHERE user =",
                    "'",active_user(),"';")
    
    sqlQuery(query) %>%
      dplyr::pull(school) %>%
      unique()
      }
  })
  
#If the condition or selcted collaborative changes, update the initial list
  observeEvent(list(input$comp_cond, input$collaborative), {
  updateSelectizeInput(
     inputId = "collaborative_comps",
     selected = collaborative_comps_initial() #It's an initial list because the final (collaborative_comps) can still be modified by the user
  )
  })
     
  comp_collabortives <- reactive({
     req(collaborative())
     input$collaborative_comps
     })
  
  
#reset button will clear the selected options (along with js defined in the UI)
  observeEvent(list(input$reset), {
      updateSelectizeInput(
        inputId = "collaborative_comps",
        selected = ""
      )
    })
  
    observeEvent(input$toggle_menu, { #Toggles input card (excluding footer)
        shinyjs::toggle(id = "menu-card")
    })
    
    # Change password logic
  
  observeEvent(input$pass, { #Observe for submit button. 
     shinyMobile::updateF7Popup(id = "popup1")
    })
  
  observeEvent(input$submit_pass, { #If passwords match, update on server. If not, notify user
    if(input$newPassConf == input$newPass){
      
      query <- glue::glue("UPDATE users
                           SET password = '{password}'
                           WHERE user = '{user}';
                           ", user = active_user(), password = input$newPass)
      sqlQuery(query)
      
      shinyMobile::f7Notif(
            text = "Your password has succesfully changed",
            icon = shinyMobile::f7Icon("checkmark_alt_circle"),
            title = "Password Updated",
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = F
      )

    }else {
      shinyMobile::f7Notif(
            text = "Passwords don't match. Please try again",
            icon = shinyMobile::f7Icon("xmark_circle"),
            title = "Update Error",
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = T
      )
    }
     
    })
  
  #WHen user clicks save schools, cycle through the comparison vector and upload to db
  observeEvent(input$save, {
    query <- paste0("DELETE FROM custom_collaborative
                    WHERE user =",
                    "'",active_user(),"'",";")
    sqlQuery(query)

    for (i in 1:length(comp_collabortives())) {
      query <- paste0(
        "REPLACE INTO custom_collaborative VALUES ('",
        paste0(active_user(), i),"',",
        "'",active_user(),"','",
        comp_collabortives()[i],"');"
      )
      sqlQuery(query)
    }
    
    shinyMobile::f7Notif( #Notify when update is finished. Along with total updates
            text = paste(i ,"Selections succesfully updated"),
            icon = shinyMobile::f7Icon("floppy_disk"),
            title = '"My Selections Updated"',
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = T
      )
  })
  

  
  
  # Title of home panel
    output$at_glance <- renderUI({
      req(collaborative())
      short_names <- purrr::map(comp_collabortives(), function(x) names(collaborative_list)[collaborative_list == x]) %>% 
                             unlist()
      comp_list <- paste(short_names, collapse = ", ")
     

      tags$div(
        tags$div(class = 'title',
          tags$span(paste0(collaborative()), style = "color:#DB9743"), " at a Glance"),
        tags$div(class = 'subtitle',
          paste0(collaborative(), " vs."), tags$span("Comparison Collaboratives", style = "color:#2a3a6e")),
        tags$div(class = 'comparison',
          "Comparing to: ", tags$span(comp_list, style = "color:#2a3a6e"))
        )
    })
  
    #Company Logo render
    output$main_logo <- renderUI({
        tags$img(src = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            width = '180px',
             alt = "")
    })
    
    summary_gt <- reactive({
      req(collaborative())
      coll_df <- school_data %>%
        dplyr::filter(collaborative == collaborative()) %>%
        dplyr::mutate(
          districts_served = paste("Districts Served:", districts_served),
          total_staff_headcount = paste("Total Staff:", total_staff_headcount),
          total_ft_es_of_staff = paste("Total FTE:", total_ft_es_of_staff),
          total_students = paste("Total Students:", total_students),
        ) %>%
        dplyr::select(districts_served, total_staff_headcount, total_ft_es_of_staff,total_students) %>%
        t() %>%
        as.data.frame()
      
      coll_df %>%
        gt::gt() %>%
        gt_theme_538() %>%
        gt::cols_label(V1 = gtExtras::img_header(
          label = collaborative(),
          img_url = collaborative_logo(),
          height = 60
        )) %>%
        gt::cols_width(gt::everything() ~ gt::pct(100))
    })
    
    #rendered with 100% width to fit the container
    output$summary_table <- gt::render_gt(expr = summary_gt(),
                                          width = gt::pct(100))
 
 plot_comp_df <- reactive({
   req(collaborative())
   school_data %>%
     dplyr::filter(
       collaborative %in% comp_collabortives() |
         collaborative == collaborative() |
         collaborative == 'dummy' #Dummy is necesary so that there's at least one district to compare nad the data frame is built properly
     ) %>%
     dplyr::mutate(is_district = ifelse(collaborative == collaborative(), collaborative(), "Others")) %>% #Either it's the user's collaborative or not
     dplyr::group_by(is_district) %>%
     dplyr::summarize( #Get the average for all categories for comparison collaboratives
       dplyr::across(
         fy22_executive_director_base_salary:lower_right,
         ~ mean(.x, na.rm = TRUE)
       )
     ) %>%
     dplyr::rename(aaa = is_district) %>% #make sure is_district is the first row
     dplyr::mutate(aaa  = ifelse(aaa == collaborative(), 'collaborative', aaa)) %>%
     t() %>%
     data.frame() %>%
     janitor::row_to_names(1) %>% #correct is_district column names
     dplyr::mutate(category = rownames(.)) #make a column with the stat name
   
 })

 
 sending_districts <- reactive({ #List of all districts that send to a given collaborative
   req(collaborative())
   (districts_serving %>% 
     dplyr::filter(collaborative == collaborative()) %>% 
     dplyr::pull(districts) %>% 
     stringr::str_split(., ','))[[1]] %>% 
     trimws()
 })
 
 
 
 sending_plot_df <- reactive({ #generate average figures for all the categories in the sending district database
   req(collaborative())
   sending_data %>%
     dplyr::filter(
       district_name %in% sending_districts()) %>%
     dplyr::summarize(dplyr::across(cola_2020_21:total_enrollment_2020_21,
                                    ~ mean(.x, na.rm = TRUE))) %>%
     t() %>%
     data.frame() %>%
     janitor::clean_names() %>% 
     dplyr::mutate(category = rownames(.))
 })

 
 cola_data <- reactive({ #This syntax is recurring. filter by category column to select necessary stats. In some case a conversion to numeric is needed as well
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('cola_2022_23','cola_2021_22','cola_2020_21'))
 })
 
 output$sending_cola <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        cola_data(),
        x = ~x,
        y = c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        height = 150,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.2%}<extra></extra>',
        texttemplate = '%{x:.1%}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Average COLA %',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 12)
        )
  })

 sending_budget_data <- reactive({
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('total_exp_2021_22','total_exp_2020_21'))
 })
 
 
 output$sending_budget <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        sending_budget_data(),
        x = ~x,
        y = c('2020-21', '2021-22'),
        type = 'bar',
        name = 'A',
        height = 150,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Average Budget',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 12)
        )
  })
 
 sending_enrollment_data <- reactive({
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('total_enrollment_2021_22','total_enrollment_2020_21'))
 })
 
 
 output$sending_enrollment <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        sending_enrollment_data(),
        x = ~x,
        y = c('2020-21', '2021-22'),
        type = 'bar',
        name = 'A',
        height = 150,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Average Enrollment',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), 
          font = list(size = 12)
        )
  })
 
 sending_upper_left_data <- reactive({
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('upper_left_2022_23','upper_left_2021_22', 'upper_left_2020_21'))
 })
 
 
 output$sending_upper_left <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        sending_upper_left_data(),
        x = ~x,
        y = c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        height = 150,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Upper Left',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), 
          font = list(size = 12)
        )
  })
 
 sending_lower_right_data <- reactive({
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('lower_right_2022_23','lower_right_2021_22', 'lower_right_2020_21'))
 })
 
 
 output$sending_lower_right <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        sending_lower_right_data(),
        x = ~x,
        y = c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        height = 150,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Lower Right',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), 
          font = list(size = 12)
        )
  })
 
 average_salary_data <- reactive({
   sending_plot_df() %>% 
     dplyr::filter(category %in% c('salary_avg_2020_21', 'salary_avg_2021_22', 'salary_avg_2022_23')) %>%
     dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
    
    output$sending_average_salary <- plotly::renderPlotly({
      req(collaborative())
      plotly::plot_ly(
        average_salary_data(),
        y = ~x,
        x = ~ c('2020-21', '2021-22', '2022-23'),
        type = 'scatter',
        height = 187,
        name = 'A',
        mode = 'lines+markers',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{y:.3s}<extra></extra>',
        texttemplate = '%{:.2s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T, range  = c(min(average_salary_data())*0.85, max(average_salary_data())*1.15)),
          title = 'Average Salary',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),  #margins so the title doesn't get cut off
          font = list(size = 12)
        )
  })
    
  budget_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('fy21_year_end_expenditures_actuals')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
 
    
  output$operational_budget <- plotly::renderPlotly({ #render plot with two traces.
      plotly::plot_ly(                                #One for the district column (orange), and one for the others column (second trace; blue)
        data = budget_data(),
        x = ~ Others,
        y = ~ c(''),
        type = 'bar',
        name = 'Others',
        height = 310,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = 'Others: %{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(x = ~ collaborative,
                  marker = list(color = '#DB9743'),
                   hovertemplate = paste0(collaborative(), ': %{x:.3s}<extra></extra>')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, range = c(0, max(budget_data(), na.rm = T)*1.15)),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Operational Budget',
          autosize = T,
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 12)
         )
  })
 
 
   fte_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('total_ft_es_of_staff')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    
  output$total_fte <-  plotly::renderPlotly({
        plotly::plot_ly(
        fte_data(),
        x = ~c('Total FTE'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        height = 250,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array"),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(fte_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 13)
        )
    })
  
  enrollment_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('total_students')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    
  output$total_enrollment <-  plotly::renderPlotly({
        plotly::plot_ly(
        enrollment_data(),
        x = ~c(''),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        height = 250,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array"),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(enrollment_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Total Enrollment',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 13)
        )
    })
  
  salary_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('avg_staff_annual_salary')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    
  output$average_salary <-  plotly::renderPlotly({
        plotly::plot_ly(
        salary_data(),
        x = ~c(''),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        height = 250,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array"),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(salary_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Average Staff Salary',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), 
          font = list(size = 13)
        )
    })
  
   payroll_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('gross_anual_payroll')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
    
  output$gross_payroll <-  plotly::renderPlotly({
        plotly::plot_ly(
        payroll_data(),
        x = ~c(''),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        height = 250,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.3s}<extra></extra>'),
        texttemplate = '%{y:.2s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.3s}<extra></extra>',
                   texttemplate = '%{y:,.2s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array"),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(payroll_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Gross Payroll',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 13)
        )
    })
  
  contract_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('bachelors_step_1', 'lower_right')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    
  output$contract_plot <-  plotly::renderPlotly({
        plotly::plot_ly(
        contract_data(),
        x = ~c('Upper Left', 'Lower Right'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        height = 250,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('a', 'b')),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(contract_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Contract Elements',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4),
          font = list(size = 13)
        )
    })
  
  observeEvent(input$down_home, { #take "screenshot" of home selector
    shinyscreenshot::screenshot(selector = '#home-plots' , filename = paste(collaborative(), 'Home'), scale = 4)
  })
  

# Overview ----------------------------------------------------------------

  observeEvent(input$down_overview, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#overview_table' , filename =  paste(collaborative(),'Sending Districts'), scale = 4)
  })  
  
  
  
  
   output$down_overview_csv = downloadHandler(
     filename = function() {
       paste(collaborative(), "Sending Districts.csv")
     },
     content = function(file) {
       write.csv(comp_df(), file, row.names = F)
     }
   )
   
   comp_df <- reactive({
     req(sending_districts())
     sending_overview %>% 
       dplyr::filter(district_name %in% sending_districts() ) %>% 
       t() %>%
       janitor::row_to_names(1) %>% 
       magrittr::set_rownames(c(overview_statistics)) %>%
       as.data.frame() %>% 
       dplyr::mutate(dplyr::across(.fns = as.numeric),
                     ` ` = rownames(.)) %>% 
       dplyr::select(` `, colnames(.)[1:length(colnames(.))-1]) %>% 
       dplyr::arrange(match(get(colnames(.)[1]),
                              c("Total Expenditure",
                                "Total Expenditure per Pupil",
                                "In-District Expenditure",
                                "In-District Expenditure per Pupil",

                                "Total Teacher FTE",
                                "Total Teacher Salaries",
                                "Average Salary",

                                "Total Enrollment",
                                "Total Pupil FTE",
                                "In-District Pupil FTE",
                                "Student:Teacher Ratio",

                                "Total Core Academic Classes",
                                "% Teachers Licensed",
                                "% Teachers Experienced",
                                "% Teachers w/o Waiver",
                                "% Teachers Teaching in Field",
                                "% Core Classes Taught by Exp Teachers")),
                        dplyr::desc(get(colnames(.)[1])))
   })
   
   value_cols <- reactive({
     if (ncol(comp_df()) > 2) {
       append(2, seq(3, ncol(comp_df()), 1))
     }
     else{
       c(2)
     }
   })
   
   comp_table <- reactive({
     col_labs <- purrr::set_names(colnames(comp_df()), c(seq(ncol(comp_df()))))
   comp_df() %>%
                # Arrange in order of columns so we can do row groupings later
                magrittr::set_colnames(c(seq(ncol(comp_df())))) %>%
                gt::gt() %>%
                # Set column labels
                gt::cols_label(.list = col_labs) %>%
                gt_theme_538() %>%
                gt::tab_row_group(
                    label = "Teacher Stats",
                    rows = c(12:17)
                ) %>%
                gt::tab_row_group(
                    label = "Enrollment",
                    rows = c(8:11)
                ) %>%
                gt::tab_row_group(
                    label = "Teacher Salaries",
                    rows = c(5:7)
                ) %>%
                gt::tab_row_group(
                    label = "Expenditure",
                    rows = c(1:4)
                ) %>%
                gt::fmt_number(columns = value_cols(), rows = c(8,12), decimals = 0) %>%
                gt::fmt_number(columns = value_cols(), rows = c(5,9:11), decimals = 1) %>%
                gt::fmt_currency(columns = value_cols(), rows = c(1:4,6:7), decimals = 0) %>%
                gt::fmt_percent(columns = value_cols(), rows = c(13:17), decimals = 1) %>%
                gt::cols_align("center", columns = 2:max(c(seq(ncol(comp_df()))))) %>%
                # gt::tab_style(
                #     style = list(
                #         gt::cell_borders(
                #             sides = c("left"),
                #             color = "black",
                #             weight = gt::px(2)
                #         ),
                #         gt::cell_borders(
                #             sides = c("left"),
                #             color = "black",
                #             weight = gt::px(2)
                #         )
                #     ),
                #     locations = list(
                #         gt::cells_body(
                #             columns = value_cols()
                #         ),
                #         gt::cells_column_labels(
                #             columns = value_cols()
                #         )
                #     )
                # ) %>%
                # gt::tab_source_note(
                    # source_note = "Note: Only enrollment data available for 2021/22;
                    #                 Teacher salaries available beginning in 2019/20;
                    #                 Teacher stats vary in availability by year/district") %>%
                gt::tab_header(
                    title = paste0("Detailed Info for ",collaborative(),"'s Sending Districts"),
                    subtitle = paste0("Stats for ", "2020-21", "; See footnote for data availability details")
                ) %>%
                # Hacky way to ensure that all '% Diff' columns are the same size no matter how
                # many comp districts the user enters
                gt::tab_options(table.font.size = 14)
    })
    
    # Actually render the {gt} table (need {gt} object to save in next function)
    output$comparisons_table <- gt::render_gt({comp_table()})
   
   
  

# Staff -------------------------------------------------------------------

  observeEvent(input$down_staff, { #take "screenshot" of staff selector
    shinyscreenshot::screenshot(selector = '#staff-plots' , filename = paste(collaborative(), 'Staff'), scale = 4)
  })
  
  staff_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('total_staff_headcount','total_ft_es_of_staff','professional_ft_es', 'support_ft_es')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$staff_general <-  plotly::renderPlotly(
        plotly::plot_ly(
        staff_data(),
        x = ~c('Total Staff', 'Staff FTE', 'Proffesional FTE', 'Support FTE'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.1f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.2f}<extra></extra>',
                   texttemplate = '%{y:,.1f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(staff_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  

# Finance -----------------------------------------------------------------

observeEvent(input$down_finances, { #take "screenshot" of finance selector
    shinyscreenshot::screenshot(selector = '#finance-plots' , filename = paste(collaborative(), 'Finances'), scale = 4)
  })  
  
  hourly <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('min_hrly','max_hrly', 'median_hrly')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$hourly_plot <-  plotly::renderPlotly(
        plotly::plot_ly(
        hourly(),
        x = ~c('Min. Hourly', 'Max Hourly', 'Median Hourly'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.1f}<extra></extra>'),
        texttemplate = '%{y:.1f}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.2f}<extra></extra>',
                   texttemplate = '%{y:,.1f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(hourly(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F, 
          title = 'Para-professionals',
          margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
  time_off_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('sick_days','personal_days', 'paid_holidays', 'vacation_days')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$time_off <-  plotly::renderPlotly(
        plotly::plot_ly(
        time_off_data(),
        x = ~c('Sick Days', 'Personal Days', 'Paid Holidays', 'Vacation Days'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.0f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.0f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(hourly(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F, 
          margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
 

# Tuition -----------------------------------------------------------------

 pd_member <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('per_diem_in_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$per_diem_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        pd_member(),
        x = ~c('Per Diem'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15),
          title = 'Member Districts'
        )
    )
  
  year_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('school_year_in_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$year_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        year_member_data(),
        x = ~c('School Year'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
  summer_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('summer_in_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$summer_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        summer_member_data(),
        x = ~c('Summer'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
  total_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('total_tuition_in_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$total_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        total_member_data(),
        x = ~c('Total'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
   pd_non_member <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('per_diem_out_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$per_diem_non_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        pd_non_member(),
        x = ~c('Per Diem'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15),
          title = 'Non-Member Districts'
        )
    )
  
  year_non_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('school_year_out_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$year_non_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        year_non_member_data(),
        x = ~c('School Year'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
  summer_non_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('summer_out_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
  


  output$summer_non_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        summer_non_member_data(),
        x = ~c('Summer'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )
  
  total_non_member_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('total_tuition_out_district')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$total_non_member <-  plotly::renderPlotly(
        plotly::plot_ly(
        total_non_member_data(),
        x = ~c('Total'),
        y = ~ collaborative,
        type = 'bar',
        name = collaborative(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(collaborative()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F, margin = list(l = 0,r = 0,b = 0,t = 50,pad = 4), font = list(size = 15)
        )
    )

# Plots -------------------------------------------------------------------


    #display major stat categories
output$x_cat_disp <- renderUI({
    req(input$plot_type)
    if(input$plot_type == 'Scatter'){
      shinyMobile::f7Select(
            "x_cat",
            label = h3("X-Axis Category"),
            choices = stat_categories
      )
    } else{
      
    } 
    })
    
    
    #display minor stat options
output$x_var_disp <- renderUI({
    req(input$plot_type)
    if(input$plot_type == 'Scatter'){
      shinyMobile::f7Select(
            "x_cat",
            label = h3("X-Axis Variable"),
            choices = c('')
      )
    } else{
      
    } 
    })
    
output$y_cat_disp <- renderUI({
    req(input$plot_type)
      shinyMobile::f7Select(
         'y_cat',
         label = h3("Y-Axis Category"), 
         choices = stat_categories
       )
    })

output$y_var_disp <- renderUI({
    req(input$plot_type)
      shinyMobile::f7Select(
         'y_var',
         label = h3("Y-Axis Variable"), 
         choices = c('')
       )
    })  
 
output$x_var_disp <- renderUI({
  req(input$x_cat)
  if(input$plot_type == 'Scatter'){
  if (input$x_cat == "Basic Info") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[c(4:8,16)])
  }
  else  if (input$x_cat == "Finances") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[c(2,3,12,13,17,92:104)])
  }
  else  if (input$x_cat == "Tuitions") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[c(14,15,18:25)])
  }
  else  if (input$x_cat == "Tuitions (Detailed)") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[c(27:82)])
  }
  else if (input$x_cat == "Working Conditions") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[c(9:11, 26, 83:91, 2, 106,107, 112,113)])
  
  }
  }
})

output$y_var_disp <- renderUI({
  req(input$y_cat)
  if (input$y_cat == "Basic Info") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[c(4:8,16)])
  }
  else  if (input$y_cat == "Finances") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[c(2,3,12,13,17,92:104)])
  }
  else  if (input$y_cat == "Tuitions") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[c(14,15,18:25)])
  }
  else  if (input$y_cat == "Tuitions (Detailed)") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[c(27:82)])
  }
  else if (input$y_cat == "Working Conditions") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[c(9:11, 26, 83:91, 2, 106,107, 112,113)])
  
  }
})

y <- reactive(input$y_var)
x <- reactive(input$x_var)

plotdata <- reactive({
  req(collaborative())
  plot_data <- school_data %>%
    dplyr::filter(
      collaborative %in% comp_collabortives() | collaborative == collaborative() &
      !is.na(y())
    ) %>%
    dplyr::select(collaborative, y(), x()) %>% 
    dplyr::mutate(short_collaborative =  purrr::map(collaborative, function(x) names(collaborative_list)[collaborative_list == x]) %>% 
                             unlist()) %>% 
    dplyr::filter(!is.na(y())) %>% 
    unique()
})

y_col <-reactive({
   plotdata() %>%
    dplyr::filter(!is.na(y())) %>%
    dplyr::pull(y())
})



output$userplot <- plotly::renderPlotly({
        if (input$plot_type == "Scatter"){
          req(x())
          req(y())
            plotly::plot_ly(
              data =plotdata(),
                x = ~get(x()),
                y = ~get(y()),
                type = "scatter",
                mode = "markers",
                marker = list(
                    size = 30,
                    opacity = .5
                ),
                color = ~short_collaborative,
                colors = ggsci::pal_jco()(10),
                hovertemplate = ~paste(collaborative,
                                      "<br>",
                                      names(axis_options)[axis_options == x()], ":",
                                      '%{x:.4s}',
                                      "<br>",
                                      names(axis_options)[axis_options == y()], ":",
                                      '%{y:.4s}',
                                      "<extra></extra>"),
                height = 630) %>%
              plotly::layout(
                yaxis = list(title = names(axis_options)[axis_options == y()], fixedrange = T),
                xaxis = list(title = names(axis_options)[axis_options == x()], fixedrange = T),
                title = paste0(
                  collaborative(),
                  " vs. Comparison Collaboratives",
                  "<br><sup>",
                  names(axis_options)[axis_options == y()],
                  "vs.",
                  names(axis_options)[axis_options == x()],
                  "</sup>"
                ),
                       images = list(
                           source = base64enc::dataURI(file = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png"),
                           x = 1, y = 0.1,
                           sizex = 0.2, sizey = 0.2
                       ))
        }else if (input$plot_type == "Pie"){
          req(y())
            y_mean <- mean(y_col(), na.rm = T)
            plotly::plot_ly(
              data = plotdata(),
              labels = ~ collaborative,
              values = ~ get(y()),
              marker = list(colors = ggsci::pal_jco()(10)),
              textinfo = 'label+percent',
              hoverinfo = 'text',
              text = ~paste(names(axis_options)[axis_options == y()], ':', get(y())),
              height = 630
            ) %>%
              plotly::add_pie(hole = 0.4) %>%
              plotly::layout(
                title = paste0(collaborative(), " vs. Comparison Collaboratives",
                               "<br><sup>", names(axis_options)[axis_options == y()], "</sup>"),
                               margin = list(l = 50,r = 50,b = 50,t = 50,pad = 4), 
                       legend = list(font = list(size = 11))
                )
            
        }else{
          req(y())
            y_mean <- mean(y_col(), na.rm = T)
            
            plotly::plot_ly(
              data =plotdata(),
                x = ~collaborative,
                y = ~get(y()),
                type = "bar",
                color = ~short_collaborative,
                colors = ggsci::pal_jco()(10),
                hovertemplate = paste('%{x}',
                                      "<br>",
                                      names(axis_options)[axis_options == y()], ":",
                                      '%{y:.4s}', "<extra></extra>"),
                texttemplate = '%{y:.3s}',
                textposition = 'outside',
                height = 630) %>%
                plotly::layout(xaxis = list(title = "Collaborative", showticklabels = F, fixedrange = T),
                       yaxis = list(title = names(axis_options)[axis_options == y()] , fixedrange = T),
                       title = paste0(collaborative(), " vs. Comparison Collaboratives",
                                     "<br><sup>", names(axis_options)[axis_options == y()], "</sup>"),
                       legend = list(font = list(size = 11)),
                       annotations = list(
                           x = 0,
                           y = y_mean,
                           text = paste("Avg:", round(y_mean,1)),
                           xref = "x",
                           yref = "y",
                           showarrow = TRUE,
                           arrowhead = 7,
                           ax = 20,
                           ay = -40
                       ),
                       shapes = list(type='line', x0 = 0, x1 = (length(y_col()) - sum(is.na(y_col()))), y0=y_mean, y1=y_mean, line=list(dash='dot', width=1)),
                       images = list(
                           source = base64enc::dataURI(file = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png"),
                           x = 1, y = 0.1,
                           sizex = 0.2, sizey = 0.2
                       ))
        }
        
    })

observeEvent(input$down_plot, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#plot_card' , filename =  paste(collaborative(),'Custom Plot'), scale = 4)
  })


 
 }
 
shinyApp(ui = ui, server = server)
 
 
 
 
 
 
 
