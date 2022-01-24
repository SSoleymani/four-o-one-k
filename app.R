library(shiny)
library(tidyverse)
library(patchwork)
library(data.table)
library(plotly)

payroll_schedule <- tibble(
    "cutoff_date" = c(
        "12/29/21",
        "1/12/22",
        "1/26/22",
        "2/9/22",
        "2/23/22",
        "3/9/22",
        "3/23/22",
        "4/6/22",
        "4/20/22",
        "5/4/22",
        "5/18/22",
        "6/1/22",
        "6/15/22",
        "6/29/22",
        "7/13/22",
        "7/27/22",
        "8/10/22",
        "8/24/22",
        "9/7/22",
        "9/21/22",
        "10/5/22",
        "10/19/22",
        "11/2/22",
        "11/16/22",
        "11/30/22",
        "12/14/22"
    ),
    "eff_pay_date" = c(
        "1/7/22",
        "1/21/22",
        "2/4/22",
        "2/18/22",
        "3/4/22",
        "3/18/22",
        "4/1/22",
        "4/15/22",
        "4/29/22",
        "5/13/22",
        "5/27/22",
        "6/10/22",
        "6/24/22",
        "7/8/22",
        "7/22/22",
        "8/5/22",
        "8/19/22",
        "9/2/22",
        "9/16/22",
        "9/30/22",
        "10/14/22",
        "10/28/22",
        "11/10/22",
        "11/25/22",
        "12/9/22",
        "12/23/22"
    ),
    "num_remaining_pay_periods" = 26:1
) %>%
    mutate(across(1:2, as.Date, format = "%m/%d/%y"))
payroll_schedule


ui <- fluidPage(
    titlePanel("401k Contribution Simulator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            width = 3,
            numericInput(
                "base",
                "Base Salary",
                value = 100000,
                step = 500,
                min = 0
            ),
            numericInput(
                "num_pay_periods",
                "Pay Periods",
                value = 26,
                min = 1,
                max = 26
            ),
            tags$hr(),
            tags$h3("401k"),
            numericInput(
                "personal_401k_contri_pc",
                "Personal 401k Contribution %",
                6,
                min = 0,
                max = 100,
                step = 1
            ),
            sliderInput(
                "company_match_pc",
                "Company 401k Match %",
                75,
                min = 50,
                max = 100,
                step = 25,
                ticks = FALSE
            ),
            tags$h3("After-Tax"),
            numericInput(
                "personal_aftertax_frontload_pc",
                "After-Tax Front Load  %",
                0,
                min = 0,
                max = 100,
                step = 1
            ),
            numericInput(
                "personal_aftertax_frontload_num_periods",
                "For how many pay periods?",
                0,
                min = 0,
                max = 26,
                step = 1
            ),
            numericInput(
                "personal_aftertax_frontload_start_period",
                "Starting at which pay period?",
                1,
                min = 1,
                max = 26,
                step = 1
            ),
            numericInput(
                "personal_aftertax_contri_pc",
                "After-Tax % After Front Loading",
                0,
                min = 0,
                max = 100,
                step = 1
            ),
            tags$hr(),
            tags$h3("YTD Contributions"),
            tags$p("If you have completed any contributions already"),
            numericInput(
                "personal_contri_done",
                "401k Personal Contribution YTD Complete",
                0,
                0
            ),
            numericInput(
                "personal_aftertax_contri_done",
                "After Tax Personal Contribution YTD Complete",
                0,
                0
            ),
            numericInput("pay_periods_finished",
                         "Pay Periods Finished",
                         0, 0),
            tags$hr(),
            tags$h3("2022 IRS Limits"),
            numericInput("limit_401k_personal",
                         "401k Personal Limit",
                         20500, 0),
            numericInput("limit_401k_total",
                         "401k Total Limit",
                         61000, 0)
        ),

        mainPanel(width = 9,
                  fluidRow(
                      plotlyOutput("monthly_tsplot"),
                      plotlyOutput("ytd_areaplot")
                  )
        )
    )
)

server <- function(input, output) {
    df <- reactive({
        # Add limits & salary
        out <- payroll_schedule %>%
            mutate(
                irs_limit_401k_total = input$limit_401k_total,
                irs_limit_401k_personal = input$limit_401k_personal,
                salary_per_paycheck = input$base / input$num_pay_periods
            ) %>%
            data.table::as.data.table()

        # Add any existing 401k personal contributions
        personal_401k_done_contri_pc <-
            (input$personal_contri_done / input$pay_periods_finished) /
            (input$base / input$num_pay_periods)
        out[1:input$pay_periods_finished, personal_401k_contri_pc := personal_401k_done_contri_pc]
        out[(input$pay_periods_finished + 1):input$num_pay_periods, personal_401k_contri_pc := input$personal_401k_contri_pc /
                100]
        out[, personal_401k_contri := personal_401k_contri_pc * salary_per_paycheck]
        out[, personal_401k_ytd := cumsum(personal_401k_contri)]



        # Add any existing after-tax personal contributions
        personal_aftertax_done_contri_pc <-
            (input$personal_aftertax_contri_done / input$pay_periods_finished) /
            (input$base / input$num_pay_periods)
        out[1:input$pay_periods_finished, after_tax_contri_pc := personal_aftertax_done_contri_pc]

        # After-tax front loading
        out[, after_tax_contri_pc := 0]
        out[, after_tax_contri := 0]
        frontload_start <- input$personal_aftertax_frontload_start_period
        frontload_end <- input$personal_aftertax_frontload_start_period+input$personal_aftertax_frontload_num_periods
        out[frontload_start:frontload_end, after_tax_contri_pc := input$personal_aftertax_frontload_pc / 100]
        remaining_pay_period_start <- max(frontload_end, input$pay_periods_finished + 1)
        out[remaining_pay_period_start:input$num_pay_periods, after_tax_contri_pc := input$personal_aftertax_contri_pc / 100]
        out[, after_tax_contri := after_tax_contri_pc * salary_per_paycheck]
        out[, after_tax_ytd := cumsum(after_tax_contri)]

        # Company match 401k Contribution
        company_match_pc = (input$company_match_pc / 100) * min(0.06, input$personal_401k_contri_pc)
        out[, company_401k_contri := company_match_pc * salary_per_paycheck]
        out[, company_401k_ytd := cumsum(company_401k_contri)]

        # Check of violation of over-contribution
        # In which case Company will stop any matching contributions
        out[, company_match_pc := ifelse(
            personal_401k_ytd + after_tax_ytd + company_401k_ytd > input$limit_401k_total,
            0,
            company_match_pc
        )]
        out[, company_401k_contri := company_match_pc * salary_per_paycheck]
        out[, company_401k_ytd := cumsum(company_401k_contri)]

        # Final Summations
        out[, total_pretax_contri := personal_401k_contri + company_401k_contri]
        out[, total_pretax_ytd := cumsum(total_pretax_contri)]

        out[, total_contri := total_pretax_contri + after_tax_contri]
        out[, total_contri_ytd := cumsum(total_contri)]

        # Violations against the two IRS rules
        out[, personal_contri_violation := personal_401k_ytd > irs_limit_401k_personal]
        out[, total_contri_violation := total_contri_ytd > irs_limit_401k_total]

        out
    })

    # shiny::observe({
    #     df() %>% glimpse()
    # })

    output$monthly_tsplot <- renderPlotly({
        df() %>%
            select(date = eff_pay_date,
                   personal_401k_contri,
                   after_tax_contri,
                   company_401k_contri) %>%
            janitor::clean_names("title") %>%
            pivot_longer(-Date) %>%
            ggplot(aes(Date, value, color = name)) +
            geom_line() +
            # geom_step(direction = "mid") +
            geom_point(pch = 21) +
            scale_x_date(breaks = "1 month", date_labels = "%b") +
            scale_y_continuous(labels = scales::label_dollar(big.mark = ",")) +
            labs(y = "Monthly Contributions", x = "Effective Pay Date") +
            scale_color_manual(
                values = c(
                    "After Tax Contri" = "#118ab2",
                    "Company 401k Contri" = "#ffd166",
                    "Personal 401k Contri" = "#06d6a0"
                )
            ) +
            theme_minimal() -> p
        ggplotly(p)
    })

    output$ytd_areaplot <- renderPlotly({

        plot_df <-  df()[, .(date = eff_pay_date, personal_401k_ytd, after_tax_ytd, company_401k_ytd)] %>%
            janitor::clean_names("title") %>%
            pivot_longer(-Date)
        limits_df <- df()[, .(date = eff_pay_date, irs_limit_401k_total, irs_limit_401k_personal)] %>%
            janitor::clean_names("title") %>%
            pivot_longer(-Date)
        violation_df <- df()[personal_contri_violation | total_contri_violation,
            .(date = eff_pay_date,
              personal_contri_violation,
              personal_401k_ytd,
              total_contri_violation,
              total_contri_ytd)]
        violation_df[, v1 := personal_contri_violation * personal_401k_ytd]
        violation_df[, v2 := total_contri_violation * total_contri_ytd]
        violation_df[, .(date, v1, v2)] %>%
            pivot_longer(-date) %>%
            filter(value > 0) -> violation_df
        p <- plot_df %>%
            ggplot(aes(Date, value, fill = name)) +
            geom_area(alpha = 0.7) +
            scale_fill_manual(
                values = c(
                    "After Tax Ytd" = "#118ab2",
                    "Company 401k Ytd" = "#ffd166",
                    "Personal 401k Ytd" = "#06d6a0"
                )
            ) +
            geom_line(
                aes(Date, value, group = name),
                data = limits_df,
                lty = "dashed"
            ) +
            geom_point(
                data = violation_df,
                aes(x = date, y = value),
                pch = 21,
                color = "red",
                inherit.aes = FALSE
            ) +
            scale_x_date(breaks = "1 month", date_labels = "%b") +
            scale_y_continuous(labels = scales::label_dollar(big.mark = ",")) +
            labs(y = "YTD USD", x = "Effective Pay Date") +
            theme_minimal()

        ggplotly(p)

    })

    output$ytd_tsplot <- renderPlotly({

        plot_df <- df() %>%
            select(
                date = eff_pay_date,
                irs_limit_401k_total,
                irs_limit_401k_personal,
                total_pretax_ytd,
                company_401k_ytd,
                personal_401k_ytd,
                total_contri_ytd,
                contains("violation")
            )

        plot_df %>%
            filter(personal_contri_violation |
                       total_contri_violation) %>%
            mutate(
                personal_contri_violation = ifelse(
                    personal_contri_violation,
                    personal_contri_violation * personal_401k_ytd,
                    NA
                ),
                total_contri_violation = ifelse(
                    total_contri_violation,
                    total_contri_violation * total_contri_ytd,
                    NA
                )
            ) %>%
            select(date, contains("violation")) %>%
            pivot_longer(-date) %>%
            tidyr::drop_na() -> plot_df2

        plot_df %>%
            select(-contains('violation')) %>%
            janitor::clean_names("title") %>%
            pivot_longer(-Date) %>%
            ggplot(aes(Date, value, color = name, linetype = name)) +
            geom_line() +
            geom_point(pch = 21) +
            scale_color_manual(
                values = c(
                    "Total Contri Ytd" = "#30638e",
                    "Total Pretax Ytd" = "#00798c",
                    "Company 401k Ytd" = "#d1495b",
                    "Personal 401k Ytd" = "#edae49",
                    "Irs Limit 401k Total" = "#aa4465",
                    "Irs Limit 401k Personal" = "#aa4465"
                )
            ) +
            scale_linetype_manual(
                values = c(
                    "Total Contri Ytd" = "solid",
                    "Total Pretax Ytd" = "solid",
                    "Company 401k Ytd" = "solid",
                    "Personal 401k Ytd" = "solid",
                    "Irs Limit 401k Total" = "dashed",
                    "Irs Limit 401k Personal" = "dashed"
                )
            ) +
            geom_point(
                data = plot_df2,
                aes(x = date, y = value),
                pch = 21,
                color = "red",
                inherit.aes = FALSE
            ) +
            scale_x_date(breaks = "1 month", date_labels = "%b") +
            scale_y_continuous(labels = scales::label_dollar(big.mark = ",")) +
            labs(y = "YTD USD", x = "Effective Pay Date") +
            theme_minimal() -> p

        ggplotly(p)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
