
###website indicators
###plotly

###m2 
plot <- plot_ly(data=df,
                x=~date, y= ~change,
                type='bar')  %>%
  layout(title="M2 Supply",
         yaxis=list(title="Percent Change"),
         xaxis=list(title="Date"))


htmlwidgets::saveWidget(plot, "m2.html") 

Sys.setenv("plotly_username"="jconns")
Sys.setenv("plotly_api_key"="wwqtSXsDe6gBBR2kI1a1")

api_create(plot, "M2 Supply")


###output_gap

gdp_indicator <- gdp %>%
  filter_index("2018" ~ .)
head(gdp_indicator)

gdp_gap <- plot_ly(data=gdp_indicator,
                x=~date, y= ~gdp_change,
                type='bar')  %>%
  layout(title="Output Gap",
         yaxis=list(title="Gap"),
         xaxis=list(title="Date"))


htmlwidgets::saveWidget(gdp_gap, "gdp_gap.html") 

Sys.setenv("plotly_username"="jconns")
Sys.setenv("plotly_api_key"="wwqtSXsDe6gBBR2kI1a1")

api_create(gdp_gap, "Gap")


###funds rate

taylor_indicator <- taylor_tsibble %>%
  filter_index("2018" ~ .)
head(gdp_indicator)

taylor_plotly <- plot_ly(data=taylor_indicator,
                   x=~date, y= ~funds_rate,
                   type="bar")  %>%
  layout(title="FUnds Rate",
         yaxis=list(title="Rate"),
         xaxis=list(title="Date"))


htmlwidgets::saveWidget(taylor_plotly, "taylor_plotly.html") 

Sys.setenv("plotly_username"="jconns")
Sys.setenv("plotly_api_key"="wwqtSXsDe6gBBR2kI1a1")

api_create(taylor_plotly, "Funds Rate")

###unemployment

unemploy_plotly <- plot_ly(data=taylor_indicator,
                         x=~date, y= ~unemployment,
                         type="bar")  %>%
  layout(title="Unemployment Rate",
         yaxis=list(title="Rate"),
         xaxis=list(title="Date"))


htmlwidgets::saveWidget(unemploy_plotly, "unemploy_plotly.html") 

Sys.setenv("plotly_username"="jconns")
Sys.setenv("plotly_api_key"="wwqtSXsDe6gBBR2kI1a1")

api_create(unemploy_plotly, "Unemployment")

