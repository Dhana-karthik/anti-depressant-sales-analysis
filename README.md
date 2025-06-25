
## Summary: 

I have worked on NHS datasets during my virtual work experience in Digdata. This is report created on anti-depressant drugs sold in UK between 2021-2024. I have analyzed and predicted the sales of anti depressant drugs, found highly co-related variables, best fit model to predict its future sales. Along gave clear data visualization with R language using libraries tidyr, dplyr, and highcharter.


<br><br><br>

<br><br><br>





```{r setup, include=FALSE, eval=TRUE}
if (!requireNamespace("knitr", quietly = TRUE)) {install.packages("knitr")}
knitr::opts_knit$set(root.dir = rprojroot::find_root(criterion = rprojroot::is_rstudio_project))
```

```{r,echo=FALSE, message=FALSE}
library(dplyr)
library(highcharter)
library(tidyr)
library(knitr)

data = readRDS("Data/STEP_UP_REGIONAL_ANTIDEPRESSANTS.Rds")
```

Part One: To set the context for the report, this will be overall national and regional figures Part Two: The will be followed-up by a more exploratory analysis that delves into antidepressant prescribing cost trends.
Part Two Extension: Those attempting the extension task can then look at creating standardized metrics to understand more specific prescribing patterns.

# Part One

## 1.

```{r,echo=FALSE, message=FALSE}
#data
anual_items = data %>% 
  group_by(YEAR) %>%
  summarise(ITEMS = sum(ITEMS)) %>%
  ungroup()

# Chart
hchart(anual_items, "column", hcaes(YEAR, ITEMS)) %>% 
  hc_yAxis(title = list(text = "No. of drugs prescribed (in millions)")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text ="Annual anti-depressant sales (items)")

#data
anual_cost = data %>% 
  group_by(YEAR) %>%
  summarise(COST = sum(COST)) %>%
  ungroup()


# Chart
hchart(anual_cost, "column", hcaes(YEAR, COST)) %>% 
  hc_yAxis(title = list(text = "cost prescribed in millions (£)")) %>% 
  hc_xAxis(title = list(text = "Year")) %>% 
  hc_title(text ="Annual cost of anti-depressant prescribed in UK (2021-2024)")
```

## Observation: 

The annual prescribed of items increases gradually and every year approximately adds up to 3 million in the period of 2021-2024.

While the annual cost of items started with a drastic drop in 2022, from 285 Million in 2021 to 222 Million in 2022.
But after that, there was a little increase (227 Million) in 2023, and a little decrement in 2024 at last coming to the same cost similar (223 Million) in 2022.

## 2.

```{r,echo=FALSE, message=FALSE}
colnames(data)
#aggregation of data 

aggregated_data = data %>%
  group_by(REGION,YEAR) %>%
  summarise(ITEMS = sum(ITEMS,na.rm = TRUE),COST=sum(COST,na.rm = TRUE) ,.groups = "drop")

# new table for Total ITEMS for annually

items_table = aggregated_data %>% 
  select(REGION, YEAR, ITEMS)%>% 
  pivot_wider(names_from = YEAR, values_from = ITEMS) %>%
  mutate(changeRate_2021_24 = (`2021`-`2024`)*100/(`2021`),,change_2021_2024 = (`2021`-`2024`))%>%
  kable


# new table for total cost annually

cost_table = aggregated_data %>% 
  select(REGION, YEAR, COST) %>%
  pivot_wider(names_from = YEAR, values_from = COST) %>%
  mutate(changeRate_2021_24 = (`2021`-`2024`)*100/(`2021`),change_2021_2024 = (`2021`-`2024`))%>%
  kable
# printing both the tables of cost and items.

items_table
cost_table

```

## Observation :

For items annually prescribed from 2021 to 2024, East of England has a 9% decrease in prescribed medicines (993k decrease in medicines ), London 17.5% decrement which is 1.3 Million, Midlands 14.5% decrease which is 2.2 Million prescriptions, North east and Yorkshire 11% decrease (1.8 Million), Northwest sees 10.4% decrement (1.3 Million), Southeast experiences 9.5% of fall in medicine prescription (1 Million), Southwest has 7.2% of which least decrement rate than all regions (600K).

For the total cost annual prescribed from 2021 to 2024, East of England sees a 19.8% increment in medicines cost prescribed (a rise of 6.7 million pounds increase), for London 18.8% increment (a jump of 5.1 million pounds), in Midlands 19% increase (10 million pounds), Northeast and Yorkshire 23.8% increase (11.7 million pounds), Northwest 25% rise (10.6 million), Southeast 21% increase (9.9 million pounds), southwest experiences 23% of increment in the annual cost of medicines prescribed ( increment of 7.2 million pounds).

## 3.

data of top 10 sold items

```{r,echo=FALSE, message=FALSE}
# data of top 10 sold items
volume_AD_precribed = data %>% 
  group_by(DRUG) %>% 
  summarise(ITEMS = sum(ITEMS)) %>%
  ungroup()%>%
  top_n(10,ITEMS) %>%
  arrange(ITEMS)
 
#data:top 10 cost spent drugs

cost_AD_prescribed = data %>% 
  group_by(DRUG) %>% 
  summarise(COST=sum(COST)) %>%
  ungroup()%>%
  top_n(10,COST)%>%
  arrange(COST)

# items graph 
hchart(volume_AD_precribed, "column", hcaes(DRUG, ITEMS)) %>% 
  hc_yAxis(title = list(text = "total prescribed drugs in millions ")) %>% 
  hc_xAxis(title = list(text = "DRUG names")) %>% 
  hc_title(text ="top 10 prescribed anti-depressants")

# cost graph
hchart(cost_AD_prescribed, "column", hcaes(DRUG, COST)) %>% 
  hc_yAxis(title = list(text = "Total money spent on the drug (in pounds)")) %>% 
  hc_xAxis(title = list(text = "DRUG names")) %>% 
  hc_title(text ="Top 10 antidepressants on which huge money is spent by the UK citizens")

```

## OBSERVATION:

Uk mentally illed citizens are highly prescribed sertraline hydrochloride which is large (89 Million drugs sold) in number when compared to sum of all last 5 in the graph, next 3 drugs to that are, mirtrazapine, Citalopram hydrobromide, Amitripyline hydrochloride, are low in number when compared individually to sertraline hydrochloride, but almost double in count when all combined together.

Mental health patients in UK have spent 200Million pounds on consuming sertraline hydrochloride, at the period of 2021-2024, which makes it to stand on first position.
Followed by Venlafaxine 157 Million a year, which had 6th position in 1st graph.
After comes drugs like, Amitripyline hydrochloride, fluoxetine hydrochloride, Citalopram hydrobromide.

# Part Two (Longitudinal Analysis)

```{r,echo=FALSE, message=FALSE}
monthly_total = data %>%
  group_by(YM) %>%
  summarise(ITEMS = sum(ITEMS), COST = sum(COST))%>%
  ungroup()

# monthly trend for items sold monthly
total_monthly_trend = highchart() %>%
  hc_add_series(data = monthly_total, type = "line", hcaes(x = YM, y = ITEMS), name = "Total Items") %>%
  hc_title(text = "Monthly Trend of Antidepressant Prescribing (Volume)") %>%
  hc_xAxis(title = list(text = "Month")) %>%
  hc_yAxis(title = list(text = "Total Items")) 


total_monthly_trend

```

```{r,echo=FALSE, message=FALSE}
highchart() %>%
  hc_add_series(data = monthly_total, type = "line", hcaes(x = YM, y = COST), name = "Total COST") %>%
  hc_title(text = "Monthly Trends in Antidepressant Prescribing Patterns in COST") %>%
  hc_xAxis(title = list(text = "Month")) %>%
  hc_yAxis(title = list(text = "Total COST"))
```

```{r,echo=FALSE, message=FALSE}
monthly_drug_trends <- data %>%
  group_by(YM, DRUG) %>%
  summarise(total_items = sum(ITEMS, na.rm = TRUE),
            total_cost = sum(COST, na.rm = TRUE),
            .groups = "drop")


monthly_drug_trends%>%
  head(10)%>%
  kable

```

```{r,echo=FALSE, message=FALSE}
top_5_drugs <- monthly_drug_trends %>%
  group_by(DRUG) %>%
  summarise(total_items = sum(total_items)) %>%
  arrange(desc(total_items)) %>%
  head(5) %>%
  kable

top_5_drugs
```

For the overall sales graph of drugs monthly is affected by these top 5 highly sold drugs.

```{r,echo=FALSE, message=FALSE}
top_5_drugs <- monthly_drug_trends %>%
  group_by(DRUG) %>%
  summarise(total_cost = sum(total_cost)) %>%
  arrange(desc(total_cost)) %>%
  head(5) 

top_5_drugs%>%kable

```

For the overall cost spend on of drugs graph monthly is affected by these top 5 highly sold drugs.

```{r,echo=FALSE, message=FALSE}

top_drug_trends <- monthly_drug_trends %>%
  filter(DRUG %in% top_5_drugs$DRUG)

#Chart for Drug Trends
highchart()%>%
  hc_add_series(data =top_drug_trends,type ="line",hcaes(x = YM, y =total_items,group =DRUG))%>%
  hc_title(text = "Top 5 DRUGS affecting Monthly Volume Trends")%>%
  hc_xAxis(title = list(text = "Month"))%>%
  hc_yAxis(title = list(text = "Total Items sold"))


```

```{r,echo=FALSE, message=FALSE}
total_monthly_trend
```

# Part Two Extension (Antidepressant Case studies)

## 1.

```{r,echo=FALSE, message=FALSE}
# total no. of sertraline hydrochloride sold
sertraline_HC_count = data %>% 
  filter (DRUG =="Sertraline hydrochloride") %>%
  group_by(DRUG) %>%
  summarize(ITEMS = sum(ITEMS))
sertraline_HC_count%>%
  kable

#total drug sold count
total_drug_sold = data %>% 
  summarize(ITEMS = sum(ITEMS))

#precentage of sertraline hydrochloride in total drug sales
precentage_ofsertraline_HC = (sertraline_HC_count$ITEMS/total_drug_sold)*100
precentage_ofsertraline_HC%>%
  head(10)%>%
  kable
```

In conclusion, the highest sold drug sertraline hydrochloride's contribution in affecting the total prescribed by 25.65%, which is the greatest contribution than other drugs.

```{r,echo=FALSE, message=FALSE}
totS_HC_cost_items = data %>%
  filter (DRUG =="Sertraline hydrochloride") %>%
  summarise(ITEMS =sum(ITEMS),COST=sum(COST))

mean_ = totS_HC_cost_items$COST/totS_HC_cost_items$ITEMS 
mean_%>%kable

```

```{r,echo=FALSE, message=FALSE}
library(ggplot2)
library(dplyr)

data_i = data %>% 
  mutate(cost_per_items = COST/ITEMS )

ggplot(data_i %>% filter(DRUG== "Sertraline hydrochloride"), aes(x = cost_per_items)) +
  geom_histogram(binwidth =0.5, fill ="red", color ="black", alpha = 0.7) +
  labs(title = "Distribution of Mean Cost per sales count for Sertraline Hydrochloride",
       x = "Mean Cost per sales",
       y = "Frequency ") +
  theme_minimal()

```

### Personal interpretations:

```{r,echo=FALSE, message=FALSE}
# All drugs total volume
items_sales_monthly = data %>% 
  group_by(YM,YEAR) %>%
  summarise(ITEMS = sum(ITEMS))%>%
  ungroup

#Chart for Drug Trends
highchart()%>%
  hc_add_series(data =items_sales_monthly, type ="line", hcaes(x =YM, y =ITEMS, group =YEAR))%>%
  hc_title(text = "overall sales analysis of drugs in every period of the year (in months)")%>%
  hc_xAxis(title = list(text = "Month"))%>%
  hc_yAxis(title = list(text = "Total Items sold every month"))

```







```{r,echo=FALSE, message=FALSE}
library(treemap)
library(RColorBrewer)
data_result = data %>% 
  group_by(REGION)%>%
  summarise(ITEMS = sum(ITEMS))%>%
  ungroup()

treemap(data,
            index="REGION",
            vSize="ITEMS",
            type="index",title="Total sales of drugs in every region",
            palette = brewer.pal(7, "Blues"))

```


```{r,echo=FALSE, message=FALSE}
treemap(data,
            index="REGION",
            vSize="COST",
            type="index",title="Assert created by all drugs in every region",
            palette = brewer.pal(7, "Blues"))
```

The ratios of money attained from each regions are equivalent to the no. of sales from every region.

