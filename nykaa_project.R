install.packages("kableExtra")
install.packages("janitor")
library(janitor)
library(kableExtra)

setwd('C:/Users/sujal/OneDrive/Desktop')
getwd()


orders <- read.csv("nykaa_orders.csv") %>% clean_names()
customers <- read_csv("nykaa_customers.csv") %>% clean_names()
products <- read_csv("nykaa_products.csv") %>% clean_names()
marketing <- read_csv("nykaa_marketing.csv") %>% clean_names()
reviews <- read_csv("nykaa_reviews.csv") %>% clean_names()

orders <- orders %>%
  mutate(order_date = dmy(order_date),
         order_month = floor_date(order_date,"month"))

marketing <- marketing %>%
  mutate(marketing_month = ym(month))

order_details <- orders %>%
  left_join(customers, by = "customer_id") %>%
  left_join(products, by = "product_id") %>%
  left_join(reviews, by = "product_id")

monthly_marketing <- marketing %>%
  group_by(marketing_month) %>%
  summarise(total_spend = sum(spend, na.rm = TRUE),
            total_conversions = sum(conversions, na.rm = TRUE))

#TOP CONSUMERS
top_customers <- orders %>%
  group_by(customer_id) %>%
  summarise(TotalOrders = n()) %>%
  arrange(desc(TotalOrders)) %>%
  slice_head(n=10)

ggplot(top_customers,mapping = aes(x = reorder(customer_id,-TotalOrders), 
                                   y = TotalOrders)) +
  geom_col(fill = "orange") +
  labs(title = "Top 10 Customers by Order Volume", x = "Customer ID", y = "No. of Orders") +
  theme(panel.background = element_blank())

monthly_orders <- orders %>%
  group_by(order_month) %>%
  summarise(TotalOrders = n())

trend_data <- monthly_marketing %>%
  left_join(monthly_orders, by=c("marketing_month" = "order_month"))

trend_data_graph <- ggplot(trend_data, mapping = aes(x = marketing_month)) +
  geom_line(aes(y = total_spend, colour = "Marketing Spend")) +
  geom_point(aes(y = total_spend, colour = "Marketing Spend")) +
  geom_line(aes(y = TotalOrders*100000, colour = "Total Orders")) +
  geom_point(aes(y = TotalOrders*100000, colour = "Total Orders")) +
  scale_y_continuous(name = "Marketing Spend",sec.axis = 
                       sec_axis(~./100000,name = "Total Orders")) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Monthly Trend: Marketing Spend vs Orders", colour = "Legend") 
trend_data_graph
 
product_sales <- orders %>%
  group_by(product_id) %>%
  summarise(unit_sales = n())

product_ratings <- reviews %>%
  group_by(product_id) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE))
 
ratings_vs_sales <- product_sales %>%
  left_join(product_ratings, by = "product_id")

ratings_vs_sales_graph <- ratings_vs_sales %>%
  ggplot(aes(x = avg_rating, y = unit_sales)) +
  geom_point(color = "#FF69B4") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Do Higher-Rated Products Sell More?", 
       x = "Average Rating", y = "Units Sold") +
  theme(panel.background = element_blank())
ratings_vs_sales_graph

trend_data <- trend_data %>%
  mutate(conversion_rate = total_conversions/total_spend *100)
ggplot(trend_data, aes(x = marketing_month, y = conversion_rate)) +
  geom_line(color = "#20B2AA") +
  geom_point(color = "#20B2AA") +
  labs(title = "Monthly Conversion Rate from Marketing Campaigns",
       x = "Month", y = "Conversion Rate") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  theme(panel.background = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1))








