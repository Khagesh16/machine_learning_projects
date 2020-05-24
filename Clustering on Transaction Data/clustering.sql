#SQL Script for extracting attributes for Products based on Quarters:
SELECT StockCode, Quarter(InvoiceDateTime) as Quarter
sum(UnitPrice*Quantity) as TOTAL_REVENUE,
count(distinct InvoiceNo) as BASKETS,
count(distinct CustomerID) as DISTINCT_CUSTOMERS,
sum(UnitPrice * Quantity)/Sum(Quantity) as AVERAGE_PRICE
FROM onlineretail where Year(InvoiceDateTime) = 2011
GROUP BY StockCode
ORDER BY TOTAL_REVENUE DESC
LIMIT 2000;


#SQL Script for extracting attributes for Customer Data:
select CustomerID,
Count(distinct (StockCode)) as "distinct products bought",
Count(StockCode) as "number of products bought",
sum(UnitPrice * Quantity) AS Revenue,
count(distinct InvoiceNo) as Vists,
sum(Quantity * UnitPrice)/count(distinct InvoiceNo) as Avg_spent
from datamining.onlineretail
where customerID > 0  and InvoiceNo > 0 and Quantity > 0 and UnitPrice >= 0 and year(InvoiceDateTime) = 2011
Group by CustomerID
ORDER BY CustomerID 
limit 2000;


