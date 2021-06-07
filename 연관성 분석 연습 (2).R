install.packages("arules")
library(arules)
list.files()

basket <- readLines("basket.csv")
basket

basket_trans <- strsplit(basket, ",")
basket_trans

basket_trans <- as(basket_trans, "transactions")
basket_trans

inspect(basket_trans)

# 연관성규칙 도출(지지도 0.1 이상, 신뢰도 0.8 이상)
basket_apriori <- apriori(basket_trans, 
                          parameter = list(support = 0.1, confidence = 0.8))
basket_apriori
inspect(basket_apriori)

## 조건을 통해 원하는 연관성 분석 도출
# 향상도가 1.2 이상인 데이터 확인
inspect(subset(basket_apriori, subset = lift > 1.2))

# 조건에 삼겹살이 포함된 데이터 확인
inspect(subset(basket_apriori, subset = lhs %in% "삼겹살"))

# 조건에 삼겹살이나 사과가 포함된 데이터 확인
inspect(subset(basket_apriori, subset = lhs %in% c("삼겹살", "사과")))

# 조건에 맥주와 땅콩이 모두 포함된 데이터 확인
inspect(subset(basket_apriori, subset = lhs %ain% c("맥주", "땅콩")))

# 조건에 양파, 장어, 감을 제외한 데이터 확인
without3 <- apriori(basket_trans,
                    parameter = list(support = 0.1, confidence = 0.8),
                    appearance = list(none =  c("양파", "장어", "감")))
inspect(without3)


# 연관성 분석 시각화
set.seed(1234) # 변수 지정
visual <- head(sort(without3, by = "lift"), 20) # 오름차순 정렬
inspect(visual)

plot(visual)
plot(visual, method = "graph")
plot(visual, method = "graph", interactive = T)



subrules <- head(sort(basket_apriori, by="lift"), 20) # 오름차순 정렬
inspect(subrules)

plot(subrules)
plot(subrules, method = "graph")
plot(subrules, method = "graph", interactive = T)
