library(ggplot2)
library(dplyr)
library(car) # 회귀분석과 관련된 패키지
library(corrplot) # 상관 행렬 패키지

# 1. 철사의 공칭두께는 1.5mm 데이터 작성
x <- c(1.47, 1.53, 1.63, 1.77, 1.64,
       1.60, 1.50, 1.59, 1.73, 1.51,
       1.58, 1.42, 1.48, 1.62, 1.44,
       1.56, 1.47, 1.54, 1.62, 1.49,
       1.44, 1.44, 1.61, 1.38, 1.54,
       1.62, 1.38, 1.54, 1.55, 1.46,
       1.60, 1.60, 1.50, 1.70, 1.53,
       1.58, 1.45, 1.48, 1.47, 1.56,
       1.39, 1.34, 1.57, 1.53, 1.56,
       1.35, 1.47, 1.42, 1.46, 1.50,
       1.52, 1.37, 1.53, 1.34, 1.60,
       1.38, 1.48, 1.60, 1.54, 1.67,
       1.32, 1.34, 1.55, 1.64, 1.57,
       1.65, 1.58, 1.67, 1.47, 1.63,
       1.53, 1.43, 1.57, 1.75, 1.47)
x

hist(x) # 히스토그램
boxplot(x) # 상자 그림

df1 = data.frame(thickness = c(1.47, 1.53, 1.63, 1.77, 1.64,
                              1.60, 1.50, 1.59, 1.73, 1.51,
                              1.58, 1.42, 1.48, 1.62, 1.44,
                              1.56, 1.47, 1.54, 1.62, 1.49,
                              1.44, 1.44, 1.61, 1.38, 1.54,
                              1.62, 1.38, 1.54, 1.55, 1.46,
                              1.60, 1.60, 1.50, 1.70, 1.53,
                              1.58, 1.45, 1.48, 1.47, 1.56,
                              1.39, 1.34, 1.57, 1.53, 1.56,
                              1.35, 1.47, 1.42, 1.46, 1.50,
                              1.52, 1.37, 1.53, 1.34, 1.60,
                              1.38, 1.48, 1.60, 1.54, 1.67,
                              1.32, 1.34, 1.55, 1.64, 1.57,
                              1.65, 1.58, 1.67, 1.47, 1.63,
                              1.53, 1.43, 1.57, 1.75, 1.47))
df1

# 히스토그램
ggplot(data = df1, aes(thickness)) + # ggplot2 패키지 사용
  geom_histogram(fill = "#F8766D", colour="black") + # 히스토그램 함수 사용(색과 선 지정)
  theme_light() + # 그래프 테마 설정
  ggtitle("히스토그램") + # 그래프 제목 
  xlab('두께') + # x축 제목
  ylab('빈도수') + # y축 제목
  scale_y_continuous(breaks = c(1:10)) + # y축 라벨링
  theme(plot.title = element_text(face = "bold", # 제목 서식 지정
                                  size = 20,
                                  hjust = 0.5))
# 상자 그림
ggplot(data = df1, aes(thickness)) + # ggplot2 패키지 사용
  geom_boxplot() + # 상자 그림 함수 사용
  theme_light() + # 그래프 테마 설정
  ggtitle("상자 그림") + # 그래프 제목 
  xlab('두께') + # x축 제목
  coord_flip() + # 그래프 90도 회전
  theme(plot.title = element_text(face = "bold", # 제목 서식 지정
                                  size = 20,
                                  hjust = 0.5))

# 2. 상기 데이터는 정규분포를 따르는가?
# (1) Shaprio-Wilks test
# 귀무가설: 모집단은 정규분포를 따른다. 대립가설: 귀무가설은 정규분포를 따르지 않는다.

shapiro.test(df1$thickness)

# 유의확률이 0.6582로 유의수준 0.05를 넘어서므로 귀무가설을 기각할 수 없다. 따라서 본 자료는 정규분포를 따른다고 할 수 있다. 하지만 통계량만으로 정규성 여부를 검정하는 것은 부족하기에 반드시 그래프를 통해서 정규성  여부를 확인할 필요가 있다.

# (2) Q-Q plot

qqnorm(df1$thickness)
qqline(df1$thickness, lwd = 2, col = 'red')

# 그래프의 점들이 직선에 가깝게 붙어 있으면 정규성을 띤다고 가정한다. 도출한 그래프는 산점도 점들이 직선에 거의 일치하므로 정규분포를 따른다고 할 수 있다.


# 3. 다음 데이터에 대해 각 물음에 답하시오.

df2 <- data.frame(x=c(18.8, 21.1, 21.4, 21.9, 23, 24, 24.8, 27, 29),
                 y=c(8.3, 6.0, 5.4, 5.1, 5, 4.5, 4.3, 3.9, 3.8))
df2

# 3.1 산점도
ggplot(data=df2, aes(x = x, y = y)) + geom_point()

# 3.2 상관계수
cor.test(df2$x, df2$y)

# 3.3 회귀직선
ggplot(data=df2, aes(x = x, y = y)) +
  geom_point() + 
  stat_smooth(color = "#FC4E07", method = "lm")

# 3.4 결정계수 구하기
summary(lm(y ~ x, data = df2)) # 결정계수: 0.7642

# 4. iris 데이터 분석(단위:cm)
head(iris)
str(iris)
names(iris)

# Species: 종류
# Sepal.Width: 꽃받침 너비
# Sepal.Length: 꽃받침 길이
# Petal.Width: 꽃잎 너비
# Petal.Length: 꽃잎 길이

# 4.1 그래프 그리기
## 히스토그램 
hist(iris$Sepal.Width, main = '꽃받침 너비')
hist(iris$Sepal.Length, main = '꽃받침 길이')
hist(iris$Petal.Width, main = '꽃잎 너비')
hist(iris$Petal.Length, main = '꽃잎 길이')
ggplot(data = iris, aes(x = Species)) + geom_bar() + ggtitle('종 개수') + 
  theme(plot.title = element_text(face = "bold", # 제목 서식 지정
                                  size = 20,
                                  hjust = 0.5))

## 산점도
plot(iris$Sepal.Length, iris$Petal.Length, main ='꽃받침 길이와 꽃잎 길이 산점도')
plot(iris$Sepal.Width, iris$Petal.Width, main ='꽃받침 너비와 꽃잎 너비 산점도')

  ## 상자 그림
boxplot(iris$Sepal.Width, main = '꽃받침 너비')
boxplot(iris$Sepal.Length, main = '꽃받침 길이')
boxplot(iris$Petal.Width, main = '꽃잎 너비')
boxplot(iris$Petal.Length, main = '꽃잎 길이')

# 4.2 다중회귀분석, 적절한 회귀식 제시(종속변수: 꽃받침 너비, 독립변수: 꽃받침 길이, 꽃잎 너비, 꽃잎 길이)
model = lm(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length, data = iris)
summary(model)

# 전진선택법
summary(step(model, direction = "forward"))

# 후진소거법
summary(step(model, direction = "backward"))

# 단계적 선택법
summary(step(model, direction = "both"))

## (1) 상관관계 파악
cor_iris <- cor(iris %>% select(-Species)) # 상관행렬 작성
cor_iris

# 상관행렬 히트맵 생성
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) # 색상 코드 지정
corrplot(cor_iris,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

# Petal.Width와 Sepal.Length는 0.82, Petal.Length와 Sepal.Length는 0.87, Petal.Width와 Petal.Length 0.96으로 세 부분에서 상관이 높게 나왔다. 따라서 다중공선성을 의심할 수 있다.

## (2) VIF 확인
vif(model)

summary(lm(Sepal.Width ~ Sepal.Length, data = iris))
