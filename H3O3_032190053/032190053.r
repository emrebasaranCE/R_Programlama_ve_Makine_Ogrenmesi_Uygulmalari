# Gerekli paketlerin indirilmesi

install.packages("ggplot2")
install.packages("tidymodels")
install.packages("rsample")
install.packages("mlbench")

# ------------ Gorev 1 ------------

library(rsample)

# Veri setini yükle
data(mtcars)

# mtcars veri setinin kullanılması
set.seed(123)  # Tekrar üretilebilirlik için rastgelelik ayarlanıyor
split <- initial_split(mtcars, prop = 0.6)  # mtcars veri seti %60 eğitim, %40 test olarak bölünüyor

# Eğitim ve test setlerini oluşturma
train_data <- training(split)  # Eğitim verisi
test_data <- testing(split)  # Test verisi

# Eğitim setindeki "mpg" değişkeninin ortalamasını hesaplama
mean_train_mpg <- mean(train_data$mpg)
cat("Eğitim Setindeki mpg Ortalaması:", mean_train_mpg, "\n")

# Test setindeki "mpg" değişkeninin ortalamasını hesaplama
mean_test_mpg <- mean(test_data$mpg)
cat("Test Setindeki mpg Ortalaması:", mean_test_mpg, "\n")


# ------------ Gorev 2 ------------

library(mlbench)

# Veri setini yükle
data("PimaIndiansDiabetes")

# Stratified Sampling ile verilerin dagitilmasi (%70 Eğitim, %30 Test)
set.seed(123) # Tekrar üretilebilirlik için rastgelelik ayarlanıyor
split <- initial_split(PimaIndiansDiabetes, prop = 0.7, strata = "diabetes")

# Eğitim ve test setlerini oluştur
train_data <- training(split)
test_data <- testing(split)

# Orijinal veri setindeki sınıf dağılımı
table_original <- prop.table(table(PimaIndiansDiabetes$diabetes))

# Eğitim setindeki sınıf dağılımı
table_train <- prop.table(table(train_data$diabetes))

# Test setindeki sınıf dağılımı
table_test <- prop.table(table(test_data$diabetes))

cat("Orijinal Veri Seti:\n", table_original, 
    "\nEğitim Seti:\n", table_train, 
    "\nTest Seti:\n", table_test, "\n")

# ------------ Gorev 3 ------------

library(ggplot2)
library(tidymodels)

# Veri setini yükle
data("diamonds")

# Küçük bir örneklem alalım (hesaplamaları hızlandırmak için)
set.seed(123)
diamonds_sample <- diamonds[sample(nrow(diamonds), 50000), ]

# 5 Katlı Cross Validation
set.seed(123)  # Tekrarlanabilirlik için
cv_folds <- vfold_cv(diamonds_sample, v = 5)

# RMSE hesaplamak için fonksiyon
calculate_rmse <- function(split) {
  train_data <- analysis(split)  # Eğitim verisi
  test_data <- assessment(split) # Test verisi
  
  # Lineer regresyon modeli (basit model: carat ile price tahmini)
  model <- lm(price ~ carat, data = train_data)
  
  # Test verisi üzerinde tahmin yap
  predictions <- predict(model, newdata = test_data)
  
  # Gerçek fiyatlarla karşılaştırarak RMSE hesapla
  rmse_val <- yardstick::rmse_vec(truth = test_data$price, estimate = predictions)
  
  return(rmse_val)
}

# Her kat için RMSE hesapla
rmse_values <- map_dbl(cv_folds$splits, calculate_rmse)

# Sonuçları göster
cat("Her kat için RMSE:\n")
print(rmse_values)

# Ortalama RMSE'yi hesapla
mean_rmse <- mean(rmse_values)
cat("\nOrtalama RMSE:", mean_rmse, "\n")


