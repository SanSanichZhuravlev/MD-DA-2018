# Урал (Домашние матчи)
ural_home <- c(2, 0, 1, 0)

# Выездные
ural_away <- c(0, 0, 1, 1)

#Напечатайте на консоль оба вектора
print(ural_home)
print(ural_away)

# Назначим имена элеметом вектора (Команды Гости)
names(ural_home) <- c("Ufa", "CSKA", "Arsenal", "Anzhi")

#Проделайте то же самое для вектора ural_away назначив имена команд гостей (away_names)
away_names <- c("Rostov", "Amkar", "Rubin", "Orenburg")

#Напечатайте на консоль оба вектора, заметьте разниц
print(ural_home)
print(away_names)

#Посчитайте статистикку домашних и выездных матчей (общее кол-во голов, среднее количество голов)
mean
sum(ural_home, ural_away)
mean(ural_home)
mean(ural_away)

#сравните векторы ural_home и ural_away и сделайте вывод
ural_home > ural_away
ural_home < ural_away
ural_home  == ural_away

#Векторы числовой и строковый
num_vector <- c(1, 10, 49)
char_vector <- c("a", "b", "c")

# Заполните булевый вектор
bool_vector <- c(TRUE, FALSE)

# Выберите из числового вектора значения при помощи булевого вектора
num_vector[bool_vector]

# Повторите то же самое для строкововго вектора
char_vector[bool_vector]

#Составте булевый вектор из числового, выбрав элеметны большие 10
x<-num_vector>10

#С помощью вектора x выберите из числового вектора данные
num_vector[x]

# Запишите то же самое без применения промежуточной переменной x
num_vector[num_vector>10]

#Придумайте подобный пример для строковго вектора
y <- char_vector