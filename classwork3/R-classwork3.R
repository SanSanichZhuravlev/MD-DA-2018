#Загрузите данные о землятресениях
anss <- readLines("https://github.com/SergeyMirvoda/MD-DA-2018/blob/master/data/earthquakes_2011.html", warn=FALSE)

#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
pattern.check<- "\\d{4}(\\/\\d{2}){2}\\s(\\d{2}:){2}\\d{2}\\.\\d{2}(,[^,]*){10},\\d*"
anss.isdata <- grepl(pattern.check, anss)
print(anss.isdata)
stats.isdata <- regmatches(anss, regexpr(pattern.check, anss))
print(stats.isdata)

#Проверьте что все строки (all.equal) в результирующем векторе подходят под шаблон. 
all.equal(anss[which(anss.isdata)], stats.isdata)

