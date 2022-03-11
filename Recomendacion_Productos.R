

install.packages("arules")

library(arules)

library(Matrix)

setwd("C:/Users/sepul/OneDrive/Documentos/Oscar Ayala/Personal/Personal/Posgrado/USabana/Reglas_asociacion")





groceries <- read.transactions("groceries.csv", sep = ",")



head(groceries)



str(groceries)



summary(groceries)



inspect(groceries[100:105])




itemFrequency(groceries[, 1:169])


itemFrequencyPlot(groceries, support = 0.10)


itemFrequencyPlot(groceries, topN = 5)





image(groceries[150:220])




image(sample(groceries, 70))



library(arules)




apriori(groceries)




groceryrules <- apriori(groceries, parameter = list(support =
                                      0.01, confidence = 0.25, minlen = 2))


groceryrules


summary(groceryrules)


inspect(groceryrules[21:30])



inspect(sort(groceryrules, by = "lift")[16:30])

install.packages("dplyr")
library(dplyr)



berryrules <- subset(groceryrules, items %in% "berries")


inspect(berryrules)


n


getwd()


groceryrules_df <- as(groceryrules, "data.frame")

str(groceryrules_df)

head(groceryrules_df)

library(tm)
library(ggplot2)




# Descarga http://www.cs.cornell.edu/people/pabo/movie-review-data/


path = "C:/Users/on186001/OneDrive - Teradata/OscarAyala/Teradata/R/R/text_asociacion/txt_sentoken/"

dir = DirSource(paste(path,"pos/",sep=""), encoding = "UTF-8")

corpus = Corpus(dir)

head(dir)

length(corpus)

corpus[[1]]

myStopwords = c(stopwords(),"film","films","movie","movies")


tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopwords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))




asoc.star = as.data.frame(findAssocs(tdm,"star", 0.5))
asoc.star$names <- rownames(asoc.star) 
asoc.star


library(ggplot2)

ggplot(asoc.star, aes(reorder(names,star), star)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Correlation") +
  ggtitle("\"star\" associations")


asoc.indi = as.data.frame(findAssocs(tdm,"indiana", 0.5))
asoc.indi$names <- rownames(asoc.indi) 
asoc.indi



ggplot(asoc.indi, aes(reorder(names,indiana), indiana)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Correlation") +
  ggtitle("\"indiana\" associations")



svydesign

install.packages("rms")
library(rms)

fit = lrm(death ??? blood.pressure + sex * rcs(age ,5))

install.packages("RODBC")

library(RODBC)


2^20

round(memory.limit()/2^20, 2)

memory.limit()

32487/1048576


memory.limit(size=500)


install.packages("benchmarkme")
library(benchmarkme)

ram <- get_ram()



get_cpu()



setwd("C:/R")

data=read.csv("base_cp.csv", header = TRUE, sep = ",")

head(data)

base_cp= as.data.frame(data)

install.packages("Gifi")
library(Gifi)

library(dplyr)



str(base_cp[,3:22])



base_cp$ingresos_SMLV <- floor(base_cp$ingresos_hogar/781242)
head(base_cp)


base_cp[,"ingresos"] <- cut(base_cp$ingresos_SMLV, breaks = c(-1,1,2,5,10,20,600), labels = c("menor_1SMLV", "1-2_SMLV", "3-5_SMLV", "6-10_SMLV", "11-20_SMLV", "mayor_20_SMLV"))

str(base_cp)

str(base_cp[,-"base_cp$ingresos_hogar"])

base_cp1 <- select(base_cp,-ingresos_hogar,-X, -REGION, -ingresos_SMLV, -esco15)

str(base_cp1)

base_cp=sample_frac(base_cp1, 0.025)


fitord <- princals(base_cp1, ndim = 3) ## ordinal PCA

summary


data2=as.data.frame(fitord$weights)
write.csv(data2, file = "pesosvariables.csv")


data1=as.data.frame(fitord$objectscores)
write.csv(data1, file = "pesosobjetos.csv")

data3=as.data.frame(fitord$scoremat)
write.csv(data3, file = "sobjetos_transformados.csv")


write.csv(base_cp1, file = "data_muestreada.csv")





head(fitord)
fitord
summary(fitord)
head(fitord$weights)  #varibles

data1=head(as.data.frame(base_cp))
write.csv(data2, file = "data2.csv")

data3=head(as.data.frame(fitord$objectscores))
write.csv(data3, file = "data3.csv")



head(fitord$lambda)  
head(fitord$objectscores)
dim(fitord$objectscores)  

data=fitord$objectscores





head(fitord$transform)
dim(fitord$transform)

muestra <- sample_frac(as.data.frame(fitord$objectscores), 0.99)

str(muestra)

plot(muestra$D1,muestra$D2)
x1 <- c(1:10)
x2 <- c(1:10)

plot(x1,x2)

plot(fitord, plot.type = "transplot",)
plot(fitord, "loadplot", main = "Loadings Plot ABC Data")  ## aspect ratio = 1
plot(fitord, "biplot", labels.scores = TRUE, main = "Biplot ABC Data")
plot(fitord, "screeplot")


head(fitord$loadings)



plot(fitord, "loadplot", main = "Loadings Plot ABC Data")  ## aspect ratio = 1

plot(fitord$weights, xlim =  c(-1.5,1.5), ylim = c(-0.4, 1),  pch=2, col="red")


# grafico biplot de usuarios
plot(fitord$objectscores, pch=20)

# grafico biplot de usuarios con variables
points(fitord$weights,  pch=20, col="red")


help("points")



save()



## linear restrictions (mimics standard PCA)
abc_knots <- knotsGifi(base_cp, "E")     ## 0 interior knots
fitlin <- princals(base_cp, knots = abc_knots, degrees = 1)  
fitlin
fitlin$evals
plot(fitlin, plot.type = "transplot")












f=function(x){
  y=2+x^(.2)*cos(x/.15)/x^-.45
  return(y)
}
plot(f,0,5)
#Points simulation: you change n and sigma
N=400
sigma=1.2
x=runif(N,0,5);x=sort(x) #For convenience, the input x is sorted
y=rep(0,times=N)
for(i in 1:N){
  y[i]=f(x[i])+rnorm(1,0,sigma)
}
plot(x,y)
points(x,f(x),type="l",col=2,lwd=2)




head(backolan)

hist(backolan$income)
boxplot(backolan$income)









