## Aceite de Oliva

load("~/Desktop/AnalisisDatos/Informes/AO.rdata")

## DESCRIPCIÓN DE LOS DATOS
dim(AO)
round(numSummary(AO[-1])$table, 2)

## PREPARACIÓN DE LOS DATOS
AO <- AO[-1]
AO <- AO + 1
AO <- AO/apply(AO, 1, sum)
AO <- log(AO)

## ANÁLISIS CLUSTER
#unidades homogeneas y magnitudes directamente comparables, al tipificar modificamos la geometria de los aceites
names(AO)
cov(AO)
pc <- princomp(~palmitic+palmitoleic+stearic+oleic+linoleic+linolenic+arachidic+eicosenoic, data=AO, cor=FALSE) #por defecto, matriz var-cov
names(pc)
summary(pc)
pc$loadings #coef de las variables
plot(pc$scores[,1], pc$scores[,2])


## Decidir cuantos grupos tomo
pc <- data.frame(pc$scores)
# Determine number of clusters
wss <- (nrow(pc)-1)*sum(apply(pc,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(pc,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de Clusters", ylab="Suma de cuadrados", main="Suma de cuadrados de distancias dentro de cada grupo")

# K-Means Cluster Analysis
fit <- kmeans(pc, 8) # 5 cluster solution
# get cluster means
aggregate(AO, by=list(fit$cluster),FUN=mean)
# append cluster assignment
CP <- data.frame(pc$Comp.1, pc$Comp.2, pc$Comp.3, fit$cluster)
names(CP)[c(1,2,3)] <- c("Comp.1","Comp.2","Comp.3")


## Representación gráfica de los k grupos
ng <- 8

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "k-means")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$fit.cluster==j], CP$Comp.2[CP$fit.cluster==j], col=colores[j], pch=16)
  }

plot(CP$Comp.2, CP$Comp.3, xlab="Comp.2", ylab="Comp.3", main= "k-means")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.2[CP$fit.cluster==j], CP$Comp.3[CP$fit.cluster==j], col=colores[j], pch=16)
}

plot(CP$Comp.1, CP$Comp.3, xlab="Comp.1", ylab="Comp.3", main= "k-means")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$fit.cluster==j], CP$Comp.3[CP$fit.cluster==j], col=colores[j], pch=16)
}



##Average con euclidea con cp
ng <- 5
d <- dist(pc, method = "euclidean") # matriz distancias
fit <- hclust(d, method="average")
plot(fit)
CP$g <- cutree(fit, k=ng)
rect.hclust(fit, k=ng, border="red")

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "Método average")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.2[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.1, CP$Comp.3, xlab="Comp.1", ylab="Comp.3", main= "Metodo average")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.2, CP$Comp.3, xlab="Comp.2", ylab="Comp.3", main= "Metodo average")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.2[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}

## Descripción por grupos
table(CP$g)
round(numSummary(CP[,c("Comp.1","Comp.2","Comp.3")], groups=CP$g)$table, 3)
round(numSummary(AO, groups=CP$g)$table, 3)

## Average con Canberra
#Si utilizo distancia canberra me fijo sobretodo en las componentes menos frecuentes de los aceites
#Si me quiero fijar en otra cosa, utilizo otra distancia
ng <- 5
d <- dist(pc, method = "canberra") # matriz distancias
fit <- hclust(d, method="average")
plot(fit)
CP$g <- cutree(fit, k=ng)
rect.hclust(fit, k=ng, border="red")

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "Método average")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.2[CP$g==j], col=colores[j], pch=16)
}


## Salto minimo (sale igual que average)
ng <- 5
d <- dist(pc, method = "euclidean") # matriz distancias
fit <- hclust(d, method="single")
plot(fit)
CP$g <- cutree(fit, k=ng)
rect.hclust(fit, k=ng, border="red")

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "Método salto mínimo")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.2[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.1, CP$Comp.3, xlab="Comp.1", ylab="Comp.3", main= "Metodo salto minimo")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.2, CP$Comp.3, xlab="Comp.2", ylab="Comp.3", main= "Metodo salto minimo")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.2[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}


## Centroide
ng <- 5
d <- dist(pc, method = "euclidean") # matriz distancias
fit <- hclust(d, method="centroid")
plot(fit)
CP$g <- cutree(fit, k=ng)
rect.hclust(fit, k=ng, border="red")

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "Método salto mínimo")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.2[CP$g==j], col=colores[j], pch=16)
}

## Ward
ng <- 5
d <- dist(pc, method = "euclidean") # matriz distancias
fit <- hclust(d, method="ward.D")
plot(fit)
CP$g <- cutree(fit, k=ng)
rect.hclust(fit, k=ng, border="red")

plot(CP$Comp.1, CP$Comp.2, xlab="Comp.1", ylab="Comp.2", main= "Método ward")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.2[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.1, CP$Comp.3, xlab="Comp.1", ylab="Comp.3", main= "Metodo ward")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.1[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}

plot(CP$Comp.2, CP$Comp.3, xlab="Comp.2", ylab="Comp.3", main= "Metodo ward")
colores<- c("red", "blue", "magenta", "green", "black", "yellow", "orange", "brown", "pink")
for (j in c(1:ng)) { 
  points(CP$Comp.2[CP$g==j], CP$Comp.3[CP$g==j], col=colores[j], pch=16)
}


