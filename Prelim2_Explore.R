library("vcd")
# Prelim
data <- read.csv("C:\\Users\\USA\\Downloads\\germination_data.csv")

############## STEP 1 GET FREQUENCY  ###########
# Categorical variable
prop.table(table(data$shade))
prop.table(table(data$genotype))
prop.table(table(data$soiltype))
prop.table(table(data$germinate))

#for soiltype there's one category with only 1%
#according to the slide we will combine J with A (the second smallest) 

index <- data[data$soiltype == "J" , "id" ]
data[index , "soiltype"] <- "A"
summary(data)
data$soiltype <- factor(data$soiltype)
#Bar chart of the above results
#
par(mfrow=c(2,2))
barplot(prop.table(table(data$shade)) , border = "dark blue" , main =  "Shade"  ) 
barplot(prop.table(table(data$genotype)) , border = "dark blue" , main =  "Genotype")
barplot(prop.table(table(data$soiltype)) , border = "dark blue" , main =  "Soiltype")
barplot(prop.table(table(data$germinate)) , border = "dark blue" , main =  "Germinate")
dev.off()
############## STEP 2  ###########

#continuous variable
summary(data)
par(mfrow=c(2,2))
boxplot(data$susceptible , main = "Susceptible")
boxplot(data$soilqual , main = "Soilqual")
boxplot(data$chemx , main = "Chemx")
boxplot(data$depth , main = "Depth")
#might need to transform
boxplot(log(data$heavy) , main = "log(Heavy)" )

######### WHAT TO DO WITH OUTLIER #########
#“subject matter expert” about each outlier. Keep or remove. 

######### HIST 
par(mfrow=c(2,2))
hist(data$susceptible , main = "Susceptible")
hist(data$soilqual , main = "Soilqual")
hist(data$chemx , main = "Chemx")
hist(data$depth , main = "Depth")

#Chemx and Soilqual is skewed --> take log ? 
data$soilqual <- log(data$soilqual)
data$chemx <- log(data$chemx +1)

#Plot again
hist(data$susceptible , main = "Susceptible")
hist(data$soilqual , main = "Soilqual")
hist(data$chemx , main = "Chemx")
hist(data$depth , main = "Depth")

############## STEP 3 Bivariate plot  ###########
# 
#categorical data and target variable 
#There seems to be trend in soiltype  (decreasing trend)
par(mfrow=c(1,3))

mosaicplot(prop.table(table(data$soiltype , data$germinate ) , 
                      margin = 2) , main ="Soiltype")

mosaicplot(prop.table(table(data$shade , data$germinate ) , margin = 2))

mosaicplot(prop.table(table(data$genotype , data$germinate ) , margin = 2))

#numerical data and target variable
mosaicplot(prop.table(table(cut(data$susceptible,10) , data$germinate ) , margin = 2))

mosaicplot(prop.table(table(cut(data$chemx,10) , data$germinate ) , margin = 2))

mosaicplot(prop.table(table(cut(data$soilqual,10) , data$germinate ) , margin = 2))
#heavy
#depth
mosaicplot(prop.table(table(cut(data$heavy,10) , data$germinate ) , margin = 2))
mosaicplot(prop.table(table(cut(data$depth,10) , data$germinate ) , margin = 2))

############## STEP 4 Interaction plot  ###########
# Interaction plot Y axis always dependent variable
# From a given data -> Y = log(odd) , X = factor1 and y = factor2
# This is only Var1 and Var2 = factor
# Create a helper function 

plot.interaction <- function(X , Var1 , Var2 ){
 # browser()
  
  var1 <- unique(X[[Var1]])
  var2 <- unique(X[[Var2]])
  
  all <- data.frame()
  for(i in 1:length(var1)){
    for(j in 1:length(var2)){
      
      temp <- X[X[[Var1]] == var1[i] & X[[Var2]] == var2[j] ,
                c(Var1 , Var2 , "germinate")]
      
      ratio <- table(temp$germinate)
      p <- ratio[2]/sum(ratio)
      
      log_dd <- log(p/(1-p))
      dim_name <- c(as.character(var1[i]) ,  
                    as.character(var2[j]))
      all <- rbind(all , cbind(data.frame(t(dim_name)) , log_dd))
      }
    }
  
  return(all)
}

############## CATEGORY INTERACTION WITH LOG ODDS #############################
par(mfrow=c(1,3))

plot.interaction(data , "shade" , "genotype") -> interaction_plot
names(interaction_plot) <- c("shade" , "genotype" , "logodds")
interaction.plot(interaction_plot$shade , interaction_plot$genotype ,
                 interaction_plot$logodds , 
                 ylab =  "log odds" , xlab = "shade" 
                 ,main = "Interaction plot between Shade and Genotype")

plot.interaction(data , "shade" , "soiltype") -> k
k <- k[is.finite(k$log_dd),]  
interaction.plot(k$X1 , k$X2 , k$log_dd)

plot.interaction(data , "genotype" , "soiltype") -> k
k <- k[is.finite(k$log_dd),]  
interaction.plot(k$X1 , k$X2 , k$log_dd)

#there seems to be interaction between shade and genotype
dev.off()

# Y axis should be log odds the below command is not coorect
#interaction.plot(data$germinate, data$soiltype, data$depth)


#table(data$germinate , data$soilcut)
# Make it more beautiful with the command below 
# interaction.plot(x.factor     = Data$Country,
#                  trace.factor = Data$Diet,
#                  response     = Data$Weight_change,
#                  fun = mean,
#                  type="b",
#                  col=c("black","red","green"),  ### Colors for levels of trace var.
#                  pch=c(19, 17, 15),             ### Symbols for levels of trace var.
#                  fixed=TRUE,                    ### Order by factor order in data
#                  leg.bty = "o")