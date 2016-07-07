refine_original<-mutate(refine_original,company=tolower(company))
install.packages(diplyr)
install.packages("reshape")
library(reshape)
install.packages("dplyr")
library(dplyr)
install.packages(tidyr)
install.packages("tidyr")
library(tidyr)
library("car")
refine_original
refine_original
"refine_original"
refine_original
library(refine_original)
read.csv(refine_original)
read.csv("refine_original")
refine_original <- read.csv("~/Downloads/refine_original.csv")
View(refine_original)
refine_original
gsub("fillips","philips",refine_original$company)
gsub("fillips","philips",refine_original)
refine_original
tolower(c(refine_original$company))
refine_original
refine_original<-mutate(refine_original,company=tolower(company))
refine_original
refine_original1<-as.data.frame(sapply(refine_original,gsub,pattern="fillips",replacement="philips"))
refine_original1
refine_original1<-as.data.frame(sapply(refine_original,gsub,pattern="phillips",replacement="philips"))
refine_original1
refine_original2<-as.data.frame(sapply(refine_original1,gsub,pattern="fillips",replacement="philips")
refine_original2<-as.data.frame(sapply(refine_original1,gsub,pattern="fillips",replacement="philips"))
refine_original2
refine_original3<-as.data.frame(sapply(refine_original2,gsub,pattern="phllips",replacement="philips"))
refine_original3
refine_original4<-as.data.frame(sapply(refine_original3,gsub,pattern="phillps",replacement="philips"))
refine_original4
refine_original5<-as.data.frame(sapply(refine_original4,gsub,pattern="unilver",replacement="unilever"))
refine_original5
refine_original6<-as.data.frame(sapply(refine_original5,gsub,pattern="phlips",replacement="philips"))
refine_original6
refine_original7<-as.data.frame(sapply(refine_original6,gsub,pattern="akz0|ak zo",replacement="akzo"))
refine_original7
separate(refine_original7,product.code...number,c("product_code","product_number"),sep = "-")
library(tidyr)
separate(refine_original7, product.code...number, c("product_code","product_number"), sep = "-")
separate(refine_original7, Product.code...number,c("product_code","product_number"),sep = "-")
refine_original8<-unite(refine_original7,"full_address",address,city,country,sep = ",")
refine_original8
separate(refine_original7, product.code...number, c("product_code","product_number"), sep = "-")
separate(refine_original7, Product.code...number,c("product_code","product_number"),sep = "-")
refine_original7
refine_original8
refine_original9<-separate(refine_original8, Product.code...number,c("product_code","product_number"),sep = "-")
refine_original9
refine_original<-refine_original9$product_category
refine_original10<-refine_original9$product_category
refine_original10
refine_original9
newcolumn1<-c("product_category")
newcolumn1
refine_original9[,newcolumn1]<-NA
refine_original9
refine_original11<-refine_original9$product_category <- ifelse(refine_original9$product_category=="p", "smartphone")
refine_original11
refine_original10<-refine_original9$product_category[refine_original9$product_code=="p"] <- "smartphone"
refine_original10
refine_original9
refine_original9$product_category[refine_original9$product_code=="v"] <- "tv"
refine_original9
refine_original9$product_category[refine_original9$product_code=="x"] <- "laptop"
refine_original9
refine_original9$product_category[refine_original9$product_code=="q"] <- "tablet"
refine_original9
newcolumn2<-c("company_philips","company_akzo", "company_van_houten","company_unilever","product_smartphone", "product_tv", "product_laptop", "product_tablet")
refine_original9[,newcolumn2]<-NA
refine_original9
refine_original9$company_philips[refine_original9$company=="philips"] <- 1;else<-0
install.packages("car")
library(car)
refine_original9$company_philips<-recode(refine_original9$company, "philips=1; else=0")
refine_original9$company_philips[refine_original9$company=="philips"] <- 1
refine_original9
refine_original9$company_philips[refine_original9$company=="philips"] <- 1 else 0
refine_original9$company_philips[refine_original9$company=="philips"] <- 1 else<-0
refine_original9$company_akzo[refine_original9$company=="akzo"] <- 1
refine_original9
refine_original9$company_van_houten[refine_original9$company=="van houten"] <- 1
refine_original9
refine_original9$company_unilever[refine_original9$company=="unilevel"] <- 1
refine_original9
refine_original9$company_unilever[refine_original9$company=="unilever"] <- 1
refine_original9
refine_original9$product_smartphone[refine_original9$product_category=="smartphone"] <- 1
refine_original9
refine_original9$product_tv[refine_original9$product_category=="tv"] <- 1
refine_original9
refine_original9$product_laptop[refine_original9$product_category=="laptop"] <- 1
refine_original9
refine_original9$product_tablet[refine_original9$product_category=="tablet"] <- 1
refine_original9
refine_original9$company_philips[refine_original9$company_philips=="NA"] <-0
refine_original9
refine_original9$company_philips[refine_original9$company != "philips"] <- 0
refine_original9
refine_original9$company_akzo[refine_original9$company != "akzo"] <- 0
refine_original9
refine_original9$company_van_houten[refine_original9$company != "van houten"] <- 0
refine_original9
refine_original9$company_unilever[refine_original9$company != "unilever"] <- 0
refine_original9
refine_original9$product_smartphone[refine_original9$product_smartphone != "smartphone"] <- 0
refine_original9
refine_original9$product_smartphone[refine_original9$product_smartphone -= "smartphone"] <- 1
refine_original9$product_smartphone[refine_original9$product_smartphone == "smartphone"] <- 1
refine_original9
refine_original9$product_smartphone[refine_original9$product_category == "smartphone"] <- 1
refine_original9
refine_original9$product_smartphone[refine_original9$product_category != "smartphone"] <- 0
refine_original9
refine_original9$product_smartphone[refine_original9$product_category != "tv"] <- 0
refine_original9
refine_original9$product_tv[refine_original9$product_category != "tv"] <- 0
refine_original9
refine_original9$product_laptop[refine_original9$product_category != "laptop"] <- 0
refine_original9
refine_original9$product_tablet[refine_original9$product_category != "tablet"] <- 0
refine_original9
