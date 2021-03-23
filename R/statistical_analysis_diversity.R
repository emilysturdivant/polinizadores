# Analyze relationship between the four variables (ANP zone, biome, land cover, pollinator)
# Input: Diversity data frame (normalized richness variable for unique combinations of the 4 variables)

# After looking at multivariate regression, try random forest.

# Load libraries ----
library(tidyverse)
box::use(here[here])
box::use(units[set_units])
library(randomForest)
library(tree)


# ~ from richness stacked SDMs ----
unq_cells = TRUE
mutually_exclusive_pa = TRUE
filt_dates = TRUE
pol_group <- 'Colibries'
nspecies <- 15
buffer_distance <- 10

# directory paths
unq_code <- ifelse(unq_cells, 'unq_cells', 'unq_pts')
unq_code <- ifelse(mutually_exclusive_pa, 'unq_cells_exclusive', unq_code)
datefilt_code <- ifelse(filt_dates, '2000to2020', 'alldates')

fp_tail <- file.path('sdm', str_c(unq_code, '_', datefilt_code), 'rf1', pol_group)
pred_dir <- file.path('data', 'data_out', fp_tail)

# Load
fn <- str_glue('Likhd_rich_{pol_group}_{datefilt_code}_{nspecies}species')
fn <- str_glue('Likhd_richness_{nspecies}species')
# fn <- str_glue('PA_rich_{pol_group}_{datefilt_code}_{nspecies}species')
rich_tif_fp <- file.path(pred_dir, str_glue('{fn}.tif'))
rich_ras <- raster::raster(rich_tif_fp)

# Zonal statistics using raster::extract
ex <- raster::extract(rich_ras, polys, fun=median, na.rm=TRUE, df=TRUE)

# Compute mean AGB for each LC (zonal statistics) 
rich_ras <- terra::rast(rich_tif_fp)
polys_ras <- polys %>% to_raster()
zonal_stats <- terra::zonal(rich_ras, polys, 'median', na.rm=T)

# ~ from pollinator points ----
# Load diversity DF (singlepart) ----
buffer_distance <- set_units(10, 'km')
div_single_fp <- here::here("data", "data_out", "diversity_by_zones", 
  str_glue('div_ANPs_usv3_singlepart_buff{buffer_distance}km.csv'))
div_df <- read_csv(div_single_fp) %>% filter(!is.na(richness_norm))

# Filter to one pollinator group
pol_name <- 'Colibries'
div_df2 <- div_df %>% 
  filter(pol_group == pol_name) %>% 
  select(rich_by_1kkm, biome, zone, tipo)

# Data distribution ----
hist(div_df2$rich_by_1kkm)
hist(log(div_df2$rich_by_1kkm))
hist(log10(div_df2$rich_by_1kkm))

# Random forest ----
set.seed(1)
train <- sample(1:nrow(div_df2), nrow(div_df2)/10)
div_test <- div_df2[-train, "rich_by_1kkm"] %>% deframe %>% log()

rf.div <- randomForest(rich_by_1kkm ~ biome + zone + tipo, 
                        data=div_df2, mtry=2, proximity=T,
                       importance=T, subset=train,
                        ntree=100)
rf.div

# get MSE from prediction on the test set
yhat <- predict(rf.div, newdata=div_df2[-train,])
mean((yhat-div_test)^2)
mean(abs(yhat-div_test))

# View importance of each variable
importance(rf.div)
varImpPlot(rf.div)

# From https://stats.stackexchange.com/questions/21152/obtaining-knowledge-from-a-random-forest
#Setup a binary classification problem
require(randomForest)
set.seed(1)
dat <- div_df2
# dat$ <- factor(ifelse(dat$Species=='virginica','virginica','other'))
trainrows <- runif(nrow(dat)) > 0.3
train <- dat[trainrows,]
test <- dat[!trainrows,]

#Build a decision tree
require(rpart)
(model.rpart <- rpart(rich_by_1kkm ~., train))

# Random forest
model.rf <- randomForest(log(rich_by_1kkm) ~., train, ntree=25, proximity=TRUE, importance=TRUE, nodesize=5)
model.rf

plot(model.rf)

# See a tree in the forest
tree_25 <- getTree(model.rf, k=25, labelVar=T)
plot(tree_25)

# Examine predicted responses vs. actual responses
library(ggplot2)
pSpecies <- predict(model.rf,test)
plotData <- lapply(names(test[,1:4]), function(x){
  out <- data.frame(
    var = x,
    type = c(rep('Actual',nrow(test)),rep('Predicted',nrow(test))),
    value = c(test[,x],test[,x]),
    species = c(as.numeric(test$Species)-1,pSpecies)
  )
  out$value <- out$value-min(out$value) #Normalize to [0,1]
  out$value <- out$value/max(out$value)
  out
})
plotData <- do.call(rbind,plotData)
qplot(value, species, data=plotData, facets = type ~ var, geom='smooth', span = 0.5)


# Other plots
plot(rf.div)
# plot(margin(rf.div)) 
# MDSplot(rf.div, div_df2$rich_by_1kkm, k=2)
# plot(outlier(rf.div), type="h", col=c("red", "green", "blue")[as.numeric(dat$rich_by_1kkm)])


# Boosting ----
library(gbm)
set.seed(1)

# Look for best shrinkage values
gbm_div <- function(shrinkage, div_df2, train){
  boost.div <- gbm(log10(rich_by_1kkm) ~ biome + zone + tipo, 
                   data=div_df2[train,], 
                   distribution='gaussian', n.trees=5000, interaction.depth=4, 
                   shrinkage = shrinkage)
  
  yhat <- predict(boost.div, newdata=div_df2[-train,], n.trees=5000)
  return(mean((yhat-div_test)^2))
}

shrink_list <- seq(0.005, 0.15, 0.01)
mse <- shrink_list %>% map(gbm_div, div_df2, train)
sens_df <- tibble(shrink=shrink_list, mse=unlist(mse))
ggplot(sens_df, aes(x=shrink, y=mse)) +
  geom_line()

# Try with optimized shrinkage - the importance changes greatly
boost.div <- gbm(log(rich_by_1kkm) ~ biome + zone + tipo, 
                 data=div_df2[train,], 
                 distribution='gaussian', n.trees=5000, interaction.depth=4, 
                 shrinkage = 0.07)
summary(boost.div)

boost.div <- gbm(log(rich_by_1kkm) ~ biome + zone + tipo, 
                 data=div_df2[train,], 
                 distribution='gaussian', n.trees=5000, interaction.depth=4, 
                 shrinkage = 0.03)
summary(boost.div)

boost.div <- gbm(log(rich_by_1kkm) ~ biome + zone + tipo, 
                 data=div_df2[train,], 
                 distribution='gaussian', n.trees=5000, interaction.depth=4, 
                 shrinkage = 0.11)
summary(boost.div)

plot(boost.div, i='biome')
plot(boost.div, i='tipo')
plot(boost.div, i='zone')







# Simple regression tree ----
(tree.div <- tree(log10(rich_by_1kkm) ~ . -pol_group, div_df2))
summary(tree.div)
plot(tree.div)
text(tree.div, pretty=0)

# Training and testing
set.seed(2)
train <- sample(1:nrow(div_df2), nrow(div_df2)/2)
tree_div <- tree(log(rich_by_1kkm) ~ biome + zone + tipo, div_df2, subset=train)
summary(tree_div)
plot(tree_div)
text(tree_div, pretty=0)

# Check whether we need to prune
cv.div <- cv.tree(tree_div)
plot(cv.div$size, cv.div$dev, type='b')

# Prune if necessary
prune_div <- prune.tree(tree_div, best=5)
plot(prune_div)
text(prune_div, pretty=0)

# Make predictions on the test set
yhat <- predict(tree_div, newdata=div_df2[-train,])
div_test <- div_df2[-train, "rich_by_1kkm"] %>% deframe %>% log()
plot(yhat, div_test)
abline(0,1)
mean((yhat-div_test)^2)

# Use rpart and fancy plot
tree.sub <- rpart::rpart(log10(rich_by_1kkm) ~ biome + zone + tipo, div_df2)
plot(tree.sub)
text(tree.sub, pretty=0)
rattle::fancyRpartPlot(tree.sub,
                       yesno=2, split.col="black", nn.col="black", 
                       caption="", palette="Set2", branch.col="black")

# Multivariate regression ----
div_df1 <- div_df %>% filter(!is.na(richness_norm)) 
model <- lm(rich_by_1kkm ~ biome + zone + tipo, data=div_df1)
summary(model)
car::Anova(model)

# Individual pollinator groups
div_df2 <- div_df1 %>% 
  filter(pol_group == 'Murcielagos') 
model <- lm(rich_by_1kkm ~ biome + zone + tipo, data=div_df2)
summary(model)
car::Anova(model)

# Individual pollinator groups and biome
div_df2 <- div_df1 %>% 
  filter(biome == 'Selvas Calido-Humedas',
         pol_group == 'Abejas') 
model <- lm(rich_by_1kkm ~ zone + tipo, data=div_df2)
summary(model)
car::Anova(model)

# Individual pollinator groups
div_df2 <- div_df1 %>% 
  filter(biome == 'Sierras Templadas',
         pol_group == 'Abejas',
         zone == 'buff') 
model <- lm(rich_by_1kkm ~ tipo, data=div_df2)
summary(model)
car::Anova(model)
