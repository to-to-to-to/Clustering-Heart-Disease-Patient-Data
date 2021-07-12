
# Load the data
heart_disease <- read.csv("datasets/heart_disease_patients.csv")

# Print the first ten rows of the dataset
head(heart_disease, n = 10) 

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

soln_heart_disease <- read.csv('datasets/heart_disease_patients.csv')

run_tests({
    test_that("heart disease data loaded correctly", {
        expect_is(heart_disease, "data.frame",
              info = "heat_disease is not a data frame. Did you use read.csv() (not read_csv())?")
        expect_equal(heart_disease, soln_heart_disease, 
                 info="heart_disease does not have the correct data. Please check the csv file name.")
   })
})

# Evidence that the data should be scaled?
summary(heart_disease)

# Remove id
heart_disease <- heart_disease[ , !(names(heart_disease) %in% c("id"))]

# Scaling data and saving as a data frame
scaled <- scale(heart_disease)

# What do the data look like now?
summary(scaled)

soln_heart_disease <- soln_heart_disease[ , !(names(soln_heart_disease) %in% c("id"))]
soln_scaled <- scale(soln_heart_disease)


run_tests({
    test_that("remove correct column", {
        expect_identical(colnames(soln_heart_disease), colnames(heart_disease), 
                         info = "Did you remove the id column?")
    })
    
    test_that("scaled data properly", {
        expect_identical(soln_scaled, scaled, 
                         info = "Did you scale the proper data set?")
    })
})

# Set the seed so that results are reproducible
seed_val <- 10
set.seed(seed_val)

# Select a number of clusters
k <- 5

# Run the k-means algorithm
first_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
first_clust$size

soln_seed_val <- 10
set.seed(soln_seed_val)
soln_k <- 5
soln_first_clust <- kmeans(soln_scaled, centers = soln_k, nstart = 1)


run_tests({
    test_that("correct seed", {
        expect_equal(soln_seed_val, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(soln_k, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_first_clust$size, first_clust$size, info = "What is your nstart value?")
    })
})

# Set the seed
seed_val <- 38
set.seed(seed_val)

# Select a number of clusters and run the k-means algorithm
k <- 5
second_clust <- kmeans(scaled, centers = k, nstart = 1)

# How many patients are in each cluster?
second_clust$size

seed_val_2 <- 38
set.seed(seed_val_2)
k_2 <- 5
soln_second_clust <- kmeans(soln_scaled, centers = k_2, nstart = 1)

run_tests({
    test_that("correct seed", {
        expect_equal(seed_val_2, seed_val, info = "Is the seed set to 10?")
    })
    
    test_that("correct number of clusters", {
        expect_equal(k_2, k, info = "Are you using five clusters?")
    })
    test_that("correct implmentation of algorithm", {
        expect_equal(soln_second_clust$size, second_clust$size, info = "What is your nstart value?")
    })
})

# Add cluster assignments to the data
heart_disease["first_clust"] <- first_clust$cluster
heart_disease["second_clust"] <- second_clust$cluster

# Load ggplot2
library(ggplot2)

# Create and print the plot of age and chol for the first clustering algorithm
plot_one <- ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) +
  geom_point()
plot_one

# Create and print the plot of age and chol for the second clustering algorithm
plot_two <- ggplot(heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + 
  geom_point()
plot_two

soln_heart_disease["first_clust"] <- soln_first_clust$cluster
soln_heart_disease["second_clust"] <- soln_second_clust$cluster

# creating the correct graphs and getting fingerprints
soln_plot_one <- ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(first_clust))) + geom_point()
soln_plot_two <- ggplot(soln_heart_disease, aes(x=age, y=chol, color=as.factor(second_clust))) + geom_point()

run_tests({
    test_that("cluster assignments added", {
        expect_identical(soln_heart_disease, heart_disease, 
                         info = "Did you add a column for both the first and second iteration?")
    })
    
    test_that("ggplot2 loaded", {
        expect_true('ggplot2' %in% .packages(), 
                    info = "Did you load ggplot2?")
    })

    test_that("first plot is correct", {
        expect_equal(soln_plot_one$labels, plot_one$labels, 
                         info = "Do you have the correct variables on the axes and used to color code?")
    })
    
    test_that("second plot is correct", {
        expect_equal(soln_plot_two$labels, plot_two$labels, 
                         info = "Do you have the correct variables on the axes and used to color code?")
    })
})

# Execute hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method = "complete")

# Print the dendrogram
plot(hier_clust_1)

# Get cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hier_clust_1, 5)

soln_hier_clust_1 <- hclust(dist(soln_scaled), method='complete')
soln_hc_1_assign <- cutree(soln_hier_clust_1, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_1$merge, hier_clust_1$merge, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$labels, hier_clust_1$labels, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_1$method, hier_clust_1$method, 
                         info = "Did you use complete linkage?")
    })
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_1_assign, hc_1_assign, 
                         info = "Did you select five clusters?")
    })
})

# Execute hierarchical clustering with single linkage
hier_clust_2 <- hclust(dist(scaled), method = "single")

# Print the dendrogram
plot(hier_clust_2)

# Get cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)

soln_hier_clust_2 <- hclust(dist(soln_scaled), method = "single")
soln_hc_2_assign <- cutree(soln_hier_clust_2, 5)
                          
run_tests({
    test_that("correctly implemented clustering algorithm", {
        expect_identical(soln_hier_clust_2$merge, hier_clust_2$merge, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$labels, hier_clust_2$labels, 
                         info = "Did you make the distance matrix?")
        expect_identical(soln_hier_clust_2$method, hier_clust_2$method, 
                         info = "Did you use single linkage?")     })
    
    test_that("correct cutoff for cluster assignments", {
        expect_identical(soln_hc_2_assign, hc_2_assign, info = "Did you select five clusters?")
    })
})

# Add assignment of chosen hierarchical linkage
heart_disease["hc_clust"] <- hc_1_assign

# Remove the sex, first_clust, and second_clust variables
hd_simple <- heart_disease[, !(names(heart_disease) %in% c("sex", "first_clust", "second_clust"))]

# Get the mean and standard deviation summary statistics
clust_summary <- do.call(data.frame, aggregate(. ~hc_clust, data = hd_simple, function(x) c(avg = mean(x), sd = sd(x))))
clust_summary

soln_heart_disease["hc_clust"] <- soln_hc_1_assign

soln_hd_simple <- soln_heart_disease[, !(names(soln_heart_disease) %in% c("sex", "first_clust", "second_clust"))]

soln_clust_summary <- do.call(data.frame, aggregate(. ~hc_clust, data = soln_hd_simple, function(x) c(avg = mean(x), sd = sd(x))))


run_tests({
    test_that("selected first cluster assignments", {
        expect_identical(heart_disease['hc_clust'], soln_heart_disease['hc_clust'], 
                         info = "You choose the incorrect hierarchical clustering assignments.")
    })
    
    test_that("removed columns properly", {
        expect_identical(soln_hd_simple, hd_simple, 
                         info = "Did you remove three columns?")
    })
    test_that("proper summary analysis", {
        expect_identical(soln_clust_summary, clust_summary, 
                         info = "Did you find the mean and standard deviation using mean(x) and sd(x)?")
    })
})

# Plot age and chol
plot_one <- ggplot(heart_disease, aes(x = age, y = chol, 
                                      color = as.factor(hc_clust))) + 
  geom_point()
plot_one 

# Plot oldpeak and trestbps
plot_two <- ggplot(heart_disease, aes(x = oldpeak, y = trestbps, 
                                      color = as.factor(hc_clust))) + 
  geom_point()
plot_two

soln_plot_one <- ggplot(soln_heart_disease, aes(x = age, y = chol, 
                                                color = as.factor(hc_clust))) + 
  geom_point()
soln_plot_two <- ggplot(soln_heart_disease, aes(x=oldpeak, y=trestbps, 
                                                color=as.factor(hc_clust))) + 
  geom_point()

run_tests({
    test_that("plot one is correct", {
        expect_identical(soln_plot_one$labels, plot_one$labels, 
                         info = "Check that you are using the correct variables for the first plot")
    })
    
    test_that("plot two is correct", {
        expect_identical(soln_plot_two$labels, plot_two$labels, 
                         info = "Check that you are using the correct variables for the second plot")
    })
})

# Add TRUE if the algorithm shows promise, add FALSE if it does not
explore_kmeans <- FALSE
explore_hierarch_complete <- TRUE
explore_hierarch_single <- FALSE

soln_1 <- FALSE
soln_2 <- TRUE
soln_3 <- FALSE

run_tests({
    test_that("correct kmeans results", {
        expect_identical(soln_1, explore_kmeans, info = "Are the clusters stable between kmeans iterations?")
    })
    
    test_that("correct hierarchical with complete linkage results", {
        expect_identical(soln_2, explore_hierarch_complete, info = "Would you want to explore this method further?")
    })
    
    test_that("correct hierarchical with single linkage results", {
        expect_identical(soln_3, explore_hierarch_single, info = "Is the number of patients in each cluster balanced?")
    })
})
