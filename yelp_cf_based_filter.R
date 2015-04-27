# http://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# http://datasciencelondon.org/recommender-systems-in-r-by-tamas-jambor-sky/

#install.packages("recommenderlab")
#install.packages("plyr")

# construct a matrix for users and businesses
library(recommenderlab)
library(plyr)

# read the reviews
yelp_review = read.csv("yelp_academic_dataset_review_peoria_no_text.csv", stringsAsFactors=FALSE)
# TODO: filter by Phoenix
# TODO: #NAME?

# project relevant columns
yelp_rating_flat = yelp_review[,c("user_id","business_id","stars")]
# discard non-unique entries (based on user_id and business_id only)
yelp_rating_unique = yelp_rating_flat[!duplicated(yelp_rating_flat[1:2]),]

# select reviews of only the most reviewed businesses
# topNReviewedBusinesses = 50
# business_review_frequency = data.frame(table(yelp_rating_unique$business_id))
# top_n_most_reviewed_businesses = business_review_frequency[order(-business_review_frequency$Freq),][1:topNReviewedBusinesses,]
# yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$business_id %in% top_n_most_reviewed_businesses[,1], ]

# filter users who have made less than x reviews
#TODO: how does min user review affect evaluation?
minUserReviews = 3
user_ids <- table(yelp_rating_unique$user_id)
yelp_rating_unique = yelp_rating_unique[yelp_rating_unique$user_id %in% names(user_ids)[user_ids >= minUserReviews], ]
yelp_rating = daply(yelp_rating_unique, .(user_id, business_id), function(x) x$stars)
# convert to a matrix
m = data.matrix(yelp_rating)
# create a "rating matrix"
r = as(m, "realRatingMatrix")
# 58 x 247 rating matrix of class ‘realRatingMatrix’ with 1030 ratings.

var_nn = 5
var_minRating = 4
var_alpha = 0.5
algorithms = list(
	"random" = list(name="RANDOM", param=NULL),
	"popular" = list(name="POPULAR", param=NULL),
	"UBCF_COSINE" = list(name="UBCF", param=list(method="Cosine", nn=var_nn, minRating=var_minRating)),
	"UBCF_PEARSON" = list(name="UBCF", param=list(method="Pearson", nn=var_nn, minRating=var_minRating)),
	"UBCF_JACCARD" = list(name="UBCF", param=list(method="Jaccard", nn=var_nn, minRating=var_minRating)),
	"IBCF_COSINE" = list(name="IBCF", param=list(method="Cosine", na_as_zero=FALSE, minRating=var_minRating)),
	"IBCF_PEARSON" = list(name="IBCF", param=list(method="Pearson", na_as_zero=FALSE, minRating=var_minRating)),
	"IBCF_JACCARD" = list(name="IBCF", param=list(method="Jaccard", na_as_zero=FALSE, minRating=var_minRating)),
	"UBCF_COSINE_ROWCENTERING" = list(name="UBCF", param=list(method="Cosine", normalize="center", nn=var_nn, minRating=var_minRating)),
	"UBCF_PEARSON_ROWCENTERING" = list(name="UBCF", param=list(method="Pearson", normalize="center", nn=var_nn, minRating=var_minRating)),
	"UBCF_JACCARD_ROWCENTERING" = list(name="UBCF", param=list(method="Jaccard", normalize="center", nn=var_nn, minRating=var_minRating)),
	"IBCF_COSINE_ROWCENTERING" = list(name="IBCF", param=list(method="Cosine", normalize="center", na_as_zero=FALSE, minRating=var_minRating)),
	"IBCF_PEARSON_ROWCENTERING" = list(name="IBCF", param=list(method="Pearson", normalize="center", na_as_zero=FALSE, minRating=var_minRating)),
	"IBCF_JACCARD_ROWCENTERING" = list(name="IBCF", param=list(method="Jaccard", normalize="center", na_as_zero=FALSE, minRating=var_minRating)),
	"SVD_COSINE" = list(name="SVD", param=list(method="Cosine", normalize="center", normalize_sim_matrix=FALSE, treat_na="median", alpha=var_alpha, minRating=var_minRating)),
	"SVD_PEARSON" = list(name="SVD", param=list(method="Pearson", normalize="center", normalize_sim_matrix=FALSE, treat_na="median", alpha=var_alpha, minRating=var_minRating)),
	"SVD_JACCARD" = list(name="SVD", param=list(method="Jaccard", normalize="center", normalize_sim_matrix=FALSE, treat_na="median", alpha=var_alpha, minRating=var_minRating)),
	"PCA" = list(name="PCA", param=NULL)
)


# evaluation schemes
split = evaluationScheme(r, method="cross-validation", k=10, given=minUserReviews, goodRating=4)
split_evals = evaluate(split, algorithms, n=c(1, 3, 5, 10, 15, 20))
# evaluation results
getConfusionMatrix(split_evals[["popular"]])
getConfusionMatrix(split_evals[["UBCF_COSINE"]])
getConfusionMatrix(split_evals[["UBCF_PEARSON"]])
getConfusionMatrix(split_evals[["UBCF_JACCARD"]])
getConfusionMatrix(split_evals[["IBCF_COSINE"]])
getConfusionMatrix(split_evals[["IBCF_PEARSON"]])
getConfusionMatrix(split_evals[["IBCF_JACCARD"]])
getConfusionMatrix(split_evals[["UBCF_COSINE_ROWCENTERING"]])
getConfusionMatrix(split_evals[["UBCF_PEARSON_ROWCENTERING"]])
getConfusionMatrix(split_evals[["UBCF_JACCARD_ROWCENTERING"]])
getConfusionMatrix(split_evals[["IBCF_COSINE_ROWCENTERING"]])
getConfusionMatrix(split_evals[["IBCF_PEARSON_ROWCENTERING"]])
getConfusionMatrix(split_evals[["IBCF_JACCARD_ROWCENTERING"]])
getConfusionMatrix(split_evals[["SVD_COSINE"]])
getConfusionMatrix(split_evals[["SVD_PEARSON"]])
getConfusionMatrix(split_evals[["SVD_JACCARD"]])
getConfusionMatrix(split_evals[["PCA"]])

# visualization
plot(split_evals, annotate=1:length(algorithms), legend="topleft")
plot(split_evals, "prec/rec", legend="topright")

