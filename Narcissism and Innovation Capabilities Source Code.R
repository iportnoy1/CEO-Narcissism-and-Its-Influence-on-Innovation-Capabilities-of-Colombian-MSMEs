###################################################### NPI
#Calling libraries
library(psych); library(GPArotation); library(lavaan); library("corrplot")

#Loading dataset
npi_data_raw <- read.csv("npi40_responses.csv")

#Keeping only the 40-item values
npi_data <- npi_data_raw[ , 6:ncol(npi_data_raw)]

Negativo <- read.csv("Negativo.csv")

npi_data[ , Negativo == "No"] <- 7 - npi_data[ , Negativo == "No"]

# Bartlett’s Test of Sphericity
cortest.bartlett(cor(npi_data), n = nrow(npi_data))

# Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy
KMO(npi_data)$MSA

#Parallel Analysis
fa.parallel(npi_data, fa = "both", fm = "pa", n.iter = 100)

#Exploratory Factor Analysis (EFA)
efa_result <- fa(npi_data, nfactors = 6, rotate = "oblimin", fm = "pa")

#Print factor loadings
print(efa_result$loadings, cutoff = 0.4)

# View the factor correlation matrix
efa_result$Phi  # How factors correlate

# View the structure matrix (item-factor correlations)
efa_result$Structure

# View the communalities
efa_result$communality

# Ploting Factor Analysis Diagram
fa.diagram(efa_result)

#Classifying items into factors 
authority_items <- c("NAR07","NAR13","NAR15","NAR20","NAR30","NAR37","NAR38")
exhibitionism_items <- c("NAR08","NAR09","NAR10","NAR11","NAR34","NAR40")
superiority_items <- c("NAR19", "NAR29")
vanity_items <- c("NAR04", "NAR05")
exploitativeness_items <- c("NAR06","NAR17","NAR21","NAR32","NAR33","NAR35",
                            "NAR36")
Leadership_items <- c("NAR01","NAR14","NAR18","NAR26","NAR27")

#Adding factors to the original dataset
npi_data$Authority <- rowMeans(npi_data[, authority_items], na.rm = TRUE)
npi_data$Exhibitionism <- rowMeans(npi_data[, exhibitionism_items], na.rm=TRUE)
npi_data$Superiority <- rowMeans(npi_data[, superiority_items], na.rm = TRUE)
npi_data$Vanity <- rowMeans(npi_data[, vanity_items], na.rm = TRUE)
npi_data$Exploitativeness <- rowMeans(npi_data[, exploitativeness_items], 
                                      na.rm = TRUE)
npi_data$Leadership <- rowMeans(npi_data[, Leadership_items], na.rm = TRUE)

#Cronbach's alpha Testing 
alpha(npi_data[, authority_items], check.keys = TRUE)$total$raw_alpha
alpha(npi_data[, exhibitionism_items], check.keys = TRUE)$total$raw_alpha
alpha(npi_data[, superiority_items], check.keys = TRUE)$total$raw_alpha
alpha(npi_data[, vanity_items], check.keys = TRUE)$total$raw_alpha
alpha(npi_data[, exploitativeness_items], check.keys = TRUE)$total$raw_alpha
alpha(npi_data[, Leadership_items], check.keys = TRUE)$total$raw_alpha

#Confirmatory Factor Analysis with Lavaan
npi_model <- '
   Authority     =~ NAR07 + NAR13 + NAR15 + NAR20 + NAR30 + NAR37 + NAR38
  Exhibitionism =~ NAR08 + NAR09 + NAR10 + NAR11 + NAR34 + NAR40
  Superiority   =~ NAR19 + NAR29
  Vanity        =~ NAR04 + NAR05
  Exploitativeness =~ NAR06 + NAR17 + NAR21 + NAR32 + NAR33 + NAR35 + NAR36
  Leadership =~ NAR01 + NAR14 + NAR18 + NAR26 + NAR27
'

npi_data_cfa <- npi_data[, !names(npi_data) %in% c(
  "Exhibitionism", "Authority", "Exploitativeness", 
  "Leadership", "Vanity", "Superiority"
)]

cfa_result <- cfa(model = npi_model, data = npi_data_cfa, estimator = "MLR")

summary(cfa_result, fit.measures = TRUE, standardized = TRUE)

npi_model_refined <- '
  Authority =~ NAR07 + NAR13 + NAR20 + NAR30
  Exhibitionism =~ NAR08 + NAR10 + NAR11
  Superiority =~ NAR19 + NAR29
  Vanity =~ NAR04 + NAR05
  Exploitativeness =~ NAR17 + NAR21 + NAR33 + NAR36
  Leadership =~ NAR14 + NAR18 + NAR27
'
cfa_result_refined <- cfa(model = npi_model_refined, data = npi_data_cfa, 
                          estimator = "MLR")

summary(cfa_result_refined, fit.measures = TRUE, standardized = TRUE)

#Re-computing Factors
authority_items <- c("NAR07", "NAR13", "NAR20", "NAR30")
exhibitionism_items <- c("NAR08", "NAR10", "NAR11")
superiority_items <- c("NAR19", "NAR29")
vanity_items <- c("NAR04", "NAR05")
exploitativeness_items <- c("NAR17", "NAR21", "NAR33", "NAR36")
leadership_items <- c("NAR14", "NAR18", "NAR27")
npi_data$Authority <- rowMeans(npi_data[, authority_items], na.rm = TRUE)
npi_data$Exhibitionism <- rowMeans(npi_data[, exhibitionism_items], na.rm=TRUE)
npi_data$Superiority <- rowMeans(npi_data[, superiority_items], na.rm = TRUE)
npi_data$Vanity <- rowMeans(npi_data[, vanity_items], na.rm = TRUE)
npi_data$Exploitativeness <- rowMeans(npi_data[, exploitativeness_items], 
                                      na.rm = TRUE)
npi_data$Leadership <- rowMeans(npi_data[, leadership_items], na.rm = TRUE)

# Create revised summary table
factor_summary_original <- data.frame(
  Factor = c("Authority", "Exhibitionism", "Superiority", "Vanity",
             "Exploitativeness", "Leadership"),
  Items = c(
    paste(authority_items, collapse = ", "),
    paste(exhibitionism_items, collapse = ", "),
    paste(superiority_items, collapse = ", "),
    paste(vanity_items, collapse = ", "),
    paste(exploitativeness_items, collapse = ", "),
    paste(leadership_items, collapse = ", ")
  ),
  Cronbach_Alpha = c(
    round(alpha(npi_data[, authority_items], check.keys = TRUE)$total$raw_alpha,3),
    round(alpha(npi_data[, exhibitionism_items], check.keys = TRUE)$total$raw_alpha,3),
    round(alpha(npi_data[, superiority_items], check.keys = TRUE)$total$raw_alpha,3),
    round(alpha(npi_data[, vanity_items], check.keys = TRUE)$total$raw_alpha,3),
    round(alpha(npi_data[, exploitativeness_items], check.keys = TRUE)$total$raw_alpha,3),
    round(alpha(npi_data[, leadership_items], check.keys = TRUE)$total$raw_alpha,3)
  ),
  Num_Items = c(length(authority_items),length(exhibitionism_items),
                length(superiority_items),length(vanity_items),
                length(exploitativeness_items),length(leadership_items)),
  Included_in_CFA = rep("Yes", 6)
)

print(factor_summary_original)


################################################################ Inno. Capab.
#loading data
Inn_data_raw <- read.csv("Inn_Data.csv")

#Keeping items values
Inn_data <- Inn_data_raw[ , 8:34]

# Inn_model <- '
#  Product.Innovation =~ Prod1 + Prod3 + Prod4 + Prod5
#  Process.Innovation =~ Proc2 + Proc3
#  Organizational.Innovation =~ Org1 + Org2 + Org3
#  Marketing.Innovation =~ Mark1 + Mark2 + Mark3 + Mark4
#  Innovation.Culture =~ Cult1 + Cult2 + Cult3 + Cult4 + Cult5
#  Resources.for.Innovation =~ Res1+Res2+Res3+Res4+Res5+Res6+Res7+Res8+Res9
# '
# cfa_Inn <- cfa(model = Inn_model, data = Inn_data, estimator = "MLR")

Inn_model <- '
 Product.Innovation =~ Prod1 + Prod3 + Prod4 + Prod5
 Process.Innovation =~ Proc2 + Proc3
 Organizational.Innovation =~ Org1 + Org2 + Org3
 Marketing.Innovation =~ Mark1 + Mark2 + Mark3 + Mark4
 Innovation.Culture =~ Cult2 + Cult3 + Cult4 + Cult5
 Resources.for.Innovation =~ Res1+Res2+Res3+Res4+Res5+Res6+Res7+Res8
'
cfa_Inn <- cfa(model = Inn_model, data = Inn_data, estimator = "MLR")

summary(cfa_Inn, fit.measures = TRUE, standardized = TRUE)

Product.Innovation <- Inn_data[, c("Prod1", "Prod3", "Prod4", "Prod5")]
Process.Innovation <- Inn_data[, c("Proc2", "Proc3")]
Organizational.Innovation <- Inn_data[, c("Org1", "Org2", "Org3")]
Marketing.Innovation <- Inn_data[, c("Mark1", "Mark2", "Mark3", "Mark4")]
Innovation.Culture <- Inn_data[, c("Cult1", "Cult2", "Cult3", "Cult4", "Cult5")]
Resources.for.Innovation <- Inn_data[, c("Res1","Res2","Res3","Res4","Res5",
                                         "Res6","Res7","Res8","Res9")]

# Cronbach's alpha
alpha(Product.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Process.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Organizational.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Marketing.Innovation, check.keys = TRUE)$total$raw_alpha
alpha(Innovation.Culture, check.keys = TRUE)$total$raw_alpha
alpha(Resources.for.Innovation, check.keys = TRUE)$total$raw_alpha

Inn_data$Product.Innovation <- rowMeans(Inn_data[,c("Prod1","Prod4","Prod3",
                                                    "Prod5")])

Inn_data$Process.Innovation <- rowMeans(Inn_data[,c("Proc2","Proc3")])

Inn_data$Organizational.Innovation <- rowMeans(Inn_data[,c("Org1","Org2",
                                                           "Org3")])

Inn_data$Marketing.Innovation <- rowMeans(Inn_data[,c("Mark1","Mark2","Mark3",
                                                      "Mark4")])

Inn_data$Innovation.Culture <- rowMeans(Inn_data[,c("Cult1","Cult2","Cult3",
                                                    "Cult4","Cult5")])


Inn_data$Resources.for.Innovation <- rowMeans(Inn_data[,c("Res1","Res2","Res3",
                                  "Res4","Res5","Res6","Res7","Res8","Res9")])

# Inn_data$Product.Innovation <- rowMeans(Inn_data[,grepl("Prod",colnames(Inn_data))])

# Inn_data$Process.Innovation <- rowMeans(Inn_data[,grepl("Proc",colnames(Inn_data))])
# 
# Inn_data$Organizational.Innovation <- rowMeans(Inn_data[,grepl("Org",colnames(Inn_data))])
# 
# Inn_data$Marketing.Innovation <- rowMeans(Inn_data[,grepl("Mark",colnames(Inn_data))])
# 
# Inn_data$Innovation.Culture <- rowMeans(Inn_data[,grepl("Cult",colnames(Inn_data))])
# 
# 
# Inn_data$Resources.for.Innovation <- rowMeans(Inn_data[,grepl("Res",colnames(Inn_data))])

Data_Factors <- cbind(npi_data[ , (ncol(npi_data)-5):ncol(npi_data)], 
                      Inn_data[ , (ncol(Inn_data)-5):ncol(Inn_data)])


############################################ Factor-wise Correlation Analysis
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "pearson", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corMat=cor(Data_Factors, method="pearson")
temp <- is.na(corMat)
corMat[temp] <- as.numeric(0)
p.mat <- cor.mtest(Data_Factors)
corrplot(corMat, type = "upper", order = "original", p.mat = p.mat, 
         sig.level = 0.05, pch.cex = 1.1, cl.cex = 0.7, tl.cex = 0.7)

#################################### Innovation Capabilities Factor-wise ANOVAs
ANOVA.Product.Innovation <- aov(Product.Innovation ~ (Authority + Exhibitionism + 
   Superiority + Vanity + Exploitativeness + Leadership)^2, data = Data_Factors)
summary(ANOVA.Product.Innovation)

ANOVA.Process.Innovation <- aov(Process.Innovation ~ (Authority + Exhibitionism + 
  Superiority + Vanity + Exploitativeness + Leadership)^2, data = Data_Factors)
summary(ANOVA.Process.Innovation)

ANOVA.Organizational.Innovation <- aov(Organizational.Innovation ~ (Authority + 
     Exhibitionism + Superiority + Vanity + Exploitativeness + Leadership)^2, 
     data = Data_Factors)
summary(ANOVA.Organizational.Innovation)

ANOVA.Marketing.Innovation <- aov(Marketing.Innovation ~ (Authority + 
      Exhibitionism + Superiority + Vanity + Exploitativeness + Leadership)^2, 
      data = Data_Factors)
summary(ANOVA.Marketing.Innovation)

ANOVA.Innovation.Culture <- aov(Innovation.Culture ~ (Authority + Exhibitionism + 
   Superiority + Vanity + Exploitativeness + Leadership)^2, data = Data_Factors)
summary(ANOVA.Innovation.Culture)

ANOVA.Resources.for.Innovation <- aov(Resources.for.Innovation ~ (Authority + 
      Exhibitionism + Superiority + Vanity + Exploitativeness + Leadership)^2, 
      data = Data_Factors )
summary(ANOVA.Resources.for.Innovation)

#Effect Size
library(effectsize)
eta_squared(ANOVA.Product.Innovation)
eta_squared(ANOVA.Process.Innovation)
eta_squared(ANOVA.Organizational.Innovation)
eta_squared(ANOVA.Marketing.Innovation)
eta_squared(ANOVA.Innovation.Culture)
eta_squared(ANOVA.Resources.for.Innovation)