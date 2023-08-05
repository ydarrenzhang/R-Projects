## Darren Zhang 
#Summary:Goal to develop a solution for a problem regarding classification of cherry tree(Species A) leaves and pear tree(Species B) leaves. 
#On the basis of R.A. Fisher’s Linear Discriminant Analysis(LDA) and collected data, conception of a classification method was produced. 
#System was designed and then conducted through R software, subsequent results given indicate the species of leaf(A or B) based on measurements 
#of width and length(mm). 
-----
#load libraries
#read data generation on cherry and pear tree leaves
library(readxl)
library(tidyverse)
library(ramify)
leafData = read_excel("C:/Users/.../leafdata.xlsx") #Note: make sure to change to your working directory!
head(leafData)

##est. of unknown parameters mu_A, mu_B, and covariance matrix for leaf species A and B:##
#cherry tree leaves(species A)
Xa = mean(leafData$widthA) #avg. width
Ya = mean(leafData$lengthA) #avg. length
mu_A = c(Xa, Ya)
mu_A = c(36.3750,80.0625)
#pear tree leaves(species B)
Xb = mean(leafData$widthB) #avg. width
Yb = mean(leafData$lengthB) #avg. length
mu_B = c(Xb, Yb)
mu_B = c(41.3125,70.6250)
#covariance matrix, note: we assume species A and B share same covariance matrix thus we use "pooled" estimate to get common sigma
leafDataA = data.frame(leafData$widthA, leafData$lengthA)
leafDataB = data.frame(leafData$widthB, leafData$lengthB)
covMatrixA = cov(leafDataA)
covMatrixB = cov(leafDataB)
covMatrix = (covMatrixA + covMatrixB) / 2 #common covariance matrix
matrixvalues = c(24.10625,33.08333,33.08333,137.28958)
covMatrix = matrix(matrixvalues, nrow = 2, ncol = 2,byrow=TRUE)

##classification rule function##
mu_A = c(36.3750,80.0625)
mu_B = c(41.3125,70.6250)
matrixvalues = c(24.10625,33.08333,33.08333,137.28958)
covMatrix = matrix(matrixvalues, nrow = 2, ncol = 2,byrow=TRUE)
classif = function(X,Y) {
  V = c(X,Y)
  VA = V - mu_A
  VB = V - mu_B
  f_xyA = (1/(2*pi*sqrt(det(covMatrix)))) * exp((-1/2) * t(VA) %*% solve(covMatrix) %*% (VA))
  f_xyB = (1/(2*pi*sqrt(det(covMatrix)))) * exp((-1/2) * t(VB) %*% solve(covMatrix) %*% (VB))
  CR = f_xyA/f_xyB
  if (CR > 1) {
    print(CR)
    return("Leaf is Species A")
  }
  else if (CR < 1) {
    print(CR)
    return("Leaf is Species B")
  }
  else if (CR == 1) {
    print(CR)
    return("Undetermined")
  }
}

#check for classification errors
#species A:
classif(48,89) #errors: 1(31,52), 5(37,68), 8(48,89), note: seems when diff. of length and width is less than width => species B
#species B:
classif(33,64) #errors: 10(41,91), 12(33,64)

#classify new leaves w/ measurements in mm(width,length): (32,82), (38,52), (40,76)
classif(32, 82) 
classif(38,52) 
classif(40,76)

#plot
#reformat original data.frame, leafData for easier plotting
species = c(rep("A",16), rep("B",16))
X = c(leafData$widthA, leafData$widthB)
Y = c(leafData$lengthA, leafData$lengthB)
lfData = data.frame(X, Y, species)
#calc. LDA decision boundary
midpoint = (mu_A + mu_B) / 2
ortho_to = solve(covMatrix) %*% (mu_A - mu_B)
slope_lda =0.4469877/0.1764544 #slope of lda decision line
int_lda = 75.34375 - (slope_lda * 38.84375) #intercept of lda decision line
#our line equation in slope-int form: y = 2.533163x - 23.05379
ggplot(data = lfData, aes(X, Y)) + geom_point(aes(color = species)) + xlab("width in mm") + ylab("length in mm") + ggtitle("Linear Discriminant Analysis Matrix for Species A and B") + geom_abline(slope = slope_lda, intercept = int_lda)

##new classification rule QDA, two species do not share same covariance matrix:##
newclassif = function(X,Y) {
  V = c(X,Y)
  VA = V - mu_A
  VB = V - mu_B
  p = 0.5 #note: this is log(pi_k), the proportion of training obs. that belong to kth class
  qf_xyA = -(log(det(covMatrixA)) / 2) - ((1/2) * t(VA) %*% solve(covMatrixA) %*% VA) + (log(p))
  qf_xyB = -(log(det(covMatrixB)) / 2) - ((1/2) * t(VB) %*% solve(covMatrixB) %*% VB) + (log(p))
  qCR = qf_xyA/qf_xyB
  if (qCR < 1) {
    print(qCR)
    return("Leaf is Species A")
  }
  else if (qCR > 1) {
    print(qCR)
    return("Leaf is Species B")
  }
  else if (qCR == 1) {
    print(qCR)
    return("Undetermined")
  }
}
newclassif(32,60)

#check for classification errors
#species A:
newclassif(31,52) #errors: 6(37,68), 8(48,89), note: seems when diff. of length and width is less than width => species B
#species B:
newclassif(33,64) #errors: 10(41,91), 12(33,64)
-----

#test plot *IGNORE*
#midpoint = (mu_A + mu_B) / 2
#ortho_to = solve(covMatrix) %*% (mu_A - mu_B)
#slope_lda =0.4469877/0.1764544 #slope of lda decision line
#int_lda = 75.34375 - (slope_lda * 38.84375) #intercept of lda decision line
ggplot(data = lfData, aes(X, Y)) + geom_point(aes(color = species)) + xlab("width in mm") + ylab("length in mm") + ggtitle("Quadratic Discriminant Analysis Matrix for Species A and B")
