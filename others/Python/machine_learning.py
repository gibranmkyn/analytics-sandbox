# import
import numpy as np
import matplotlib as plt
import seaborn as sns
from sklearn.neighbors import KNeighborsClassifier

# -------------- Classifier -------------------
# exploratory data analysis
df.head() 
df.info() #-- info of the datafram -- 
df.describe() 
iris['data'].shape # check (rows, columns)

# ----- Visual EDA
plt.figure()
sns.countplot(x='education', hue='party', data=df, palette='RdBu') # hue is the counter 
plt.xticks([0,1], ['No', 'Yes']) # categorise the data 
plt.show() 

# ----- ScikitLearn API requires continious data in Pandasdf or NumpyArray, and no missing values in the data

from sklearn.neighbors import KNeighborsClassifier # Import KNeighborsClassifier from sklearn.neighbors
y = df['party'].values # Create arrays for the features and the response variable
X = df.drop('party', axis=1).values

knn = KNeighborsClassifier(n_neighbors=6) # Create a k-NN classifier with 6 neighbors
knn.fit(X,y) # Fit the classifier to the data

y_pred = knn.predict(X) # Predict the labels for the training data X
new_prediction = knn.predict(X_new) # Predict and print the label for the new data point X_new
print("Prediction: {}".format(new_prediction))


train_test_split(X, y, test_size =0.3, random_state=21, stratify=y)
knn.score(X_test,y_test) # to know the accuracy
y_pred = knn.predict(X_test)



# ----- digit recognition dataset
from sklearn import datasets # dataset on number images
import matplotlib.pyplot as plt

digits = datasets.load_digits()

print(digits.keys()) #dictionary keys
print(digits.DESCR) #description of the dataset
print(digits.images.shape)# Print the shape of the images and data keys
print(digits.data.shape)

plt.imshow(digits.images[1010], cmap=plt.cm.gray_r, interpolation='nearest') # Display digit 1010
plt.show()


# ----- Train/Test split + Fit/Predict/Accuracy
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split

X = digits.data # Create feature and target arrays
y = digits.target 

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.2, random_state=42, stratify=y) #split 

knn = KNeighborsClassifier(n_neighbors=7) 
knn.fit(X_train, y_train) #fit to training data
print(knn.score(X_test, y_test)) #test on test data

# ----- Overfitting & Underfitting - build an iteration to know the optimum k
neighbors = np.arange(1, 9) # Setup arrays to store train and test accuracies
train_accuracy = np.empty(len(neighbors))
test_accuracy = np.empty(len(neighbors))

for i, k in enumerate(neighbors): # Loop over different values of k (neighbors)

    knn = KNeighborsClassifier(n_neighbors=k)
    knn.fit(X_train, y_train)
    train_accuracy[i] = knn.score(X_train, y_train)
    test_accuracy[i] = knn.score(X_test, y_test)

plt.title('k-NN: Varying Number of Neighbors')
plt.plot(neighbors, test_accuracy, label = 'Testing Accuracy')
plt.plot(neighbors, train_accuracy, label = 'Training Accuracy')
plt.legend()
plt.xlabel('Number of Neighbors')
plt.ylabel('Accuracy')
plt.show()


# -------------- Regression -------------------

# ----- importing data with reshape
import numpy as np
import pandas as pd

df = pd.read_csv('gapminder.csv')
y = df['life'].values # Create arrays for features and target variable
X = df['fertility'].values

print("Dimensions of y before reshaping: {}".format(y.shape)) # Print the dimensions of X and y before reshaping
print("Dimensions of X before reshaping: {}".format(X.shape))
y = y.reshape(-1, 1) # Reshape X and y
X = X.reshape(-1, 1)
print("Dimensions of y after reshaping: {}".format(y.shape)) # Print the dimensions of X and y after reshaping
print("Dimensions of X after reshaping: {}".format(X.shape))


# ----- Explore the dataset
df.head() #explore the top values in dataset
df.describe() #descriptive statistics; count, mean, std, min, 25%, 50%, 75%, maxs
df.info() #general info of dataframe
sns.heatmap(df.corr(), square=True, cmap='RdYlGn') #make heatmap of correlation between columns


# ----- Fit & Predict
from sklearn.linear_model import LinearRegression

reg = LinearRegression() # Create the regressor: reg
prediction_space = np.linspace(min(X_fertility), max(X_fertility)).reshape(-1,1)

reg.fit(X_fertility, y) # Fit the model to the data
y_pred = reg.predict(prediction_space) # Compute predictions over the prediction space
print(reg.score(X_fertility, y)) # Print R^2 

plt.plot(prediction_space, y_pred, color='black', linewidth=3) # Plot regression line
plt.show()

# ----- Train/test split for regression
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import train_test_split

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state=42)

reg_all = LinearRegression()
reg_all.fit(X_train, y_train) #fit
y_pred = reg_all.predict(X_test) #predict

print("R^2: {}".format(reg_all.score(X_test, y_test))) # Compute and print R^2 and RMSE
rmse = np.sqrt(mean_squared_error(y_test, y_pred))
print("Root Mean Squared Error: {}".format(rmse))


# ----- 5 fold cross-validation
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score

reg = LinearRegression()
cv_scores = cross_val_score(reg,X,y,cv=5) # Compute 5-fold cross-validation scores: cv_scores

print(cv_scores) # Print the 5-fold cross-validation scores
print("Average 5-Fold CV Score: {}".format(np.mean(cv_scores)))


# ----- Regularised regression
# Linear regression minimises a loss function, it chooses a coefficient for each feature variable -> Overfitting
# Riddged regression better when most variables are useful, lasso can turn variables into 0 coefficient

# 1) Rigged regression -> Loss function  = SUM(squared residuals) + alpha * SUM(each coefficient^2)

		# high alpha -> underfitting | 0 alpha (back to OLS) -> overfitting. Bigger alpha lower slope
		# by introducing a bias in a linear regression, it may not really fit the training data as well, but might be better for the long run
		# use 10 fold cross-validation to deterimine alpha
		# even when there isn't enough data to find OLS, ridge still can find a solution with cross validation & ridge regression penalty

# 2) Lasso regression -> Loss function  =  SUM(squared residuals) + alpha * SUM(|each coefficient|)
		# can be used to select important features of a dataset [shrinks the less important features to 0]
		# the bias in linear regression can have slope of 0, rigged only until close to 0

# ----- Lasso regression
from sklearn.linear_model import Lasso

lasso = Lasso(alpha=0.4, normalize=True) # Instantiate a lasso regressor: lasso

lasso.fit(X,y) #fit

lasso_coef = lasso.coef_ #compute coefficient
print(lasso_coef)

plt.plot(range(len(df_columns)), lasso_coef) # Plot the coefficients -> child mortality most important to predict life expectancy
plt.xticks(range(len(df_columns)), df_columns.values, rotation=60)
plt.margins(0.02)
plt.show()


# ----- Ridge regression
from sklearn.linear_model import Ridge
from sklearn.model_selection import cross_val_score


alpha_space = np.logspace(-4, 0, 50) # Setup the array of alphas and lists to store scores
ridge_scores = []
ridge_scores_std = []

ridge = Ridge(normalize=True) # Create a ridge regressor: ridge

for alpha in alpha_space: # Compute scores over range of alphas

    ridge.alpha = alpha
    ridge_cv_scores = cross_val_score(ridge, X,y, cv=10)
    ridge_scores.append(np.mean(ridge_cv_scores)) #append mean
    ridge_scores_std.append(np.std(ridge_cv_scores)) #append std score
    
display_plot(ridge_scores, ridge_scores_std)

