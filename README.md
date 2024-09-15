# 💳 Credit Card Default Prediction: Unlocking Credit Risk with Machine Learning

This project explores various machine learning techniques to predict the likelihood of credit card default among clients, providing insights into risk management in the financial sector. By comparing the performance of different models, we aim to empower decision-making with data-driven insights.

## 🎯 Project Objective
To predict whether a client will default on their credit card payment next month using four different machine learning models. We analyze each method's effectiveness in terms of accuracy, sensitivity, and specificity, ultimately providing recommendations for better credit risk management.

## 📊 Dataset Overview
The dataset includes 30,000 records from Taiwan's credit card clients, featuring:
- **Demographic Data**: `LIMIT_BAL` (Credit Limit), `SEX`, `EDUCATION`, `MARRIAGE`, `AGE`
- **Repayment History**: `PAY_APR` to `PAY_SEP`
- **Billing & Payment Info**: `BILL_AMT_APR` to `BILL_AMT_SEP`, `PAY_AMT_APR` to `PAY_AMT_SEP`
- **Target Variable**: `default.payment.next.month` (binary classification for default prediction)

## 🔍 Models & Techniques
1. **Logistic Regression**:
   - Conducted **feature selection** using Forward Selection, Backward Elimination, and Stepwise Selection with `StepAIC`.
   - **Key Insight**: As the threshold for prediction decreases, sensitivity increases, but specificity decreases. All selection methods returned identical accuracy.
  
2. **K-Nearest Neighbors (KNN)**:
   - Optimized K-value using cross-validation for the best performance.
   - **Key Insight**: Optimal performance observed with **K = 161**, achieving a balance between accuracy, sensitivity, and specificity.

3. **Classification and Regression Trees (CART)**:
   - Performed complexity parameter pruning to prevent overfitting.
   - **Key Insight**: The pruned tree offered better generalization, though the full tree had higher initial accuracy.

4. **Random Forest**:
   - Built an ensemble of decision trees for robust prediction.
   - **Key Insight**: Random Forest demonstrated the highest accuracy at **99% on the training set** and **84% on the test set**.

## 📈 Model Performance Summary
| Model            | Accuracy (Test Set) | Sensitivity | Specificity |
|------------------|---------------------|-------------|-------------|
| Logistic Regression | 75.66%           | 77.8%       | 69.7%       |
| K-Nearest Neighbors | 83.4%            | 51.9%       | 75.7%       |
| CART (Pruned)       | 70.61%           | 33.7%       | 90.2%       |
| Random Forest       | 84%              | 76.4%       | 83.3%        |


## 🔍 Key Insights & Recommendations
- **Logistic Regression**: Provides a good balance between interpretability and performance, making it useful for actionable insights in credit risk management.
- **Random Forest**: Offers the highest accuracy and robustness, ideal for large-scale default prediction tasks.
- **KNN & CART**: While effective, they require more tuning to achieve optimal sensitivity, particularly in imbalanced datasets.

## 🛠️ Applications
The insights from this project are crucial for:
- **Credit Card Companies**: To better assess creditworthiness and manage lending risks.
- **Financial Institutions**: Implementing machine learning models to improve predictive accuracy for default risk.
- **Risk Analysts**: Leveraging machine learning to inform credit risk decisions and policies.

## 🚀 Future Work
- **Hyperparameter Optimization**: Further tuning of models like Random Forest and KNN for improved performance.
- **Ensemble Methods**: Combining multiple models for even more robust prediction.
- **Exploring More Features**: Incorporating additional financial and behavioral data to enhance predictive power.

