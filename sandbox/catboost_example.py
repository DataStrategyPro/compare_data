import catboost
from catboost import *
import shap
shap.initjs()
from sklearn.metrics import roc_auc_score

# To filter a pandas DataFrame on Pass and one type of Fail for each fail value in a loop

# for fail_value in df['result_detail'].unique():
#     fail_type = 'Fail-' + fail_value
#     # Create a copy of the original DataFrame for each fail type
#     filtered_df = df.copy()
#     # Filter the DataFrame for Pass and the current fail type
#     filtered_df = filtered_df[(filtered_df['result'] == 'Pass') | 
#                               ((filtered_df['result'] == 'Fail') & 
#                                (filtered_df['result_detail'] == fail_value))]
#     # Do something with the filtered DataFrame
#     print('Fail type:', fail_type)
#     print(filtered_df)
# 



from catboost.datasets import *
train_df, test_df = catboost.datasets.amazon()
y = train_df.ACTION
X = train_df.drop('ACTION', axis=1)
cat_features = list(range(0, X.shape[1]))

model = CatBoostClassifier(iterations=300, learning_rate=0.1, random_seed=12, )
model.fit(X, y, cat_features=cat_features, verbose=False, plot=False)

roc_auc_score(y, model.predict_proba(X)[:, 1])

explainer = shap.TreeExplainer(model)
shap_values = explainer.shap_values(Pool(X, y, cat_features=cat_features))

test_objects = [X.iloc[0:1], X.iloc[91:92]]

for obj in test_objects:
    print('Probability of class 1 = {:.4f}'.format(model.predict_proba(obj)[0][1]))
    print('Formula raw prediction = {:.4f}'.format(model.predict(obj, prediction_type='RawFormulaVal')[0]))
    print('\n')
    

shap.force_plot(explainer.expected_value, shap_values[0,:], X.iloc[0,:])

shap.summary_plot(shap_values, X)

df_feature_importance = pd.DataFrame(shap_values, columns=X.columns)

df_feature_importance.sum()

selected_cols = df_feature_importance.columns[df_feature_importance.mean() != 0].tolist()
selected_cols
