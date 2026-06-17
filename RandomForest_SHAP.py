# %%
# Import necessary libraries
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report, fbeta_score, precision_score, recall_score, f1_score
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.feature_selection import mutual_info_regression, mutual_info_classif
from sklearn.metrics import mutual_info_score
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.neighbors import BallTree

from tqdm import tqdm  # For progress bars

import shap
from imblearn.over_sampling import SMOTE

from sklearn.model_selection import KFold
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
from sklearn.neighbors import BallTree
from sklearn.impute import SimpleImputer
from imblearn.over_sampling import SMOTE
from tqdm import tqdm

 


import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
from sklearn.neighbors import BallTree
from sklearn.impute import SimpleImputer
from imblearn.over_sampling import SMOTE
from functools import reduce
from IPython.display import display

# %%
import os

# Path setup
mac = os.path.expanduser('~/Library/CloudStorage/OneDrive-UCIrvine/')
wndw = 'C:/Users/Somnath_UCI/OneDrive - UC Irvine/'

if os.path.exists(wndw):
    location = wndw
    print(f"Location is: {location} (Windows path)")
elif os.path.exists(mac):
    location = mac
    print(f"Location is: {location} (Mac path)")
else:
    raise FileNotFoundError("Error, no valid path found")
	
	
	
path = os.path.join(location, "BuildingDamagePotential/Modellings/CalDins_upd_extractions_subset_03.csv")

df_withoutBalancing = pd.read_csv(path)
df_withoutBalancing

# %%
plt.hist(df_withoutBalancing['BuildingDensity'])

# %%
Data = df_withoutBalancing


# %%
# Split the data into x and y 
X = Data.drop(columns=['IDs','Damage','Damage01','Firename',"MinRH","Tmin",'EVC','BTGS','GrsShr_Area','BuildYear', 'Longitude', 'Latitude'])
                                         
                                         #, 'CBFR'  ])
                                      #         , "prcp", "MaxRH",
                                       #          "Srad",'tdmean','WD',"Tmax","VPD",'WS10m'])
X1 = Data.drop(columns=['IDs','Damage','Damage01' , 'CBFR', 'Firename',"MinRH","Tmin",  'Longitude',
                        'BuildYear', 
                        'Latitude', 'EVC','BTGS','GrsShr_Area' ])
                                      #         , "prcp", "MaxRH",
                                       #          "Srad",'tdmean','WD',"Tmax","VPD",'WS10m'])

X2 = Data.drop(columns=['IDs','Damage','Damage01' , 'CBFR', 'Firename',"MinRH","Tmin",  'Longitude', 'Latitude'
                                               , "prcp", "MaxRH",  'EVC','BTGS','GrsShr_Area',
                                            #  'BuildYear', 
                                                 "Srad",'tdmean','WD',"Tmax","VPD",'WS10m'])

y = Data['Damage01']


# %%


def evaluate_spatial_cv(X, y, coords, model, n_splits=10, radius_m=200.0, random_state=123):
    """
    Perform 10-fold cross-validation with a spatial dead-zone and SMOTE class balancing.
    Missing values (NaNs) are imputed using the median within each fold to prevent data leakage.
    For each fold, training points within `radius_m` of any test point are excluded.
    SMOTE is then applied to balance the training data before model fitting.
    """
    print(f"Preparing {n_splits}-Fold Spatial CV with {radius_m}m exclusion zone and SMOTE balancing...")
    
    # Convert Latitude and Longitude to radians for Haversine distance
    coords_rad = np.radians(coords[['Latitude', 'Longitude']].values)
    
    # Build BallTree for efficient spatial queries
    tree = BallTree(coords_rad, metric='haversine')
    
    # Convert radius in meters to radians (Earth radius = ~6,371,000 meters)
    EARTH_RADIUS_M = 6371000.0
    radius_rad = radius_m / EARTH_RADIUS_M
    
    kf = KFold(n_splits=n_splits, shuffle=True, random_state=random_state)
    
    # Track the excluded observations along with the standard metrics
    metrics = {
        'accuracy': [], 'precision': [], 'recall': [], 'f1': [], 'roc_auc': [], 
        'excluded_obs': [] 
    }
    
    # Ensure numpy arrays for consistent indexing
    X_arr = X.values if hasattr(X, 'values') else X
    y_arr = y.values if hasattr(y, 'values') else y
    
    is_multiclass = len(np.unique(y_arr)) > 2
    
    for train_idx, test_idx in tqdm(kf.split(X_arr), total=n_splits, desc="Spatial CV Folds"):
        # Query the BallTree for all points within the radius of the test points
        indices_within_radius = tree.query_radius(coords_rad[test_idx], r=radius_rad)
        
        # Flatten the list of arrays and get unique indices to drop from the training set
        indices_to_exclude = np.unique(np.concatenate(indices_within_radius))
        
        # Remove the spatially close points from the training set
        valid_train_idx = np.setdiff1d(train_idx, indices_to_exclude)
        
        # Track the number of excluded training observations for this fold
        num_excluded = len(train_idx) - len(valid_train_idx)
        metrics['excluded_obs'].append(num_excluded)
        
        X_train_fold, y_train_fold = X_arr[valid_train_idx], y_arr[valid_train_idx]
        X_test_fold, y_test_fold = X_arr[test_idx], y_arr[test_idx]
        
        # -- Impute NaNs before passing to SMOTE --
        imputer = SimpleImputer(strategy='median')
        X_train_fold_imputed = imputer.fit_transform(X_train_fold)
        X_test_fold_imputed = imputer.transform(X_test_fold)
        
        # Apply SMOTE to balance the imputed training fold
        smote = SMOTE(random_state=random_state)
        X_train_fold_balanced, y_train_fold_balanced = smote.fit_resample(X_train_fold_imputed, y_train_fold)
        
        # Train model on the balanced data
        model.fit(X_train_fold_balanced, y_train_fold_balanced)
        
        # Predict on the imputed test data
        y_pred = model.predict(X_test_fold_imputed)
        
        # Calculate accuracy metrics
        metrics['accuracy'].append(accuracy_score(y_test_fold, y_pred))
        
        if is_multiclass:
            y_proba = model.predict_proba(X_test_fold_imputed)
            metrics['roc_auc'].append(roc_auc_score(y_test_fold, y_proba, multi_class='ovr', average='weighted'))
            metrics['precision'].append(precision_score(y_test_fold, y_pred, average='weighted', zero_division=0))
            metrics['recall'].append(recall_score(y_test_fold, y_pred, average='weighted', zero_division=0))
            metrics['f1'].append(f1_score(y_test_fold, y_pred, average='weighted', zero_division=0))
        else:
            y_proba = model.predict_proba(X_test_fold_imputed)[:, 1] # Probability of positive class
            metrics['roc_auc'].append(roc_auc_score(y_test_fold, y_proba))
            metrics['precision'].append(precision_score(y_test_fold, y_pred, zero_division=0))
            metrics['recall'].append(recall_score(y_test_fold, y_pred, zero_division=0))
            metrics['f1'].append(f1_score(y_test_fold, y_pred, zero_division=0))
            
        # -- NEW: Print metrics for this specific fold --
        fold_num = len(metrics['accuracy'])
        tqdm.write(
            f"Fold {fold_num:2d} | "
            f"OA: {metrics['accuracy'][-1]:.3f} | "
            f"F1: {metrics['f1'][-1]:.3f} | "
            f"Prec: {metrics['precision'][-1]:.3f} | "
            f"Rec: {metrics['recall'][-1]:.3f} | "
            f"AUC: {metrics['roc_auc'][-1]:.3f} | "
            f"Excluded Obs: {num_excluded}"
        )
            
    # Calculate average and standard deviation of accuracy metrics
    results_df = pd.DataFrame(metrics)
    
    summary_df = pd.DataFrame({
        'Metric': results_df.columns,
        'Mean': results_df.mean().values,
        'Std Dev': results_df.std().values
    })
    
    print(f"\n=== {n_splits}-Fold Spatial CV Results Summary ===")
    print(summary_df.to_string(index=False))
    
    return results_df, summary_df


# %%
# Define the Coordinates explicitly from the original dataframe
coords = Data[['Latitude', 'Longitude']]

# 2. Setup your three models
rf_model00 = RandomForestClassifier(n_estimators=2000, random_state=123, n_jobs=-1, criterion='entropy')
rf_model01 = RandomForestClassifier(n_estimators=2000, random_state=123, n_jobs=-1, criterion='entropy')
rf_model02 = RandomForestClassifier(n_estimators=2000, random_state=123, n_jobs=-1, criterion='entropy')

# Create a list of datasets and their corresponding models
datasets = [
    ('Model 00 (Features: X)', X, rf_model00),
    ('Model 01 (Features: X1)', X1, rf_model01),
    ('Model 02 (Features: X2)', X2, rf_model02)
]

all_fold_results = {}
all_summaries = {}

# Loop through and evaluate each model sequentially
for name, features, model in datasets:
    print(f"\n=======================================================")
    print(f"Evaluating {name}")
    print(f"=======================================================")
    
    fold_res, summary = evaluate_spatial_cv(features, y, coords, model, radius_m=200.0)
    
    all_fold_results[name] = fold_res
    all_summaries[name] = summary


# %%
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score, roc_auc_score
from sklearn.neighbors import BallTree
from sklearn.impute import SimpleImputer
from imblearn.over_sampling import SMOTE
from functools import reduce
from IPython.display import display


#  Setup coordinates and target variable
coords = Data[['Latitude', 'Longitude']]
y_arr = y.values if hasattr(y, 'values') else y
coords_rad = np.radians(coords.values)

#  Create the index splits 
indices = np.arange(len(y_arr))
train_idx_raw, test_idx = train_test_split(indices, test_size=0.1, random_state=123, stratify=y_arr)

#  Apply the Spatial Dead Zone using BallTree
tree = BallTree(coords_rad, metric='haversine')
EARTH_RADIUS_M = 6371000.0
radius_rad = 200.0 / EARTH_RADIUS_M

print("Filtering training points within 200m of test points...")
indices_within_radius = tree.query_radius(coords_rad[test_idx], r=radius_rad)
indices_to_exclude = np.unique(np.concatenate(indices_within_radius))

# Valid train indices after applying the 200m dead zone
valid_train_idx = np.setdiff1d(train_idx_raw, indices_to_exclude)

num_excluded = len(train_idx_raw) - len(valid_train_idx)
print(f"Original Train size: {len(train_idx_raw)}")
print(f"Excluded Train points (too close to test set): {num_excluded}")
print(f"Final Train size: {len(valid_train_idx)}")
print(f"Test size: {len(test_idx)}\n")

# Prepare datasets to loop over
datasets = [
    ("Model 00 (Features: X)", X, "Comprehensive"),
    ("Model 01 (Features: X1)", X1, "Enviro-weather hybrid"),
    ("Model 02 (Features: X2)", X2, "Environmental exposure")
]

# Dictionary to store feature importances for plotting later
fi_data = {}
fi_dfs_list = [] # List for the final merged table

#  Loop over all three models
for model_id, features, model_short_name in datasets:
    print(f"===========================================================")
    print(f"Evaluating {model_id}")
    print(f"===========================================================")
    
    # Extract feature names and arrays
    feature_names = features.columns if hasattr(features, 'columns') else [f"Feature {i}" for i in range(features.shape[1])]
    X_arr = features.values if hasattr(features, 'values') else features
    
    # Slice the data
    X_train_raw = X_arr[valid_train_idx]
    y_train_raw = y_arr[valid_train_idx]
    X_test_raw = X_arr[test_idx]
    y_test_raw = y_arr[test_idx]
    
    # -- Impute and SMOTE --
    print("Imputing NaNs and balancing with SMOTE...")
    imputer = SimpleImputer(strategy='median')
    X_train_imputed = imputer.fit_transform(X_train_raw)
    X_test_final = imputer.transform(X_test_raw)
    
    smote = SMOTE(random_state=123)
    X_train_final, y_train_final = smote.fit_resample(X_train_imputed, y_train_raw)
    
    # Initialize and Train
    print("Training final model (2000 trees)...")
    model = RandomForestClassifier(n_estimators=3000, random_state=123, n_jobs=-1, criterion='entropy')
    model.fit(X_train_final, y_train_final)
    
    # Predict
    y_pred = model.predict(X_test_final)
    y_proba = model.predict_proba(X_test_final)[:, 1]
    
    # Calculate Metrics
    acc = accuracy_score(y_test_raw, y_pred)
    prec = precision_score(y_test_raw, y_pred, zero_division=0)
    rec = recall_score(y_test_raw, y_pred, zero_division=0)
    f1 = f1_score(y_test_raw, y_pred, zero_division=0)
    roc_auc = roc_auc_score(y_test_raw, y_proba)

    print("\nMetrics:")
    print(f"Accuracy:  {acc:.4f} | Precision: {prec:.4f} | Recall: {rec:.4f} | F1: {f1:.4f} | ROC AUC: {roc_auc:.4f}")
    
    # Store Feature Importances (CONVERTED TO PERCENTAGE)
    fi_df = pd.DataFrame({
        'Feature': feature_names,
        'Importance (%)': model.feature_importances_ * 100
    }).sort_values(by='Importance (%)', ascending=False)
    
    # Store for plotting
    fi_data[model_id] = fi_df
    
    # Store renamed column for the combined table
    fi_table_df = fi_df.copy()
    fi_table_df.rename(columns={'Importance (%)': f"{model_short_name} (%)"}, inplace=True)
    fi_dfs_list.append(fi_table_df)

# Generate Subplots for all Feature Importances
print("\nGenerating Feature Importance Plots...")
fig, axes = plt.subplots(1, 3, figsize=(24, 10))

for ax, (model_id, fi_df) in zip(axes, fi_data.items()):
    sns.barplot(x='Importance (%)', y='Feature', data=fi_df, ax=ax, palette='viridis')
    ax.set_title(f'Feature Importances\n{model_id}')
    ax.set_xlabel('Importance (%)')
    ax.set_ylabel('')

plt.tight_layout()
plt.show()

#  Print Consolidated Feature Importance Table
print("\n=== Consolidated Feature Importance Table (%) ===")
# Merge all 3 feature importance dataframes on 'Feature'
combined_fi_table = reduce(lambda left, right: pd.merge(left, right, on='Feature', how='outer'), fi_dfs_list)

# Sort by the Comprehensive model's importance (or any column you prefer)
combined_fi_table = combined_fi_table.sort_values(by='Comprehensive (%)', ascending=False).reset_index(drop=True)

# Round to 2 decimal places and fill NaNs with '-' for features that don't exist in a specific model
combined_fi_table_display = combined_fi_table.round(2).fillna('-')
display(combined_fi_table_display)


# %%
print("Re-evaluating models to generate detailed classification matrix...")

# We assume datasets, valid_train_idx, test_idx, y_arr are still in your notebook's memory
for model_id, features, model_short_name in datasets:
    print(f"\nTraining {model_short_name}...")
    
    X_arr = features.values if hasattr(features, 'values') else features
    
    X_train_raw = X_arr[valid_train_idx]
    y_train_raw = y_arr[valid_train_idx]
    X_test_raw = X_arr[test_idx]
    y_test_raw = y_arr[test_idx]
    
    # Impute and SMOTE
    imputer = SimpleImputer(strategy='median')
    X_train_imputed = imputer.fit_transform(X_train_raw)
    X_test_final = imputer.transform(X_test_raw)
    
    smote = SMOTE(random_state=123)
    X_train_final, y_train_final = smote.fit_resample(X_train_imputed, y_train_raw)
    
    # Train
    model = RandomForestClassifier(n_estimators=2000, random_state=123, n_jobs=-1, criterion='entropy')
    model.fit(X_train_final, y_train_final)
    
    # Predict
    y_pred = model.predict(X_test_final)
    
    # Get detailed metrics dictionary
    report = classification_report(y_test_raw, y_pred, output_dict=True)
    
    # Dynamically find the class keys (usually '0' and '1', or '0.0' and '1.0')
    classes = [str(c) for c in np.unique(y_test_raw)]
    c0, c1 = classes[0], classes[1]
    
    # Extract values
    p_0, r_0, f_0 = report[c0]['precision'], report[c0]['recall'], report[c0]['f1-score']
    p_1, r_1, f_1 = report[c1]['precision'], report[c1]['recall'], report[c1]['f1-score']
    acc = report['accuracy']
    p_ma, r_ma, f_ma = report['macro avg']['precision'], report['macro avg']['recall'], report['macro avg']['f1-score']
    p_wa, r_wa, f_wa = report['weighted avg']['precision'], report['weighted avg']['recall'], report['weighted avg']['f1-score']
    
    # Print as a clean table
    print(f"\n{'='*55}")
    print(f"  {model_short_name} Model")
    print(f"{'='*55}")
    print(f"{'Class':<18}{'Precision':>12}{'Recall':>12}{'F1-score':>12}")
    print(f"{'-'*55}")
    print(f"{'No-Damage: 0':<18}{p_0:>12.2f}{r_0:>12.2f}{f_0:>12.2f}")
    print(f"{'Damage: 1':<18}{p_1:>12.2f}{r_1:>12.2f}{f_1:>12.2f}")
    print(f"{'-'*55}")
    print(f"{'Overall Accuracy':<18}{acc:>36.2f}")
    print(f"{'Macro avg':<18}{p_ma:>12.2f}{r_ma:>12.2f}{f_ma:>12.2f}")
    print(f"{'Weighted avg':<18}{p_wa:>12.2f}{r_wa:>12.2f}{f_wa:>12.2f}")

print(f"\n{'='*55}")
print("Done.")

# %%
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.lines import Line2D

#  Define Categories and Colors 
feature_categories = {
    'CBFR': 'Building Vulnerability',
    'tdmean': 'Weather', 'Srad': 'Weather', 'WS10m': 'Weather', 'WD': 'Weather',
    'MaxRH': 'Weather', 'Tmax': 'Weather', 'VPD': 'Weather', 'prcp': 'Weather',
    'Elevation': 'Topography', 'TRI': 'Topography', 'Aspect': 'Topography', 'Slope': 'Topography',
    'WUI_Area': 'Human footprint', 'BuildingDensity': 'Human footprint', 
    'BuildYear': 'Human footprint', 'RoadLength': 'Human footprint',
    'PFTk9': 'Ecosystem', 'Frs_Area': 'Ecosystem', 'BTF': 'Ecosystem', 
    'EVT': 'Ecosystem', 'EVH': 'Ecosystem'
}

# The hex colors map closely to the ones used in your reference image
category_colors = {
    'Topography': '#3b82c4',             # Blue
    'Ecosystem': '#2f9c85',              # Teal/Green
    'Weather': '#eb7f44',                # Orange
    'Human footprint': '#c93f2c',        # Red
    'Building Vulnerability': '#000000', # Black
    'Unknown': 'gray'                    # Fallback for unmapped features
}

#  Generate the 3 separate plots
# (This assumes 'fi_data' is still in your notebook's memory from the last code block)
for idx, (model_name, fi_df) in enumerate(fi_data.items()):
    # Create figure
    fig, ax = plt.subplots(figsize=(6, 8), dpi=300)
    
    # ---------------------------------------------------------
    # --> CRITICAL FIX: Only grab the top 15 features! <--
    # ---------------------------------------------------------
    df_plot = fi_df.head(15).copy()
    
    # Map the categories and colors
    df_plot['Category'] = df_plot['Feature'].map(feature_categories).fillna('Unknown')
    df_plot['Color'] = df_plot['Category'].map(category_colors).fillna('gray')
    
    # Sort ascending for plotting from bottom to top
    df_plot = df_plot.sort_values(by='Importance (%)', ascending=True).reset_index(drop=True)
    
    # Draw horizontal dashed lines
    ax.hlines(y=df_plot['Feature'], xmin=0, xmax=df_plot['Importance (%)'], 
              color=df_plot['Color'], linestyles='dashed', linewidth=1.5)
    
    # Draw the large dots
    ax.scatter(df_plot['Importance (%)'], df_plot['Feature'], 
               color=df_plot['Color'], s=120, zorder=3)
    
    # Add percentage text right next to the dots
    for i, row in df_plot.iterrows():
        ax.text(row['Importance (%)'] + 0.4, i, f"{row['Importance (%)']:.1f}%", 
                va='center', ha='left', fontsize=10)
    
    # Axis formatting
    ax.set_xlabel('Mean Decreasing Accuracy (MDA; %)', fontsize=11, fontweight='bold')
    ax.set_ylabel('')
    
    # Clean up grid and spines
    ax.xaxis.grid(True, linestyle='-', color='#e0e0e0', alpha=1.0)
    ax.yaxis.grid(False)
    ax.set_axisbelow(True) # Put vertical grid behind the horizontal lines and dots
    
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['left'].set_visible(False)
    
    # Create Custom Legend
    # Filter to only include categories that actually exist in the Top 15 of this specific model
    existing_cats = df_plot['Category'].unique()
    legend_elements = [
        Line2D([0], [0], marker='o', color='w', markerfacecolor=category_colors[cat], 
               markersize=10, label=cat)
        # Order the legend manually to match the image
        for cat in ['Topography', 'Ecosystem', 'Weather', 'Human footprint', 'Building Vulnerability', 'Unknown']
        if cat in existing_cats
    ]
    
    # Add legend to the bottom right
    ax.legend(handles=legend_elements, loc='lower right', frameon=False, 
              prop={'weight':'bold', 'size': 10})
    
    # Add a), b), c) annotation to the top left
    letter = chr(97 + idx) # Generates 'a', 'b', 'c' dynamically
    ax.text(-0.30, 1.02, f"{letter})", transform=ax.transAxes, fontsize=16, fontweight='bold', va='top')
    
    # Adjust X limits to make room for the text labels on the right
    max_val = df_plot['Importance (%)'].max()
    ax.set_xlim(0, max_val + (max_val * 0.18))
    
    plt.tight_layout()
    
    # Save the figure as a PDF in your local directory!
    safe_name = model_name.split()[1] # Extracts '00', '01', or '02'
    filename = f"Lollipop_Feature_Importance_Model_{safe_name}.svg"
    plt.savefig(filename, bbox_inches='tight', transparent=True)
    print(f"Saved: {filename}")
    
    # Show it in the notebook
    plt.show()


# %%

from sklearn.metrics import roc_curve, auc

# Dictionary to safely store the ROC curve data for plotting
roc_data = {}

print("Evaluating models to generate ROC data...")

# Assuming datasets, valid_train_idx, test_idx, and y_arr are still in your memory
for model_id, features, model_short_name in datasets:
    print(f"Processing {model_short_name}...")
    
    X_arr = features.values if hasattr(features, 'values') else features
    
    X_train_raw = X_arr[valid_train_idx]
    y_train_raw = y_arr[valid_train_idx]
    X_test_raw = X_arr[test_idx]
    y_test_raw = y_arr[test_idx]
    
    # 1. Impute and balance
    imputer = SimpleImputer(strategy='median')
    X_train_imputed = imputer.fit_transform(X_train_raw)
    X_test_final = imputer.transform(X_test_raw)
    
    smote = SMOTE(random_state=123)
    X_train_final, y_train_final = smote.fit_resample(X_train_imputed, y_train_raw)
    
    # 2. Train Model
    model = RandomForestClassifier(n_estimators=2000, random_state=123, n_jobs=-1, criterion='entropy')
    model.fit(X_train_final, y_train_final)
    
    # 3. Predict probabilities for the positive class (Damage: 1)
    y_proba = model.predict_proba(X_test_final)[:, 1]
    
    # 4. Calculate False Positive Rate, True Positive Rate, and AUC
    fpr, tpr, _ = roc_curve(y_test_raw, y_proba)
    roc_auc = auc(fpr, tpr)
    
    # Store for plotting
    roc_data[model_short_name] = {
        'fpr': fpr,
        'tpr': tpr,
        'auc': roc_auc
    }

# ---------------------------------------------------------
# Plotting the Combined ROC Curves
# ---------------------------------------------------------
print("\nGenerating ROC Plot...")
plt.figure(figsize=(8, 8), dpi=300)

# Define the colors mapping exactly to your image
colors = {
    'Comprehensive': 'blue',
    'Enviro-weather hybrid': 'green',
    'Environmental exposure': 'red'
}

# Plot each model's ROC curve
for model_name, data in roc_data.items():
    plt.plot(
        data['fpr'], 
        data['tpr'], 
        color=colors.get(model_name, 'black'), 
        lw=2.5, 
        label=f'{model_name} Model (AUC = {data["auc"]:.2f})'
    )

# Plot the diagonal 50% random-chance line
plt.plot([0, 1], [0, 1], color='black', lw=1.5, linestyle='--')

# Formatting the plot to match your aesthetic exactly
plt.xlim([0.0, 1.0])
plt.ylim([0.0, 1.05])
plt.xlabel('False Positive Rate', fontsize=14, fontweight='bold')
plt.ylabel('True Positive Rate', fontsize=14, fontweight='bold')
plt.title('Comparison of ROC Curves', fontsize=18, fontweight='bold')

# Gridlines
plt.grid(True, linestyle='--', color='lightgray', alpha=0.7)
plt.tick_params(axis='both', which='major', labelsize=12)

# Legend
plt.legend(loc="lower right", fontsize=12, frameon=True, edgecolor='lightgray')

plt.tight_layout()

# Save the plot as a PDF
# Save and show
output_dir = r'C:\Users\Somnath_UCI\OneDrive - UC Irvine\BuildingDamagePotential/MS/V05/Updated_Figs'
os.makedirs(output_dir, exist_ok=True)
plt.savefig(os.path.join(output_dir, 'AUC_Comparison.pdf'),
            dpi=300, bbox_inches='tight', format='pdf')
plt.show()

print("Saved plot as 'ROC_Curves_Comparison.pdf'")

# Display the plot
plt.show()


# %%


# %% [markdown]
# # SHAP Analaysis and Figure generation

# %%

# The model IDs you exported
model_ids = ['00', '01', '02']

for idx, m_id in enumerate(model_ids):
    feat_file = f"SHAP_Features_Model_{m_id}.csv"
    shap_file = f"SHAP_Values_Model_{m_id}.csv"
    
    # Check if files exist in the directory
    if os.path.exists(feat_file) and os.path.exists(shap_file):
        print(f"Loading data and plotting SHAP for Model {m_id}...")
        
        # 1. Load the pre-processed Top 15 data from CSV
        X_test_sample = pd.read_csv(feat_file)
        shap_df = pd.read_csv(shap_file)
        
        # 2. Convert SHAP DataFrame back to a raw numpy array for the plotting function
        shap_values_array = shap_df.values
        
        # 3. Setup the Figure
        fig, ax = plt.subplots(figsize=(6, 8), dpi=300)
        
        # 4. Generate the Plot
        # 'sort=False' guarantees it uses the exact Top-to-Bottom order saved in the CSV
        shap.summary_plot(
            shap_values_array, 
            X_test_sample, 
            show=False, 
            sort=False, 
            alpha=0.6
        )
        
        # 5. Format the axes to match your manuscript aesthetic
        ax = plt.gca()
        ax.set_xlabel('SHAP value (impact on model output)', fontsize=11, fontweight='bold')
        ax.xaxis.grid(True, linestyle=':', color='#c0c0c0', alpha=1.0)
        
        # Add dynamic letter annotations (e.g., 'b)', 'd)', 'f)')
        #letter = chr(98 + (idx * 2)) 
        #ax.text(-0.35, 1.02, f"{letter})", transform=ax.transAxes, fontsize=16, fontweight='bold', va='top')
        
        plt.tight_layout()
        
        # Save as SVG
       # filename = f"C:/Users/Somnath_UCI/OneDrive - UC Irvine/BuildingDamagePotential/MS/V05/Updated_Figs/SHAP_Beeswarm_Model_{m_id}_Top15.svg"
        #plt.savefig(filename, format='svg', bbox_inches='tight', transparent=True)
       # print(f"Saved highly-scalable vector plot: {filename}\n")
        
        # Display inline
        plt.show()
    else:
        print(f"Warning: Could not find {feat_file} or {shap_file}. Did the previous export finish?")


# %% [markdown]
# # Spatial Blocking CV: Leave One Grid Out (LOGO) 

# %%


# IMPORTANT: Added recall_score here!
from sklearn.metrics import accuracy_score, f1_score, recall_score
import warnings
warnings.filterwarnings("ignore")

print("---  Generating 1x1 Degree Geographic Grids ---")
# Use the coordinates from your existing 'Data' dataframe
coords = Data[['Latitude', 'Longitude']].copy()
coords['lat_grid'] = np.floor(coords['Latitude'])
coords['lon_grid'] = np.floor(coords['Longitude'])

# Create a unique grid ID like "35:-120"
grid_ids = coords['lat_grid'].astype(int).astype(str) + ":" + coords['lon_grid'].astype(int).astype(str)
unique_grids = grid_ids.unique()

print(f"Total unique 1x1 degree grids found: {len(unique_grids)}")

# Datasets loaded in memory
datasets = [
    ("Comprehensive", X),
    ("Enviro-weather hybrid", X1),
    ("Environmental exposure", X2)
]

y_arr = y.values if hasattr(y, 'values') else y
logo_results = []

print("\n---  Running Leave-One-Grid-Out (LOGO) CV ---")
for model_name, features in datasets:
    print(f"\nProcessing {model_name} Model...")
    X_arr = features.values if hasattr(features, 'values') else features
    
    # Loop over every single grid
    for grid in tqdm(unique_grids, desc=f"{model_name} Grids"):
        # The test set is the grid we are leaving out
        test_mask = (grid_ids == grid).values
        train_mask = ~test_mask
        
        n_obs = test_mask.sum()
        if n_obs == 0:
            continue # Skip empty grids
            
        X_train_raw = X_arr[train_mask]
        y_train_raw = y_arr[train_mask]
        X_test_raw = X_arr[test_mask]
        y_test_raw = y_arr[test_mask]
        
        # Impute
        imputer = SimpleImputer(strategy='median')
        X_train_imputed = imputer.fit_transform(X_train_raw)
        X_test_final = imputer.transform(X_test_raw)
        
        # SMOTE
        try:
            smote = SMOTE(random_state=123)
            X_train_final, y_train_final = smote.fit_resample(X_train_imputed, y_train_raw)
        except ValueError:
            # Fallback if a grid exclusion makes a class too small for SMOTE
            X_train_final, y_train_final = X_train_imputed, y_train_raw
            
        # Train (Lowered to 1000 trees so it runs 2x faster for you!)
        model = RandomForestClassifier(n_estimators=1000, random_state=123, n_jobs=-1, criterion='entropy')
        model.fit(X_train_final, y_train_final)
        
        # Predict
        y_pred = model.predict(X_test_final)
        
        # =============================================================
        # NEW METRICS ADDED HERE
        # =============================================================
        acc = accuracy_score(y_test_raw, y_pred)
        
        # pos_label=1 explicitly forces it to calculate ONLY for the Damage class
        f1_class1 = f1_score(y_test_raw, y_pred, pos_label=1, zero_division=0)
        recall_class1 = recall_score(y_test_raw, y_pred, pos_label=1, zero_division=0)
        
        lat_cell = float(grid.split(':')[0])
        lon_cell = float(grid.split(':')[1])
        
        # Store results
        logo_results.append({
            'Model': model_name,
            'Grid': grid,
            'Lat': lat_cell,
            'Lon': lon_cell,
            'OA': acc,
            'F1_1': f1_class1,          # <-- Added
            'Recall_1': recall_class1,  # <-- Added
            'N_Obs': n_obs
        })

df_logo = pd.DataFrame(logo_results)
# Save to a new CSV file
csv_filename = "cv_logo_gridwise_three_models_updated.csv"
df_logo.to_csv(csv_filename, index=False)
print(f"\nAll Grids Processed! Saved data to: {csv_filename}")


# %%
import requests

print("\n---  Preparing Geospatial Data for Plotting ---")
def download_us_counties_2024(local_zip="tl_2024_us_county.zip"):
    url = "https://www2.census.gov/geo/tiger/TIGER2024/COUNTY/tl_2024_us_county.zip"
    if not os.path.exists(local_zip):
        print("Downloading US counties shapefile (TIGER 2024)...")
        r = requests.get(url, timeout=120)
        r.raise_for_status()
        with open(local_zip, "wb") as f:
            f.write(r.content)
    try:
        gdf = gpd.read_file(f"zip://{os.path.abspath(local_zip)}!tl_2024_us_county.shp")
    except Exception:
        gdf = gpd.read_file(f"zip://{os.path.abspath(local_zip)}")
    if gdf.crs is None:
        gdf = gdf.set_crs("EPSG:4326")
    else:
        gdf = gdf.to_crs("EPSG:4326")
    return gdf

gdf_all = download_us_counties_2024()
gdf_ca = gdf_all[gdf_all["STATEFP"] == "06"].copy()
gdf_ca_outline = gdf_ca.dissolve().reset_index(drop=True)

# Build polygons for the grids
def grid_polygon(lat, lon, size=1.0):
    return Polygon([(lon, lat), (lon+size, lat), (lon+size, lat+size), (lon, lat+size), (lon, lat)])

df_logo['geometry'] = df_logo.apply(lambda r: grid_polygon(r['Lat'], r['Lon']), axis=1)
gdf_logo = gpd.GeoDataFrame(df_logo, geometry='geometry', crs="EPSG:4326")

# Clip grids to California border to make it look clean
try:
    gdf_logo = gpd.clip(gdf_logo, gdf_ca_outline)
except Exception:
    gdf_logo = gpd.overlay(gdf_logo, gdf_ca_outline, how="intersection")

minx, miny, maxx, maxy = gdf_ca_outline.total_bounds
extent = (minx - 0.5, maxx + 0.5, miny - 0.5, maxy + 0.5)




# %%

print("\n---  Loading the UPDATED CSV and Rebuilding Geometries ---")

# 1. LOAD THE NEW CSV EXPLICITLY
df_logo = pd.read_csv("cv_logo_gridwise_three_models_updated.csv")

# 2. REBUILD THE GEOMETRIES
def grid_polygon(lat, lon, size=1):
    return Polygon([(lon, lat), (lon+size, lat), (lon+size, lat+size), (lon, lat+size), (lon, lat)])

df_logo['geometry'] = df_logo.apply(lambda r: grid_polygon(r['Lat'], r['Lon']), axis=1)
gdf_logo = gpd.GeoDataFrame(df_logo, geometry='geometry', crs="EPSG:4326")

# 3. CLIP TO CALIFORNIA (Assumes 'gdf_ca_outline' is still in your notebook's memory)
try:
    gdf_logo = gpd.clip(gdf_logo, gdf_ca_outline)
except Exception:
    gdf_logo = gpd.overlay(gdf_logo, gdf_ca_outline, how="intersection")

print("Successfully loaded new data and built geospatial grids!")


print("\n---  Generating Maps (Shared Colorbars on Right) ---")

COL_OA = 'OA'
COL_N = 'N_Obs'

# Only OA and N now
metrics_to_plot = [
    (COL_OA, 'Overall Accuracy (OA)', 'YlOrBr', True),
    (COL_N, 'Test Set Observations (N)', 'YlOrBr', False)
]

# Get the model names directly from the new CSV
models_to_plot = df_logo['Model'].unique()

# Master geodataframe that drops the <0.2 OA grids across the board
valid_gdf = gdf_logo[gdf_logo[COL_OA] >= 0.2].copy()

for metric_col, metric_title, cmap, is_percentage in metrics_to_plot:
    fig, axes = plt.subplots(1, 3, figsize=(24, 7), constrained_layout=True)
    fig.suptitle(f"{metric_title}", fontsize=28, fontweight='bold')
    
    print(f"\n========================================================")
    print(f"=== Statistics for: {metric_title} ===")
    print(f"========================================================")
    
    if is_percentage:
        vmin = 0.2
        vmax = 1.0
    else:
        # Scale dynamically based on the lowest and highest grid globally
        vmin = valid_gdf[metric_col].min()
        vmax = valid_gdf[metric_col].max()
    
    for idx, model_name in enumerate(models_to_plot):
        ax = axes[idx]
        
        # Filter data for this specific model
        model_gdf = valid_gdf[valid_gdf['Model'] == model_name]
        
        # Calculate Mean, SD, and Standard Error (SE)
        val_mean = model_gdf[metric_col].mean()
        val_sd = model_gdf[metric_col].std()
        val_se = val_sd / np.sqrt(len(model_gdf))
        
        # Print for the table!
        print(f"{model_name:30s} | Mean: {val_mean:.3f} | SD: ±{val_sd:.3f} | SE: ±{val_se:.3f} | Grids Used: {len(model_gdf)}")
        
        # Plot California outlines
        gdf_ca.boundary.plot(ax=ax, color="black", linewidth=0.5, zorder=1, alpha=0.5)
        gdf_ca_outline.boundary.plot(ax=ax, color="black", linewidth=1.5, zorder=2)
        
        # Plot the actual metric map
        model_gdf.plot(column=metric_col, ax=ax, cmap=cmap, vmin=vmin, vmax=vmax, 
                       edgecolor="k", linewidth=0.8, legend=False, zorder=3)
                       
        # Plot Text values directly on the grid centroids
        for i, row in model_gdf.iterrows():
            cx, cy = row.geometry.centroid.x, row.geometry.centroid.y
            
            if is_percentage:
                txt = f"{row[metric_col]:.2f}"
            else:
                txt = f"{row[metric_col]/1000:.1f}k" if row[metric_col] > 999 else str(int(row[metric_col]))
                
            ax.text(cx, cy, txt, ha="center", va="center", fontsize=12, color="black", fontweight='bold',
                    path_effects=[pe.withStroke(linewidth=3.0, foreground="white", alpha=0.9)], zorder=4)
                    
        # Clean up axes
        ax.set_title(f"{model_name}", fontsize=22, fontweight='bold')
        ax.set_xlim(extent[0], extent[1])
        ax.set_ylim(extent[2], extent[3])
        ax.axis('off')

    # ---------------------------------------------------------
    # CREATE THE SINGLE SHARED COLORBAR ON THE RIGHT
    # ---------------------------------------------------------
    norm = mcolors.Normalize(vmin=vmin, vmax=vmax)
    sm = cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    
    # orientation='vertical' places it on the right side of the frame
    cbar = fig.colorbar(sm, ax=axes, orientation='vertical', shrink=0.8, pad=0.02, aspect=30)
    cbar.set_label(metric_title, fontsize=22, fontweight='bold', rotation=270, labelpad=30)
    cbar.ax.tick_params(labelsize=18)

    # Save each metric as a separate vector image
    safe_title = metric_title.split()[0].replace('-', '_')
    plt.savefig(f"LOGO_Spatial_CV_{safe_title}_Comparison.svg", format='svg', bbox_inches='tight', transparent=True)
    plt.show()

print("\nAll spatial blocking maps successfully generated and stats printed for table!")

# %%

print("--- Generating Boxplots for Spatial Cross-Validation ---")

#  Load Data and filter exactly like the maps (< 0.2 dropped)
df_logo = pd.read_csv("cv_logo_gridwise_three_models_updated.csv")
valid_df = df_logo[df_logo['OA'] >= 0.2].copy()

# Define metrics
metrics_to_plot = {
    'OA': 'Overall Accuracy (OA)',
    'F1_1': 'F1-Score (Class 1: Damage)',
    'Recall_1': 'Recall (Class 1: Damage)',
    'N_Obs': 'Test Set Observations (N)'
}

# Choose a professional color palette for the 3 models
model_palette = ['#4c72b0', '#dd8452', '#55a868']

#  Generate a separate, perfectly scaled boxplot for each metric
for metric_col, metric_title in metrics_to_plot.items():
    fig, ax = plt.subplots(figsize=(8, 6), dpi=300)
    
    # Draw the Boxplot (show means as white dots)
    sns.boxplot(
        x='Model', y=metric_col, data=valid_df, 
        palette=model_palette, width=0.5, ax=ax,
        showfliers=False, # We turn off standard outliers because we plot the raw points below
        showmeans=True, 
        meanprops={"marker":"o", "markerfacecolor":"white", "markeredgecolor":"black", "markersize":"10"}
    )
    
    # Overlay the actual grid data points (jittered so they don't overlap)
    sns.stripplot(
        x='Model', y=metric_col, data=valid_df, 
        color='black', alpha=0.4, jitter=0.15, size=5, ax=ax
    )
    
    # Formatting
    ax.set_title(f"{metric_title} Distribution across Grids", fontsize=16, fontweight='bold')
    ax.set_ylabel(metric_title, fontsize=14, fontweight='bold')
    ax.set_xlabel("") # Remove 'Model' x-label since the names are self-explanatory
    
    # Make model names bold
    plt.xticks(fontsize=12, fontweight='bold')
    plt.yticks(fontsize=12)
    
    # Set vertical scale: 0 to 1 for accuracy metrics, let N_obs auto-scale
    if metric_col != 'N_Obs':
        # Start at 0.15 since we explicitly dropped grids < 0.2
        ax.set_ylim(0.15, 1.05) 
        
    # Clean up the background
    ax.grid(axis='y', linestyle='--', alpha=0.7)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    
    # Save as high-res SVG
    safe_title = metric_title.split()[0].replace('-', '_')
    filename = f"LOGO_Boxplot_{safe_title}.svg"
    plt.savefig(filename, format='svg', bbox_inches='tight', transparent=True)
    plt.show()
    
    print(f"Success! Saved: {filename}\n")

print("All boxplots generated successfully!")




