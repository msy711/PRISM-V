## Dependencies (requires Python3)
```
pip install scikit-learn==1.3.1
pip install xgboost==2.0.3
pip install tqdm
```

## Feature ranking
```
python feature_importances.py -r 20240625_feature_importances.json
python rank_features.py -r 20240625_cutoff_runs.json -p 20240625_feature_importances.json
```

## Train prediction models
```
python train_models.py -r 20240627.json -p 20240625_cutoff_runs.json
```

## Train prediction models (male/female )
```
python train_models_male_female.py -r 20240724_male_female.json -p 20240625_cutoff_runs.json
```
