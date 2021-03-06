# [Kaggle Yelp Restaurant Photo Classification](https://www.kaggle.com/c/yelp-restaurant-photo-classification)

There is no click-get-submission-file script here, only set of the small scripts for different parts of the final solution.

[/model/](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/tree/master/model/21k)  
folder contains photo-level feature extraction scripts with different pretrained netoworks/layers.

[compress.py](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/blob/master/compress.py)  
Photo-level feature preprocessing: normalization or/and PCA transformation

[fisher.cpp](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/blob/master/cpp/feature_extractor/fisher.cpp)  
Fisher Vectors computation, input: features generated by compress.py

[vlad.cpp](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/blob/master/cpp/feature_extractor/vlad.cpp)  
VLAD descriptor computation, input: features generated by compress.py

[pool.py](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/blob/master/pool.py)  
Business-level feature extraction. Input: features generated by compress.py in case of feature averaging, or from fisher.cpp/vlad.cpp in case of FV/VLAD features.  

[predict_test.py](https://github.com/u1234x1234/kaggle-yelp-restaurant-photo-classification/blob/master/predict_test.py)  
Model training/prediction/submission file generation.  
Input: features generated by different execution of pool.py script.

