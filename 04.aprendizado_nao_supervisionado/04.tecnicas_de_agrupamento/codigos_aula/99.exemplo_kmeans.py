
from sklearn.experimental import enable_iterative_imputer

from sklearn.pipeline import make_pipeline
from sklearn.compose import make_column_transformer
from sklearn.preprocessing import OneHotEncoder
from sklearn.preprocessing import StandardScaler
from sklearn.impute import IterativeImputer, SimpleImputer
from sklearn.decomposition import PCA


class GlobalStandardScaler(StandardScaler):
  def fit(self, X, y=None, sample_weight=None):
    super().partial_fit(X, y)
    self.mean_[:] = X.mean()
    self.scale_[:] = np.maximum(X.std(), 1e-5)
    return self


CAT_FEATS = ["aspiration", "drive-wheels", 'num-of-cylinders', 'fuel-system']
NUM_FEATS = ['wheel-base', 'length', 'width', 'height', 'bore', 'stroke', 'city-mpg', 'highway-mpg', 'compression-ration', 'horsepower', 'pweak-rpm']

model = make_pipeline(
  make_column_transformer(
    (
      make_pipeline(SimpleImputer(missing_values="?", strategy="most_frequent"),
                    OneHotEncoder(drop="if_binary", sparse_output=False, min_frequency=0.1)),
      CAT_FEATS
    ),
    (
      make_pipeline(
        make_column_transformer((GlobalStandardScaler(), ['wheel-base', 'length', 'width', 'height']),
                                (GlobalStandardScaler(), ['bore', 'stroke']),
                                (GlobalStandardScaler(), ['city-mpg', 'highway-mpg']),
                                (StandardScaler(), ['compression-ration', 'horsepower', 'pweak-rpm']),
                                verbose_feature_names_out=False),
        PCA(num_components=0.97)),
      NUM_FEATS
    ),
    verbose_feature_names_out=False,
  ),
  IterativeImputer(),
  KMeans(n_clusters=8, random_state=15689),
)


train_features = model.fit_transform(train_data)  # Train preprocessors/K-Means and infer clusters.
test_features = model.transform(test_data)  # Apply preprocessors and infer clusters.