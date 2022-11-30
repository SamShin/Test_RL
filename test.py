import pandas as pd
import altair as alt
from IPython import display
alt.renderers.enable('mimetype')

df = pd.read_csv("data/clean_county.csv", low_memory=False)
display(df.head(10))