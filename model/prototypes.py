import pandas as pd

import torch
from transformers import RobertaConfig, RobertaModel, RobertaTokenizer, RobertaTokenizerFast
import sqlite3
import altair as alt
import umap
import datetime

if __name__ == "__main__":
    conn = sqlite3.connect("note2self.db")
    df_cache = pd.read_sql_query("SELECT * FROM cache ", conn)
    titles = df_cache.cache_title
    tokenizer = RobertaTokenizerFast.from_pretrained("roberta-base")
    tokenized = tokenizer(list(titles), return_tensors="pt", padding=True, truncation=True)

    print("Loading roberta")
    model = RobertaModel.from_pretrained('roberta-base')

    print("Embedding")
    (last_hidden_state, pooler_output) = model(tokenized.input_ids, tokenized.attention_mask)

    print("Dimensionality Reduction")
    vis = umap.UMAP(output_metric='euclidean',
                            n_components=2,
                            random_state=42).fit_transform(pooler_output.detach().numpy())

    vis_df = pd.DataFrame({'umap1': vis[:, 0], 'umap2':vis[:, 1],
        'titles': titles, 
        'date': [datetime.datetime.strptime(d, '%Y-%m-%d') for d in df_cache.date]})

    filt = [not x.startswith('http') for x in vis_df.titles]
    vis_df_clean = vis_df[pd.Series(filt)]

    chart = alt.Chart(vis_df_clean).mark_circle(size=60).encode(
        x='umap1',
        y='umap2',
        color='date',
        tooltip=['titles']
    ).properties(
    width=800,
    height=400
    ).interactive(bind_x = False)
    chart.save('frontend-rs/static/contentspace.html')
    chart.show()


