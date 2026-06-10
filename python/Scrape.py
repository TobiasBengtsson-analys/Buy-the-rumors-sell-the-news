import pandas as pd
from newspaper import Article
import os

# ---------------------------
# 1. Load CSV
# ---------------------------

folder_path = # Din folder path
file_name = "apple_news_data.csv"

csv_path = os.path.join(folder_path, file_name)

# Load the CSV
news = pd.read_csv(csv_path)

urls = news['link'].tolist()
print(f"Found {len(urls)} URLs.")

# ---------------------------
# 2. Extract full articles with timestamps
# ---------------------------

articles_data = []

for i, url in enumerate(urls, 1):
    try:
        article = Article(url, language='en')
        article.download()
        article.parse()

        publish_dt = article.publish_date  # this can be None if not available

        # Optional: ensure datetime object, or keep as None
        if publish_dt is not None:
            publish_dt = pd.to_datetime(publish_dt)  # keeps date + time

        articles_data.append({
            'url': url,
            'title': article.title,
            'text': article.text,
            'publish_datetime': publish_dt  # now includes time
        })
        print(f"[{i}/{len(urls)}] Fetched: {article.title}")
    except Exception as e:
        print(f"[{i}/{len(urls)}] Failed to fetch {url}: {e}")

# ---------------------------
# 3. Save to new CSV
# ---------------------------

df_articles = pd.DataFrame(articles_data)

output_path = os.path.join(folder_path, "Alla_artiklar.csv")
df_articles.to_csv(output_path, index=False, encoding="utf-8")
print(f"✅ Saved {len(df_articles)} articles to {output_path}")
