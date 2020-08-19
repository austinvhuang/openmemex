# note2self

Bikeshedding personal project.

General concept:

- Dump notes to myself into a sqlite database.
- Notes can also be associated with topic tags.

This sets the stage for various automated processes which consume these.

- For example have a client that automatically crawls through notes consisting of URLs and pulls web content.
- Run NLP / other algorithms on content, either the notes themselves or associated page data downloaded off url links.
- Also various visualizations of notes and their topics (views of embeddings, etc.), put a small search engine in front of it, etc.
