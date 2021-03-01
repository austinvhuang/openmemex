# note2self

General concept:

- Dump notes to myself into a sqlite database.
- Notes can also be associated with topic tags.
- Crawl notes consisting of URLs. Crawled metadata would be dumped in a new table in the schema of crawl-extracted metadata (for example, the title of the page corresponding to a URL note)

Some immediate short-term TODOs:

- Implement commands properly in optparse-applicative (resetting the database should be a command not a flag)
- Add a few more convenient commands (undo / delete a note, add a tag to an existing note)
- Add some crawling functionality that's specific to a target site. For example, with arxiv links, pull together abstracts from the link.
- Add functionality to automatically decorate notes with auto-generated tags (for example, github links would have a "repo" tag, medium posts have a "blog" tag, arxiv etc.)

This sets the stage for various automated processes which consume these (not implemented yet).

- For example have a client that automatically crawls through notes consisting of URLs and pulls web content.
- Run NLP / other algorithms on content, either the notes themselves or associated page data downloaded off url links.
- Also various visualizations of notes and their topics (views of embeddings, etc.), put a small search engine in front of it, etc.
