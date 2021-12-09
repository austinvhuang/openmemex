PRAGMA user_version = 1;

# ocr is deprecated
DROP TABLE IF EXISTS ocr;

# new tables
DROP TABLE IF EXISTS type;
DROP TABLE IF EXISTS content;
DROP TABLE IF EXISTS annotation;
DROP TABLE IF EXISTS artifact;
DROP TABLE IF EXISTS link;

# Create new tables

CREATE TABLE type (entry_id INTEGER,
  type TEXT CHECK (type IN ('TEXT', 'TEXT_UPDATE', 'IMAGE', 'AUDIO', 'ANNOTATION', 'ANNOTATION_UPDATE', 'LINK', 'OTHER')),
  UNIQUE(entry_id, type)
);

CREATE TABLE content (entry_id INTEGER, 
  content_id INTEGER,
  UNIQUE(entry_id, content_id));

CREATE TABLE annotation (entry_id INTEGER UNIQUE, annotation TEXT);

CREATE TABLE text (entry_id INTEGER UNIQUE, content TEXT);

CREATE TABLE link (enrty_id INTEGER UNIQUE, url TEXT);

CREATE TABLE artifact (entry_id INTEGER UNIQUE, artifact BLOB);

CREATE TABLE event (entry_id INTEGER PRIMARY KEY AUTOINCREMENT, date TEXT, time TEXT);

# Populate new tables

INSERT INTO link (entry_id, url) SELECT entry_id, content FROM entries WHERE content LIKE 'http%';

INSERT INTO text (entry_id, content) SELECT entry_id, content FROM entries WHERE content NOT LIKE 'http%';

INSERT INTO type (entry_id, type) SELECT entry_id, 'TEXT' FROM entries WHERE content NOT LIKE 'http%';

INSERT INTO type (entry_id, type) SELECT entry_id, 'LINK' FROM entries WHERE content NOT LIKE 'http%';

INSERT INTO event (entry_id, date, time) SELECT entry_id, date, time FROM entries

DROP TABLE entries; 

