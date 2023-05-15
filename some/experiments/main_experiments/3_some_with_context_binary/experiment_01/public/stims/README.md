# What are all the files in this folder?

`items.json` -- the only file in the folder directly relevant to the experiment: the full set of stims to sample from

`swbdext.tab` -- the result of extracting all instances of "some" from the sw.backtrans_011410.t2c.gz Switchboard version on AFS (`run -c swbdext -e -o` with CtxtVar 20-b.ptn). 1951 cases

`some_database.csv` -- the data file analyzed in the 2015 S&P paper, duplicated of https://github.com/thegricean/corpus_some/blob/master/data/some_database.csv

`duplicate_some.txt` -- 119 cases with more than one instance of "some" before manual annotation (extracted from `some_database.csv` with `extract_stims.R`)

`duplicates_butnotall.tsv` -- the 119 cases in `duplicate_some.txt` with the ComparisonSentence annotated with "some, but not all"

`extract_stims.R` -- R file to add contexts from `swbdext.tab` to `some_database.csv` and create a ComparisonSentence column that's the original sentence with "some" replaced by "some, but not all"

`items.txt` -- all 1362 items to test in study, with intuitively named columns `Item_ID`, `OriginalSentence`, `ComparisonSentence`, `Context`.

`tsv_to_json.py` -- python file that converts `items.txt` to `items.json` for easy loading into experiment




