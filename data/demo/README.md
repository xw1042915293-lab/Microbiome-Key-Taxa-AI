# Demo Data Folder

This folder is optional.

If you want to provide your own demo dataset for "Demo Mode", place the following three files here:

- `abundance.csv` or `abundance.tsv`
- `metadata.csv` or `metadata.tsv`
- `taxonomy.csv` or `taxonomy.tsv`

Notes:

- The app will copy these files into a new `results/job_*` directory and run the existing workflow.
- `metadata` must include a `SampleID` column, and at least one grouping column (e.g., `Treatment`).
- If this folder is empty, the app will fall back to the built-in example files under `data/` (if present).

