# Raw survey data

Place the unmodified survey exports in this directory. The default filenames are configured in `Main.R`:

```text
1951BEreadyHauptstud-VectorborneDiseases_DATA_2026-03-05_1544.csv
NCCSimpactsStakehold_DATA_2025-06-26_0710.csv
NCCSimpactsStakehold_DATA_2025-06-26_0710_SUPP.csv
```

The BeReady file is expected to be a comma-separated REDCap-style export. The stakeholder files are expected to be semicolon-separated exports. The supplementary stakeholder file must contain `record_id`, `type`, and `canton`; `type` is used to identify human-health, animal-health, and environment departments.

`[PLACEHOLDER: document the definitive export filenames, dates, provenance, access conditions, encoding, and data dictionary.]`

These files are ignored by Git.
