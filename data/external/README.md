# External lookup data

The BeReady pipeline reads the three downloaded Excel workbooks directly. No manually prepared `municipality_topography.csv` file is required.

Place the following files in this directory using their original filenames:

```text
Gemeindestand.xlsx
HCL_CH_ISCO_19_PROF_level_1.xlsx
Raumgliederungen.xlsx
```

## `Gemeindestand.xlsx`

The script reads the worksheet `Daten` and uses these columns:

| Column | Use |
|---|---|
| `BFS Gde-nummer` | Municipality BFS code; joined to the survey field `bl_commune` |
| `Gemeindename` | Municipality name |
| `Kanton` | Canton abbreviation |

The lookup is used to add `municipality_name` and `municipality_canton` to the prepared BeReady data.

The attached workbook describes the municipal status on 6 April 2025 and reports an extraction date of 26 June 2025.

`[PLACEHOLDER: add the permanent source URL, licence, and reason for selecting this municipal-status date.]`

## `HCL_CH_ISCO_19_PROF_level_1.xlsx`

The script reads the first worksheet and uses:

| Column | Use |
|---|---|
| `Code` | One-digit CH-ISCO 2019 level-1 code |
| `Name_en` | English occupational-group label |

The detailed occupation strings in the survey contain an eight-digit code in brackets, for example `[45304021]`. The first digit is extracted and matched to `Code`.

`[PLACEHOLDER: add the exact i14y nomenclature URL, download date, language choice, version, and licence.]`

## `Raumgliederungen.xlsx`

The script reads the worksheet `Daten`. The workbook contains a title in row 1, so the import skips that row and uses row 2 as the header. It then uses:

| Column | Use |
|---|---|
| `BFS Gde-nummer` | Municipality BFS code |
| `Stadt/Land-Typologie` | Spatial classification code |

The spatial codes are converted as follows:

```text
1 = Urban
2 = Intermediate
3 = Rural
```

The attached workbook describes the spatial classifications on 1 January 2026 and reports an extraction date of 26 March 2026.

`[PLACEHOLDER: add the permanent source URL, licence, and justification for using the 2026 classification with the survey data.]`

## Reproducibility and redistribution

The scripts expect the original workbook layouts described above and stop with a clear error when a required column is absent.

These files are ignored by Git by default. Before publishing the repository, decide whether the licences permit redistribution. Otherwise, retain only this README and provide download instructions or archived source URLs.
