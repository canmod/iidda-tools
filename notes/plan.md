# File Format Notes

## Plan

### Directory Structure

```
GitHub
    davidearn/iidda/
        dataset-1/
            metadata
            original-sources/
                scan-image.png
                digitized-sheets.xlsx
            scripts/
                original-to-csv.R
                csv-to-csvy.R
        dataset-2/
            metadata
            original-sources/
                idda-sheet.xls
            scripts/
                original-to-csv.R
                csv-to-csvy.R
    stevencarlislewalker/iidda-tools/
        R/
        Python/
RHPCS
    portal (probably not ckan)
        database
```

### Metadata Structure

* Good list -- https://www.dcc.ac.uk/guidance/standards/metadata/list?page=1
* Datacite looks pretty good
  * [schema](https://schema.datacite.org/meta/kernel-4.4/metadata.xsd)
  * [docs](https://schema.datacite.org/meta/kernel-4.4/doc/DataCite-MetadataKernel_v4.4.pdf)
  * JSON is a little nicer to look at, but XML is fine
  * NEON is a datacite 'member'
  * If you are a member you can issue doi's through [Fabrica](https://support.datacite.org/docs/doi-fabrica)
  * Looks like it is more for people who want the data that they collect to be citable
    * is digitizing/organizing the same as collecting?
    * might not matter because it is at least a standard metadata schema for data archiving
    * do we have all of the information required/recommended by this schema? -- what is the current state of metadata in iidda?
* DebateGraph seems to work with http://nanopub.org/wordpress/ ... but as I look more closely this is less about a metadata standard and more about citing small pieces of original research

### Pipeline

1. 

## Thoughts

Adopting standards -- doi? datacite.org?

What are the relationships amongst the tables?
Should we try to 'normalize' the database?
It would be good to have schemas of tables that share join keys or can be unioned.

GitHub IIDDA has the ‘original’ data — what is the original?  Are there more metadata besides what is in the header?
GitHub IIDDA tools has functions for producing the data in different formats, including database tables

Some data will need to be updated
Some data are digitized


## Examples

Work through the files one by one taking notes.  This code opens the third file, for example.
 `open "$(head -3 all_files.txt | tail -1)"`

### School Term Data

* Unique file -- `./schoolterm/ca/schoolcal_ca___1950-2002_yr_pr.xls`
* One sheet per province
* Each sheet has the same format but really needs to be parsed
* Don't know what all of the codes mean (e.g. ndg)

### Demography

* Old xls file -- `./demography/ca/bth_ca___1921-65_mn_pr.xls`
* Excel says I'm in 'Compatibility Mode'
* 