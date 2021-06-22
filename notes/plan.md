## Plan

### Objectives and Principles

* Lasting resource
* Easy to maintain
* Prioritize but plan for big future

### Roadmap and Priority Order

1. Organize original source data 
2. Make organized original sources available -- with formal data citation guidelines (what are these?)
3. Tools for extracting and transforming data into more convenient formats (e.g. long-form `csv` with metadata header like `csvy`)
4. Tools for plotting and analyzing data
5. Proper normalized database automatically populated from source data
6. API for pulling from the database or source files
7. Frontend website/portal with nice clickable interface and access control
8. Integration with other data portals

### Tentative Choices

* Separate original sources from derived data files and formal database tables (what counts as a 'source file'?)
* Separate data (both source and derived) from frontend web interface/portal
* Use GitHub as repository of source data
  * The data are not _that_ big and so GitHub can handle it (we can always open a second, third, ... repo if it gets too big)
  * People around here understand GitHub and how to use it
* Don't use CKAN
  * It is a bit of a beast -- hard to maintain
  * On the other hand it does have lots of great features

### Project Structure

#### High-Level

```
GitHub
    davidearn/iidda/
    stevencarlislewalker/iidda-tools/
RHPCS
    website/portal
    database
```

#### IIDDA High-Level

```
davidearn/iidda/
    README.md
    data/
        dataset-1/
        dataset-2/
        ...
        dataset-i/
        ...
    pipelines/
    data-templates/
```

#### IIDDA Details

```
davidearn/iidda/
    README.md
    data/
        dataset-1/
            metadata
            original-sources/
                scan-image.png
                digitized-sheets.xlsx
            conversion-scripts/
                original-to-csv.R
                csv-to-csvy.R
        dataset-2/
            metadata
            original-sources/
                idda-sheet.xls
            conversion-scripts/
                original-to-csv.R
                csv-to-csvy.R
        dataset-i/
        ...
    pipelines/
        rhpcs-database/
            make-files ...
            shell-scripts ...
            etc ...
        covid-ontario-to-r/
        phac-vaccination-programme/
        etc ...
    data-templates/
        digitized-dataset/
            metadata
            original-sources/
                read-and-delete-me
            scripts/
                read-and-delete-me
                makefile
        demography-dataset/
            metadata
            original-sources/
                read-and-delete-me
            scripts/
                read-and-delete-me
                makefile
```

### Full Details

```
GitHub
    davidearn/iidda/
        README.md
        data/
            dataset-1/
                metadata
                original-sources/
                    scan-image.png
                    digitized-sheets.xlsx
                conversion-scripts/
                    original-to-csv.R
                    csv-to-csvy.R
            dataset-2/
                metadata
                original-sources/
                    idda-sheet.xls
                conversion-scripts/
                    original-to-csv.R
                    csv-to-csvy.R
            dataset-i/
            ...
        pipelines/
            rhpcs-database/
                make-files ...
                shell-scripts ...
                etc ...
            covid-ontario-to-r/
            phac-vaccination-programme/
            etc ...
        data-templates/
            digitized-dataset/
                metadata
                original-sources/
                    read-and-delete-me
                scripts/
                    read-and-delete-me
                    makefile
            demography-dataset/
                metadata
                original-sources/
                    read-and-delete-me
                scripts/
                    read-and-delete-me
                    makefile
    stevencarlislewalker/iidda-tools/
        README.md
        R/
        Python/
RHPCS
    website/portal
    database
```

### Pipeline Vision

1. Add/Edit/Delete dataset folder from `davidearn/iidda`, possibly copying and modifying code snipets from `stevencarlislewalker/iidda-tools`
   1. Could involve 
2. 

### Metadata Structure/Notes

* Basic information
  * Free-form Title and Description
  * Author?
  * Digitizer when applicable
  * Spatial/temporal coverage of the data
  * ...
* Information about how datasets are related
  * Used in pipeline steps for pushing to databases
  * Helps to ensure that database is properly normalized
    * Related tables share join keys
    * Tables with identical and related structure have the same schema so that they can be easily appended/unioned to each other
  * Process issue -- for this to work, people will need to be disciplined when they write the code associated with each dataset
* Do not want to reinvent the wheel and also want to work well with others
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

Work through some (many?) of the files one-by-one taking notes.
 `open "$(head -3 all_files.txt | tail -1)"`

### School Term Data

* Unique file -- `./schoolterm/ca/schoolcal_ca___1950-2002_yr_pr.xls`
* One sheet per province
* Each sheet has the same format but really needs to be parsed
* Don't know what all of the codes mean (e.g. ndg)

### Demography

* Old xls file -- `./demography/ca/bth_ca___1921-65_mn_pr.xls`
* Excel says I'm in 'Compatibility Mode'