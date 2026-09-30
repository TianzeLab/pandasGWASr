# pandasGWASr

**pandasGWASr** is an R package for programmatic access to the [GWAS Catalog](https://www.ebi.ac.uk/gwas) RESTful API V2. It provides a unified R interface for retrieving GWAS Catalog data, including associations, studies, traits, variants, publications, genes, ancestries, loci, genomic contexts and summary statistics. It is the R port of the Python package [pandasGWAS2](https://github.com/TianzeLab/pandasGWAS2).

## Installation

```r
# install.packages("remotes")
remotes::install_github("TianzeLab/pandasGWASr")
```

## Example

Get associations for a given study accession ID:

```r
library(pandasGWASr)

associations <- get_associations(accession_id = "GCST000854")
```

Fetch a single association by its association ID:

```r
association <- get_associations(association_id = "169106883")
association@associations
```

Associations can be queried with any combination of the following filters:

- `pubmed_id` — PubMed ID of the publication
- `rs_id` — SNP rsID (may contain several rs numbers for a haplotype)
- `accession_id` — the study's GWAS Catalog accession ID
- `efo_trait` / `efo_id` — EFO trait label or shortform identifier
- `mapped_gene` — gene overlapping the variant
- `full_pvalue_set`, `show_child_trait`, `extended_geneset` — boolean options

```r
result <- get_associations(
  efo_trait = "celiac disease",
  show_child_trait = TRUE
)
```

### Fat and Thin modes

`mode = "Fat"` (default) builds the main table together with nested tables (`efo_traits`, `snp_allele`, `locations`, `mapped_genes`, ...), while `mode = "Thin"` keeps only the raw records:

```r
thin <- get_associations(association_id = "169106883", mode = "Thin")
```

### Indexing

Association objects support position and identifier indexing (R is 1-based):

```r
association[1]
association["169106883"]
associations[1:2]
```

### Set operations

Aggregate queried results using mathematical symbols. In addition to the plus sign (`+`), the package also supports `-`, `&`, `|` and `^` to perform corresponding set operations on Association objects:

```r
a <- get_associations(efo_trait = "celiac disease")
b <- get_associations(mapped_gene = "CCR6")

a + b     # concatenation
a - b     # difference
a & b     # intersection
a | b     # union
a ^ b     # symmetric difference
a == b    # equality
```

Low-level single-object requests by full URL are also available: `get_association()`, `get_study()`, `get_trait()`, `get_snp()`, `get_ancestry()`, `get_publication()`, `get_genomicContext()`, `get_gene()` and `get_locus()`.

```r
trait <- get_trait(
  "https://www.ebi.ac.uk/gwas/rest/api/v2/efo-traits/EFO_0009713"
)
```

## Licensing information

### Source code

This package is licensed under the GPL (>= 3).

### Data from NHGRI-EBI GWAS Catalog

The NHGRI-EBI GWAS Catalog and all its contents are available under the general [Terms of Use for EMBL-EBI Services](https://www.ebi.ac.uk/about/terms-of-use/). Summary statistics are made available under [CC0](https://creativecommons.org/publicdomain/zero/1.0/) unless [otherwise stated](https://www.ebi.ac.uk/gwas/docs/faq/#faq-H7).

## Similar projects

- Python package [pandasGWAS2](https://github.com/TianzeLab/pandasGWAS2)
- R package [gwasrapidd](https://github.com/ramiromagno/gwasrapidd) by Ramiro Magno

## Cite this work

Cao, T., Li, A. & Huang, Y. pandasGWAS: a Python package for easy retrieval of GWAS catalog data. BMC Genomics 24, 238 (2023). https://doi.org/10.1186/s12864-023-09340-2
