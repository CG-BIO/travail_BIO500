---
title: Short Paper
author:
  - name: Nicolas Beaudoin
    email: bean1710@usherbrooke.ca
    affiliation: Universite_minuscule de Sherbrooke
    correspondingauthor: true
    footnote: 1
  - name: Camille Gagnon
    email: gagc3901@usherbrooke.ca
    affiliation: Universite_minuscule de Sherbrooke
  - name: Marilou Cournoyer
    email: coum3903@userhbrooke.ca
    affiliation: Universite_minuscule de Sherbrooke
    footnote: 2
  - name: Francis Boily 
    email: boif2411@usherbrooke.ca
    affiliation: Universite_minuscule de Sherbrooke
    footnote: 2
address:
  - code: Département des sciences
    address: Department, Street, City, State, Zip
  - code: Another University
    address: Department, Street, City, State, Zip
footnote:
  - code: 1
    text: "This is the first author footnote."
  - code: 2
    text: "Another author footnote."
abstract: |
  This is the abstract.Question d'analyse et de rechercheNous voulons regarder l'immigration et l'émigration ainsi que la dispersion au sein des différentes communautés de la classe. Analyse basée sur les spécialistes et généralistes OU introverties et extraverties ? On pourrait aussi comparer l'immigration comme étant un indicateur d'adaptation et de survie au niveau psychologique chez l'être humain. Exemple, l'être humain ne se reconnait pas ou vit un stress dans ses relations sociales (peu de compatibilité) alors il va migrer ailleurs.
 

  It consists of two paragraphs.
keywords: 
  - keyword1
  - keyword2
journal: "An awesome journal"
date: "2023-04-20"
classoption: preprint, 3p, authoryear
bibliography: mybibfile.bib
linenumbers: false
numbersections: true
# Use a CSL with `citation_package = "default"`
# csl: https://www.zotero.org/styles/elsevier-harvard
output: 
  rticles::elsevier_article:
    keep_tex: true
    citation_package: natbib
---

Please make sure that your manuscript follows the guidelines in the 
Guide for Authors of the relevant journal. It is not necessary to 
typeset your manuscript in exactly the same way as an article, 
unless you are submitting to a camera-ready copy (CRC) journal.

For detailed instructions regarding the elsevier article class, see   <https://www.elsevier.com/authors/policies-and-guidelines/latex-instructions>

# Bibliography styles

Here are two sample references: @Feynman1963118 [@Dirac1953888].

By default, natbib will be used with the `authoryear` style, set in `classoption` variable in YAML. 
You can sets extra options with `natbiboptions` variable in YAML header. Example 
```yaml
natbiboptions: longnamesfirst,angle,semicolon
```

There are various more specific bibliography styles available at
<https://support.stmdocs.in/wiki/index.php?title=Model-wise_bibliographic_style_files>. 
To use one of these, add it in the header using, for example, `biblio-style: model1-num-names`.

## Using CSL 

If `citation_package` is set to `default` in `elsevier_article()`, then pandoc is used for citations instead of `natbib`. In this case, the `csl` option is used to format the references. Alternative `csl` files are available from <https://www.zotero.org/styles?q=elsevier>. These can be downloaded
and stored locally, or the url can be used as in the example header.

# Equations

Here is an equation:
$$ 
  f_{X}(x) = \left(\frac{\alpha}{\beta}\right)
  \left(\frac{x}{\beta}\right)^{\alpha-1}
  e^{-\left(\frac{x}{\beta}\right)^{\alpha}}; 
  \alpha,\beta,x > 0 .
$$

Here is another:
\begin{align}
  a^2+b^2=c^2.
\end{align}

Inline equations: $\sum_{i = 2}^\infty\{\alpha_i^\beta\}$

# Figures and tables

Figure \ref{fig2} is generated using an R chunk.

\begin{figure}

{\centering \includegraphics[width=0.5\linewidth]{rapport_files/figure-latex/fig2-1} 

}

\caption{\label{fig2}A meaningless scatterplot.}\label{fig:fig2}
\end{figure}

# Tables coming from R

Tables can also be generated using R chunks, as shown in Table \ref{tab1} for example.


```r
knitr::kable(head(mtcars)[,1:4], 
    caption = "\\label{tab1}Caption centered above table"
)
```



Table: \label{tab1}Caption centered above table

|                  |  mpg| cyl| disp|  hp|
|:-----------------|----:|---:|----:|---:|
|Mazda RX4         | 21.0|   6|  160| 110|
|Mazda RX4 Wag     | 21.0|   6|  160| 110|
|Datsun 710        | 22.8|   4|  108|  93|
|Hornet 4 Drive    | 21.4|   6|  258| 110|
|Hornet Sportabout | 18.7|   8|  360| 175|
|Valiant           | 18.1|   6|  225| 105|

# References {-}

