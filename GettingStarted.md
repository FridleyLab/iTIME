---
title: "Getting Started"
author: "Everyone"
date: "3/14/2021"
output: 
  html_document:
    mathjax: local
    self_contained: false
---



### Input Files

iTIME accepts 3 files as input: a summary level file, a clinical file, and a spatial file. All files should be csv files. The summary file should contain summary level statistics and should contain one row per sample, while the clinical data file should contain one row per patient. Both the clinical and summary files should contain a variable to link sample IDs and clinical IDs ('Merge Variable'), though the variables do not need to be named the same in both datasets. iTIME does not need to have a spatial data file if only univariate or multivariate summaries are of interest, however, for the spatial data tab clinical and summary data are needed.

### Summary Pages

#### Univariate Summary

The summary page can be used without uploading any spatial data. This page provides a summary of the marker chosen from the dropdown menu below and allows users to plot a marker against clinical variables and select the appropriate plot type. Plot types available are boxplot, violin plot, histogram, and scatter plot, all of which have the clinical variable along the x-axis. Additionally, a contingency table threshold can be set to compute greater than or less than by the clinical variable selected.

Currently, there is the ability to apply a **square root** transformation for the summary plots.

#### Multivariate Summary

The data are displayed as a heatmap of the selected markers (rows). These data rows have the ability to be clustered through the toggle, where the columns (clinical variable) has the ability to be grouped together based on clinical variable annotation. The order of the samples (columns) and markers (rows) of the heatmap are determined by hierarchical clustering which assigns clusters based on ‘complete’ dissimilarity. Complete dissimilarity refers to the distance of two clusters being determined by the largest distance between elements in either cluster.  The dataset will be sorted by the selected clinical variable to allow for a heatmap where the samples with the same class of a clinical variable are grouped together or appear in increasing order for continuous variables (such as age). Markers to include in the plot are selected through toggling the check-box next to the marker name.

### Spatial Page

The spatial page plots the individual cells by positivity for the markers and plots Ripley's K estimates over a range of r values using default 100 simulated point patterns.

Ripley’s K, Besag’s L, and Marcon’s M quantify the of the degree of spatial clustering for a particular cell type.  These quantities assume that the point process of interest follows complete spatial randomness (CSR) that is that the location of the cells do not form clusters nor do they occur in a regular pattern. Each measure is computed by counting the number of neighboring cells for each cell, where two cells are said to be neighboring if they are within a specified distance, r as seen in the x-axis of ‘Plot of Spatial Clustering Estimator’, of each other. 

Border corrections are needed for these methods to account for the fact that these cell types are assumed occur outside of the TMA at the same rate. The edge corrections inflate the number of neighboring cells based on their proximity to the border and other cells. Two popular methods border corrections are isotropic and translational. Translation border correction translates the region of interest based on the distance between two points and then measures the area of the intersection of the original region and translated region. An isotropic border correction weights each pair of points based on how much of the circumference of a circle centered around one point and going through the other is outside of the region of interest.

$$
\widehat{K}\left(r\right) = (n(n-1))^{-1}A \sum^{n}_{i=1} \sum_{j\neq 1}w_{ij}|(x_j:d(x_i,x_j)\lt r)|,
$$

where $n$ is the number of cells, $A$ is the area of the TMA, $d(x_i,x_j)$ is the distance between the $ith$ and $jth$ cell, and $w_{ij}$ is weighting factor from the border correction. The expected value of $K$ under the csr is $\pi r^2$. Both Besag's L and Marcon's M are slight modifications to Ripley's K and both more easily interpretale than Rpley's K.



|   Metric   |                    Formula                     | Expected Value | Formula Displayed in Plot |
|:----------:|:----------------------------------------------:|:--------------:|:-------------------------:|
| Besag's L  | $L\left(r\right) = \sqrt{K\left(r\right)/\pi}$ |      $r$       |   $L\left(r\right) - r$   |
| Marcon's M | $M\left(r\right) = K\left(r\right)/(\pi r^2)$  |      $l$       |     $M\left(r\right)$     |

<br/>
Besag's L Interpretation: If value is larger than 0, than there is evidence of spatial clustering.

Marcon's M Interpretation: If value is larger than 1, than there is evidence of spatial clustering. Additionally, $M\\left(r\\right)$ is 1.50 at some chosen radius means you're observing 50% more clustering than Poisson distribution would predict.

### Summary File

![Summary File Example](figures/summary-file.png)

### Clinical File

![Clinical File Example](figures/clinical-file.png)

### Spatial File

![Spatial File Example](figures/spatial-file.png)

### Help

**If you have any questions or comments, we would love to hear them. You can email us at [Fridley.Lab@moffitt.org](mailto:Fridley.Lab@moffitt.org) or feel free to [open an issue](https://github.com/FridleyLab/iTIME/issues) in our github page.**
