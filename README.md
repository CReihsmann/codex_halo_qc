# **HORNS** : 
**H**ALO **O**bject **R**eport and a**N**alysis tool**S**

Link to [**HORNS**][horns]

### **Background Info**
One of the most reliable and important tools in biological research and medical diagnostics is the technique of fluorescenc-based immunohistochemistry (**IHC**). In simplistic terms, this is the technique of marking specific cells with antibodies (cell-specific proteins) and visualizing using fluorophores detected by a microscope. Traditionally, this technique is limited to the use of only 3 markers or channels at a time requiring the use of adjacent tissue slices to infer tissue composition. With advancement of exciting new imaging techniques over the past decade, the development and availability of [multiplexed imaging technology][multi] has taken off. These techniques allow for the staining of dozens of markers per tissue sample and result in high-dimensional data with precise spatial coordinates. However, tools and workflows to analyze these images lag considerably behind advances in image acquisition leading to a struggle within research groups to process and extract relevant data.

One program to analyze multiplexed imaging data type of data is the [**HALO®**][indica] suite. This program includes modules for cell classification and basic spatial analysis, but due to variability across different tissue samples, thresholding (setting the intensity cutoff values used to classify individual cells) must be done manually. To accurately discern biological variability and mitigate human experimenter bias, quality control of the output data is crucial. As such, I saw an opportunity to build a tool that would streamline this process and provide visualizations and simple statistics to help interpret outputs during iterative data processing 

### **Motivation**
Currently, producing decriptive statistics and visualizations relies on transferring data to different programs or requires the user to have some coding experience. With this app, I aim to make initial and basic exploratory data analysis (EDA) more accessible to those with fewer resources or coding experience.

### **Data Question**
1. What are the relative proportions and overlap of certain markers from a processed image and does the algorithm accurately classify positivity/negativity for different markers.
2. Can I develop an app that produces detailed visualizations and statistics from object analysis data output from the **HALO** platform that helps answers these questions and make it flexible enough to handle varied marker inputs and allow users to specify and compare individual markers within a dataset

### **Challenges**
- Large file sizes (100mb - 1000mb+)
- Creating flexible input selections for specific files
- Graphing interactable plots containing up to 500K+ while maintaining ease of use

### **Data Sources**
*[Powers/Brissova Research Group][p&b], Vanderbilt University Medical Center*

#### **App Features**
- **Data**: Import an object data .csv file -(e.g., output from HALO)
- **Marker Comparisons**: Investigate multiple markers by comparing relative proportions and double positive rates
- **Intensity Comparisons**: Graph intensity values between two markers to investigate where cells are classified positive, double positive, or negative based on intensity cutoffs
- **Tissue Mapping**: Visualize spatial distribution of markers by mapping cells positivie for an individual markers or by comparing distributions of two separate markers
- **Datasets**: Downloadable datatables of parsed data that include:
    - Classification columns 
    - Intensity columns
    - Frequency of cells double-positive for select sets of markers (both as a percentage of total cells or as a percentage of either single-positive population)
    - Filterable full dataset

[Author Linkedin page][li] | [Example multiplexed images][panc]

[li]: <https://www.linkedin.com/in/conrad-reihsmann/>
[horns]: <http://creihsmann.shinyapps.io/HORNS>
[multi]: <https://doi.org/10.1038/s41592-021-01316-y>
[indica]: <https://indicalab.com/halo>
[p&b]: <https://www.powersbrissovaresearch.org/>
[panc]: <https://pancreatlas.org/>