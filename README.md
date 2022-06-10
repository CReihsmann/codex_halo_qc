# **HORNS** : 
**H**ALO **O**bject **R**eport and a**N**alysis tool**S**

### **Background Info**
One of the most reliable and important tools in biological research and medical diagnostics is the technique of fluorescent immunohistochemistry (**IHC**). In simplistic terms, this is the technique of marking specific cells with immunofluorescent antibodies for imaging via fluorescence microscopy. Traditionally, this technique is limited to the use of only 3 fluorescent markers at a time requiring the use of successive tissue slices to possibly understand tissue composition. In an exciting development over the past few years, the development and availability of multiplex imaging technology has taken off. These techniques allow for the staining for dozens of markers per tissue sample from the traditional 3. However, the technology and methods to analyze these images has not quite kept up leading to a struggle within research groups to process and extract relevant data.

One of the popular programs to analyze this type of data is the **HALO** image analysis program. This program allows for cell classification and basic spatial analysis. 

Unfortunately, while this program simplifies many of those tasks, due to variability across different tissue samples the intensity cutoff values used to classify individual cells have to be set manually. With variability due to variable tissue samples and human bias, quality control of the output data is crucial. As such, I saw an opportunity to build a tool to help streamline this process and provide visualizations and stats to scrutinize results. 

### **Motivation**
Currently, producing these stats and visualizations relies on either several different programs or some coding experience. With this app, I aim to make initial and basic EDA more accessible to those with fewer resources or coding experience.

### **Data Question**
1. What are the proportions and overlap of certain markers from a processed image and are they representative of the actual tissue
2. Can I develop an app that produces detailed visualizations and statistics from object analysis data output from the **HALO** platform that helps answers these questions and make it flexible enough to handle varied marker inputs and allow users to specify and compare individual markers within a dataset

### **Challenges**
- Large file size
- Creating updatable input selections for specific files
- Graphing interactable plots containing large amounts of data and maintaining ease of use

### **Data Sources**
*Powers/Brissova Research Group, Vanderbilt University Medical Center*

#### **App Features**
- **Data**: Import a HALO generated csv of object data
- **Marker Comparisons**: Compare multiple markers comparing tissue composition and double positive rates
- **Intensity Comparisons**: Graph intensity values between two tissues to investigate where cells are classified positive, double positive, or negative based on intensity cutoffs
- **Tissue Mapping**: Visualize spatial relationships of markers with cell mapping for both individual markers and comparisons between two separate markers
- **Datasets**: Downloadable datatables of parsed data that include:
    - Classification columns 
    - Intensity columns
    - Double positive amounts, both total and percentage
    - Filterable full dataset

[Linkedin page][li]

[li]: <https://www.linkedin.com/in/conrad-reihsmann/>
