# Instructions  
&nbsp;

### **Data Select**
- **File Input** Allows for single file upload. To analyze additional datasets, refresh the app.
- **Select File** gives option to choose either the example dataset provided or personal file if uploaded
- To explore selected dataset, press **Parse Data**

&nbsp;

### **Marker Comparisons**
- Choose which markers to compare under **Markers** and click **Update** to filter results
- Produces Bar and Pie chart visualizations based on selected markers 
    - Bar charts will show proportion of selected cells and the double positive rates between them
    - Pie charts will only show proportion of selected cells
- For both Bar and Pie Charts, there is the choice to see selected cells as a proportion of the whole dataset or just the selected subset

&nbsp;

### **Intensity Comparisons**
- Choose which markers to plot under the **y-axis** and **x-axis** inputs and click **Update** to generate plot 
    - Only one marker per input
    - Can **not** choose the same marker for both inputs
- Resulting plot will show distribution and density of intensity values for select markers for individual cells
- The **Intensity Values** options allow you to highlight the following conditions  
    - **all**: All intensitie values
    - **positive**: Positive for at least one of the cells
    - **double positive**: Positive for both cells
    - **positive (no double positive)**: Positive for one cell but not the other 
    - **negative**: Cells classified as negative for either cell
- The **Log Transformation** options allow for intensity values to have log transformations or not

&nbsp;

### **Tissue Mapping**
- **Two Markers**
    - Choose one target marker for both **Marker 1** and **Marker 2** and click **Update**
    - Resulting plot maps location of cells positive for selected markers and those cells that are positive for both
- **One Marker**
    - Choose one targer marker for **Marker**
    - Resulting plot maps location of all cells positive for selected marker

    &nbsp;

### **Data Tables**
- For selected dataset filters and parses the dataset to produce the following downloadable .csv files:
    - **Classifications**: All classification columns 
    - **Intensities**: All Intensity columns
    - **Double Positives**: Generates both sums and percentages of double positive rates between markers
    - **Full Dataset**: Full searchable dataset 