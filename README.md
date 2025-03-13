# LSAT
## Estimating the scale relationship of structural components (slope, rugosity) of the coral reef benthos on LSAT metrics: Turf Length and Sediment Depth

# Data processing background: 
I went out in the field and performed a survey of the transect site of interest using photogrammetry techniques. The large area image (LAI) of the 25m transect was processed in Agisoft Metashape. The point cloud of the model was input into [Viscore](https://chei.ucsd.edu/viscore/),where at each plot site that LSAT measurements are conducted, rugosity values were extracted. This was done by the program drawing 20 transect lines in the 25cm x 25cm box, and along each transect line, selecting 100 points to collect x,y,z data. In order to test the sclae-relationships, this was repeated for a box of 25cm x 25cm, to represent the original scale of the data, 50cm x 50cm, and 100cm x 100cm. Information was output into csv's that were input in R to calculate rugosity using the 0-1 scale and also the ratio scale where a value of 1 = completely flat, and increasing as a ratio (value of 2 = surface is 2x more rugose than equivalent linear distance).Next, the DEM of the model was extracted from Metashape and input into GIS, and the location of each of the boxes used to extract structural data (25cm, 50cm, 100cm) was drawn in as best as possible to match the location on Viscose. The slope at each pixel in the DEM was calucated and thus a new raster layer was created. This slope layer is what was used to create average slop in degrees for each of the boxes. 

[Image of the Large Area Image with boxes on top representing each of the different areas that structural data was extracted from.](lai.png)


# Files: 
The data from GIS was outputted as CSVs ready to be analyzed. The data from Viscore needed to be ran through R to calculate average rugosity values. Start with that script (Rugosity_code.R) first, to obtain rugosity calculations. Then, proceed to combining all the data in the data_visualization.R script and running the models. 

# To-Do:
I still need to clean this repository up. Currently, the data sheets from Viscore are missing which I need to add, and Delete old files. So the names on these scripts of spreadsheets won't work, because they aren't available. 