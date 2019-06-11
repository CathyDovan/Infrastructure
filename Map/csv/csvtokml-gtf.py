import csv
import simplekml

inputfile = csv.reader(open('gtf_data_longlat.csv','r', encoding='ISO-8859-1'))
kml=simplekml.Kml()

for row in inputfile:
    
    theName = ""+row[13]+""

    
    theCoords = [(row[14],row[15])]
    
    pnt = kml.newpoint(name=theName, coords=theCoords)
    
kml.save('../kml/gtfn5.kml')
