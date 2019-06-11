import csv
import simplekml

inputfile = csv.reader(open('gtf_data_provincelonglat.csv','r', encoding='ISO-8859-1'))
kml=simplekml.Kml()

for row in inputfile:
    
    theName = ""+row[0]+""

    
    theCoords = [(row[4],row[3])]
    
    pnt = kml.newpoint(name=theName, coords=theCoords)
    
kml.save('../kml/prov.kml')
