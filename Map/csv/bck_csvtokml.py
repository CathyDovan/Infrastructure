import csv
import simplekml

inputfile = csv.reader(open('sample_output2.csv','r', encoding='ISO-8859-1'))
kml=simplekml.Kml()

for row in inputfile:
    pnt = kml.newpoint(name=row[1], description="<![CDATA[Project Description:"+row[11]+" <br />Construction Start Date:"+row[7]+" <br />Construction End Date:"+row[8]+"]]>", coords=[(row[9],row[10])])
kml.save('../kml/gtf5.kml')

