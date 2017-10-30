# Uses the NYC API to geocode addresses
# sudo pip install nyc_geoclient (https://github.com/talos/nyc-geoclient)
# Create an app Id and app key in http://developer.cityofnewyork.us/api/geoclient-api

import csv
from nyc_geoclient import Geoclient

# Read csv with ID and key.
# csv must be in same folder and be like the following
# "appID","appKey"
# "309245e","c45458765e3h8560erg898160"

with open('app_id_nyc.csv', 'rb') as f:
    reader = csv.reader(f)
    id_key = map(tuple, reader)

my_app_ID = id_key[1][0]
my_app_key = id_key[1][1]

g = Geoclient(my_app_ID, my_app_key)

with open('../out/nyc_sales_clean.csv', 'rb') as f:
    reader = csv.reader(f)
    nyc_sales = map(tuple, reader)

nrow_nyc = len(nyc_sales)

with open("../out/coords_nyc_api.csv", "wb") as csv_file:
        writer = csv.writer(csv_file, delimiter=',')
        writer.writerow(['id_sale', 'lat', 'long', 'returned_street_name', 'returned_zip_code'])
        for i in range(1, len(nyc_sales)):
            print(str(i) + ' / ' + str(nrow_nyc))
            # address(houseNumber, street, borough)
            addr = g.address(nyc_sales[i][24], nyc_sales[i][23],  nyc_sales[i][25])
            out = [nyc_sales[i][22], addr.get('latitudeInternalLabel'), addr.get('longitudeInternalLabel'), addr.get('streetName1In'), addr.get('zipCode')]
            print(out)
            print('\n\n')
            writer.writerow(out)

