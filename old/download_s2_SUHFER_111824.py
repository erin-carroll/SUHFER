import os
import datetime
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import json
import requests
import getpass
import geopandas as gpd
from datetime import datetime, timedelta
from zipfile import ZipFile
import shutil
import gc
import glob
import time
import rasterio
from rasterio import features
from rasterio.plot import show
from rasterio.merge import merge
from rasterio.windows import Window
from rasterio.mask import mask
from rasterio.transform import Affine

# define filepaths, generate output folders
home = '/global/scratch/users/erin_carroll/SUFHER/'
s2 = os.path.join(home, 'tifs_extension')

years = range(2017, 2025)
months = ['jul', 'aug', 'sep']

for y in years:
    for m in months:
        os.makedirs(os.path.join(s2, str(y), m), exist_ok=True)


# define AOI (to iterate through features)

aois = []
aoi = gpd.read_file(os.path.join(home, 'shp', 'all_new_AOI')) # 4326
for i in range(0, len(aoi)):
    s = str(aoi.geometry[0]).replace(" ", "", 1) + "'"
    aois.append(s)

# get access token
def get_access_token(username: str, password: str) -> str:
    data = {
        "client_id": "cdse-public",
        "username": username,
        "password": password,
        "grant_type": "password",
        }
    try:
        r = requests.post("https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token",
        data=data,
        )
        r.raise_for_status()
    except Exception as e:
        raise Exception(
            f"Access token creation failed. Reponse from the server was: {r.json()}"
            )
    return r.json()["access_token"]

data_collection = "SENTINEL-2"

# only download zip files to start

for y in years:
    for m in months:
        for a in range(0, 1):
            print('------------------------------')
            print(f'MONTH YEAR - {y} {m} - AOI {a}')
            print('------------------------------')
            
            print('downloading data...')
            
            if m == 'jul':
                mm = 7
            elif m == 'aug':
                mm = 8
            elif m == 'sep':
                mm = 9
                    
            start_date = datetime(y, mm, 1)
            end_date = datetime(y, mm+1, 1)
    
            # Initialize an empty list
            date_list = []
    
            # Loop through the range of dates and append to the list
            while start_date <= end_date:
                date_list.append(start_date.isoformat())
                start_date += timedelta(days=1)
            print(date_list)
    
            # Print the list of dates and the number of tiles that meet criteria, download the data to drive
            for d in range(0,len(date_list)-1):
                start_date=date_list[d]
                end_date=date_list[d+1]
    
                try:
                    r = requests.get(f"https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=Collection/Name eq '{data_collection}' and OData.CSC.Intersects(area=geography'SRID=4326;{aois[a]}) and ContentDate/Start gt {start_date}.000Z and ContentDate/Start lt {end_date}.000Z").json()
                    tmp = pd.DataFrame.from_dict(r["value"])
                    tmp = tmp[tmp['Name'].str.contains('L2A')]
                    ids = tmp['Id']
                    # filter list of ids to only those which haven't been done yet
                    done = [x.strip('.zip') for x in os.listdir(os.path.join(s2, str(y), m)) if x.endswith('.zip')]
                    ids = [x for x in ids if x not in done]
                    
                    print('\n', start_date)
                    print(tmp.shape[0])
    
                    for id in ids:
                        access_token = get_access_token("erin_carroll@berkeley.edu", "CQW_kc_2K-Pq!4u")
                        headers = {"Authorization": f"Bearer {access_token}"}
    
                        print(id)
                        url = f"https://zipper.dataspace.copernicus.eu/odata/v1/Products({id})/$value"
                        session = requests.Session()
                        session.headers.update(headers)
                        response = session.get(url, headers=headers, stream=True, timeout=60)
    
                        fp = os.path.join(s2, str(y), m, f'{id}.zip')
                        with open(fp, "wb") as file:
                            for chunk in response.iter_content(chunk_size=8192):
                                if chunk:
                                    file.write(chunk)
    
                        response.close()
                        time.sleep(30)
                        gc.collect()
    
    
                except: pass
    
            print('     data download round 1 complete')
