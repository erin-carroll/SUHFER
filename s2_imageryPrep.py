import os
import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import json
import requests
import getpass
from datetime import datetime, timedelta
from zipfile import ZipFile
import shutil
import gc
import glob
import time
import rasterio
from rasterio.crs import CRS

# define filepaths, generate output folders
aoi_name = 'rmbl_'

home = '/global/scratch/users/erin_carroll/'
tif_folder = home+'/data/tifs/'+aoi_name
years=range(2017,2025)
months=['jul','aug','sep']

for y in years:
    for m in months:
        os.makedirs(os.path.join(tif_folder, str(y), m), exist_ok=True)

nodatavalue = -9999

# identify s2 tiles to download
aoi = gpd.read_file(home+'/data/spatial_data/min_phase_cover_boundary')
s2_tiles = gpd.read_file(home+'/data/spatial_data/S2A_OPER_GIP_TILPAR_MPC__20151209T095117_V20150622T000000_21000101T000000_B00').to_crs(4326)
aoi = aoi.to_crs(s2_tiles.crs)
s2_tiles = s2_tiles[s2_tiles.geometry.intersects(aoi.unary_union)]
# fig, ax = plt.subplots()
# s2_tiles.boundary.plot(ax=ax)
# aoi.plot(ax=ax)
# plt.show()

# limit to only the tiles that contains the whole aoi
s2_tiles = s2_tiles[s2_tiles.geometry.contains(aoi.unary_union)]
tiles = s2_tiles.Name
print('s2 tiles', tiles)
# fig, ax = plt.subplots()
# s2_tiles.boundary.plot(ax=ax)
# aoi.plot(ax=ax)
# plt.show()

# download zip files

print('downloading zip files...')

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

for y in years:
    for m in months:
        for t in tiles:
            print(f'{y} {m} {t}')
            
            # get list of possible dates to download
            if m == 'jul':
                mm = 7
            elif m == 'aug':
                mm = 8
            elif m == 'sep':
                mm = 9
            start_date = datetime(y, mm, 1).isoformat()
            end_date = datetime(y, mm+1, 1).isoformat()
    
            try:
                r = requests.get(f"https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=contains(Name, 'MSIL2A') and contains(Name, '{t}') and ContentDate/Start gt {start_date}.000Z and ContentDate/Start lt {end_date}.000Z").json()
                ids = pd.DataFrame.from_dict(r["value"])['Id']
                print(len(ids)) # n of available tiles for that month
                for id in ids:
                    print(id)
                    fp = os.path.join(tif_folder, str(y), m, f'{id}.zip')
                    if not os.path.exists(fp): # if not already done
                        access_token = get_access_token("erin_carroll@berkeley.edu", "CQW_kc_2K-Pq!4u")
                        headers = {"Authorization": f"Bearer {access_token}"}
                        url = f"https://zipper.dataspace.copernicus.eu/odata/v1/Products({id})/$value"
                        session = requests.Session()
                        session.headers.update(headers)
                        response = session.get(url, headers=headers, stream=True, timeout=60)
                        with open(fp, "wb") as file:
                            for chunk in response.iter_content(chunk_size=8192):
                                if chunk:
                                    file.write(chunk)
    
                        response.close()
                        # time.sleep(30)
                        # gc.collect()

            except: pass

print('     all zip folders downloaded', '\n')

# unzip all downloaded zip files

print('unzipping downloaded folders...')

zips = glob.glob(tif_folder + '/*' + '/*' + '/*.zip')
bad = []
for z in zips:
    print(z)
    folder = z.rpartition('/')[0]
    try:
        shutil.unpack_archive(z, folder, 'zip')
    except Exception as error:
        print(f'     unable to unzip - {z}')
        print(f'     {error} \n')
        bad.append(z)
print('FAILED TO UNZIP', bad, '\n')
print('     all folders unzipped', '\n')

# generate score tif per tile

print('generating tile scores...')

tiles_ = sorted(glob.glob(tif_folder + '/*' + '/*' + '/*.SAFE'))
print('     n day tiles', len(tiles_))
ct = 0
for t in tiles_:
    print(ct, t)
    folder = t.rpartition('/')[0]
    name = t.rpartition('/')[2].replace('.SAFE', '_score.tif')
    fp = os.path.join(folder, name)
    ct = ct+1

    if not os.path.exists(fp): # if not already done
        files = glob.glob(t + '/*/*/*/*/*.jp2', recursive=True)

        fp_b4 = [x for x in files if x.endswith('B04_10m.jp2')][0]
        fp_b8 = [x for x in files if x.endswith('B08_10m.jp2')][0]
        fp_scl = [x for x in files if x.endswith('SCL_20m.jp2')][0]

        b4 = rasterio.open(fp_b4).read(1).astype('float')
        b8 = rasterio.open(fp_b8).read(1).astype('float')
        scl = rasterio.open(fp_scl).read(1).astype('float')
        scl = np.repeat(scl, 2, axis=0).repeat(2, axis=1) # resample from 20m to 10m

        ndvi = np.divide(np.subtract(b8, b4), np.add(b8, b4))

        scl[np.isin(scl, [0])==True] = nodatavalue
        scl[np.isin(scl, [1,2,3,7,8,9,10,11])==True] = 0
        scl[np.isin(scl, [4,5,6])==True] = 1
        score = np.add(scl, ndvi)

        with rasterio.open(fp_b4, 'r') as src:
            profile = src.profile
        profile.update(driver='GTiff', dtype='float64', nodata=nodatavalue, crs=CRS.from_epsg(32613))

        with rasterio.open(fp, 'w', **profile) as dst:
            dst.write(score, 1)

        del b4, b8, scl, ndvi
        gc.collect()
    else: pass

print('     all tile scores generated', '\n')

# generate argmax tif for each month/year/tile

print('generating argmax from scores...')

for y in years:
    for m in months:
        folder = os.path.join(tif_folder, str(y), m)
        print(y, m)
        for t in tiles:
            fp = os.path.join(folder, f'{t}_argmax.tif')
            files = sorted(glob.glob(folder + f'/*{t}*_score.tif'))
            arrs = []
            for f in files:
                with rasterio.open(f) as r:
                    arrs.append(r.read(1))
            arrs = np.stack(arrs, axis=0)
            arrs[np.isnan(arrs)==True] = nodatavalue
            print('stacked shape', arrs.shape)
            maxTile = np.argmax(arrs, axis=0).astype(np.float64)
            print('maxTile shape', maxTile.shape)
            with rasterio.open(files[0]) as src:
                profile = src.profile
            profile.update(crs=CRS.from_epsg(32613))
            with rasterio.open(fp, 'w', **profile) as dst:
                dst.write(maxTile, 1)

print('     all argmax generated', '\n')


# mosaic each tile individually

print('mosaicking individual bands...')

bands = ['B02_10m',
         'B03_10m',
         'B04_10m',
         'B05_20m',
         'B06_20m',
         'B07_20m',
         'B08_10m',
         'B8A_20m',
         'B11_20m',
         'B12_20m']

for y in years:
    for m in months:
        folder = os.path.join(tif_folder, str(y), m)
        print(y, m)
        for t in tiles:
            print(t)
            fp = os.path.join(folder, f'{t}_argmax.tif')
            with rasterio.open(fp) as src:
                argmax  = src.read().astype('int64')
            for band in bands:
                print(band)
                fp_out = os.path.join(folder, f'{t}_{band}_mosaic.tif')
                fps_b = sorted(glob.glob(folder + f'/*{t}*' + '/GRANULE' + '/*' + '/IMG_DATA' + '/*' + f'/*{band}.jp2'))
                b = []
                for f in fps_b:
                    with rasterio.open(f) as r:
                        b.append(r.read(1))
                b = np.stack(b, axis=0)
                ## special case for _20m bands
                if '20m' in band:
                    b = np.repeat(b, 2, axis=1).repeat(2, axis=2)
                b_mosaic = b[argmax, np.arange(b.shape[1])[:, None], np.arange(b.shape[2])].astype('uint16')
                # export band mosaic
                with rasterio.open(fp) as src:
                    profile = src.profile
                profile.update(driver='GTiff', dtype='uint16', nodata=None, crs=CRS.from_epsg(32613))
                with rasterio.open(fp_out, 'w', **profile) as dst:
                    dst.write(b_mosaic)

print('     all individual bands mosaicked', '\n')

# stack bands into final mosaic

print('stacking bands into final mosaic...')

for y in years:
    for m in months:
        print(y, m)
        folder = os.path.join(tif_folder, str(y), m)
        for t in tiles:
            arr = []
            fps = [os.path.join(folder, x) for x in os.listdir(folder) if x.startswith(t) and x.endswith('m_mosaic.tif')]
            fps = sorted([os.path.join(folder, x) for x in os.listdir(folder) if x.startswith(t) and x.endswith('m_mosaic.tif')])
            fps = fps[0:7] + [fps[9]] + fps[7:9]
            print(t)
            for f in fps:
                with rasterio.open(f) as r:
                    arr.append(r.read(1))
            arr = np.stack(arr, axis=0)
            print(arr.shape)
            with rasterio.open(fps[0]) as src:
                profile = src.profile
                profile.update(count=10, crs=CRS.from_epsg(32613))
            fp_out = os.path.join(folder, f'{t}_{str(y)}_{m}_mosaic.tif')
            with rasterio.open(fp_out, 'w', **profile) as dst:
                dst.write(arr)

print('     all bands stacked in final mosaic', '\n')

