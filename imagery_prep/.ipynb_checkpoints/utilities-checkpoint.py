# figure out what of this we actually need here
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
from rasterio.merge import merge
from rasterio.warp import calculate_default_transform, reproject, Resampling

data_collection = "SENTINEL-2"
nodatavalue = -9999

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

def download_raw_zip_files(username, password, years, months, tile, tif_folder, skip=list()):
    print('     downloading raw zip files...')
    for y in years:
        for m in months:
            print(f'{y} {m} {tile}')

            if m+str(y) in skip: continue
            
            # get list of possible dates to download
            if m == 'jul':
                mm = 7
            elif m == 'aug':
                mm = 8
            elif m == 'sep':
                mm = 9
            start_date = datetime(y, mm, 1).isoformat()
            end_date = datetime(y, mm+1, 1).isoformat()

            # Generate a token. Tokens stay active for 10 minutes, can be refreshed within 60 minutes. Each token activated counts as a session. There is a max of 4 concurrent sessions before I get locked out.
            access_token = get_access_token(username, password)
            headers = {"Authorization": f"Bearer {access_token}"}
            session = requests.Session()
            session.headers.update(headers)
            
            try:
                r = requests.get(f"https://catalogue.dataspace.copernicus.eu/odata/v1/Products?$filter=contains(Name, 'MSIL2A') and contains(Name, '{tile}') and ContentDate/Start gt {start_date}.000Z and ContentDate/Start lt {end_date}.000Z").json()
                ids = pd.DataFrame.from_dict(r["value"])['Id']
                print(len(ids)) # n of available tiles for that month
                for id in ids:
                    print(id)
                    fp = os.path.join(tif_folder, tile, str(y), m, f'{id}.zip')
                    if not os.path.exists(fp): # if not already done
                        url = f"https://zipper.dataspace.copernicus.eu/odata/v1/Products({id})/$value"
                        response = session.get(url, headers=headers, stream=True, timeout=60)
                        if response.status_code!=200: # if it doesn't work, generate a new token (assuming reason failed is expired token)
                            print(response.status_code)
                            print('****generating new token*****')
                            try: # if it's just that the token expired, get a new token
                                access_token = get_access_token(username, password)
                                headers = {"Authorization": f"Bearer {access_token}"}
                                session = requests.Session()
                                session.headers.update(headers)
                                response = session.get(url, headers=headers, stream=True, timeout=60)
                            except: # if it still doesn't work, wait 30 minutes and then continue
                                print('****30 minute break starting now****')
                                print(time.strftime("%H:%M:%S", time.localtime()))
                                time.sleep(1800)
                                access_token = get_access_token(username, password)
                                headers = {"Authorization": f"Bearer {access_token}"}
                                session = requests.Session()
                                session.headers.update(headers)
                                response = session.get(url, headers=headers, stream=True, timeout=60)
                        with open(fp, "wb") as file:
                            for chunk in response.iter_content(chunk_size=8192):
                                if chunk:
                                    file.write(chunk)
    
                        response.close()
                        time.sleep(20)

            except: pass
    print('     all raw zip files downloaded')
    

def unzip_tiles(tif_folder, tile):
    # print('     unzipping all raw zip files...')
    zips = glob.glob(tif_folder + f'/{tile}' + '/*' + '/*' + '/*.zip')
    bad = []
    for z in zips:
        # print(z)
        folder = z.rpartition('/')[0]
        try:
            shutil.unpack_archive(z, folder, 'zip')
        except Exception as error:
            # print(f'     unable to unzip - {z}')
            # print(f'     {error} \n')
            bad.append(z)
    # print('FAILED TO UNZIP', bad, '\n')
    # print('     all folders unzipped', '\n')
    os.sync()
    return bad

def generate_image_scores(unzipped_folder_path):
    folder = unzipped_folder_path.rpartition('/')[0]
    name = unzipped_folder_path.rpartition('/')[2].replace('.SAFE', '_score.tif')
    fp = os.path.join(folder, name)
    
    if not os.path.exists(fp): # if not already done
        files = glob.glob(unzipped_folder_path + '/*/*/*/*/*.jp2', recursive=True)

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
        if '12' in folder:
            profile.update(driver='GTiff', dtype='float64', nodata=nodatavalue, crs=CRS.from_epsg(32612))
        if '13' in folder:
            profile.update(driver='GTiff', dtype='float64', nodata=nodatavalue, crs=CRS.from_epsg(32613))
    
        with rasterio.open(fp, 'w', **profile) as dst:
            dst.write(score, 1)
    
        del b4, b8, scl, ndvi

        os.sync()
        
    else: pass
    return folder

def generate_argmax(folder_path):
    t = folder_path.split('/')[9]
    fp = os.path.join(folder_path, f'{t}_argmax.tif')
    if not os.path.exists(fp): # remove this if I want to overwrite, but good for now
        files = sorted(glob.glob(folder_path + f'/*{t}*_score.tif'))
        arrs = []
        for f in files:
            try:
                with rasterio.open(f) as r:
                    arrs.append(r.read(1))
            except: pass
        arrs = np.stack(arrs, axis=0)
        arrs[np.isnan(arrs)==True] = nodatavalue
        maxTile = np.argmax(arrs, axis=0).astype(np.float64)
        try:
            with rasterio.open(files[0]) as src:
                profile = src.profile
        except:
            with rasterio.open(files[1]) as src:
                profile = src.profile
        if '12' in t:
            profile.update(crs=CRS.from_epsg(32612))
        if '13' in t:
            profile.update(crs=CRS.from_epsg(32613))
        with rasterio.open(fp, 'w', **profile) as dst:
            dst.write(maxTile, 1)
        os.sync()
        return fp
    else: pass

bands = ['B02_10m',
         'B03_10m',
         'B04_10m',
         'B05_20m',
         'B06_20m',
         'B07_20m',
         'B08_10m',
         'B8A_20m',
         'B11_20m',
         'B12_20m',
         'SCL_20m']

def mosaic_tiles(folder_path, bands=bands):
    t = folder_path.split('/')[9]
    fp = os.path.join(folder_path, f'{t}_argmax.tif')
    try:
        with rasterio.open(fp) as src:
            argmax  = src.read().astype('int64')
        for band in bands:
            fp_out = os.path.join(folder_path, f'{t}_{band}_mosaic.tif')
            if not os.path.exists(fp_out): # remove this if I want to overwrite, but good for now
                fps_b = sorted(glob.glob(folder_path + f'/*{t}*' + '/GRANULE' + '/*' + '/IMG_DATA' + '/*' + f'/*{band}.jp2'))
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
                if '12' in t:
                    profile.update(driver='GTiff', dtype='uint16', nodata=None, crs=CRS.from_epsg(32612))
                if '13' in t:
                    profile.update(driver='GTiff', dtype='uint16', nodata=None, crs=CRS.from_epsg(32613))
                with rasterio.open(fp_out, 'w', **profile) as dst:
                    dst.write(b_mosaic)  
            else: pass
        os.sync()
    except:
        return fp
    

def stack_bands_final_mosaic(folder_path):
    t = folder_path.split('/')[9]
    y = folder_path.split('/')[10]
    m = folder_path.split('/')[11]
    fp_out = os.path.join(folder_path, f'{t}_{str(y)}_{m}_mosaic.tif')
    if not os.path.exists(fp_out): # remove this if I want to overwrite, but good for now
        arr = []
        fps = [os.path.join(folder_path, x) for x in os.listdir(folder_path) if x.startswith(t) and x.endswith('m_mosaic.tif') and 'SCL' not in x]
        fps = sorted([os.path.join(folder_path, x) for x in os.listdir(folder_path) if x.startswith(t) and x.endswith('m_mosaic.tif')])
        try:
            fps = fps[0:7] + [fps[9]] + fps[7:9]
            for f in fps:
                with rasterio.open(f) as r:
                    arr.append(r.read(1))
            arr = np.stack(arr, axis=0)
            if arr.shape[0]==10:
                with rasterio.open(fps[0]) as src:
                    profile = src.profile
                if '12' in t:
                    profile.update(driver='GTiff', dtype='uint16', nodata=None, crs=CRS.from_epsg(32612), count=10)
                if '13' in t:
                    profile.update(driver='GTiff', dtype='uint16', nodata=None, crs=CRS.from_epsg(32613), count=10)
                with rasterio.open(fp_out, 'w', **profile) as dst:
                    dst.write(arr)
                os.sync()
        except: pass
    else: pass

def reproject_raster(src, output_raster, dst_crs):
    # with rasterio.open(input_raster) as src:
    transform, width, height = calculate_default_transform(
        src.crs, dst_crs, src.width, src.height, *src.bounds
    )

    out_meta = src.meta.copy()
    out_meta.update({
        "crs": dst_crs,
        "transform": transform,
        "width": width,
        "height": height
    })

    with rasterio.open(output_raster, "w", **out_meta) as dst:
        for i in range(1, src.count + 1):  # Reproject each band
            reproject(
                source=rasterio.band(src, i),
                destination=rasterio.band(dst, i),
                src_transform=src.transform,
                src_crs=src.crs,
                dst_transform=transform,
                dst_crs=dst_crs,
                resampling=Resampling.nearest
                )


def calculate_ndvi_ndmi(fp):
    try:
        with rasterio.open(fp) as src:
            profile=src.profile
            arr = src.read()
        arr = arr/10000
        arr[arr>1] = 1
        b4 = arr[2,...]
        b8 = arr[6,...]
        b11 = arr[8,...]
        ndvi = (b8-b4)/(b8+b4)
        ndmi = (b8-b11)/(b8+b11)
        profile.update(dtype='float64', count=1)
        fp_ndvi = fp.replace('mosaic', 'NDVI')
        fp_ndmi = fp.replace('mosaic', 'NDMI')
        with rasterio.open(fp_ndvi, 'w', **profile) as dst:
            dst.write(ndvi, 1)
        with rasterio.open(fp_ndmi, 'w', **profile) as dst:
            dst.write(ndmi, 1)
        os.sync()
    except:
        return fp














    

