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
from shapely.geometry import box
from itertools import product

import rasterio
from rasterio.crs import CRS
from rasterio.merge import merge
from rasterio.warp import calculate_default_transform, reproject, Resampling
from rasterio.features import shapes
from rasterio.plot import show
import rasterio.mask
from rasterio.mask import mask
from rasterio.enums import Resampling as ResamplingEnum
from rasterio.transform import Affine, rowcol, xy
from rasterio.windows import from_bounds

import tensorflow as tf
import keras
import keras.models
from bfgn.architectures import unet
from bfgn.experiments import losses





#########################################
## IMAGERY PREP
#########################################

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

def clip_raster_to_gdf_bounds(raster_array, transform, gdf):
    # Get bounds of the GeoDataFrame
    minx, miny, maxx, maxy = gdf.total_bounds

    # Convert bounds to pixel coordinates (row/col)
    row_start, col_start = rowcol(transform, minx, maxy)  # upper-left
    row_stop, col_stop = rowcol(transform, maxx, miny)    # lower-right

    # Ensure indices are in proper order
    row_min, row_max = sorted((row_start, row_stop))
    col_min, col_max = sorted((col_start, col_stop))

    # Clip the array
    clipped_array = raster_array[row_min:row_max, col_min:col_max]

    # Update transform to reflect new upper-left corner
    new_x, new_y = xy(transform, row_min, col_min)
    new_transform = Affine.translation(new_x, new_y) * Affine.scale(transform.a, transform.e)

    return clipped_array, new_transform

def mosaic_gmug(year, month, index, tif_folder, target_crs, gdf):
    # get filepaths
    fps = sorted(glob.glob(tif_folder + f'/*/{year}/{month}/*_{index}.tif'))
    # define filepath out
    fp_out = tif_folder+f'/{year}_{month}_{index}.tif' 

    # get final merged output transform
    bounds_list = []
    resolutions = []
    for path in fps:
        with rasterio.open(path) as src:
            res = src.res
            resolutions.append(res)
            reprojected_bounds = rasterio.warp.transform_bounds(src.crs, target_crs, *src.bounds)
            bounds_list.append(reprojected_bounds)
    # Union of all AOI-intersecting raster bounds
    minxs, minys, maxxs, maxys = zip(*bounds_list)
    union_bounds = (min(minxs), min(minys), max(maxxs), max(maxys))
    # Use the resolution of the first raster if not specified
    xres, yres = resolutions[0]
    # Define output raster dimensions and transform
    out_transform = Affine.translation(union_bounds[0], union_bounds[3]) * Affine.scale(xres, -yres)
    out_width = int((union_bounds[2] - union_bounds[0]) / xres)
    out_height = int((union_bounds[3] - union_bounds[1]) / yres)

    # max composite all rasters into one
    stack = [None]*2 # empty list of length 2 to hold arrays to merge
    for i in range(len(fps)):
        path = fps[i]
        with rasterio.open(path) as src:
            # mask values outside polygon to zero
            gmug_src_aoi = gdf.to_crs(src.crs)
            src_clipped_data, src_clipped_transform = mask(src, [gmug_src_aoi.geometry.iloc[0]], crop=True, nodata=np.nan, filled=True)
    
            # create temp destination array
            dst_array = np.full((out_height, out_width), np.nan, dtype=np.float32)
            reproject(
                source=src_clipped_data,
                destination=dst_array,
                src_transform=src_clipped_transform,
                src_crs=src.crs,
                dst_transform=out_transform,
                dst_crs=target_crs,
                dst_nodata=np.nan,
                resampling=Resampling.bilinear
            )
        if i==0:
            stack[0] = dst_array
        else:
            stack[1] = dst_array
            max_composite = np.nanmax(np.stack(stack), axis=0)
            stack[0] = max_composite

    # clip to gmug extent (cut large NA regions)
    max_composite, out_transform = clip_raster_to_gdf_bounds(max_composite, out_transform, gdf)

    # export
    meta = {
        'driver': 'GTiff',
        'height': max_composite.shape[0],
        'width': max_composite.shape[1],
        'count': 1,
        'dtype': 'float32',
        'crs': target_crs,
        'transform': out_transform,
        'nodata': np.nan
    }
    
    with rasterio.open(fp_out, 'w', **meta) as dst:
        dst.write(max_composite, 1)

    os.sync()
#########################################
## DEPLOY LWC MODEL
#########################################

def get_coords(n, window_radius, buffer):
    c1 = 0
    cs = []
    while c1 < n:
        c1 = c1
        c2 = c1 + 2*window_radius
        if c2 > n:
            break
        cs.append([c1, c2])
        c1 = c1 + 2*(window_radius-buffer)
    if cs[len(cs)-1][1] < n:
        c2 = n
        c1 = c2 - 2*window_radius
        cs.append([c1, c2])
    return cs

def _cropped_loss(y_true, y_pred):
    if (buffer is not None):
        y_true = y_true[:, buffer:-buffer, buffer:-buffer, :]
        y_pred = y_pred[:, buffer:-buffer, buffer:-buffer, :]
    mse = keras.losses.MeanSquaredError()
    loss = mse(y_true, y_pred)
    if weighted:
        weights = sample_weight
        loss = loss * weights
    return loss

# define custom loss function
def mse_cropped_loss(buffer, weighted):
    def _cropped_loss(y_true, y_pred):
        if (buffer is not None):
            y_true = y_true[:, buffer:-buffer, buffer:-buffer, :]
            y_pred = y_pred[:, buffer:-buffer, buffer:-buffer, :]
        mse = keras.losses.MeanSquaredError()
        loss = mse(y_true, y_pred)
        if weighted:
            weights = sample_weight
            loss = loss * weights
        return loss
    return _cropped_loss







    

