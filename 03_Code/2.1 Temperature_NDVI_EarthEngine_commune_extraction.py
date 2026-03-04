"""
Code 2.1: Extract Temperature and NDVI data using Google Earth Engine
Extracts daily temperature (min, max, mean) and NDVI for each commune geometry
Period: 2017-01-01 to 2025-11-01

Reference: https://developers.google.com/earth-engine/guides/quickstart_python

Installation:
    pip uninstall ee  # Remove incorrect package if installed
    pip install earthengine-api geopandas

Note: This script exports data to Google Drive. After export completes,
download the CSV files and merge them manually.
"""

import ee
import geopandas as gpd
import os

# Initialize Earth Engine
# First time: run ee.Authenticate() and ee.Initialize(project='YOUR_PROJECT_ID')

ee.Authenticate()
ee.Initialize(project='quadrant-rm')

# Set paths
data_out = "02_Data/Output/district_geo/"
os.makedirs(data_out, exist_ok=True)

# Option 1: Load from existing shapefile if available
shapefile_path = os.path.join(data_out, "district_geo.shp")

if os.path.exists(shapefile_path):
    print(f"Loading shapefile from {shapefile_path}")
    commune_gdf = gpd.read_file(shapefile_path)
else:
    # Option 2: Create commune geometries using chilemapas (requires R)
    # First, create a simple R script to export communes
    print("Shapefile not found. Creating commune geometries...")
    print("Note: This requires R and chilemapas package.")
    print("Run the following R code first to create the shapefile:")
    print("""
    library(chilemapas)
    library(sf)
    library(dplyr)
    
    # Get communes from Región Metropolitana (codigo_region == 13)
    comunas_rm <- mapa_comunas %>%
      mutate(codigo_comuna = as.numeric(codigo_comuna)) %>%
      filter(codigo_region == 13) %>%
      mutate(geometry_id = row_number() - 1) %>%
      select(geometry_id, codigo_comuna, nombre_comuna, codigo_region, nombre_region, geometry)
    
    # Ensure CRS is WGS84
    comunas_rm <- st_transform(comunas_rm, crs = 4326)
    
    # Save as shapefile
    st_write(comunas_rm, "02_Data/Output/district_geo/district_geo.shp", delete_dsn = TRUE)
    """)
    
    # Try to load anyway in case it was just created
    if os.path.exists(shapefile_path):
        commune_gdf = gpd.read_file(shapefile_path)
    else:
        raise FileNotFoundError(
            f"Shapefile not found: {shapefile_path}\n"
            "Please create it first using the R code shown above."
        )

print(f"Loaded {len(commune_gdf)} district geometries")

# Ensure CRS is WGS84 (EPSG:4326) for Earth Engine
if commune_gdf.crs != 'EPSG:4326':
    print(f"Reprojecting from {commune_gdf.crs} to EPSG:4326")
    commune_gdf = commune_gdf.to_crs('EPSG:4326')

# Add geometry ID if not present
if 'geometry_id' not in commune_gdf.columns:
    commune_gdf['geometry_id'] = range(len(commune_gdf))

# Add district code if not present (for reference)
if 'codigo_comuna' not in commune_gdf.columns:
    if 'codigo_comuna' in commune_gdf.columns:
        pass  # Already exists
    else:
        # Try to extract from other columns
        commune_gdf['codigo_comuna'] = commune_gdf['geometry_id']

# Date range
start_date = '2017-01-01'
end_date = '2025-11-01'

# Convert GeoDataFrame to Earth Engine FeatureCollection
def gdf_to_ee_featurecollection(gdf):
    """Convert GeoDataFrame to Earth Engine FeatureCollection"""
    features = []
    for idx, row in gdf.iterrows():
        geom = row.geometry
        try:
            if geom.geom_type == 'Polygon':
                coords = [[[p[0], p[1]] for p in geom.exterior.coords]]
                ee_geom = ee.Geometry.Polygon(coords)
            elif geom.geom_type == 'MultiPolygon':
                coords = [[[[p[0], p[1]] for p in poly.exterior.coords] for poly in geom.geoms]]
                ee_geom = ee.Geometry.MultiPolygon(coords)
            else:
                print(f"Skipping geometry {idx}: type {geom.geom_type}")
                continue
            
            # Create feature with geometry ID and commune code
            geometry_id = int(row.get('geometry_id', idx))
            codigo_comuna = int(row.get('codigo_comuna', geometry_id))
            
            feature = ee.Feature(ee_geom, {
                'geometry_id': geometry_id,
                'codigo_comuna': codigo_comuna
            })
            features.append(feature)
        except Exception as e:
            print(f"Error processing geometry {idx}: {e}")
            continue
    
    return ee.FeatureCollection(features)

# Convert to EE FeatureCollection
ee_fc = gdf_to_ee_featurecollection(commune_gdf)
fc_size = ee_fc.size().getInfo()
print(f"Converted to Earth Engine FeatureCollection with {fc_size} features")

# Function to extract temperature data
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_DAILY_AGGR?hl=es-419#description
def extract_temperature_data(feature_collection, start_date, end_date):
    """Extract daily temperature data (min, max, mean) from ERA5-Land"""
    
    # Load ERA5-Land daily dataset
    # Available bands: temperature_2m, temperature_2m_min, temperature_2m_max
    era5 = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") \
        .filterDate(start_date, end_date) \
        .select(['temperature_2m', 'temperature_2m_min', 'temperature_2m_max'])
    
    n_images = era5.size().getInfo()
    print(f"Found {n_images} daily temperature images")
    
    # Function to process each image
    def process_image(image):
        date = ee.Date(image.get('system:time_start'))
        date_str = date.format('YYYY-MM-dd')
        
        # Calculate statistics for each geometry
        def extract_stats(feature):
            geom = feature.geometry()
            stats = image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=geom,
                scale=11132,  # ~1km resolution for ERA5-Land
                maxPixels=1e9
            )
            
            # Convert from Kelvin to Celsius
            # Note: temperature_2m is the mean temperature
            temp_mean = ee.Algorithms.If(
                stats.get('temperature_2m'),
                ee.Number(stats.get('temperature_2m')).subtract(273.15),
                None
            )

            temp_min = ee.Algorithms.If(
                stats.get('temperature_2m_min'),
                ee.Number(stats.get('temperature_2m_min')).subtract(273.15),
                None
            )

            temp_max = ee.Algorithms.If(
                stats.get('temperature_2m_max'),
                ee.Number(stats.get('temperature_2m_max')).subtract(273.15),
                None
            )
            
            geom_id = feature.get('geometry_id')
            codigo_comuna = feature.get('codigo_comuna')
            
            return ee.Feature(None, {
                'geometry_id': geom_id,
                'codigo_comuna': codigo_comuna,
                'date': date_str,
                'temperature_2m': temp_mean,
                'temperature_2m_min': temp_min,
                'temperature_2m_max': temp_max
            })
        
        return feature_collection.map(extract_stats)
    
    # Map over all images and flatten
    daily_features = era5.map(process_image).flatten()
    
    return daily_features

## ------------------------------------------------ ##

# Function to extract NDVI data
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD13Q1?hl=es-419#description
# MOD13Q1 is a 16-day composite product with NDVI band already calculated
def extract_ndvi_data(feature_collection, start_date, end_date):
    """Extract NDVI data from MODIS MOD13Q1 (16-day composite)"""
    
    # Load MODIS Terra Vegetation Indices (MOD13Q1)
    # This is a 16-day composite product, not daily
    modis = ee.ImageCollection('MODIS/061/MOD13Q1') \
        .filterDate(start_date, end_date) \
        .select(['NDVI'])  # NDVI band (scale factor: 0.0001)
    
    n_images = modis.size().getInfo()
    print(f"Found {n_images} MODIS MOD13Q1 images (16-day composites)")
    
    # Function to process each image
    def process_image(image):
        date = ee.Date(image.get('system:time_start'))
        date_str = date.format('YYYY-MM-dd')
        
        # NDVI band needs to be scaled by 0.0001
        ndvi = image.select('NDVI').multiply(0.0001).rename('NDVI')
        
        # Calculate statistics for each geometry
        def extract_stats(feature):
            geom = feature.geometry()
            stats = ndvi.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=geom,
                scale=250,  # MODIS MOD13Q1 resolution is 250m
                maxPixels=1e9
            )
            
            ndvi_value = ee.Number(stats.get('NDVI'))
            geom_id = feature.get('geometry_id')
            codigo_comuna = feature.get('codigo_comuna')
            
            return ee.Feature(None, {
                'geometry_id': geom_id,
                'codigo_comuna': codigo_comuna,
                'date': date_str,
                'ndvi': ndvi_value
            })
        
        return feature_collection.map(extract_stats)
    
    # Map over all images and flatten
    daily_features = modis.map(process_image).flatten()
    
    return daily_features

## ------------------------------------------------ ##

# Extract temperature data
print("\n=== Extracting Temperature Data ===")
temp_features = extract_temperature_data(ee_fc, start_date, end_date)

# Extract NDVI data
print("\n=== Extracting NDVI Data ===")
ndvi_features = extract_ndvi_data(ee_fc, start_date, end_date)

## ------------------------------------------------ ##

# Export method: Export to Google Drive
# Export temperature data
task_temp = ee.batch.Export.table.toDrive(
    collection=temp_features,
    description='temperature_daily_district',
    folder='EarthEngine_Exports',
    fileFormat='CSV'
)
task_temp.start()
print(f"✓ Temperature export started")
print(f"  Task ID: {task_temp.id}")
print(f"  Status: {task_temp.state}")

# Export NDVI data
task_ndvi = ee.batch.Export.table.toDrive(
    collection=ndvi_features,
    description='ndvi_daily_district',
    folder='EarthEngine_Exports',
    fileFormat='CSV'
)
task_ndvi.start()
print(f"✓ NDVI export started")
print(f"  Task ID: {task_ndvi.id}")
print(f"  Status: {task_ndvi.state}")

# Function to check task status
def check_task_status(task_id):
    """Check the status of an Earth Engine export task"""
    task = ee.batch.Task.list().filter(ee.Filter.eq('id', task_id)).get(0)
    if task:
        return task.state
    return None
