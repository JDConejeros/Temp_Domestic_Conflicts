"""
Code 4.2: Google Earth Engine — temperature re-extraction for quadrants with all-NA series

Reads Temp_quadrant_data_RM_2017_2025.RData (object: temp_qua_join), identifies
geometry_id / quadrant groups where temperature_2m is NA for every row, then
exports daily ERA5-Land values for those features only.

Improvements vs baseline extraction:
  - reduceRegion(..., bestEffort=True, tileScale=4) on the full polygon
  - Fallback: reduceRegion on the polygon centroid (same scale) if polygon mean is null

Requirements:
  pip install earthengine-api geopandas pandas
  R (with sf, dplyr, readr) on PATH for RData discovery

Run from repository root:
  python 02_Code/4.2 Temperature_GEE_fill_missing_quadrants.py
"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

import ee
import geopandas as gpd
import pandas as pd

# -----------------------------------------------------------------------------
# Paths (repository root = parent of 02_Code)
# -----------------------------------------------------------------------------
SCRIPT_PATH = Path(__file__).resolve()
REPO_ROOT = SCRIPT_PATH.parent.parent
DATA_OUT = REPO_ROOT / "01_Data" / "Output"
QUAD_SHP_DIR = DATA_OUT / "quad_geo"
SHAPEFILE_PATH = QUAD_SHP_DIR / "quad_geo.shp"
RDATA_PATH = DATA_OUT / "Temp_quadrant_data_RM_2017_2025.RData"
MISSING_IDS_CSV = DATA_OUT / "missing_temp_quadrant_ids.csv"

# Match 4.3 Temp_data.R filter (adjust if you extend the climate CSVs)
START_DATE = "2017-01-01"
END_DATE = '2025-11-01'

EE_PROJECT = os.environ.get("EE_PROJECT", "quadrant-rm")


def write_missing_quadrant_ids_csv() -> pd.DataFrame:
    """Call R to find quadrants with all NA temperature_2m in temp_qua_join."""
    if not RDATA_PATH.is_file():
        raise FileNotFoundError(f"RData not found: {RDATA_PATH}")

    r_path = str(RDATA_PATH.resolve()).replace("\\", "/")
    csv_path = str(MISSING_IDS_CSV.resolve()).replace("\\", "/")

    r_code = f"""
suppressPackageStartupMessages({{
  library(sf)
  library(dplyr)
  library(readr)
}})
load("{r_path}")
x <- temp_qua_join
if (inherits(x, "sf")) {{
  x <- sf::st_drop_geometry(x)
}}
miss <- x |>
  dplyr::group_by(.data$geometry_id, .data$quadrant) |>
  dplyr::summarise(
    all_na_temp = all(is.na(.data$temperature_2m)),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$all_na_temp)
readr::write_csv(miss, "{csv_path}")
message("N missing quadrants: ", nrow(miss))
"""

    print("Running R to list quadrants with all-NA temperature_2m …")
    subprocess.run(
        ["Rscript", "-e", r_code],
        check=True,
        cwd=str(REPO_ROOT),
    )
    miss = pd.read_csv(MISSING_IDS_CSV)
    print(f"Wrote {MISSING_IDS_CSV} ({len(miss)} rows).")
    return miss


def gdf_to_ee_featurecollection(gdf: gpd.GeoDataFrame) -> ee.FeatureCollection:
    """Convert GeoDataFrame to Earth Engine FeatureCollection."""
    features = []
    for idx, row in gdf.iterrows():
        geom = row.geometry
        try:
            if geom.geom_type == "Polygon":
                coords = [[[p[0], p[1]] for p in geom.exterior.coords]]
                ee_geom = ee.Geometry.Polygon(coords)
            elif geom.geom_type == "MultiPolygon":
                # One exterior ring per part: MultiPolygon = [ polygon, ... ], polygon = [ ring, ... ]
                coords = [
                    [[[p[0], p[1]] for p in poly.exterior.coords]]
                    for poly in geom.geoms
                ]
                ee_geom = ee.Geometry.MultiPolygon(coords)
            else:
                print(f"Skipping geometry {idx}: type {geom.geom_type}")
                continue

            geometry_id = int(row.get("geometry_id", idx))
            quadrant = row.get("quadrant", "")
            feature = ee.Feature(
                ee_geom,
                {"geometry_id": geometry_id, "quadrant": str(quadrant)},
            )
            features.append(feature)
        except Exception as ex:
            print(f"Error processing geometry {idx}: {ex}")
            continue

    return ee.FeatureCollection(features)


def kelvin_to_c_or_null(deg_k) -> ee.Number | None:
    """EE Number in K -> °C, or None if missing."""
    return ee.Algorithms.If(deg_k, ee.Number(deg_k).subtract(273.15), None)


def extract_temperature_data_missing(
    feature_collection: ee.FeatureCollection,
    start_date: str,
    end_date: str,
) -> ee.FeatureCollection:
    """
    Daily ERA5-Land temperatures with polygon mean + centroid fallback.
    """
    era5 = (
        ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR")
        .filterDate(start_date, end_date)
        .select(["temperature_2m", "temperature_2m_min", "temperature_2m_max"])
    )

    scale_m = 9000  # ~native ERA5-Land sampling; keeps overlap with small polygons

    def process_image(image: ee.Image) -> ee.FeatureCollection:
        date = ee.Date(image.get("system:time_start"))
        date_str = date.format("YYYY-MM-dd")

        def extract_stats(feature: ee.Feature) -> ee.Feature:
            geom = feature.geometry()
            centroid = geom.centroid(maxError=1)

            stats_poly = image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=geom,
                scale=scale_m,
                maxPixels=1e13,
                bestEffort=True,
                tileScale=4,
            )

            stats_pt = image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=centroid,
                scale=scale_m,
                maxPixels=1e13,
                bestEffort=True,
                tileScale=2,
            )

            def pick_k(key: str):
                a = stats_poly.get(key)
                return ee.Algorithms.If(a, a, stats_pt.get(key))

            temp_mean = kelvin_to_c_or_null(pick_k("temperature_2m"))
            temp_min = kelvin_to_c_or_null(pick_k("temperature_2m_min"))
            temp_max = kelvin_to_c_or_null(pick_k("temperature_2m_max"))

            return ee.Feature(
                None,
                {
                    "geometry_id": feature.get("geometry_id"),
                    "quadrant": feature.get("quadrant"),
                    "date": date_str,
                    "temperature_2m": temp_mean,
                    "temperature_2m_min": temp_min,
                    "temperature_2m_max": temp_max,
                    "source": ee.String("ERA5_LAND_polygon_or_centroid"),
                },
            )

        return feature_collection.map(extract_stats)

    return era5.map(process_image).flatten()


def main() -> None:
    os.chdir(REPO_ROOT)

    miss = write_missing_quadrant_ids_csv()
    if miss.empty:
        print("No quadrants with all-NA temperature_2m; nothing to export.")
        sys.exit(0)

    missing_ids = set(miss["geometry_id"].astype(int).tolist())
    print(f"geometry_id to fill: {sorted(missing_ids)}")

    if not SHAPEFILE_PATH.is_file():
        raise FileNotFoundError(f"Shapefile not found: {SHAPEFILE_PATH}")

    quad_gdf = gpd.read_file(SHAPEFILE_PATH)
    if quad_gdf.crs is None or str(quad_gdf.crs) != "EPSG:4326":
        quad_gdf = quad_gdf.to_crs("EPSG:4326")

    if "geometry_id" not in quad_gdf.columns:
        quad_gdf["geometry_id"] = range(len(quad_gdf))

    quad_missing = quad_gdf[quad_gdf["geometry_id"].astype(int).isin(missing_ids)].copy()
    if quad_missing.empty:
        raise RuntimeError(
            "RData listed missing geometry_ids but none match quad_geo.shp geometry_id."
        )

    print(f"Subset shapefile: {len(quad_missing)} features for Earth Engine.")

    ee.Authenticate()
    ee.Initialize(project=EE_PROJECT)

    ee_fc = gdf_to_ee_featurecollection(quad_missing)
    fc_size = ee_fc.size().getInfo()
    print(f"Earth Engine FeatureCollection size: {fc_size}")

    temp_features = extract_temperature_data_missing(ee_fc, START_DATE, END_DATE)

    task = ee.batch.Export.table.toDrive(
        collection=temp_features,
        description="temperature_daily_missing_quadrants_fill",
        folder="EarthEngine_Exports",
        fileFormat="CSV",
    )
    task.start()
    print("✓ Export started: temperature_daily_missing_quadrants_fill → Google Drive")
    print(f"  Task ID: {task.id}")
    print(f"  State: {task.state}")
    print("  Merge this CSV into your main temperature table by geometry_id + date.")


if __name__ == "__main__":
    main()
