#!/bin/bash
# Download GODAS MLD files from 1990 to 2024

for year in {1990..2024}; do
  wget https://downloads.psl.noaa.gov/Datasets/godas/dbss_obml.${year}.nc
done

