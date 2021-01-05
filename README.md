# ufs-land-forcing
Make sure your computer has FORTRAN compiler and NetCDF software installed.

First clone this repository:

`git clone https://github.com/barlage/ufs-land-forcing.git`

Modify the **user_build_config** based on the location of the NetCDF libraries and FORTRAN compiler.
## Example of single_point test:
1) Change directory to the **single_point** directory

2) The initial data are in **bondville.forcing** that includes location information , initial states, meta data, 
and conversion information.

3) Invoke `make` to compile. This should create executable file **create_point_data.exe**

4) Run the point_data and create the NetCDF files using: `./create_point_data.exe`

5) The output NetCDF files should be **ufs_land_forcing**, **ufs_land_driver_static**, and **ufs_land_driver_init**
