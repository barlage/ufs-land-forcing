
use netcdf
use time_utilities
implicit none

! Location Information

real    :: latitude                  ! latitude [degrees]
real    :: longitude                 ! longitude [degrees]
real    :: forcing_height            ! height of forcing [m]
integer :: vegetation_category       ! category based on land_cover_source
integer :: soil_category             ! soil texture class
integer :: slope_category            ! soil drainage class
real    :: deep_soil_temperature     ! deep soil temperature [K]
real    :: elevation                 ! elevation [m]
integer :: land_mask                 ! land mask [-] (should be 1 for a land point)
integer :: num_soil_levels           ! number of soil levels
real    :: max_snow_albedo           ! albedo when fully snow-covered
real    :: emissivity                ! snow-free emissivity
real, dimension(12) :: gvf_monthly   ! monthly GVF 
real, dimension(12) :: albedo_monthly! monthly albedo 
real, dimension(12) :: lai_monthly   ! monthly LAI 
real, dimension(12) :: z0_monthly    ! monthly roughness length 

! Initial States

character*19                    :: initial_date            ! timestamp of initial conditions
real                            :: snow_depth              ! snow depth [m]
real                            :: snow_water_equivalent   ! snow water equivalent [m]
real                            :: canopy_water            ! canopy water [mm]
real                            :: skin_temperature        ! skin temperature [K]
real, allocatable, dimension(:) :: soil_level_thickness    ! soil level thicknesses [m]
real, allocatable, dimension(:) :: soil_level_nodes        ! soil level centroids from surface [m]
real, allocatable, dimension(:) :: soil_temperature        ! soil level soil temperature [K]
real, allocatable, dimension(:) :: soil_moisture           ! soil level volumetric total water content [m3/m3]
real, allocatable, dimension(:) :: soil_liquid             ! soil level volumetric liquid content [m3/m3]

! Metadata

integer       :: water_classification   ! water type in land classification
integer       :: urban_classification   ! urban type in land classification
integer       :: ice_classification     ! snow/ice type in land classification
character*100 :: land_cover_source      ! land classification
character*100 :: soil_class_source      ! soil classification

! Conversion

logical :: have_relative_humidity       ! set to true if you need to convert relative to specific
real    :: temperature_offset           ! 273.15 to convert C to K
real    :: temperature_scale            ! can be used to convert F to C
real    :: pressure_scale               ! 100.0 to convert mb to Pa
real    :: precipitation_scale          ! 0.014111 to convert inches/30min to mm/s
logical :: local_time                   ! is the data in local time?
integer :: local_time_offset            ! offset in seconds
character*19 :: forcing_start           ! timestamp of first row of forcing
character*19 :: forcing_end             ! timestamp of last row of forcing
integer :: forcing_timestep             ! frequency of forcing in seconds

! Text forcing variables

real          :: wind_speed
real          :: temperature
real          :: humidity
real          :: pressure
real          :: solar_radiation
real          :: longwave_radiation
real          :: precipitation

! NETCDF variables

integer :: ncid
integer :: iret
integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_date
integer :: var_id_xlat   , var_id_xlong , var_id_veg    , var_id_soil  , &
           var_id_slope  , var_id_tmn   , var_id_hgt    , var_id_mask  , &
           var_id_thick  , var_id_nodes , var_id_maxalb ,                &
	   var_id_emiss  , var_id_gvf   , var_id_time   , var_id_date  , &
	   var_id_albedo , var_id_lai   , var_id_z0
integer :: var_id_depth  , var_id_swe   , var_id_canwat , var_id_tsk   ,  &
           var_id_stemp  , var_id_smois , var_id_sliq
integer :: var_id_force_temp  , var_id_force_humid , var_id_force_pres   , &
           var_id_force_wind  ,                                            &
	   var_id_force_sw    , var_id_force_lw    , var_id_force_precip    

! Misc variables

integer          :: itime
character*34     :: filename
real             :: e, svp     ! for relative humidity conversion
double precision :: nowtime    ! seconds after reference_date
double precision :: initime    ! seconds after reference_date for initial time
character*19     :: nowdate    ! current date
character*19     :: reference_date = "1970-01-01 00:00:00"
integer          :: yyyy,mm,dd,hh,nn,ss
logical          :: add_date_forcing = .false.    ! hard coded to add date string to forcing, by default keep files small


namelist / location / latitude, longitude, forcing_height, vegetation_category, &
  soil_category, slope_category, deep_soil_temperature, elevation, land_mask, &
  num_soil_levels, max_snow_albedo, emissivity, gvf_monthly, albedo_monthly, &
  lai_monthly, z0_monthly
  
namelist / initial / snow_depth, snow_water_equivalent, canopy_water, &
  skin_temperature,  soil_temperature, soil_moisture, soil_liquid, &
  soil_level_thickness, soil_level_nodes, initial_date

namelist / metadata / water_classification, urban_classification, & 
  ice_classification, land_cover_source, soil_class_source
  
namelist / conversion / have_relative_humidity, temperature_offset, &
  temperature_scale, pressure_scale, precipitation_scale, &
  local_time_offset, forcing_start, forcing_end, forcing_timestep

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Start creation of static and initialization file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

open(30, file="bondville.forcing", form="FORMATTED")
read(30, location)

allocate(soil_level_thickness(num_soil_levels))
allocate(soil_level_nodes    (num_soil_levels))
allocate(soil_temperature    (num_soil_levels))
allocate(soil_moisture       (num_soil_levels))
allocate(soil_liquid         (num_soil_levels))

read(30, initial)
read(30, metadata)
read(30, conversion)
close(30)

! Create the NetCDF static fields file.

print*, "Starting static file"

  iret = nf90_create("ufs_land_driver_static.nc", NF90_CLOBBER, ncid)

! Define dimensions in the file.

  iret = nf90_def_dim(ncid, "location"   , 1              , dim_id_loc)
  iret = nf90_def_dim(ncid, "soil_levels", num_soil_levels, dim_id_soil)
  iret = nf90_def_dim(ncid, "months"     , 12             , dim_id_time)

  iret = nf90_def_var(ncid, "latitude", NF90_FLOAT, dim_id_loc, var_id_xlat)
    iret = nf90_put_att(ncid, var_id_xlat, "description", "latitude")
    iret = nf90_put_att(ncid, var_id_xlat, "units", "degrees_north")

  iret = nf90_def_var(ncid, "longitude", NF90_FLOAT, dim_id_loc, var_id_xlong)
    iret = nf90_put_att(ncid, var_id_xlong, "description", "longitude")
    iret = nf90_put_att(ncid, var_id_xlong, "units", "degrees_east")

  iret = nf90_def_var(ncid, "vegetation_category", NF90_INT, dim_id_loc, var_id_veg)
    iret = nf90_put_att(ncid, var_id_veg, "description", "vegetation type")
    iret = nf90_put_att(ncid, var_id_veg, "units", "-")

  iret = nf90_def_var(ncid, "soil_category", NF90_INT, dim_id_loc, var_id_soil)
    iret = nf90_put_att(ncid, var_id_soil, "description", "soil texture type")
    iret = nf90_put_att(ncid, var_id_soil, "units", "-")

  iret = nf90_def_var(ncid, "slope_category", NF90_INT, dim_id_loc, var_id_slope)
    iret = nf90_put_att(ncid, var_id_slope, "description", "soil drainage class")
    iret = nf90_put_att(ncid, var_id_slope, "units", "-")

  iret = nf90_def_var(ncid, "deep_soil_temperature", NF90_FLOAT, dim_id_loc, var_id_tmn)
    iret = nf90_put_att(ncid, var_id_tmn, "description", "soil temperature lower boundary")
    iret = nf90_put_att(ncid, var_id_tmn, "units", "K")

  iret = nf90_def_var(ncid, "elevation", NF90_FLOAT, dim_id_loc, var_id_hgt)
    iret = nf90_put_att(ncid, var_id_hgt, "description", "elevation")
    iret = nf90_put_att(ncid, var_id_hgt, "units", "m")

  iret = nf90_def_var(ncid, "land_mask", NF90_INT, dim_id_loc, var_id_mask)
    iret = nf90_put_att(ncid, var_id_mask, "description", "land mask: 1=land")
    iret = nf90_put_att(ncid, var_id_mask, "units", "-")

  iret = nf90_def_var(ncid, "soil_level_thickness", NF90_FLOAT, dim_id_soil, var_id_thick)
    iret = nf90_put_att(ncid, var_id_thick, "description", "soil level thicknesses")
    iret = nf90_put_att(ncid, var_id_thick, "units", "m")

  iret = nf90_def_var(ncid, "soil_level_nodes", NF90_FLOAT, dim_id_soil, var_id_nodes)
    iret = nf90_put_att(ncid, var_id_nodes, "description", "soil node depths")
    iret = nf90_put_att(ncid, var_id_nodes, "units", "m")

  iret = nf90_def_var(ncid, "max_snow_albedo", NF90_FLOAT, dim_id_loc, var_id_maxalb)
    iret = nf90_put_att(ncid, var_id_maxalb, "description", "albedo when fully snow-covered")
    iret = nf90_put_att(ncid, var_id_maxalb, "units", "fraction")

  iret = nf90_def_var(ncid, "emissivity", NF90_FLOAT, dim_id_loc, var_id_emiss)
    iret = nf90_put_att(ncid, var_id_emiss, "description", "emissivity")
    iret = nf90_put_att(ncid, var_id_emiss, "units", "fraction")

  iret = nf90_def_var(ncid, "gvf_monthly", NF90_FLOAT, (/dim_id_loc, dim_id_time/), var_id_gvf)
    iret = nf90_put_att(ncid, var_id_gvf, "description", "monthly green vegetation fraction")
    iret = nf90_put_att(ncid, var_id_gvf, "units", "fraction")

  iret = nf90_def_var(ncid, "albedo_monthly", NF90_FLOAT, (/dim_id_loc, dim_id_time/), var_id_albedo)
    iret = nf90_put_att(ncid, var_id_albedo, "description", "monthly snow-free albedo")
    iret = nf90_put_att(ncid, var_id_albedo, "units", "fraction")

  iret = nf90_def_var(ncid, "lai_monthly", NF90_FLOAT, (/dim_id_loc, dim_id_time/), var_id_lai)
    iret = nf90_put_att(ncid, var_id_lai, "description", "monthly leaf area index")
    iret = nf90_put_att(ncid, var_id_lai, "units", "m2/m2")

  iret = nf90_def_var(ncid, "z0_monthly", NF90_FLOAT, (/dim_id_loc, dim_id_time/), var_id_z0)
    iret = nf90_put_att(ncid, var_id_z0, "description", "monthly roughness length")
    iret = nf90_put_att(ncid, var_id_z0, "units", "m")

  ! File metadata:

  iret = nf90_put_att(ncid, NF90_GLOBAL, "iswater"             , water_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "isurban"             , urban_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "isice"               , ice_classification)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "land_cover_source"   , land_cover_source)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "soil_class_source"   , soil_class_source)
  iret = nf90_put_att(ncid, NF90_GLOBAL, "title"               , "Created from create_point_data")

  iret = nf90_enddef(ncid)

  iret = nf90_put_var(ncid, var_id_xlat  , latitude              )
  iret = nf90_put_var(ncid, var_id_xlong , longitude             )
  iret = nf90_put_var(ncid, var_id_tmn   , deep_soil_temperature )
  iret = nf90_put_var(ncid, var_id_hgt   , elevation             )
  iret = nf90_put_var(ncid, var_id_mask  , land_mask             )
  iret = nf90_put_var(ncid, var_id_veg   , vegetation_category   )
  iret = nf90_put_var(ncid, var_id_slope , slope_category        )
  iret = nf90_put_var(ncid, var_id_soil  , soil_category         )
  iret = nf90_put_var(ncid, var_id_thick , soil_level_thickness  )
  iret = nf90_put_var(ncid, var_id_nodes , soil_level_nodes      )
  iret = nf90_put_var(ncid, var_id_maxalb, max_snow_albedo       )
  iret = nf90_put_var(ncid, var_id_emiss , emissivity            )
  iret = nf90_put_var(ncid, var_id_gvf   , gvf_monthly           , count = (/1,12/))
  iret = nf90_put_var(ncid, var_id_albedo, albedo_monthly        , count = (/1,12/))
  iret = nf90_put_var(ncid, var_id_lai   , lai_monthly           , count = (/1,12/))
  iret = nf90_put_var(ncid, var_id_z0    , z0_monthly            , count = (/1,12/))
  
  iret = nf90_close(ncid)

! Create the NetCDF initial fields file.

print*, "Starting initial file"

call calc_sec_since(reference_date, initial_date, local_time_offset, initime)

  iret = nf90_create("ufs_land_driver_init.nc", NF90_CLOBBER, ncid)

! Define dimensions in the file.

  iret = nf90_def_dim(ncid, "location"   , 1              , dim_id_loc)
  iret = nf90_def_dim(ncid, "soil_levels", num_soil_levels, dim_id_soil)
  iret = nf90_def_dim(ncid, "date_length", 19             , dim_id_date)

  iret = nf90_def_var(ncid, "time", NF90_DOUBLE, varid = var_id_time)
    iret = nf90_put_att(ncid, var_id_time, "long_name", "time")
    iret = nf90_put_att(ncid, var_id_time, "units", "seconds since "//reference_date)

  iret = nf90_def_var(ncid, "date", NF90_CHAR, dim_id_date, var_id_date)
    iret = nf90_put_att(ncid, var_id_date, "long_name", "UTC date")

  iret = nf90_def_var(ncid, "latitude", NF90_FLOAT, dim_id_loc, var_id_xlat)
    iret = nf90_put_att(ncid, var_id_xlat, "description", "latitude")
    iret = nf90_put_att(ncid, var_id_xlat, "units", "degrees_north")

  iret = nf90_def_var(ncid, "longitude", NF90_FLOAT, dim_id_loc, var_id_xlong)
    iret = nf90_put_att(ncid, var_id_xlong, "description", "longitude")
    iret = nf90_put_att(ncid, var_id_xlong, "units", "degrees_east")

  iret = nf90_def_var(ncid,   "snow_water_equivalent", NF90_FLOAT, dim_id_loc, var_id_swe)
    iret = nf90_put_att(ncid, var_id_swe, "description", "snow water equivalent")
    iret = nf90_put_att(ncid, var_id_swe, "units", "mm")

  iret = nf90_def_var(ncid, "snow_depth",  NF90_FLOAT, dim_id_loc, var_id_depth)
    iret = nf90_put_att(ncid, var_id_depth, "description", "snow depth")
    iret = nf90_put_att(ncid, var_id_depth, "units", "m")

  iret = nf90_def_var(ncid, "canopy_water", NF90_FLOAT, dim_id_loc, var_id_canwat)
    iret = nf90_put_att(ncid, var_id_canwat, "description", "canopy surface water")
    iret = nf90_put_att(ncid, var_id_canwat, "units", "mm")

  iret = nf90_def_var(ncid,    "skin_temperature", NF90_FLOAT, dim_id_loc, var_id_tsk)
    iret = nf90_put_att(ncid, var_id_tsk, "description", "surface skin temperature")
    iret = nf90_put_att(ncid, var_id_tsk, "units", "K")

  iret = nf90_def_var(ncid,   "soil_temperature", NF90_FLOAT, (/dim_id_loc,dim_id_soil/), var_id_stemp)
    iret = nf90_put_att(ncid, var_id_stemp, "description", "soil temperature")
    iret = nf90_put_att(ncid, var_id_stemp, "units", "K")

  iret = nf90_def_var(ncid,  "soil_moisture", NF90_FLOAT, (/dim_id_loc,dim_id_soil/), var_id_smois)
    iret = nf90_put_att(ncid, var_id_smois, "description", "volumetric soil moisture")
    iret = nf90_put_att(ncid, var_id_smois, "units", "m3/m3")

  iret = nf90_def_var(ncid,  "soil_liquid", NF90_FLOAT, (/dim_id_loc,dim_id_soil/), var_id_sliq)
    iret = nf90_put_att(ncid, var_id_sliq, "description", "volumetric soil liquid")
    iret = nf90_put_att(ncid, var_id_sliq, "units", "m3/m3")

  iret = nf90_def_var(ncid, "soil_level_thickness", NF90_FLOAT, dim_id_soil, var_id_thick)
    iret = nf90_put_att(ncid, var_id_thick, "description", "soil level thicknesses")
    iret = nf90_put_att(ncid, var_id_thick, "units", "m")

  iret = nf90_def_var(ncid, "soil_level_nodes", NF90_FLOAT, dim_id_soil, var_id_nodes)
    iret = nf90_put_att(ncid, var_id_nodes, "description", "soil node depths")
    iret = nf90_put_att(ncid, var_id_nodes, "units", "m")

  ! File metadata:

  iret = nf90_put_att(ncid, NF90_GLOBAL, "title"             , "Created from create_point_data")

  iret = nf90_enddef(ncid)

  iret = nf90_put_var(ncid, var_id_time  , initime               )
  iret = nf90_put_var(ncid, var_id_date  , initial_date          )
  iret = nf90_put_var(ncid, var_id_xlat  , latitude              )
  iret = nf90_put_var(ncid, var_id_xlong , longitude             )
  iret = nf90_put_var(ncid, var_id_swe   , snow_water_equivalent )
  iret = nf90_put_var(ncid, var_id_depth , snow_depth            )
  iret = nf90_put_var(ncid, var_id_canwat, canopy_water          )
  iret = nf90_put_var(ncid, var_id_tsk   , skin_temperature      )
  iret = nf90_put_var(ncid, var_id_stemp , soil_temperature      , count = (/1,4/))
  iret = nf90_put_var(ncid, var_id_smois , soil_moisture         , count = (/1,4/))
  iret = nf90_put_var(ncid, var_id_sliq  , soil_liquid           , count = (/1,4/))
  iret = nf90_put_var(ncid, var_id_thick , soil_level_thickness  )
  iret = nf90_put_var(ncid, var_id_nodes , soil_level_nodes      )
  
  iret = nf90_close(ncid)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  Start creation of forcing data file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Write to NetCDF file

  call calc_sec_since(reference_date, forcing_start, local_time_offset, initime)
  call date_from_since(reference_date, initime, nowdate)
  read(nowdate( 1: 4),'(i4.4)') yyyy
  read(nowdate( 6: 7),'(i2.2)') mm
  read(nowdate( 9:10),'(i2.2)') dd
  read(nowdate(12:13),'(i2.2)') hh
  read(nowdate(15:16),'(i2.2)') nn
  read(nowdate(18:19),'(i2.2)') ss

  write(filename,'(a17,i4,5i2.2,a3)') "ufs_land_forcing.", yyyy, mm, dd, hh, nn, ss, ".nc"

  write(*,*) "Creating: "//filename

  iret = nf90_create(filename, NF90_CLOBBER, ncid)

! Define dimensions in the file.

  iret = nf90_def_dim(ncid, "location"   , 1              , dim_id_loc)
  iret = nf90_def_dim(ncid, "time"       , NF90_UNLIMITED , dim_id_time)
  if(add_date_forcing) then
    iret = nf90_def_dim(ncid, "date_length", 19             , dim_id_date)
  end if
  
! Define variables in the file.

  iret = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, var_id_time)
    iret = nf90_put_att(ncid, var_id_time, "long_name", "time")
    iret = nf90_put_att(ncid, var_id_time, "units", "seconds since "//reference_date)

  if(add_date_forcing) then
    iret = nf90_def_var(ncid, "date", NF90_CHAR, (/dim_id_date,dim_id_time/), var_id_date)
      iret = nf90_put_att(ncid, var_id_date, "long_name", "UTC date")
  end if

  iret = nf90_def_var(ncid,   "temperature", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_temp)
    iret = nf90_put_att(ncid, var_id_force_temp, "description", "temperature")
    iret = nf90_put_att(ncid, var_id_force_temp, "units", "K")

  iret = nf90_def_var(ncid,   "specific_humidity", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_humid)
    iret = nf90_put_att(ncid, var_id_force_humid, "description", "specific humidity")
    iret = nf90_put_att(ncid, var_id_force_humid, "units", "kg/kg")

  iret = nf90_def_var(ncid,   "surface_pressure", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_pres)
    iret = nf90_put_att(ncid, var_id_force_pres, "description", "surface pressure")
    iret = nf90_put_att(ncid, var_id_force_pres, "units", "Pa")

  iret = nf90_def_var(ncid,   "wind_speed", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_wind)
    iret = nf90_put_att(ncid, var_id_force_wind, "description", "wind speed")
    iret = nf90_put_att(ncid, var_id_force_wind, "units", "m/s")

  iret = nf90_def_var(ncid,   "downward_longwave", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_lw)
    iret = nf90_put_att(ncid, var_id_force_lw, "description", "downward longwave radiation")
    iret = nf90_put_att(ncid, var_id_force_lw, "units", "W/m2")

  iret = nf90_def_var(ncid,   "downward_shortwave", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_sw)
    iret = nf90_put_att(ncid, var_id_force_sw, "description", "downward shortwave radiation")
    iret = nf90_put_att(ncid, var_id_force_sw, "units", "W/m2")

  iret = nf90_def_var(ncid,   "precipitation", NF90_FLOAT, (/dim_id_loc,dim_id_time/), var_id_force_precip)
    iret = nf90_put_att(ncid, var_id_force_precip, "description", "precipitation")
    iret = nf90_put_att(ncid, var_id_force_precip, "units", "mm/s")

  iret = nf90_put_att(ncid, NF90_GLOBAL, "title"  , "forcing created from create_point_data")
  iret = nf90_put_att(ncid, NF90_GLOBAL, "forcing_height" , forcing_height)

  iret = nf90_enddef(ncid)


! Start reading from text file and writing to NetCDF

  open(30, file="bondville.forcing", form="FORMATTED")

  do itime = 1, 69
    read(30,*)
  end do

  itime = 1
  
  forcing: do 

  nowtime = initime + (itime-1)*forcing_timestep

  call date_from_since(reference_date, nowtime, nowdate)
  
  read(30,*,end=1000) wind_speed, temperature, humidity, pressure, &
      solar_radiation, longwave_radiation, precipitation

  print*, "Read forcing for: "//nowdate

! Do some conversions

  temperature = temperature_scale * temperature + temperature_offset
  pressure = pressure_scale * pressure
  precipitation = precipitation_scale * precipitation

  if(have_relative_humidity) then
    svp = 611.2*exp(17.67*(temperature-273.15)/(temperature-29.65)) ! [Pa]
    e   = humidity/100.0 * svp                                      ! [Pa]
    humidity = (0.622*e)/(pressure-(1.0-0.622)*e)                   ! now it is specific humidity
  end if

  iret = nf90_put_var(ncid, var_id_time          , nowtime           , start = (/itime/)  )
  if(add_date_forcing) then
    iret = nf90_put_var(ncid, var_id_date          , nowdate           , start = (/1,itime/)      )
  end if
  iret = nf90_put_var(ncid, var_id_force_temp    , temperature       , start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_humid   , humidity          , start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_pres    , pressure          , start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_wind    , wind_speed        , start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_sw      , solar_radiation   , start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_lw      , longwave_radiation, start = (/1,itime/)  )
  iret = nf90_put_var(ncid, var_id_force_precip  , precipitation     , start = (/1,itime/)  )

  itime = itime + 1

end do forcing

1000 continue

  iret = nf90_close(ncid)

end
