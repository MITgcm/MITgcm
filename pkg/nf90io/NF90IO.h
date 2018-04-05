C ======================================================================
C  Defines for NF90IO.h


C Functions that returns integer defined in nf90io_utils.F
      INTEGER NF90IO_VAR_PAR_ACCESS
      INTEGER NF90IO_OPEN

C nf90io_fileFormat :: which netCDF file format to create:
C                      1: CDF-1 (classic format)
C                      2: CDF-2 (classic format with 64-bit offsets)
C                      3: CDF-3 (HDF5-based)
C                      4: CDF-4 (HDF5-based, classic model)
C                      5: CDF-5 (classic format with 64-bit data)
C                      note:
C                      3 and 4 require netCDF to be compiled with HDF5
C                      5 may require parallel-netcdf (pnetcdf) with older netCDF
C                      1, 2 and 5 require parallel-netcdf when running with mpi

      COMMON/nf90io_parms_i/nf90io_fileFormat
      INTEGER nf90io_fileFormat
