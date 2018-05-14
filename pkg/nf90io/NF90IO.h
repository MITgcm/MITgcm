C ======================================================================
C  Defines for NF90IO.h

      INTEGER NF90IO_MAX_N_INFO
      INTEGER NF90IO_MAX_LEN_INFO
      PARAMETER(NF90IO_MAX_N_INFO=64)
      PARAMETER(NF90IO_MAX_LEN_INFO=1024)

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

C nf90io_info_keys   :: array of keys for MPI-IO hints
C nf90io_info_values :: array of values for MPI-IO hints
C
C permissible values will depend on the MPI-IO implementation
C generally, useful nf90io_info_keys include:
C   'romio_ds_read'  ('enable' or 'disable')
C   'romio_ds_write' ('enable' or 'disable')
C   'romio_cb_read'  ('enable' or 'disable')
C   'romio_cb_write' ('enable' or 'disable')
C   'cb_nodes' (number of nodes to use for I/O)
C
C parallel-netcdf-specific:
C   'nc_header_align_size' (preallocated size of metadata header in bytes)

      COMMON/nf90io_parms_c/nf90io_info_keys, nf90io_info_values
      CHARACTER*(NF90IO_MAX_LEN_INFO)
     &    nf90io_info_keys(NF90IO_MAX_N_INFO),
     &    nf90io_info_values(NF90IO_MAX_N_INFO)
