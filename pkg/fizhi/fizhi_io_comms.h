C $Header: /u/gcmpack/MITgcm/pkg/fizhi/fizhi_io_comms.h,v 1.2 2004/08/06 23:17:55 molod Exp $
C $Name:  $


C     FIZHI I/O flags

      logical fizhi_mdsio_read_pickup,fizhi_mdsio_write_pickup
      logical fizhi_mnc_write_pickup,fizhi_mnc_read_pickup
      COMMON /FIZHI_IO_VARS/
     &     fizhi_mdsio_read_pickup, 
     &     fizhi_mdsio_write_pickup, 
     &     fizhi_mnc_write_pickup, 
     &     fizhi_mnc_read_pickup



CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
