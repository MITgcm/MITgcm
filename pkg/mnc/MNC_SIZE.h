C $Header: /u/gcmpack/MITgcm/pkg/mnc/MNC_SIZE.h,v 1.1 2004/10/22 21:30:31 edhill Exp $
C $Name:  $
C

C     The following is the size of the buffer used by MNC to read and
C     write portions to/from NetCDF files.  The sizes of all reads and
C     writes are checked and MNC will terminate with a sensible error
C     message if the buffer is not large enough for all reads/writes.

      INTEGER     MNC_MAX_BUFF
      PARAMETER ( MNC_MAX_BUFF = 5000 + sNx + 2*OLx + sNy + 2*OLy )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
