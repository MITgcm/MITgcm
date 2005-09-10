C $Header: /u/gcmpack/MITgcm/pkg/mnc/MNC_BUFF.h,v 1.1 2005/09/10 18:30:06 edhill Exp $
C $Name:  $
C

C     The following is the size of the buffer used by MNC to read and
C     write portions to/from NetCDF files.  The sizes of all reads and
C     writes are checked and MNC will terminate with a sensible error
C     message if the buffer is not large enough for all reads/writes.

      INTEGER     MNC_MAX_BUFF
      PARAMETER ( MNC_MAX_BUFF = 15000 + sNx + 2*OLx + sNy + 2*OLy )

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
