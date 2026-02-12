c     ==================================================================
c     STREAMICE_COST_SIZE.h
c     ==================================================================

#ifdef ALLOW_STREAMICE_TC_COST

C     Maximum number of cost levels for STREAMICE transient calibration cost
C     ----------------------------------------------------------------------
C
C     streamiceMaxCostLevel :: max list of timestep levels where cost is applied

      integer     streamiceMaxCostLevel
      parameter ( streamiceMaxCostLevel = 10 )

#endif

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
