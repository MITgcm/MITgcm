C $Header: /u/gcmpack/MITgcm/pkg/streamice/STREAMICE_ADV.h,v 1.2 2013/06/21 20:49:50 jmc Exp $
C $Name:  $

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

#ifdef ALLOW_STREAMICE

!      COMMON /STREAMICE_ADV_FIELDS_RL/
!     &    hflux_x_SI, hflux_y_SI,
!     &    hflux_x_SI2, hflux_y_SI2,
!     &    h_after_uflux_SI, h_after_vflux_SI
!      _RL hflux_x_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL hflux_y_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL hflux_x_SI2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL hflux_y_SI2 (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL h_after_uflux_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
!      _RL h_after_vflux_SI (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

#endif /* ALLOW_STREAMICE */

CEH3 ;;; Local Variables: ***
CEH3 ;;; mode:fortran ***
CEH3 ;;; End: ***
