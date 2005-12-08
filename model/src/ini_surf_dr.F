C $Header: /u/gcmpack/MITgcm/model/src/Attic/ini_surf_dr.F,v 1.1 2005/12/08 15:44:34 heimbach Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: INI_SURF_DR
C     !INTERFACE:
      SUBROUTINE INI_SURF_DR( myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE INI_SURF_DR                                   
C     | o Calculate the new surface level thickness according to  
C     |   the surface r-position  (Non-Linear Free-Surf)          
C     | o take decision if grid box becomes too thin or too thick 
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     == Global variables
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SURFACE.h"

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     myThid :: Thread number for this instance of the routine.
      INTEGER myThid

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables
C     i,j,k,bi,bj  :: loop counter
C     rSurftmp     :: free surface r-position that is used to compute hFac_surf
C     adjust_nb_pt :: Nb of grid points where rSurf is adjusted (hFactInf)
C     adjust_volum :: adjustment effect on the volume (domain size)
C     numbWrite    :: count the Number of warning written on STD-ERR file
C     numbWrMax    ::  maximum  Number of warning written on STD-ERR file
      INTEGER i,j,bi,bj
      INTEGER ks, numbWrite, numbWrMax
      _RL hFacInfMOM, Rmin_tmp, hFactmp, adjust_nb_pt, adjust_volum
      _RL rSurftmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hhm, hhp
      CHARACTER*(MAX_LEN_MBUF) suff
CEOP
      DATA numbWrite / 0 /
      numbWrMax = Nx*Ny

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

       hFacInfMOM = hFacInf

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)

C-- Initialise arrays :
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
           hFac_surfC(i,j,bi,bj) = 0.
           hFac_surfW(i,j,bi,bj) = 0.
           hFac_surfS(i,j,bi,bj) = 0.
           PmEpR(i,j,bi,bj) = 0.
           Rmin_surf(i,j,bi,bj) = Ro_surf(i,j,bi,bj)
          ENDDO
         ENDDO

C-- Compute the mimimum value of r_surf (used for computing hFac_surfC)
         DO j=1,sNy
          DO i=1,sNx
           ks = ksurfC(i,j,bi,bj)
           IF (ks.LE.Nr) THEN
             Rmin_tmp = rF(ks+1)
             IF ( ks.EQ.ksurfW(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i-1,j,bi,bj))
             IF ( ks.EQ.ksurfW(i+1,j,bi,bj)) 
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i+1,j,bi,bj))
             IF ( ks.EQ.ksurfS(i,j,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j-1,bi,bj))
             IF ( ks.EQ.ksurfS(i,j+1,bi,bj))
     &          Rmin_tmp = MAX(Rmin_tmp, R_low(i,j+1,bi,bj))

             Rmin_surf(i,j,bi,bj) =
     &        MAX( MAX(rF(ks+1),R_low(i,j,bi,bj)) + hFacInf*drF(ks),  
     &                                Rmin_tmp + hFacInfMOM*drF(ks)
     &           )
           ENDIF
          ENDDO
         ENDDO

C-    end bi,bj loop.
        ENDDO
       ENDDO

       _EXCH_XY_R8( Rmin_surf, myThid ) 

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
