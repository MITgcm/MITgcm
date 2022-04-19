#include "PACKAGES_CONFIG.h"
#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_SURF_DR
C     !INTERFACE:
      SUBROUTINE CALC_SURF_DR( etaFld,
     I                         myTime, myIter, myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE CALC_SURF_DR
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
C     etaFld    :: current eta field used to update the hFactor
C     myTime    :: current time in simulation
C     myIter    :: current iteration number in simulation
C     myThid    :: thread number for this instance of the routine.
      _RL etaFld(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL myTime
      INTEGER myIter
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
      INTEGER ks, nTmp
      INTEGER numbWrite, numbWrMax
      _RL hFactmp, adjust_nb_pt, adjust_volum
      _RL rSurftmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hhm, hhp
c     CHARACTER*(MAX_LEN_MBUF) suff
CEOP
      DATA numbWrite / 0 /
      numbWrMax = Nx*Ny

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      adjust_nb_pt = 0.
      adjust_volum = 0.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C--   before updating hFac_surfC/S/W save current fields
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
           hFac_surfNm1C(i,j,bi,bj) = hFac_surfC(i,j,bi,bj)
           hFac_surfNm1S(i,j,bi,bj) = hFac_surfS(i,j,bi,bj)
           hFac_surfNm1W(i,j,bi,bj) = hFac_surfW(i,j,bi,bj)
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Compute the new fractional thickness of surface level (kSurfC):

        DO j=0,sNy+1
         DO i=0,sNx+1
          rSurftmp(i,j) = Ro_surf(i,j,bi,bj)+etaFld(i,j,bi,bj)
          ks = kSurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           IF ( rSurftmp(i,j).LT.Rmin_surf(i,j,bi,bj) ) THEN
C-- Needs to do something :
            IF ( numbWrite.LE.numbWrMax .AND.
     &           ( i.GE.1.AND.i.LE.sNx .AND.
     &             j.GE.1.AND.j.LE.sNy ) ) THEN
             numbWrite = numbWrite + 1
             hFactmp = h0FacC(i,j,ks,bi,bj)
     &         + ( rSurftmp(i,j) - Ro_surf(i,j,bi,bj) )*recip_drF(ks)
             IF (hFactmp.LT.hFacInf) THEN
              WRITE(errorMessageUnit,'(2A,6I4,I10)')
     &         'WARNING: hFacC < hFacInf at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ELSE
              WRITE(errorMessageUnit,'(2A,6I4,I10)')
     &         'WARNING: hFac < hFacInf near:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ENDIF
             WRITE(errorMessageUnit,'(A,2F10.6,1PE14.6)')
     &         ' hFac_n-1,hFac_n,eta =',
     &          hfacC(i,j,ks,bi,bj), hFactmp, etaFld(i,j,bi,bj)
            ENDIF
C-- Decide to STOP :
c            WRITE(errorMessageUnit,'(A)')
c    &        'STOP in CALC_SURF_DR : too SMALL hFac !'
c            STOP 'ABNORMAL END: S/R CALC_SURF_DR'
C-- Or continue with Rmin_surf:
            IF ( i.GE.1.AND.i.LE.sNx .AND.
     &           j.GE.1.AND.j.LE.sNy ) THEN
              adjust_nb_pt = adjust_nb_pt + 1.
              adjust_volum = adjust_volum
     &          + rA(i,j,bi,bj)*(Rmin_surf(i,j,bi,bj)-rSurftmp(i,j))
            ENDIF
            rSurftmp(i,j) = Rmin_surf(i,j,bi,bj)
C----------
           ENDIF

C-- Set hFac_surfC :
           hFac_surfC(i,j,bi,bj) = h0FacC(i,j,ks,bi,bj)
     &            + ( rSurftmp(i,j) - Ro_surf(i,j,bi,bj)
     &              )*recip_drF(ks)*maskC(i,j,ks,bi,bj)

C-- Usefull warning when hFac becomes very large:
           IF ( numbWrite.LE.numbWrMax .AND.
     &          hFac_surfC(i,j,bi,bj).GT.hFacSup ) THEN
              numbWrite = numbWrite + 1
              WRITE(errorMessageUnit,'(2A,6I4,I10)')
     &         'WARNING: hFacC > hFacSup at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
              WRITE(errorMessageUnit,'(A,2F10.6,1PE14.6)')
     &         ' hFac_n-1,hFac_n,eta =', hfacC(i,j,ks,bi,bj),
     &          hFac_surfC(i,j,bi,bj), etaFld(i,j,bi,bj)
           ENDIF
C----------
          ENDIF

         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Compute fractional thickness of surface level, for U & V point:

        DO j=1,sNy
         DO i=1,sNx+1
          ks = kSurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
C-  allows hFacW to be larger than surrounding hFacC=1 @ edge of a step with
C   different kSurfC on either side (topo in p-coords, ice-shelf in z-coords)
            hhm = rSurftmp(i-1,j)
            hhp = rSurftmp(i,j)
C-  make sure hFacW is not larger than the 2 surrounding hFacC
c           hhm = rF(ks)
c           IF(ks.EQ.kSurfC(i-1,j,bi,bj)) hhm = rSurftmp(i-1,j)
c           hhp = rF(ks)
c           IF(ks.EQ.kSurfC(i,j,bi,bj))   hhp = rSurftmp(i,j)
            hFac_surfW(i,j,bi,bj) = h0FacW(i,j,ks,bi,bj)
     &            + ( MIN(hhm,hhp) - rSurfW(i,j,bi,bj)
     &              )*recip_drF(ks)*maskW(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO

        DO j=1,sNy+1
         DO i=1,sNx
          ks = kSurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
C-  allows hFacS to be larger than surrounding hFacC=1 @ edge of a step with
C   different kSurfC on either side (topo in p-coords, ice-shelf in z-coords)
            hhm = rSurftmp(i,j-1)
            hhp = rSurftmp(i,j)
C-  make sure hFacS is not larger than the 2 surrounding hFacC
c           hhm = rF(ks)
c           IF(ks.EQ.kSurfC(i,j-1,bi,bj)) hhm = rSurftmp(i,j-1)
c           hhp = rF(ks)
c           IF(ks.EQ.kSurfC(i,j,bi,bj))   hhp = rSurftmp(i,j)
            hFac_surfS(i,j,bi,bj) = h0FacS(i,j,ks,bi,bj)
     &            + ( MIN(hhm,hhp) - rSurfS(i,j,bi,bj)
     &              )*recip_drF(ks)*maskS(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO

#ifdef ALLOW_OBCS
C-- Apply OBC to hFac_surfW,S before the EXCH calls
        IF ( useOBCS ) THEN
          CALL OBCS_APPLY_SURF_DR(
     I                    bi, bj, etaFld,
     U                    hFac_surfC, hFac_surfW, hFac_surfS,
     I                    myTime, myIter, myThid )
        ENDIF
#endif /* ALLOW_OBCS */

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    end bi,bj loop.
       ENDDO
      ENDDO

C-- Global diagnostic :
      _GLOBAL_SUM_RL( adjust_nb_pt , myThid )
      IF ( adjust_nb_pt.GE.1. ) THEN
        _GLOBAL_SUM_RL( adjust_volum , myThid )
        _BEGIN_MASTER( myThid )
C   just to avoid issue with OpenAD: copy to integer nTmp before printing
        nTmp = NINT(adjust_nb_pt)
        WRITE(standardMessageUnit,'(2(A,I10),1PE16.8)')
     &    ' SURF_ADJUSTMENT: Iter=', myIter,
     &    ' Nb_pts,Vol=', nTmp, adjust_volum
        _END_MASTER( myThid )
      ENDIF

      _EXCH_XY_RS(hFac_surfC, myThid )
      CALL EXCH_UV_XY_RS(hFac_surfW,hFac_surfS,.FALSE.,myThid)

C-----
C Note: testing kSurfW,S is equivalent to a full height mask
C   ==> no need for applying the mask here.
C and with "partial thin wall" ==> mask could be applied in S/R UPDATE_SURF_DR
C-----

c     IF ( myIter.GE.0 ) THEN
c       WRITE(suff,'(I10.10)') myIter
c       CALL WRITE_FLD_XY_RS( 'hFac_surfC.', suff, hFac_surfC,
c    &                         myIter, myThid )
c     ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
