C $Header: /u/gcmpack/MITgcm/model/src/calc_surf_dr.F,v 1.3 2001/09/26 18:09:14 cnh Exp $
C $Name:  $

#include "CPP_OPTIONS.h"

CBOP
C     !ROUTINE: CALC_SURF_DR
C     !INTERFACE:
      SUBROUTINE CALC_SURF_DR(etaFld,
     I                        myTime, myIter, myThid )
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
C     myTime :: Current time in simulation
C     myIter :: Current iteration number in simulation
C     myThid :: Thread number for this instance of the routine.
C     etaFld :: current eta field used to update the hFactor
      _RL myTime
      INTEGER myIter
      INTEGER myThid
      _RL etaFld(1-Olx:sNx+Olx,1-Oly:sNy+Oly,nSx,nSy)

#ifdef NONLIN_FRSURF

C     !LOCAL VARIABLES:
C     Local variables in common block
C     Rmin_surf :: minimum r_value of the free surface position 
C                  that satisfy  the hFacInf criteria
      COMMON /LOCAL_CALC_SURF_DR/ Rmin_surf
      _RL Rmin_surf(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
C     Local variables
C     i,j,k,bi,bj  :: loop counter
C     rSurftmp     :: free surface r-position that is used to compute hFac_surf
C     adjust_nb_pt :: Nb of grid points where rSurf is adjusted (hFactInf)
C     adjust_volum :: adjustment effect on the volume (domain size)
      INTEGER i,j,k,bi,bj
      INTEGER ks
      _RL hFacInfMOM, Rmin_tmp, hFactmp, adjust_nb_pt, adjust_volum
      _RL rSurftmp(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS hhm, hhp
      CHARACTER*(MAX_LEN_MBUF) suff
CEOP

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      IF (myIter.EQ.nIter0) THEN

       hFacInfMOM = hFacInf

       DO bj=myByLo(myThid), myByHi(myThid)
        DO bi=myBxLo(myThid), myBxHi(myThid)

C-- Initialise arrays :
         DO j=1-Oly,sNy+Oly
          DO i=1-Olx,sNx+Olx 
           hFac_surfC(i,j,bi,bj) = 0.
           hFac_surfW(i,j,bi,bj) = 0.
           hFac_surfS(i,j,bi,bj) = 0.
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

C-    end of initialization block
      ENDIF

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

      adjust_nb_pt = 0.
      adjust_volum = 0.

      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Compute the new fractional thickness of surface level (ksurfC):

        DO j=0,sNy+1
         DO i=0,sNx+1
          rSurftmp(i,j) = Ro_surf(i,j,bi,bj)+etaFld(i,j,bi,bj)
          ks = ksurfC(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
           IF (rSurftmp(i,j) .LT. Rmin_surf(i,j,bi,bj)) THEN
C-- Needs to do something :
             hFactmp = ( rSurftmp(i,j)-MAX(rF(ks+1),R_low(i,j,bi,bj))
     &                 )*recip_drF(ks)
             IF (hFactmp.LT.hFacInf) THEN
              write(0,'(2A,6I4,I10)') 'WARNING: hFacC < hFacInf at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ELSE
              write(0,'(2A,6I4,I10)') 'WARNING: hFac < hFacInf near:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
             ENDIF
              write(0,'(A,2F10.6,1PE14.6)') 'hFac_n-1,hFac_n,eta =',
     &          hfacC(i,j,ks,bi,bj), hFactmp, etaFld(i,j,bi,bj)
C-- Decide to STOP :
c             write(0,'(2A)') 'STOP in CALC_SURF_DR :',
c    &                        ' too SMALL hFac !'
c             STOP 'ABNORMAL END: S/R CALC_SURF_DR' 
C----------

C-- Continue with Rmin_surf:
             IF ( i.GE.1.AND.i.LE.sNx .AND.
     &            j.GE.1.AND.j.LE.sNy ) THEN
               adjust_nb_pt = adjust_nb_pt + 1.
               adjust_volum = adjust_volum 
     &          + rA(i,j,bi,bj)*(Rmin_surf(i,j,bi,bj)-rSurftmp(i,j))
             ENDIF
             rSurftmp(i,j) = Rmin_surf(i,j,bi,bj)
C----------
           ENDIF

C-- Set hFac_surfC :
           hFac_surfC(i,j,bi,bj) = 
     &         ( rSurftmp(i,j) - MAX(rF(ks+1), R_low(i,j,bi,bj))
     &         )*recip_drF(ks)*maskC(i,j,ks,bi,bj)

           IF (hFac_surfC(i,j,bi,bj).GT.hFacSup) THEN
C-- Usefull warning when hFac becomes very large:
              write(0,'(2A,6I4,I10)') 'WARNING: hFacC > hFacSup at:',
     &         ' i,j,k,bi,bj,Thid,Iter=',i,j,ks,bi,bj,myThid,myIter
              write(0,'(A,2F10.6,1PE14.6)') 'hFac_n-1,hFac_n,eta =',
     &          hfacC(i,j,ks,bi,bj), hFac_surfC(i,j,bi,bj), 
     &          etaFld(i,j,bi,bj)
C-- Decide to STOP :
c             write(0,'(2A)') 'STOP in CALC_SURF_DR :',
c    &                        ' too LARGE hFac !'
c             STOP 'ABNORMAL END: S/R CALC_SURF_DR' 
C----------
           ENDIF
          ENDIF

         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
C-- Compute fractional thickness of surface level, for U & V point:

        DO j=1,sNy
         DO i=1,sNx+1
          ks = ksurfW(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
            hhm = rF(ks)
            IF(ks.EQ.ksurfC(i-1,j,bi,bj)) hhm = rSurftmp(i-1,j)
            hhp = rF(ks)
            IF(ks.EQ.ksurfC(i,j,bi,bj))   hhp = rSurftmp(i,j)  
            hFac_surfW(i,j,bi,bj) = 
     &         ( MIN(hhm,hhp) 
     &          - MAX(rF(ks+1),R_low(i-1,j,bi,bj),R_low(i,j,bi,bj)) 
     &         )*recip_drF(ks)*maskW(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO

        DO j=1,sNy+1
         DO i=1,sNx
          ks = ksurfS(i,j,bi,bj)
          IF (ks.LE.Nr) THEN
            hhm = rF(ks)
            IF(ks.EQ.ksurfC(i,j-1,bi,bj)) hhm = rSurftmp(i,j-1)
            hhp = rF(ks)
            IF(ks.EQ.ksurfC(i,j,bi,bj))   hhp = rSurftmp(i,j)
            hFac_surfS(i,j,bi,bj) = 
     &         ( MIN(hhm,hhp) 
     &          - MAX(rF(ks+1),R_low(i,j-1,bi,bj),R_low(i,j,bi,bj)) 
     &         )*recip_drF(ks)*maskS(i,j,ks,bi,bj)
          ENDIF
         ENDDO
        ENDDO

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

C-    end bi,bj loop.
       ENDDO
      ENDDO

C-- Global diagnostic :
      _GLOBAL_SUM_R8( adjust_nb_pt , myThid ) 
      _GLOBAL_SUM_R8( adjust_volum , myThid ) 
      IF (adjust_nb_pt .GE.1.) THEN
        _BEGIN_MASTER( myThid ) 
        write(*,'(2(A,I10),1PE16.8)') ' SURF_ADJUSTMENT: Iter=',
     &   myIter, ' Nb_pts,Vol=', nint(adjust_nb_pt), adjust_volum
        _END_MASTER( )
      ENDIF

      _EXCH_XY_R4(hFac_surfC, myThid ) 
      CALL EXCH_UV_XY_RS(hFac_surfW,hFac_surfS,.FALSE.,myThid)

C-----
C Note: testing ksurfW,S is equivalent to a full height mask 
C   ==> no need for applying the mask here.
C and with "partial thin wall" ==> mask could be applied in S/R UPDATE_SURF_DR
C-----

      WRITE(suff,'(I10.10)') myIter
c     CALL WRITE_FLD_XY_RS('hFac_surfC.',suff,hFac_surfC,
c    &                     myIter,myThid)                    

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|
#endif /* NONLIN_FRSURF */

      RETURN
      END
