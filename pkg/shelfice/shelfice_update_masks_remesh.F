#include "SHELFICE_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

CBOP
C     !ROUTINE: SHELFICE_UPDATE_MASKS_REMESH
C     !INTERFACE:
      SUBROUTINE SHELFICE_UPDATE_MASKS_REMESH(
     I     rF, recip_drF, drF, kLowC,
     U     hFacC,
     I     myThid )
C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE SHELFICE_UPDATE_MASKS_REMESH
C     | o Loops through top level cells and determines those where
C     | o hfac is too large and hence splits into two cells,
C     | o and cells where hfac is too small, and merges cell with
C     | o below.
C     | o
C     | o Affects all dynamic arrays, as well as R_shelfice
C     | o Recomputes hFacC using formula from initialisation
C     | o based on new R_shelfice
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE
C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "DYNVARS.h"
#include "SURFACE.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE.h"
#endif /* ALLOW_SHELFICE */

C     !INPUT/OUTPUT PARAMETERS:
C     == Routine arguments ==
C     rF        :: R-coordinate of face of cell (units of r).
C     recip_drF :: Recipricol of cell face separation along Z axis ( units of r ).
C     hFacC     :: Fraction of cell in vertical which is open (see GRID.h)
C     myThid    :: Number of this instance of SHELFICE_UPDATE_MASKS
      _RS rF        (1:Nr+1)
      _RS recip_drF (1:Nr)
      _RS drF (1:Nr)
      _RS hFacC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,1:Nr,nSx,nSy)
      INTEGER kLowC     (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      INTEGER myThid

#ifdef ALLOW_SHELFICE
#ifdef ALLOW_SHELFICE_REMESHING
C     !LOCAL VARIABLES:
C     == Local variables ==
C     bi,bj   :: tile indices
C     I,J,K   :: Loop counters
      INTEGER bi, bj
      INTEGER I, J, K
      _RL hFacCtmp
      _RL hFacMnSz




C- Update etaN
      DO bj = myByLo(myThid), myByHi(myThid)
       DO bi = myBxLo(myThid), myBxHi(myThid)
        DO J = 1,sNy
         DO I = 1,sNx
          IF ( R_shelfice(I,J,bi,bj) .LT. 0.0) THEN
           K  = MAX(1,kTopC(I,J,bi,bj))
           IF (K.gt.1) THEN
            IF (etah(I,J,bi,bj)*recip_drF(K)+1.0
     &      .GT. SHELFICESplitThreshold ) THEN
             IF (etah(I,J,bi,bj)*recip_drF(K-1)
     &       .GT. SHELFICEMergeThreshold ) THEN
              etaN(I,J,bi,bj) = etaN(I,J,bi,bj)- drF(K-1)
              etaH(I,J,bi,bj) = etaH(I,J,bi,bj)- drF(K-1)
              R_shelfIce(I,J,bi,bj) = R_shelfIce(I,J,bi,bj)+drF(K-1)
              uVel(I,J,K-1,bi,bj)=uVel(I,J,K,bi,bj)
              uVel(I+1,J,K-1,bi,bj)=uVel(I+1,J,K,bi,bj)
              vVel(I,J,K-1,bi,bj)=vVel(I,J,K,bi,bj)
              vVel(I,J+1,K-1,bi,bj)=vVel(I,J+1,K,bi,bj)
              gvnm1(I,J,K-1,bi,bj)=gvnm1(I,J,K,bi,bj)
              gvnm1(I,J+1,K-1,bi,bj)=gvnm1(I,J+1,K,bi,bj)
              gunm1(I,J,K-1,bi,bj)=gunm1(I,J,K,bi,bj)
              gunm1(I+1,J,K-1,bi,bj)=gunm1(I,J,K,bi,bj)
       
              salt(I,J,K-1,bi,bj)=salt(I,J,K,bi,bj)
              theta(I,J,K-1,bi,bj)=theta(I,J,K,bi,bj)
             ENDIF
            ENDIF
           ENDIF
          ENDIF
           IF (kTopC(i,j,bi,bj) .LT.kLowC (i,j,bi,bj))THEN
            K = MAX(1,kTopC(I,J,bi,bj))
            IF (etah(I,J,bi,bj)*recip_drF(K)+1.0 .LT. 
     &       SHELFICEMergeThreshold ) THEN
             IF (etah(I,J,bi,bj)*recip_drF(K+1)+2.0 .LT.
     &        SHELFICESplitThreshold ) THEN



!               etaN(I,J,bi,bj) = etaN(I,J,bi,bj) +drF(K+1)*
!      &        hFacC(i,j,K+1,bi,bj)
!               etaH(I,J,bi,bj) = etaH(I,J,bi,bj) +drF(K+1)*
!      &        hFacC(i,j,K+1,bi,bj)
!               R_shelfice(I,J,bi,bj) = R_shelfice(I,J,bi,bj) -drF(K+1)*
!      &        hFacC(i,j,K+1,bi,bj)
              etaN(I,J,bi,bj) = etaN(I,J,bi,bj) +drF(K)
              etaH(I,J,bi,bj) = etaH(I,J,bi,bj) +drF(K)
              R_shelfice(I,J,bi,bj) = R_shelfice(I,J,bi,bj) -drF(K)

             
              vVel(I,J,K+1,bi,bj)=((vVel(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(vVel(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              vVel(I,J+1,K+1,bi,bj)=((vVel(I,J+1,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(vVel(I,J+1,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              uVel(I,J,K+1,bi,bj)=((uVel(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(uVel(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))
 
              uVel(I+1,J,K+1,bi,bj)=((uVel(I+1,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(uVel(I+1,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              gunm1(I+1,J,K+1,bi,bj)=((gunm1(I+1,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(gunm1(I+1,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              gunm1(I,J,K+1,bi,bj)=((gunm1(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(gunm1(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              gvnm1(I,J+1,K+1,bi,bj)=((gvnm1(I,J+1,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(gvnm1(I,J+1,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))
 
              gvnm1(I,J,K+1,bi,bj)=((gvnm1(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(gvnm1(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              salt(I,J,K+1,bi,bj)=((salt(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(salt(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))

              theta(I,J,K+1,bi,bj)=((theta(I,J,K,bi,bj)*(drF(K)+
     &        etaN(I,J,bi,bj)))+(theta(I,J,K+1,bi,bj)*drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)))/(drF(K)+drF(K+1)*
     &        hFacC(i,j,K+1,bi,bj)+etaN(I,J,bi,bj))
              ENDIF  
             ENDIF 
            ENDIF
C           ENDIF
          ENDDO
         ENDDO
        ENDDO
       ENDDO
     
       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1,sNy
          DO I = 1,sNx
            etaH(I,J,bi,bj)=etaN(I,J,bi,bj)
            etaHnm1(I,J,bi,bj)=etaH(I,J,bi,bj)
          ENDDO
         ENDDO
        ENDDO
       ENDDO

       DO bj = myByLo(myThid), myByHi(myThid)
        DO bi = myBxLo(myThid), myBxHi(myThid)
         DO J = 1,sNy
          DO I = 1,sNx
           K = MAX(1,kTopC(I,J,bi,bj))
            hfac_surfc(I,J,bi,bj)= ((etaH(I,J,bi,bJ) +(drF(K)))
     &     *recip_drF(K))
          ENDDO
         ENDDO
        ENDDO
       ENDDO
    
       CALL EXCH_XYZ_RL(salt,myThid)
       CALL EXCH_XYZ_RL(theta,myThid)
       CALL EXCH_XYZ_RL(uVel,myThid)
       CALL EXCH_XYZ_RL(vVel,myThid)
       CALL EXCH_XYZ_RL(gunm1,myThid)
       CALL EXCH_XYZ_RL(gvnm1,myThid)
       CALL EXCH_XYZ_RL(hFacC,myThid)
 
       CALL EXCH_XY_RL(EtaN,myThid)
       CALL EXCH_XY_RL(EtaH,myThid)
       CALL EXCH_XY_RL(EtaHnm1,myThid)
       CALL EXCH_XY_RL(R_shelfice,myThid)
       CALL EXCH_XY_RL(hFac_surfC,myThid)

           
C- fill in the overlap (+ BARRIER):
      _EXCH_XY_RS(R_shelfIce, myThid )

C--   Calculate lopping factor hFacC : Remove part outside of the domain
C     taking into account the Reference (=at rest) Surface Position Ro_shelfIce
      DO bj=myByLo(myThid), myByHi(myThid)
       DO bi=myBxLo(myThid), myBxHi(myThid)

C--   compute contributions of shelf ice to looping factors
        DO K=1, Nr
         hFacMnSz=max( hFacMin, min(hFacMinDr*recip_drF(k),1. _d 0) )
         DO J=1-OLy,sNy+OLy
          DO I=1-OLx,sNx+OLx
C      o Non-dimensional distance between grid boundary and model surface
           hFacCtmp = (rF(k)-R_shelfIce(I,J,bi,bj))*recip_drF(K)
C      o Reduce the previous fraction : substract the outside part.
           hFacCtmp = hFacC(I,J,K,bi,bj) - max( hFacCtmp, 0. _d 0)
C      o set to zero if empty Column :
           hFacCtmp = max( hFacCtmp, 0. _d 0)
C      o Impose minimum fraction and/or size (dimensional)
           IF (hFacCtmp.LT.hFacMnSz) THEN
            IF (hFacCtmp.LT.hFacMnSz*0.5) THEN
             hFacC(I,J,K,bi,bj)=0.
            ELSE
             hFacC(I,J,K,bi,bj)=hFacMnSz
            ENDIF
           ELSE
             hFacC(I,J,K,bi,bj)=hFacCtmp
           ENDIF
          ENDDO
         ENDDO
        ENDDO



C - end bi,bj loops.
       ENDDO
      ENDDO
#endif /* ALLOW_SHELFICE */
#endif
      RETURN
      END
