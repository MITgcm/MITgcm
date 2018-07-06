#include "GMREDI_OPTIONS.h"


CBOP
C     !ROUTINE: GMREDI_CALC_CALC_QG_LEITH_COEFF
C     !INTERFACE:
      SUBROUTINE GMREDI_CALC_CALC_QG_LEITH_COEFF(
     I             iMin, iMax, jMin, jMax,
     I             bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION: \bv
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_CALC_QG_LEITH_COEFF
C     | o Calculate QG Leith contribution to GMRedi tensor.
C     *==========================================================*
C     *==========================================================*
C     \ev

C     !USES:
      IMPLICIT NONE

C     == Global variables ==
#include "SIZE.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "SURFACE.h"
#include "GMREDI.h"
#include "MOM_VISC.h"


C     == Routine arguments ==
C     bi,bj   :: current tile indices
C     k       :: current vertical level
C     iMin,iMax,jMin,jMax :: loop ranges
C     myTime  :: current time
C     myIter  :: current time-step number
C     myThid  :: my Thread Id number
      INTEGER bi,bj
      INTEGER iMin,iMax,jMin,jMax
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

#ifdef ALLOW_GM_LEITH_QG

C     == Local variables ==
C     strainBC :: same as strain but account for no-slip BC
C     vort3BC  :: same as vort3  but account for no-slip BC
      _RL deepFac3, deepFac4
      _RL L2, L3, L5, L2rdt, L4rdt, recip_dt
      _RS hFacZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS h0FacZ  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL tension (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strain  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL strainBC(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsq     (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL KE      (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL omega3  (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3BC (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL grdVrt, grdDiv
      _RL viscAh_DLthQG(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL leithQG2fac
C     i,j    :: Loop counters
      INTEGER i,j,k
C     xxxFac :: On-off tracer parameters used for switching terms off.
      _RL  ArDudrFac
      _RL  ArDvdrFac
      _RL  sideMaskFac
      LOGICAL bottomDragTerms
      LOGICAL writeDiag
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER imomkey
#endif

#ifdef ALLOW_MNC
      INTEGER offsets(9)
      CHARACTER*(1) pf
#endif


C--   Initialise intermediate terms
      DO j=1-OLy,sNy+OLy
       DO i=1-OLx,sNx+OLx
        vort3(i,j) = 0.
        omega3(i,j)= 0.
        KE(i,j)    = 0.
        strain(i,j)  = 0. _d 0
        strainBC(i,j)= 0. _d 0
        stretching(i,j) = 0. _d 0
        Nsq(i,j)     = 0. _d 0
        tension(i,j) = 0. _d 0
#ifdef ALLOW_AUTODIFF
        hFacZ(i,j)   = 0. _d 0
#endif
       ENDDO
      ENDDO

C note: using standard stencil (no mask) results in under-estimating
C       vorticity at a no-slip boundary by a factor of 2 = sideDragFactor
      IF ( no_slip_sides ) THEN
        sideMaskFac = sideDragFactor
      ELSE
        sideMaskFac = 0. _d 0
      ENDIF

      IF ( selectImplicitDrag.EQ.0 .AND.
     &      (  no_slip_bottom
     &    .OR. selectBotDragQuadr.GE.0
     &    .OR. bottomDragLinear.NE.0. ) ) THEN
       bottomDragTerms=.TRUE.
      ELSE
       bottomDragTerms=.FALSE.
      ENDIF

C     start k loop since momentum code is inside one`
      DO k=1,Nr

C--     Calculate open water fraction at vorticity points
        CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C       Make local copies of horizontal flow field
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uFld(i,j) = uVel(i,j,k,bi,bj)
          vFld(i,j) = vVel(i,j,k,bi,bj)
         ENDDO
        ENDDO

C       Do not need KE for LeithQG, so no point calculating it.
C       CALL MOM_CALC_KE(bi,bj,k,selectKEscheme,uFld,vFld,KE,myThid)

        CALL MOM_CALC_RELVORT3(bi,bj,k,uFld,vFld,hFacZ,vort3,myThid)

C-      mask vort3 and account for no-slip / free-slip BC in vort3BC:
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            vort3BC(i,j) = vort3(i,j)
            IF ( hFacZ(i,j).EQ.zeroRS ) THEN
              vort3BC(i,j) = sideMaskFac*vort3BC(i,j)
              vort3(i,j)   = 0.
            ENDIF
          ENDDO
        ENDDO


C--     For viscous term, compute horizontal divergence, tension & strain
C       and mask relative vorticity (free-slip case):

        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            h0FacZ(i,j) = hFacZ(i,j)
          ENDDO
        ENDDO
#ifdef NONLIN_FRSURF
        IF ( no_slip_sides .AND. nonlinFreeSurf.GT.0 ) THEN
          DO j=2-OLy,sNy+OLy
            DO i=2-OLx,sNx+OLx
              h0FacZ(i,j) = MIN(
     &          MIN( h0FacW(i,j,k,bi,bj), h0FacW(i,j-1,k,bi,bj) ),
     &          MIN( h0FacS(i,j,k,bi,bj), h0FacS(i-1,j,k,bi,bj) ) )
            ENDDO
          ENDDO
        ENDIF
#endif /* NONLIN_FRSURF */

        CALL MOM_CALC_HDIV(bi,bj,k,2,uFld,vFld,hDiv,myThid)

        CALL MOM_VISC_QGLEITH_CALC_STRETCHING(bi,bj,k,
     O                                         stretching,Nsq,
     I                                         myThid)
        CALL MOM_VISC_QGLEITH_LIMITER(bi,bj,k,
     O                                 stretching,
     I                                 Nsq,uFld,vFld,vort3,myThid)
        CALL MOM_CALC_TENSION( bi,bj,k,uFld,vFld,tension,myThid )
        CALL MOM_CALC_STRAIN( bi,bj,k,uFld,vFld,hFacZ,strain,myThid )

C-      mask strain and account for no-slip / free-slip BC in strainBC:
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            strainBC(i,j) = strain(i,j)
            IF ( hFacZ(i,j).EQ.zeroRS ) THEN
              strainBC(i,j) = sideMaskFac*strainBC(i,j)
              strain(i,j)   = 0.
            ENDIF
          ENDDO
        ENDDO


C-      uses vort3BC & strainBC which account for no-slip / free-slip BC

        IF (useFullLeith) THEN
C       Uses correct calculation for gradients, but might not work on cube sphere
          leithQG2fac = (viscC2LeithQG/pi)**6
        ELSE
C       Uses approximate gradients, but works on cube sphere. No reason to use this
C        unless `useFullLeith` fails for your setup.
          leithQG2fac = (viscC2LeithQG/pi)**3
        ENDIF

        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            viscAh_DLthQG(i,j)= 0. _d 0
          ENDDO
        ENDDO

C-      Initialise to zero gradient of vorticity and divergence:
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            divDx(i,j) = 0.
            divDy(i,j) = 0.
            vrtDx(i,j) = 0.
            vrtDy(i,j) = 0.
          ENDDO
        ENDDO

C--     horizontal gradient of horizontal divergence:
C-       gradient in x direction:
        IF (useCubedSphereExchange) THEN
C        to compute d/dx(hDiv), fill corners with appropriate values:
          CALL FILL_CS_CORNER_TR_RL( 1, .FALSE.,
     &                                hDiv, bi,bj, myThid )
        ENDIF
        DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1
            divDx(i,j) = (hDiv(i,j)-hDiv(i-1,j))
     &                  *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
          ENDDO
        ENDDO

C-      gradient in y direction:
        IF (useCubedSphereExchange) THEN
C        to compute d/dy(hDiv), fill corners with appropriate values:
          CALL FILL_CS_CORNER_TR_RL( 2, .FALSE.,
     &                                hDiv, bi,bj, myThid )
        ENDIF
        DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1
            divDy(i,j) = (hDiv(i,j)-hDiv(i,j-1))
     &                  *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
          ENDDO
        ENDDO

C       horizontal gradient of vorticity and vortex stretching:
C        In the case of using QG Leith, we want to add a term
C        before calculating vector magnitude.
C        gradient in x direction:
        DO j=2-Oly,sNy+Oly
          DO i=2-Olx,sNx+Olx-1
            vrtDx(i,j) = (vort3(i+1,j)-vort3(i,j))
     &                  *recip_dxG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInS(i,j,bi,bj)
#endif
     &                  + (stretching(i+1,j)-stretching(i,j))
     &                  *recip_dxG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInS(i,j,bi,bj)
#endif
          ENDDO
        ENDDO
C-       gradient in y direction:
        DO j=2-Oly,sNy+Oly-1
          DO i=2-Olx,sNx+Olx
            vrtDy(i,j) = (vort3(i,j+1)-vort3(i,j))
     &                  *recip_dyG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInW(i,j,bi,bj)
#endif
     &                  + (stretching(i,j+1)-stretching(i,j))
     &                  *recip_dyG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInW(i,j,bi,bj)
#endif
          ENDDO
        ENDDO


        DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1

C These are (powers of) length scales
            L2 = L2_D(i,j,bi,bj)*deepFac2C(k)
            L2rdt = 0.25 _d 0*recip_dt*L2
            L3 = L3_D(i,j,bi,bj)*deepFac3
            L4rdt = L4rdt_D(i,j,bi,bj)*deepFac4
            L5 = (L2*L3)


            IF (useFullLeith) THEN
C This is the vector magnitude of the vorticity gradient squared
              grdVrt=0.25 _d 0*( (vrtDx(i,j+1)*vrtDx(i,j+1)
     &                        + vrtDx(i,j)*vrtDx(i,j) )
     &                     + (vrtDy(i+1,j)*vrtDy(i+1,j)
     &                        + vrtDy(i,j)*vrtDy(i,j) )  )

C This is the vector magnitude of grad (div.v) squared
C Using it in Leith serves to damp instabilities in w.
              grdDiv=0.25 _d 0*( (divDx(i+1,j)*divDx(i+1,j)
     &                        + divDx(i,j)*divDx(i,j) )
     &                     + (divDy(i,j+1)*divDy(i,j+1)
     &                        + divDy(i,j)*divDy(i,j) )  )

              viscAh_DLthQG(i,j) =
     &          leithQG2fac*SQRT(grdVrt+grdDiv)*L3
              IF (GM_useLeithQG) THEN
                GM_LeithQG_K(i,j,k,bi,bj) = viscAh_DLthQG(i,j)
              ELSE
                GM_LeithQG_K(i,j,k,bi,bj) = 0. _d 0
              ENDIF            

            ELSE
C but this approximation will work on cube (and differs by as much as 4X)
              grdVrt=MAX( ABS(vrtDx(i,j+1)), ABS(vrtDx(i,j)) )
              grdVrt=MAX( grdVrt, ABS(vrtDy(i+1,j)) )
              grdVrt=MAX( grdVrt, ABS(vrtDy(i,j))   )

C This approximation is good to the same order as above...
              grdDiv=MAX( ABS(divDx(i+1,j)), ABS(divDx(i,j)) )
              grdDiv=MAX( grdDiv, ABS(divDy(i,j+1)) )
              grdDiv=MAX( grdDiv, ABS(divDy(i,j))   )

              viscAh_DLthQG(i,j)=leithQG2fac*(grdVrt + grdDiv)*L3
              IF (GM_useLeithQG) THEN
                GM_LeithQG_K(i,j,k,bi,bj) = viscAh_DLthQG(i,j)
              ELSE
                GM_LeithQG_K(i,j,k,bi,bj) = 0. _d 0
              ENDIF
            ENDIF
          ENDDO
        ENDDO


        CALL DIAGNOSTICS_FILL(GM_LeithQG_K,'GM_LTHQG',
     &                       k,1,2,bi,bj,myThid)

      ENDDO /* k loop */

#endif /* ALLOW_GM_LEITH_QG */

      RETURN
      END
