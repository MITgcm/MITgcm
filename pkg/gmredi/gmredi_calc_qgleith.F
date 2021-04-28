#include "GMREDI_OPTIONS.h"

CBOP
C !ROUTINE: GMREDI_CALC_QGLEITH

C !INTERFACE: ==========================================================
      SUBROUTINE GMREDI_CALC_QGLEITH(
     O             leithQG_K,
     I             bi, bj, myTime, myIter, myThid )

C     !DESCRIPTION:
C     *==========================================================*
C     | SUBROUTINE GMREDI_CALC_QGLEITH
C     | Calculate QG Leith contribution to GMRedi tensor.
C     | leithQG_K is located at the cell centre.
C     *==========================================================*

C !USES: ===============================================================
      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#ifdef ALLOW_MOM_COMMON
# include "MOM_VISC.h"
#endif

C !INPUT PARAMETERS: ===================================================
C     bi, bj    :: tile indices
C     myTime    :: Current time in simulation
C     myIter    :: Current iteration number in simulation
C     myThid    :: My Thread Id. number
      INTEGER bi, bj
      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C !OUTPUT PARAMETERS: ==================================================
C  leithQG_K    :: Horizontal LeithQG viscosity, to add to GM coefficient
      _RL leithQG_K(1-OLx:sNx+OLx,1-OLy:sNy+OLy,Nr)

#ifdef ALLOW_GM_LEITH_QG
#ifdef ALLOW_MOM_COMMON

C !LOCAL VARIABLES: ====================================================
      _RS hFacZ   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RS r_hFacZ (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL uFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vFld    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL stretching(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL Nsquare (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vort3   (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL hDiv    (1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL divDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDx(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL vrtDy(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      _RL grdVrt, grdDiv
      _RL leithQG2fac
      _RL deepFac3, L3, sqarg
C     i,j,k    :: Loop counters
      INTEGER i,j,k
CEOP

C--   Initialise terms

      IF (useFullLeith) THEN
C     Uses correct calculation for gradients, but might not work on cube sphere
        leithQG2fac = (viscC2LeithQG/pi)**6
      ELSE
C     Uses approximate gradients, but works on cube sphere. No reason to use this
C      unless `useFullLeith` fails for your setup.
        leithQG2fac = (viscC2LeithQG/pi)**3
      ENDIF

      DO j=1-OLy,sNy+OLy
        DO i=1-OLx,sNx+OLx
          hFacZ(i,j)         = 0. _d 0
          r_hFacZ(i,j)       = 0. _d 0
          uFld(i,j)          = 0. _d 0
          vFld(i,j)          = 0. _d 0
          stretching(i,j)    = 0. _d 0
          Nsquare(i,j)       = 0. _d 0
          vort3(i,j)         = 0. _d 0
          hDiv(i,j)          = 0. _d 0
          divDx(i,j)         = 0. _d 0
          divDy(i,j)         = 0. _d 0
          vrtDx(i,j)         = 0. _d 0
          vrtDy(i,j)         = 0. _d 0
        ENDDO
      ENDDO

C     start k loop since momentum code is inside one
      DO k=1,Nr

        deepFac3 = deepFac2C(k)*deepFacC(k)

C--     Calculate open water fraction at vorticity points
        CALL MOM_CALC_HFACZ(bi,bj,k,hFacZ,r_hFacZ,myThid)

C       Make local copies of horizontal flow field
        DO j=1-OLy,sNy+OLy
         DO i=1-OLx,sNx+OLx
          uFld(i,j) = uVel(i,j,k,bi,bj)
          vFld(i,j) = vVel(i,j,k,bi,bj)
         ENDDO
        ENDDO

        CALL MOM_CALC_RELVORT3( bi,bj,k,uFld,vFld,hFacZ,vort3,myThid )
        CALL MOM_CALC_HDIV( bi,bj,k,2,uFld,vFld,hDiv,myThid )

C vorticity needs to be masked if using vector invariant momentum
#ifdef ALLOW_MOM_VECINV
C-      mask vort3
        DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
            IF ( hFacZ(i,j).EQ.zeroRS ) THEN
              vort3(i,j)   = 0.
            ENDIF
          ENDDO
        ENDDO
#endif /* ALLOW_MOM_VECINV */

C     Having calculated the quantitites, use them to calculate
C       LeithQG coefficient

        CALL MOM_VISC_QGL_STRETCH(bi,bj,k,
     O                                   stretching, Nsquare,
     I                                   myTime, myIter, myThid)
        CALL MOM_VISC_QGL_LIMIT(bi,bj,k,
     O                                 stretching,
     I                                 Nsquare, uFld,vFld,vort3,
     I                                 myTime, myIter, myThid)

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
        DO j=2-OLy,sNy+OLy
          DO i=2-OLx,sNx+OLx-1
            vrtDx(i,j) = (vort3(i+1,j)-vort3(i,j))
     &                  *recip_dxG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInS(i,j,bi,bj)
#endif
C        Average d/dx of stretching onto V-points to match vrtDX
     &                 + halfRL*halfRL*
     &                    ((stretching(i+1,j)-stretching(i,j))
     &                      *recip_dxC(i+1,j,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i,j)-stretching(i-1,j))
     &                      *recip_dxC(i,j,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i+1,j-1)-stretching(i,j-1))
     &                      *recip_dxC(i,j-1,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i,j-1)-stretching(i-1,j-1))
     &                      *recip_dxC(i-1,j-1,bi,bj)*recip_deepFacC(k)
     &                    )*maskS(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                     *maskInS(i,j,bi,bj)
#endif
          ENDDO
        ENDDO
C-       gradient in y direction:
        DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx
            vrtDy(i,j) = (vort3(i,j+1)-vort3(i,j))
     &                  *recip_dyG(i,j,bi,bj)*recip_deepFacC(k)
     &                  *maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                  *maskInW(i,j,bi,bj)
#endif
C        Average d/dy of stretching onto U-points to match vrtDy
     &                 + halfRL*halfRL*
     &                    ((stretching(i,j+1)-stretching(i,j))
     &                      *recip_dyC(i,j+1,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i,j)-stretching(i,j-1))
     &                      *recip_dyC(i,j,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i-1,j+1)-stretching(i-1,j))
     &                      *recip_dyC(i-1,j+1,bi,bj)*recip_deepFacC(k)
     &                    + (stretching(i-1,j)-stretching(i-1,j-1))
     &                      *recip_dyC(i-1,j,bi,bj)*recip_deepFacC(k)
     &                    )*maskW(i,j,k,bi,bj)
#ifdef ALLOW_OBCS
     &                     *maskInW(i,j,bi,bj)
#endif
          ENDDO
        ENDDO

        DO j=2-OLy,sNy+OLy-1
          DO i=2-OLx,sNx+OLx-1

C These are (powers of) length scales
            L3 = L3_D(i,j,bi,bj)*deepFac3

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

              sqarg = leithQG2fac*(grdVrt+grdDiv)
#ifdef ALLOW_AUTODIFF
C avoid derivative of SQRT(0)
              IF (sqarg .GT. 0. _d 0) sqarg = SQRT(sqarg)
#else
              sqarg = SQRT(sqarg)
#endif
              LeithQG_K(i,j,k) = sqarg*L3

            ELSE
C but this approximation will work on cube (and differs by as much as 4X)
              grdVrt=MAX( ABS(vrtDx(i,j+1)), ABS(vrtDx(i,j)) )
              grdVrt=MAX( grdVrt, ABS(vrtDy(i+1,j)) )
              grdVrt=MAX( grdVrt, ABS(vrtDy(i,j))   )

C This approximation is good to the same order as above...
              grdDiv=MAX( ABS(divDx(i+1,j)), ABS(divDx(i,j)) )
              grdDiv=MAX( grdDiv, ABS(divDy(i,j+1)) )
              grdDiv=MAX( grdDiv, ABS(divDy(i,j))   )

              LeithQG_K(i,j,k) = leithQG2fac*(grdVrt + grdDiv)*L3

            ENDIF
          ENDDO
        ENDDO

      ENDDO /* k loop */

#ifdef ALLOW_DIAGNOSTICS
      IF ( useDiagnostics ) THEN
        CALL DIAGNOSTICS_FILL( LeithQG_K, 'GM_LTHQG',
     &                         0, Nr, 2, bi, bj, myThid )
      ENDIF
#endif /* ALLOW_DIAGNOSTICS */

#endif /* ALLOW_MOM_COMMON */
#endif /* ALLOW_GM_LEITH_QG */

      RETURN
      END
