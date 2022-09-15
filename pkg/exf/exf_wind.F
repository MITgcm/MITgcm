#include "EXF_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif
#ifdef ALLOW_AUTODIFF
# include "AUTODIFF_OPTIONS.h"
#endif

      SUBROUTINE EXF_WIND( myTime, myIter, myThid )

C     ==================================================================
C     SUBROUTINE exf_wind
C     ==================================================================
C
C     o Prepare wind speed and stress fields
C
C     ==================================================================
C     SUBROUTINE exf_wind
C     ==================================================================

      IMPLICIT NONE

C     == global variables ==

#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"

#include "EXF_PARAM.h"
#include "EXF_FIELDS.h"
#include "EXF_CONSTANTS.h"
#include "DYNVARS.h"

#ifdef ALLOW_AUTODIFF_TAMC
#include "tamc.h"
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
# include "ctrl.h"
# include "optim.h"
# include "CTRL_SIZE.h"
# include "CTRL_GENARR.h"
#endif

C     == routine arguments ==

      _RL     myTime
      INTEGER myIter
      INTEGER myThid

C     == external functions ==

C     == local variables ==

      INTEGER bi,bj
      INTEGER i,j,ks
      _RL     urelw(1:sNx,1:sNy)
      _RL     vrelw(1:sNx,1:sNy)
      _RL     wsLoc(1:sNx,1:sNy)
      _RL     wsSq
#ifdef ALLOW_BULKFORMULAE
      _RL     usSq, recip_sqrtRhoA, ustar
      _RL     tmp1, tmp2, tmp3, tmp4
#endif /* ALLOW_BULKFORMULAE */
      _RL     oneThirdRL
      PARAMETER ( oneThirdRL = 1.d0 / 3.d0 )
#if !(defined ALLOW_BULKFORMULAE) || !(defined ALLOW_ATM_TEMP)
      _RL wsm, tmpbulk
#endif
#ifdef ALLOW_GENTIM2D_CONTROL
      INTEGER iarr
#endif
#ifdef ALLOW_AUTODIFF_TAMC
      INTEGER act1, act2, act3, act4
      INTEGER max1, max2, max3
      INTEGER ikey
#endif
C     == end of interface ==

C--   Use atmospheric state to compute surface fluxes.

      ks = 1
      IF ( usingPCoords ) ks = Nr

C     Loop over tiles.
      DO bj = myByLo(myThid),myByHi(myThid)
       DO bi = myBxLo(myThid),myBxHi(myThid)

#ifdef ALLOW_AUTODIFF_TAMC
        act1 = bi - myBxLo(myThid)
        max1 = myBxHi(myThid) - myBxLo(myThid) + 1
        act2 = bj - myByLo(myThid)
        max2 = myByHi(myThid) - myByLo(myThid) + 1
        act3 = myThid - 1
        max3 = nTx*nTy
        act4 = ikey_dynamics - 1
        ikey = (act1 + 1) + act2*max1
     &                    + act3*max1*max2
     &                    + act4*max1*max2*max3
#endif /* ALLOW_AUTODIFF_TAMC */

C--   Initialise
        DO j = 1,sNy
         DO i = 1,sNx
          wsLoc(i,j) = 0. _d 0
          urelw(i,j) = uwind(i,j,bi,bj)
          vrelw(i,j) = vwind(i,j,bi,bj)
          cw(i,j,bi,bj) = 0. _d 0
          sw(i,j,bi,bj) = 0. _d 0
          sh(i,j,bi,bj) = 0. _d 0
          wStress(i,j,bi,bj) = 0. _d 0
         ENDDO
        ENDDO

        IF (useRelativeWind) THEN
C     Subtract uVel and vVel from uwind and vwind.
         DO j = 1,sNy
          DO i = 1,sNx
           urelw(i,j) = uwind(i,j,bi,bj) - 0.5 _d 0
     &          * (uVel(i,j,ks,bi,bj)+uVel(i+1,j,ks,bi,bj))
           vrelw(i,j) = vwind(i,j,bi,bj) - 0.5 _d 0
     &          * (vVel(i,j,ks,bi,bj)+vVel(i,j+1,ks,bi,bj))
          ENDDO
         ENDDO
#ifdef ALLOW_DIAGNOSTICS
         IF ( useDiagnostics ) THEN
          CALL DIAGNOSTICS_FILL( urelw,'EXFurelw',0,1,3,bi,bj,myThid )
          CALL DIAGNOSTICS_FILL( vrelw,'EXFvrelw',0,1,3,bi,bj,myThid )
         ENDIF
#endif
        ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE urelw (:,:)       = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE vrelw (:,:)       = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE wspeed(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif

        IF ( useAtmWind ) THEN

C--   Wind speed and direction.
         DO j = 1,sNy
          DO i = 1,sNx
           wsSq = urelw(i,j)*urelw(i,j)
     &          + vrelw(i,j)*vrelw(i,j)
           IF ( wsSq .NE. 0. _d 0 ) THEN
             wsLoc(i,j) = SQRT(wsSq)
             cw(i,j,bi,bj) = urelw(i,j)/wsLoc(i,j)
             sw(i,j,bi,bj) = vrelw(i,j)/wsLoc(i,j)
           ELSE
             wsLoc(i,j) = 0. _d 0
             cw(i,j,bi,bj) = 0. _d 0
             sw(i,j,bi,bj) = 0. _d 0
           ENDIF
          ENDDO
         ENDDO
         IF ( wspeedfile .EQ. ' ' ) THEN
C-    wind-speed is not loaded from file: save local array into common block
          DO j = 1,sNy
           DO i = 1,sNx
             wspeed(i,j,bi,bj) = wsLoc(i,j)
           ENDDO
          ENDDO
         ENDIF

#ifdef ALLOW_BULKFORMULAE
        ELSE
C--     case useAtmWind=F

C--   Wind stress and direction.
         DO j = 1,sNy
          DO i = 1,sNx
           IF ( stressIsOnCgrid ) THEN
             usSq = ( ustress(i,  j,bi,bj)*ustress(i  ,j,bi,bj)
     &               +ustress(i+1,j,bi,bj)*ustress(i+1,j,bi,bj)
     &               +vstress(i,j,  bi,bj)*vstress(i,j  ,bi,bj)
     &               +vstress(i,j+1,bi,bj)*vstress(i,j+1,bi,bj)
     &              )*0.5 _d 0
           ELSE
             usSq = ustress(i,j,bi,bj)*ustress(i,j,bi,bj)
     &             +vstress(i,j,bi,bj)*vstress(i,j,bi,bj)
           ENDIF
           IF ( usSq .NE. 0. _d 0 ) THEN
             wStress(i,j,bi,bj) = SQRT(usSq)
c            ustar = SQRT(usSq/atmrho)
             cw(i,j,bi,bj) = ustress(i,j,bi,bj)/wStress(i,j,bi,bj)
             sw(i,j,bi,bj) = vstress(i,j,bi,bj)/wStress(i,j,bi,bj)
           ELSE
             wStress(i,j,bi,bj) = 0. _d 0
             cw(i,j,bi,bj)      = 0. _d 0
             sw(i,j,bi,bj)      = 0. _d 0
           ENDIF
          ENDDO
         ENDDO

         IF ( wspeedfile .EQ. ' ' ) THEN
C--   wspeed is not loaded ; derive wind-speed by inversion of
C     wind-stress=fct(wind-speed) relation:
C             The variables us, sh and rdn have to be computed from
C             given wind stresses inverting relationship for neutral
C             drag coeff. cdn.
C             The inversion is based on linear and quadratic form of
C             cdn(umps); ustar can be directly computed from stress;
          recip_sqrtRhoA = 1. _d 0 / SQRT(atmrho)
          DO j = 1,sNy
           DO i = 1,sNx
C     check for zero wStress to please AD tools
            IF ( wStress(i,j,bi,bj) .LE. 0. _d 0 ) THEN
             ustar      = 0. _d 0
             wsloc(i,j) = 0. _d 0
            ELSE
             ustar = SQRT(wStress(i,j,bi,bj))*recip_sqrtRhoA
             IF ( ustar .LT. ustofu11 ) THEN
              tmp1 = -cquadrag_2/cquadrag_1*exf_half
              tmp2 = SQRT(tmp1*tmp1 + ustar*ustar/cquadrag_1)
              wsLoc(i,j) = SQRT(tmp1 + tmp2)
             ELSE
              tmp1 = clindrag_2/clindrag_1*oneThirdRL
              tmp2 = ustar*ustar/clindrag_1*exf_half
     &             - tmp1*tmp1*tmp1
              tmp3 = SQRT( ustar*ustar/clindrag_1*
     &             (ustar*ustar/clindrag_1*0.25 _d 0 - tmp1*tmp1*tmp1 )
     &                    )
              tmp4 = (tmp2 + tmp3)**oneThirdRL
              wsLoc(i,j) = tmp4 + tmp1*tmp1 / tmp4 - tmp1
c             wsLoc(i,j) = (tmp2 + tmp3)**oneThirdRL +
c    &             tmp1*tmp1 * (tmp2 + tmp3)**(-oneThirdRL) - tmp1
             ENDIF
            ENDIF
           ENDDO
          ENDDO
C-    save local array wind-speed to common block
          DO j = 1,sNy
           DO i = 1,sNx
            wspeed(i,j,bi,bj) = wsLoc(i,j)
           ENDDO
          ENDDO
C-    end if wspeedfile = empty
         ENDIF

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wspeed(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE uwind (:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE vwind (:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE cw    (:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
CADJ STORE sw    (:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif

C--   infer wind field from wind-speed & wind-stress direction
         DO j = 1,sNy
          DO i = 1,sNx
           uwind(i,j,bi,bj) = wspeed(i,j,bi,bj)*cw(i,j,bi,bj)
           vwind(i,j,bi,bj) = wspeed(i,j,bi,bj)*sw(i,j,bi,bj)
          ENDDO
         ENDDO

#endif /* ALLOW_BULKFORMULAE */
C--   end if/else useAtmWind
        ENDIF

#ifdef ALLOW_GENTIM2D_CONTROL
        DO j = 1,sNy
         DO i = 1,sNx
           do iarr = 1, maxCtrlTim2D
           if (xx_gentim2d_file(iarr)(1:9).EQ.'xx_wspeed')
     &       wspeed(i,j,bi,bj)=wspeed(i,j,bi,bj)+
     &                         xx_gentim2d(i,j,bi,bj,iarr)
           enddo
         ENDDO
        ENDDO
#endif

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE wspeed(:,:,bi,bj) = comlev1_bibj, key=ikey, byte=isbyte
#endif

C--   set wind-speed lower limit
        DO j = 1,sNy
         DO i = 1,sNx
           sh(i,j,bi,bj) = MAX(wspeed(i,j,bi,bj),uMin)
         ENDDO
        ENDDO

#if !(defined ALLOW_BULKFORMULAE) || !(defined ALLOW_ATM_TEMP)
C     Note: In case ALLOW_BULKFORMULAE and ALLOW_ATM_TEMP are defined,
C     wind-stress is computed (if needed) within S/R EXF_BULKFORMULAE
        IF ( useAtmWind ) THEN
c#ifdef ALLOW_ATM_WIND
C--   Computes wind-stress:
         DO j = 1,sNy
          DO i = 1,sNx
           wsm     = sh(i,j,bi,bj)
# ifdef  ALLOW_DRAG_LARGEYEAGER09
C     Large and Yeager (2009), Climate Dynamics, equation 11a/b
           tmpbulk =  cdrag_1/wsm + cdrag_2 + cdrag_3*wsm
     &          + cdrag_8 * wsm**6
           tmpbulk = exf_scal_BulkCdn * (
     &            ( halfRL - SIGN(halfRL, wsm-umax) )*tmpbulk
     &          + ( halfRL + SIGN(halfRL, wsm-umax) )*cdragMax
     &          )
# else
           tmpbulk = exf_scal_BulkCdn
     &             * ( cdrag_1/wsm + cdrag_2 + cdrag_3*wsm )
# endif
           ustress(i,j,bi,bj) = atmrho*tmpbulk*wspeed(i,j,bi,bj)
     &                        * urelw(i,j)
           vstress(i,j,bi,bj) = atmrho*tmpbulk*wspeed(i,j,bi,bj)
     &                        * vrelw(i,j)
          ENDDO
         ENDDO
c#else /* ALLOW_ATM_WIND */
c        STOP 'ABNORMAL END: S/R EXF_WIND: missing code for useAtmWind'
c#endif /* ALLOW_ATM_WIND */
C--   end if useAtmWind
        ENDIF
#endif /* ndef ALLOW_BULKFORMULAE or ndef ALLOW_ATM_TEMP */

C--   end bi,bj loops
       ENDDO
      ENDDO

      RETURN
      END
