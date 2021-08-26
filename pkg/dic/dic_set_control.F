#include "DIC_OPTIONS.h"
#ifdef ALLOW_CTRL
# include "CTRL_OPTIONS.h"
#endif

cphc$taf COMMON DIC_XX        adname = addic_xx
cphc$taf COMMON DIC_COST_CTRL adname = ADDIC_COST_CTRL

C !INTERFACE: ==========================================================
      SUBROUTINE DIC_SET_CONTROL( myThid )

C !DESCRIPTION:

C !USES: ===============================================================
      IMPLICIT NONE

C     == GLobal variables ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#ifdef DIC_BIOTIC
# include "DIC_VARS.h"
# include "DIC_CTRL.h"
#endif
#ifdef ALLOW_CTRL
#include "CTRL_SIZE.h"
#include "ctrl.h"
#include "ctrl_dummy.h"
#include "optim.h"
#endif
C     == Routine arguments ==
      INTEGER myThid

#ifdef ALLOW_CTRL
cph#ifdef DIC_BIOTIC
C     == Local arguments ==
#if (defined ALLOW_DIC_CONTROL) || \
    (defined ALLOW_GEN2D_CONTROL && defined ECCO_CTRL_DEPRECATED)
      INTEGER bi, bj
      INTEGER i, j
#endif
#if (defined ALLOW_GEN2D_CONTROL && defined ECCO_CTRL_DEPRECATED)
      INTEGER il
      LOGICAL doglobalread
      LOGICAL ladinit
      LOGICAL equal
      CHARACTER*( 80)   fnamegen2d
      _RL     fac
      _RL     tmpfld2d(1-olx:snx+olx,1-oly:sny+oly,nsx,nsy)
c     == external ==
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
#endif

c     == end of interface ==
CEOP

#if (defined ALLOW_GEN2D_CONTROL && defined ECCO_CTRL_DEPRECATED)
      doglobalread = .FALSE.
      ladinit      = .FALSE.

      equal = .TRUE.

      IF ( equal ) THEN
        fac = 1. _d 0
      ELSE
        fac = 0. _d 0
      ENDIF

      print*,'QQ alpha before', alpha(20,10,1,1)

      DO bj=myByLo(myThid),myByHi(myThid)
        DO bi=myBxLo(myThid),myBxHi(myThid)
          DO j = 1-OLy,sNy+OLy
            DO i = 1-OLx,sNx+OLx
              tmpfld2d(i,j,bi,bj) = 0. _d 0
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      il=ILNBLNK( xx_gen2d_file )
      WRITE(fnamegen2d(1:80),'(2a,i10.10)')
     &     xx_gen2d_file(1:il),'.',optimcycle
      CALL ACTIVE_READ_XY( fnamegen2d, tmpfld2d, 1,
     &                      doglobalread, ladinit, optimcycle,
     &                      myThid, xx_gen2d_dummy )

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1, sNy
          DO i = 1, sNx
             alpha (i,j,bi,bj) = alpha(i,j,bi,bj)
     &                         + fac*tmpfld2d(i,j,bi,bj)
          ENDDO
         ENDDO
       ENDDO
      ENDDO
cswd -- QQ limits!
cph                IF (alpha(i,j,bi,bj).GT.alphamax) THEN
cph                   alpha(i,j,bi,bj)=alphamax
cph                ENDIF
cph                IF (alpha(i,j,bi,bj).LT.alphamin) THEN
cph                   alpha(i,j,bi,bj)=alphamin
cph                ENDIF
cswd -- QQ limits
      print*,'QQ - preturb alpha', alpha(20,10,1,1),
     &                             tmpfld2d(20,10,1,1)
#endif /* ALLOW_GEN2D_CONTROL */

#ifdef ALLOW_DIC_CONTROL

      DO bj=myByLo(myThid),myByHi(myThid)
       DO bi=myBxLo(myThid),myBxHi(myThid)
         DO j = 1, sNy
          DO i = 1, sNx
c           feload(i,j,bi,bj) = feload(i,j,bi,bj)*(1. _d 0 +xx_dic(1))
            rain_ratio(i,j,bi,bj) =
     &                      rain_ratio(i,j,bi,bj)*(1. _d 0 +xx_dic(2))
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      _EXCH_XY_RL( alpha, myThid )
      _EXCH_XY_RL( rain_ratio, myThid )
c     _EXCH_XY_RL( feload, myThid )

      KScav       = KScav       * ( 1. _d 0 + xx_dic(3)*1. _d 6 )
      ligand_stab = ligand_stab * ( 1. _d 0 + xx_dic(4)*1. _d 6 )
      ligand_tot  = ligand_tot  * ( 1. _d 0 + xx_dic(5)*1. _d 6 )

      print *,'COST KScav = ', KScav
      print *,'COST ligand_stab = ', ligand_stab
      print *,'COST ligand_tot = ', ligand_tot

#endif /* ALLOW_DIC_CONTROL */

cph#endif /* DIC_BIOTIC */
#endif /* ALLOW_CTRL */

      RETURN
      END
