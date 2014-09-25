C $Header: /u/gcmpack/MITgcm/pkg/obcs/obcs_sponge.F,v 1.13 2014/09/25 00:37:05 dimitri Exp $
C $Name:  $

#include "OBCS_OPTIONS.h"

C--   File obcs_sponge.F:
C--    Contents:
C--    o OBCS_SPONGE_U
C--    o OBCS_SPONGE_V
C--    o OBCS_SPONGE_T
C--    o OBCS_SPONGE_S

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SPONGE_U(
     U                gU_arr,
     I                iMin,iMax,jMin,jMax, k, bi, bj,
     I                myTime, myIter, myThid )
C     *==========================================================*
C     | S/R OBCS_SPONGE_U
C     | o Contains problem specific forcing for zonal velocity.
C     *==========================================================*
C     | Adds a relaxation term to gU near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     == Routine arguments ==
C     gU_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
      _RL gU_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#if defined(ALLOW_OBCS) && defined(ALLOW_OBCS_SPONGE)
C     == Local variables ==
C     Loop counters
      INTEGER i, j, isl, jsl
      _RL urelax, lambda_obcs_u

      IF ( useOBCSsponge .AND. spongeThickness.NE.0 ) THEN

C Northern Open Boundary
#if defined(ALLOW_OBCS_NORTH) && defined(ALLOW_OBCS_SPONGE_N) && defined(ALLOW_OBCS_U_AT_NS)
      DO i=iMin,iMax
       IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Jn(i,bi,bj)-jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN

#ifdef USE_OBCS_LINEAR
          urelax = OBNu(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          urelax=(
     &      float(spongeThickness-jsl)*OBNu(i,k,bi,bj)
     &    + float(jsl)*uVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_u = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          IF (lambda_obcs_u.ne.0.) THEN
           lambda_obcs_u = 1. _d 0 / lambda_obcs_u
          ELSE
           lambda_obcs_u = 0. _d 0
          ENDIF

          gU_arr(i,j) = gU_arr(i,j)
     &    - _maskW(i,j,k,bi,bj) * lambda_obcs_u
     &    * ( uVel(i,j,k,bi,bj) - urelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_NORTH & ALLOW_OBCS_SPONGE_N & ALLOW_OBCS_U_AT_NS */

C Southern Open Boundary
#if defined(ALLOW_OBCS_SOUTH) && defined(ALLOW_OBCS_SPONGE_S) && defined(ALLOW_OBCS_U_AT_NS)
      DO i=iMin,iMax
       IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Js(i,bi,bj)+jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN

#ifdef USE_OBCS_LINEAR
          urelax= OBSu(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          urelax=(
     &      float(spongeThickness-jsl)*OBSu(i,k,bi,bj)
     &    + float(jsl)*uVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_u = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_u.ne.0.) then
            lambda_obcs_u = 1. _d 0 / lambda_obcs_u
          else
            lambda_obcs_u = 0. _d 0
          endif

          gU_arr(i,j) = gU_arr(i,j)
     &   - _maskW(i,j,k,bi,bj) * lambda_obcs_u
     &   * ( uVel(i,j,k,bi,bj) - urelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_SOUTH & ALLOW_OBCS_SPONGE_S & ALLOW_OBCS_U_AT_NS */

C Eastern Open Boundary
#if defined(ALLOW_OBCS_EAST) && defined(ALLOW_OBCS_SPONGE_E) && defined(ALLOW_OBCS_U_AT_EW)
      DO j=jMin,jMax
       IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Ie(j,bi,bj)-isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN

#ifdef USE_OBCS_LINEAR
          urelax=OBEu(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          urelax=(
     &      float(spongeThickness-isl)*OBEu(j,k,bi,bj)
     &    + float(isl)*uVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_u = (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_u.ne.0.) then
            lambda_obcs_u = 1. _d 0 / lambda_obcs_u
          else
            lambda_obcs_u = 0. _d 0
          endif

          gU_arr(i,j) = gU_arr(i,j)
     &    - _maskW(i,j,k,bi,bj) * lambda_obcs_u
     &    * ( uVel(i,j,k,bi,bj) - urelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_EAST & ALLOW_OBCS_SPONGE_E & ALLOW_OBCS_U_AT_EW */

C Western Open Boundary
#if defined(ALLOW_OBCS_WEST) && defined(ALLOW_OBCS_SPONGE_W) && defined(ALLOW_OBCS_U_AT_EW)
      DO j=jMin,jMax
       IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Iw(j,bi,bj)+isl+1

         IF ((i.ge.imin).and.(i.le.imax)) THEN

#ifdef USE_OBCS_LINEAR
          urelax= OBWu(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          urelax=(
     &      float(spongeThickness-isl)*OBWu(j,k,bi,bj)
     &    + float(isl)*uVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_u= (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_u.ne.0.) then
            lambda_obcs_u = 1. _d 0 / lambda_obcs_u
          else
            lambda_obcs_u = 0. _d 0
          endif

          gU_arr(i,j) = gU_arr(i,j)
     &    - _maskW(i,j,k,bi,bj) * lambda_obcs_u
     &    * ( uVel(i,j,k,bi,bj) - urelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_WEST & ALLOW_OBCS_SPONGE_W & ALLOW_OBCS_U_AT_EW */

      ENDIF

#endif /* ALLOW_OBCS & ALLOW_OBCS_SPONGE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SPONGE_V(
     U                gV_arr,
     I                iMin,iMax,jMin,jMax, k, bi, bj,
     I                myTime, myIter, myThid )
C     *==========================================================*
C     | S/R OBCS_SPONGE_V
C     | o Contains problem specific forcing for merid velocity.
C     *==========================================================*
C     | Adds a relaxation term to gV near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"

C     == Routine arguments ==
C     gV_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
      _RL gV_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#if defined(ALLOW_OBCS) && defined(ALLOW_OBCS_SPONGE)
C     == Local variables ==
C     Loop counters
      INTEGER i, j, isl, jsl
      _RL vrelax,lambda_obcs_v

      IF ( useOBCSsponge .AND. spongeThickness.NE.0 ) THEN

C Northern Open Boundary
#if defined(ALLOW_OBCS_NORTH) && defined(ALLOW_OBCS_SPONGE_N) && defined(ALLOW_OBCS_V_AT_NS)
      DO i=iMin,iMax
       IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Jn(i,bi,bj)-jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN

#ifdef USE_OBCS_LINEAR
          vrelax= OBNv(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          vrelax=(
     &      float(spongeThickness-jsl)*OBNv(i,k,bi,bj)
     &    + float(jsl)*vVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_v = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          IF (lambda_obcs_v.ne.0.) THEN
            lambda_obcs_v = 1. _d 0 / lambda_obcs_v
          ELSE
            lambda_obcs_v = 0. _d 0
          ENDIF

          gV_arr(i,j) = gV_arr(i,j)
     &    - _maskS(i,j,k,bi,bj) * lambda_obcs_v
     &    * ( vVel(i,j,k,bi,bj) - vrelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_NORTH & ALLOW_OBCS_SPONGE_N & ALLOW_OBCS_V_AT_NS */

C Southern Open Boundary
#if defined(ALLOW_OBCS_SOUTH) && defined(ALLOW_OBCS_SPONGE_S) && defined(ALLOW_OBCS_V_AT_NS)
      DO i=iMin,iMax
       IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Js(i,bi,bj)+jsl+1

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN

#ifdef USE_OBCS_LINEAR
          vrelax= OBSv(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          vrelax=(
     &      float(spongeThickness-jsl)*OBSv(i,k,bi,bj)
     &    + float(jsl)*vVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_v = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_v.ne.0.) then
            lambda_obcs_v = 1. _d 0 / lambda_obcs_v
          else
            lambda_obcs_v = 0. _d 0
          endif

          gV_arr(i,j) = gV_arr(i,j)
     &    - _maskS(i,j,k,bi,bj) * lambda_obcs_v
     &    * ( vVel(i,j,k,bi,bj) - vrelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_SOUTH & ALLOW_OBCS_SPONGE_S & ALLOW_OBCS_V_AT_NS */

C Eastern Open Boundary
#if defined(ALLOW_OBCS_EAST) && defined(ALLOW_OBCS_SPONGE_E) && defined(ALLOW_OBCS_V_AT_EW)
      DO j=jMin,jMax
       IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Ie(j,bi,bj)-isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN

#ifdef USE_OBCS_LINEAR
          vrelax= OBEv(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          vrelax=(
     &      float(spongeThickness-isl)*OBEv(j,k,bi,bj)
     &    + float(isl)*vVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_v = (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_v.ne.0.) then
            lambda_obcs_v = 1. _d 0 / lambda_obcs_v
          else
            lambda_obcs_v = 0. _d 0
          endif

          gV_arr(i,j) = gV_arr(i,j)
     &    - _maskS(i,j,k,bi,bj) * lambda_obcs_v
     &    * ( vVel(i,j,k,bi,bj) - vrelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_EAST & ALLOW_OBCS_SPONGE_E & ALLOW_OBCS_V_AT_EW */

C Western Open Boundary
#if defined(ALLOW_OBCS_WEST) && defined(ALLOW_OBCS_SPONGE_W) && defined(ALLOW_OBCS_V_AT_EW)
      DO j=jMin,jMax
       IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Iw(j,bi,bj)+isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN

#ifdef USE_OBCS_LINEAR
          vrelax = OBWv(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          vrelax=(
     &      float(spongeThickness-isl)*OBWv(j,k,bi,bj)
     &    + float(isl)*vVel(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_v = (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_v.ne.0.) then
            lambda_obcs_v = 1. _d 0 / lambda_obcs_v
          else
            lambda_obcs_v = 0. _d 0
          endif

          gV_arr(i,j) = gV_arr(i,j)
     &   - _maskS(i,j,k,bi,bj) * lambda_obcs_v
     &   * ( vVel(i,j,k,bi,bj) - vrelax )
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_WEST & ALLOW_OBCS_SPONGE_W & ALLOW_OBCS_V_AT_EW */

      ENDIF

#endif /* ALLOW_OBCS & ALLOW_OBCS_SPONGE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SPONGE_T(
     U                gT_arr,
     I                iMin,iMax,jMin,jMax, k, bi, bj,
     I                myTime, myIter, myThid )
C     *==========================================================*
C     | S/R OBCS_SPONGE_T
C     | o Contains problem specific forcing for temperature.
C     *==========================================================*
C     | Adds a relaxation term to gT near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     == Routine arguments ==
C     gT_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
      _RL gT_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#if defined(ALLOW_OBCS) && defined(ALLOW_OBCS_SPONGE)
C     == Local variables ==
C     Loop counters
      INTEGER i, j, isl, jsl
      _RL trelax, lambda_obcs_t

      IF ( useOBCSsponge .AND. spongeThickness.NE.0 ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
          kkey = (ikey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C Northern Open Boundary
#if defined(ALLOW_OBCS_NORTH) && defined(ALLOW_OBCS_SPONGE_N) && defined(ALLOW_OBCS_SPONGE_THETA)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBNt(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO i=iMin,iMax
       IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Jn(i,bi,bj)-jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN
           IF  (OBNt(i,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          trelax = OBNt(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          trelax=(
     &      float(spongeThickness-jsl)*OBNt(i,k,bi,bj)
     &    + float(jsl)*theta(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_t = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          IF (lambda_obcs_t.ne.0.) THEN
           lambda_obcs_t = 1. _d 0 / lambda_obcs_t
          ELSE
           lambda_obcs_t = 0. _d 0
          ENDIF

          gT_arr(i,j) = gT_arr(i,j)
     &    -  maskC(i,j,k,bi,bj) * lambda_obcs_t
     &    * ( theta(i,j,k,bi,bj) - trelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_NORTH & ALLOW_OBCS_SPONGE_N & ALLOW_OBCS_SPONGE_THETA */

C Southern Open Boundary
#if defined(ALLOW_OBCS_SOUTH) && defined(ALLOW_OBCS_SPONGE_S) && defined(ALLOW_OBCS_SPONGE_THETA)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBSt(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO i=iMin,iMax
       IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Js(i,bi,bj)+jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN
           IF  (OBSt(i,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          trelax= OBSt(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          trelax=(
     &      float(spongeThickness-jsl)*OBSt(i,k,bi,bj)
     &    + float(jsl)*theta(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_t = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_t.ne.0.) then
            lambda_obcs_t = 1. _d 0 / lambda_obcs_t
          else
            lambda_obcs_t = 0. _d 0
          endif

          gT_arr(i,j) = gT_arr(i,j)
     &   - maskC(i,j,k,bi,bj) * lambda_obcs_t
     &   * ( theta(i,j,k,bi,bj) - trelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_SOUTH & ALLOW_OBCS_SPONGE_S & ALLOW_OBCS_SPONGE_THETA */

C Eastern Open Boundary
#if defined(ALLOW_OBCS_EAST) && defined(ALLOW_OBCS_SPONGE_E) && defined(ALLOW_OBCS_SPONGE_THETA)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBEt(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO j=jMin,jMax
       IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Ie(j,bi,bj)-isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN
           IF  (OBEt(j,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          trelax = OBEt(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          trelax=(
     &      float(spongeThickness-isl)*OBEt(j,k,bi,bj)
     &    + float(isl)*theta(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_t = (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_t.ne.0.) then
            lambda_obcs_t = 1. _d 0 / lambda_obcs_t
          else
            lambda_obcs_t = 0. _d 0
          endif

          gT_arr(i,j) = gT_arr(i,j)
     &    - maskC(i,j,k,bi,bj) * lambda_obcs_t
     &    * ( theta(i,j,k,bi,bj) - trelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_EAST & ALLOW_OBCS_SPONGE_E & ALLOW_OBCS_SPONGE_THETA */

C Western Open Boundary
#if defined(ALLOW_OBCS_WEST) && defined(ALLOW_OBCS_SPONGE_W) && defined(ALLOW_OBCS_SPONGE_THETA)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBWt(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO j=jMin,jMax
       IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Iw(j,bi,bj)+isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN
           IF  (OBWt(j,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          trelax= OBWt(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          trelax=(
     &      float(spongeThickness-isl)*OBWt(j,k,bi,bj)
     &    + float(isl)*theta(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_t= (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_t .ne. 0.) then
            lambda_obcs_t = 1. _d 0 / lambda_obcs_t
          else
            lambda_obcs_t = 0. _d 0
          endif

          gT_arr(i,j) = gT_arr(i,j)
     &    - maskC(i,j,k,bi,bj) * lambda_obcs_t
     &    * ( theta(i,j,k,bi,bj) - trelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_WEST & ALLOW_OBCS_SPONGE_W & ALLOW_OBCS_SPONGE_THETA */

      ENDIF

#endif /* ALLOW_OBCS & ALLOW_OBCS_SPONGE */

      RETURN
      END

C---+----1----+----2----+----3----+----4----+----5----+----6----+----7-|--+----|

CStartOfInterface
      SUBROUTINE OBCS_SPONGE_S(
     U                gS_arr,
     I                iMin,iMax,jMin,jMax, k, bi, bj,
     I                myTime, myIter, myThid )
C     *==========================================================*
C     | S/R OBCS_SPONGE_S
C     | o Contains problem specific forcing for salinity.
C     *==========================================================*
C     | Adds a relaxation term to gS near Open-Boundaries
C     *==========================================================*
      IMPLICIT NONE

C     == Global data ==
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "DYNVARS.h"
#include "OBCS_PARAMS.h"
#include "OBCS_GRID.h"
#include "OBCS_FIELDS.h"
#ifdef ALLOW_AUTODIFF_TAMC
# include "tamc.h"
# include "tamc_keys.h"
#endif

C     == Routine arguments ==
C     gS_arr    :: the tendency array
C     iMin,iMax :: Working range of x-index for applying forcing.
C     jMin,jMax :: Working range of y-index for applying forcing.
C     k         :: Current vertical level index
C     bi,bj     :: Current tile indices
      _RL gS_arr(1-OLx:sNx+OLx,1-OLy:sNy+OLy)
      INTEGER iMin, iMax, jMin, jMax
      INTEGER k, bi, bj
      _RL myTime
      INTEGER myIter
      INTEGER myThid
CEndOfInterface

#if defined(ALLOW_OBCS) && defined(ALLOW_OBCS_SPONGE)
C     == Local variables ==
C     Loop counters
      INTEGER i, j, isl, jsl
      _RL srelax, lambda_obcs_s

      IF ( useOBCSsponge .AND. spongeThickness.NE.0 ) THEN

#ifdef ALLOW_AUTODIFF_TAMC
          act1 = bi - myBxLo(myThid)
          max1 = myBxHi(myThid) - myBxLo(myThid) + 1
          act2 = bj - myByLo(myThid)
          max2 = myByHi(myThid) - myByLo(myThid) + 1
          act3 = myThid - 1
          max3 = nTx*nTy
          act4 = ikey_dynamics - 1
          ikey = (act1 + 1) + act2*max1
     &                      + act3*max1*max2
     &                      + act4*max1*max2*max3
          kkey = (ikey-1)*Nr + k
#endif /* ALLOW_AUTODIFF_TAMC */

C Northern Open Boundary
#if defined(ALLOW_OBCS_NORTH) && defined(ALLOW_OBCS_SPONGE_N) && defined(ALLOW_OBCS_SPONGE_SALT)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBNs(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO i=iMin,iMax
       IF ( OB_Jn(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Jn(i,bi,bj)-jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN
           IF  (OBNs(i,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          srelax= OBNs(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          srelax=(
     &      float(spongeThickness-jsl)*OBNs(i,k,bi,bj)
     &    + float(jsl)*salt(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_s = (
     &      float(spongeThickness-jsl)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          IF (lambda_obcs_s.ne.0.) THEN
           lambda_obcs_s = 1. _d 0 / lambda_obcs_s
          ELSE
           lambda_obcs_s = 0. _d 0
          ENDIF

          gS_arr(i,j) = gS_arr(i,j)
     &    - maskC(i,j,k,bi,bj) * lambda_obcs_s
     &    * ( salt(i,j,k,bi,bj) - srelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_NORTH & ALLOW_OBCS_SPONGE_N & ALLOW_OBCS_SPONGE_SALT */

C Southern Open Boundary
#if defined(ALLOW_OBCS_SOUTH) && defined(ALLOW_OBCS_SPONGE_S) && defined(ALLOW_OBCS_SPONGE_SALT)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBSs(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO i=iMin,iMax
       IF ( OB_Js(i,bi,bj).NE.OB_indexNone ) THEN
        DO jsl= 1,spongeThickness
         j=OB_Js(i,bi,bj)+jsl

         IF ((j.ge.jmin).and.(j.le.jmax)) THEN
           IF  (OBSs(i,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          srelax= OBSs(i,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          srelax=(
     &      float(spongeThickness-jsl)*OBSs(i,k,bi,bj)
     &    + float(jsl)*salt(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_s = (
     &      float(spongeThickness)*Vrelaxobcsbound
     &    + float(jsl)*Vrelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_s.ne.0.) then
            lambda_obcs_s = 1. _d 0 / lambda_obcs_s
          else
            lambda_obcs_s = 0. _d 0
          endif

          gS_arr(i,j) = gS_arr(i,j)
     &   - maskC(i,j,k,bi,bj) * lambda_obcs_s
     &   * ( salt(i,j,k,bi,bj) - srelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_SOUTH & ALLOW_OBCS_SPONGE_S & ALLOW_OBCS_SPONGE_SALT */

C Eastern Open Boundary
#if defined(ALLOW_OBCS_EAST) && defined(ALLOW_OBCS_SPONGE_E) && defined(ALLOW_OBCS_SPONGE_SALT)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBEs(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO j=jMin,jMax
       IF ( OB_Ie(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Ie(j,bi,bj)-isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN
           IF  (OBEs(j,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          srelax=  OBEs(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          srelax=(
     &      float(spongeThickness-isl)*OBEs(j,k,bi,bj)
     &    + float(isl)*salt(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_s = (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_s.ne.0.) then
            lambda_obcs_s = 1. _d 0 / lambda_obcs_s
          else
            lambda_obcs_s = 0. _d 0
          endif

          gS_arr(i,j) = gS_arr(i,j)
     &    - maskC(i,j,k,bi,bj) * lambda_obcs_s
     &    * ( salt(i,j,k,bi,bj) - srelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_EAST & ALLOW_OBCS_SPONGE_E & ALLOW_OBCS_SPONGE_SALT */

C Western Open Boundary
#if defined(ALLOW_OBCS_WEST) && defined(ALLOW_OBCS_SPONGE_W) && defined(ALLOW_OBCS_SPONGE_SALT)

#ifdef ALLOW_AUTODIFF_TAMC
CADJ STORE OBWs(:,k,bi,bj)  = comlev1_bibj_k, key=kkey, byte=isbyte
#endif

      DO j=jMin,jMax
       IF ( OB_Iw(j,bi,bj).NE.OB_indexNone ) THEN
        DO isl= 1,spongeThickness
         i=OB_Iw(j,bi,bj)+isl

         IF ((i.ge.imin).and.(i.le.imax)) THEN
           IF  (OBWs(j,k,bi,bj).ne. 0.d0) then

#ifdef USE_OBCS_LINEAR
          srelax= OBWs(j,k,bi,bj)
#else /* ifndef USE_OBCS_LINEAR */
          srelax=(
     &      float(spongeThickness-isl)*OBWs(j,k,bi,bj)
     &    + float(isl)*salt(i,j,k,bi,bj) )
     &    / float(spongeThickness)
#endif /* USE_OBCS_LINEAR */

          lambda_obcs_s= (
     &      float(spongeThickness-isl)*Urelaxobcsbound
     &    + float(isl)*Urelaxobcsinner)
     &    / float(spongeThickness)

          if (lambda_obcs_s.ne.0.) then
            lambda_obcs_s = 1. _d 0 / lambda_obcs_s
          else
            lambda_obcs_s = 0. _d 0
          endif

          gS_arr(i,j) = gS_arr(i,j)
     &    - maskC(i,j,k,bi,bj) * lambda_obcs_s
     &    * ( salt(i,j,k,bi,bj) - srelax )
           endif
         ENDIF

        ENDDO
       ENDIF
      ENDDO
#endif /* ALLOW_OBCS_WEST & ALLOW_OBCS_SPONGE_W & ALLOW_OBCS_SPONGE_SALT */

      ENDIF

#endif /* ALLOW_OBCS & ALLOW_OBCS_SPONGE */

      RETURN
      END
