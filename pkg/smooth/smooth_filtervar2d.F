#include "SMOOTH_OPTIONS.h"
#ifdef ALLOW_SHELFICE
# include "SHELFICE_OPTIONS.h"
#endif

      subroutine smooth_filtervar2D (smoothOpNb,mythid)

C     *==========================================================*
C     | SUBROUTINE smooth_filtervar2D
C     | o Routine that computes the filter variance
C     |   field associated with a diffusion operator, as part
C     |   a 2D spatial correlation operator (smooth_correld2D.F)
C     |   See Weaver and Courtier 01 for details.
C     *==========================================================*

      IMPLICIT NONE
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "SMOOTH.h"
#if (defined ALLOW_SHI2D_CONTROL) || (defined ALLOW_SHIFWFLX_CONTROL)
# include "SHELFICE.h"
#endif

      integer smoothOpNb, myThid

      Real*8   port_rand, port_rand_norm
      EXTERNAL PORT_RAND, PORT_RAND_NORM

      integer i,j,k, bi, bj, ii, jj, kk
      integer itlo,ithi,jtlo,jthi
      integer diLoc,djLoc,dkLoc
      integer nbRand, nbt_in
      character*( 80) fnamegeneric
      _RL smoothTmpFld (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL smoothTmpVar (1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RL smoothTmpMean(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)
      _RS smooth2Dmask(1-OLx:sNx+OLx,1-OLy:sNy+OLy,nSx,nSy)

      jtlo = mybylo(mythid)
      jthi = mybyhi(mythid)
      itlo = mybxlo(mythid)
      ithi = mybxhi(mythid)

c --- allow a different mask other than maskC
c     Note: this is essentially a copy of ctrl_get_mask
c     but is repeated here for package independence
c     since this subroutine is typically used simply to get
c     filter variance without actually running the model
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO j = 1,sNy
         DO i = 1,sNx
          if (smooth2DmaskName(smoothOpNb)(1:5).eq.'maskC') then
           smooth2Dmask(i,j,bi,bj) = maskC(i,j,1,bi,bj)
          elseif (smooth2DmaskName(smoothOpNb)(1:5).eq.'maskW') then
             smooth2Dmask(i,j,bi,bj) = maskW(i,j,1,bi,bj)
          elseif (smooth2DmaskName(smoothOpNb)(1:5).eq.'maskS') then
             smooth2Dmask(i,j,bi,bj) = maskS(i,j,1,bi,bj)
#if (defined ALLOW_SHI2D_CONTROL) || (defined ALLOW_SHIFWFLX_CONTROL)
          elseif (smooth2DmaskName(smoothOpNb)(1:5).eq.'maskI') then
             smooth2Dmask(i,j,bi,bj) = maskSHI(i,j,1,bi,bj)
#endif
          endif
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c if smooth2Dfilter(smoothOpNb)=0: the filter variance field
c has been computed earlier and is already in the run directory
c so this routine does not do anything

      IF (smooth2Dfilter(smoothOpNb).NE.0) then

      nbt_in=smooth2Dnbt(smoothOpNb)/2

c read smoothing [i.e diffusion] operator:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Doperator',smoothOpNb
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1,smooth2D_Kux,1,1,mythid)
      CALL READ_REC_3D_RL(fnamegeneric,smoothprec,
     &           1,smooth2D_Kvy,2,1,mythid)
      CALL EXCH_XY_RL ( smooth2D_Kux, myThid )
      CALL EXCH_XY_RL ( smooth2D_Kvy, myThid )

c initialize filter variance field:
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smooth2Dnorm(i,j,bi,bj)=0.
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      IF (smooth2Dfilter(smoothOpNb).EQ.2) then
c compute the normalization matrix using the approximate method
c
c This method can be quite expensive -- so that the approximate
c method (see below) is usually the prefered one.
c The exact method can be used to check the accuracy
c of the approximate method results (that can be predicted).
c
c note: the exact method requires the adjoint of smooth_diff2D.F (see below)

      diLoc=15 !int(5*smooth_L/smooth_dx)
      djLoc=20 !int(5*smooth_L/smooth_dx)

      DO ii=1,diLoc
      DO jj=1,djLoc

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smoothTmpFld(i,j,bi,bj)=0.
          ENDDO
         ENDDO

         DO j=jj,sNy,djLoc
          DO i=ii,sNx,diLoc
           smoothTmpFld(i,j,bi,bj)=1.
          ENDDO
         ENDDO
       ENDDO
      ENDDO

c note: as we go to adjoint part, we need to have 0 in overlaps
c       so we must NOT have done an exchange for smoothTmpFld

c adjoint:
      WRITE(errorMessageUnit,'(A,/,A)' )
     & "you need to have adsmooth_diff2D compiled and then:",
     & "uncomment the line below and comment the stop"
      CALL ALL_PROC_DIE( myThid )
      STOP 'ABNORMAL END: S/R smooth_filtervar2D'
c      call adsmooth_diff2D(smoothTmpFld,smooth2dmask,nbt_in,mythid)

c division by sqrt(area)*sqrt(area) [1 to end adj, 1 to begin fwd]
      DO bj = jtlo,jthi
       DO bi = itlo,ithi
        DO j = 1,sNy
         DO i = 1,sNx
c division by ~volume:
      smoothTmpFld(i,j,bi,bj)=smoothTmpFld(i,j,bi,bj)
     & *recip_rA(i,j,bi,bj)
         ENDDO
        ENDDO
       ENDDO
      ENDDO

c coming out of adjoint part: overlaps are 0
c going in fwd part: we need to fill them up
      CALL EXCH_XY_RL ( smoothTmpFld,myThid )

c fwd:
      CALL smooth_diff2D(smoothTmpFld,smooth2dmask,nbt_in,mythid)

c convert variance to normalization factor:
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=jj,sNy,djLoc
          DO i=ii,sNx,diLoc
           if (smooth2dmask(i,j,bi,bj).NE.0) then
              smooth2Dnorm(i,j,bi,bj)=
     &        1/sqrt(smoothTmpFld(i,j,bi,bj))
           endif
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      ENDDO      !DO ii=1,diLoc
      ENDDO      !DO jj=1,djLoc

      ELSEIF (smooth2Dfilter(smoothOpNb).EQ.1) then
c compute the normalization matrix using the approximate method

      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smoothTmpMean(i,j,bi,bj)   = 0. _d 0
           smoothTmpVar(i,j,bi,bj)   = 0. _d 0
          ENDDO
         ENDDO
       ENDDO
      ENDDO

c initialize random number generator
      smoothTmpFld(1,1,1,1)=port_rand(1.d0)
      nbRand=1000

         DO ii=1,nbRand
            WRITE(standardMessageUnit,'(A,I4,A,I4)')
     & 'smooth_filtervar2D: ',ii,' members done out of',nbRand

c fill smoothTmpFld with random numbers:
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           smoothTmpFld(i,j,bi,bj)   = 0. _d 0
           if (smooth2dmask(i,j,bi,bj).NE.0) then
           smoothTmpFld(i,j,bi,bj)=port_rand_norm()
           endif
c division by sqrt(area):
      smoothTmpFld(i,j,bi,bj)=smoothTmpFld(i,j,bi,bj)
     & *sqrt(recip_rA(i,j,bi,bj))
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      CALL EXCH_XY_RL ( smoothTmpFld, myThid )

c smooth random number field
      call smooth_diff2D(smoothTmpFld,smooth2dmask,nbt_in,mythid)

c accumulate statistics (to compute the variance later)
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
      smoothTmpVar(i,j,bi,bj)=smoothTmpVar(i,j,bi,bj)
     & +smoothTmpFld(i,j,bi,bj)*smoothTmpFld(i,j,bi,bj)/nbRand
      smoothTmpMean(i,j,bi,bj)=smoothTmpMean(i,j,bi,bj)
     & +smoothTmpFld(i,j,bi,bj)/nbRand
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      ENDDO

c compute variance and convert it to normalization factor:
      DO bj=jtlo,jthi
       DO bi=itlo,ithi
         DO j=1-OLy,sNy+OLy
          DO i=1-OLx,sNx+OLx
           if (smooth2dmask(i,j,bi,bj).NE.0) then
           smooth2Dnorm(i,j,bi,bj)=
     & 1/sqrt ( nbRand/(nbRand-1)* ( smoothTmpVar(i,j,bi,bj) -
     & smoothTmpMean(i,j,bi,bj)*smoothTmpMean(i,j,bi,bj)
     &  )  )
           endif
          ENDDO
         ENDDO
       ENDDO
      ENDDO

      ENDIF

c write smooth2Dnorm to file:
      write(fnamegeneric(1:80),'(1a,i3.3)')
     &    'smooth2Dnorm',smoothOpNb
      CALL WRITE_REC_3D_RL(fnamegeneric,smoothprec,
     &            1,smooth2Dnorm,1,1,mythid)
      CALL EXCH_XY_RL ( smooth2Dnorm,  myThid )

      ENDIF

      END

