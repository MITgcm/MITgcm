C $Id: eparam.F,v 1.1 1998/05/25 20:21:04 cnh Exp $
#include "CPP_OPTIONS.h"
#include "CPP_MACROS.h"
C******************************************************************************
C     Procedure name: EPARAM_SET_MIXING
C           Function: 
C            Written: Fall 1995, Moto Nakamura, M.I.T.
C           Comments: 
C******************************************************************************
CStartofinterface
      SUBROUTINE EPARAM_SET_MIXING(
     I    T,S,U,V,W,
     O    difH,difCX,difCY,
     O    slpxUDif,slpyUDif,slpxVDif,slpyVDif,slpxWDif,slpYwDif,slpFac,
     O    uTotal,vTotal,wTotal,
     I    DumpFlag )
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "STRINGS.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "MASKS.h"
#include "EXTERNAL.h"
C
C     ======== Routine arguments ======================
      REAL U       (_I3(Nz,Nx,Ny))
      REAL V       (_I3(Nz,Nx,Ny))
      REAL W       (_I3(Nz,Nx,Ny))
      REAL T       (_I3(Nz,Nx,Ny))
      REAL S       (_I3(Nz,Nx,Ny))
      REAL difh    (_I3(Nz,Nx,Ny))
      REAL difcx   (_I3(Nz,Nx,Ny))
      REAL difcy   (_I3(Nz,Nx,Ny))
      REAL slpxudif(_I3(Nz,Nx,Ny))
      REAL slpyudif(_I3(Nz,Nx,Ny))
      REAL slpxvdif(_I3(Nz,Nx,Ny))
      REAL slpyvdif(_I3(Nz,Nx,Ny))
      REAL slpxwdif(_I3(Nz,Nx,Ny))
      REAL slpywdif(_I3(Nz,Nx,Ny))
      REAL slpfac
      REAL utotal  (_I3(Nz,Nx,Ny))
      REAL vtotal  (_I3(Nz,Nx,Ny))
      REAL wtotal  (_I3(Nz,Nx,Ny))
      LOGICAL DumpFlag
CEndofinterface
C     ======== Local variables ========================
      CHARACTER*10 suffix
      INTEGER      iErr
      LOGICAL LFIRST
      DATA LFIRST /.TRUE./
      SAVE LFIRST
      
C     Mix layer variables
      REAL mld     (Nx,Ny)
      INTEGER  mlvu(Nx,Ny)
      INTEGER  mlvv(Nx,Ny)
      INTEGER  mlv (Nx,Ny)
C     Index of maximum mix layer depth
      INTEGER mlvmax
C     Slope variables
      REAL slpxgm  (_I3(Nz,Nx,Ny))
      REAL slpygm  (_I3(Nz,Nx,Ny))
      SAVE slpxgm,slpygm
C     Eddy velocities
      REAL ueddy   (_I3(Nz,Nx,Ny))
      REAL veddy   (_I3(Nz,Nx,Ny))
      REAL weddy   (_I3(Nz,Nx,Ny))
      SAVE ueddy, veddy
C     Density gradients
      REAL sigx    (_I3(Nz,Nx,Ny))
      REAL sigy    (_I3(Nz,Nx,Ny))
      REAL sigz    (_I3(Nz,Nx,Ny))
      save sigx,sigy,sigz
C     Switches to turn on/off mixing options
      LOGICAL gmflg,khflg,gsflg
      SAVE gmflg,khflg,gsflg
C     Depth averaged Richardson number
      REAL  rich(Nx,Ny)
      REAL  DC(Nx,Ny)
      SAVE DC
      INTEGER I, J, IO, JO, IB, JB, IP, JP
      REAL XC, YC, D
      DATA IB /3/                       ! Coast search window in I
      SAVE IB
      DATA JB /3/
      SAVE JB                           ! Coast search window in J
      INTEGER ioerr
C     ================ Begin routine ===========================================
      IF( LFIRST )THEN
c      Set flags for GM, GS, and variable Kh.  
c      khflg=true ... time- and space-dependent Kh is calculated
c      gmflg=true ... eddy-induced velocity is calculated, full-slope diffusion
c      gsflg=true ... khflg is set true, half-slope diffusion
c      If both gmflg and gsflg are on, the run will be terminated
       IF( TrDifScheme .eq.  horizDif )THEN
        khflg = .false.
        gmflg = .false.
        gsflg = .false.
        slpfac = 0.
       ELSEIF( TrDifScheme .eq. varKhonly  )THEN
        khflg = .true.
        gmflg = .false.
        gsflg = .false.
        slpfac = 0.
       ELSEIF( TrDifScheme .eq. grst  )THEN
        khflg = .true.
        gmflg = .false.
        gsflg = .true.
        slpfac = 0.5
       ELSEIF( TrDifScheme .eq. gmconKh  )THEN
        khflg = .false.
        gmflg = .true.
        gsflg = .false.
        slpfac = 1.
       ELSEIF( TrDifScheme .eq. gmvarKh  )THEN
        khflg = .true.
        gmflg = .true.
        gsflg = .false.
        slpfac = 1.
       ELSE
        STOP'Improper specification of TrDifScheme variable'
       ENDIF
C      Set profile for GMGS mixing coefficinet
       CALL EPARAM_SET_KH_VERT_PROF
C      Kh is calculated every dkcalcfreq seconds
C      isentropic slope is calculated every dslpcalcfreq seconds
C      dkcalcfreq must be a multiple of dslpcalcfreq
       IF( MOD( INT(dkcalcfreq),INT(dslpcalcfreq) ).ne. 0 )THEN
        STOP'ERROR: dkcalcfreq must be multiple of dslpcalcfreq'
       ENDIF
C      Default to single value for horizontal mixing
C      NOTE:  T and S mixing rates are the same value
       difh =  a2TempXY    
       difcx = -difh
       difcy = -difh
C      Set the vertical variation in Kh
       CALL EPARAM_SET_KH_XYZ(difh,difcx,difcy)
CcnhDebugStarts
       WRITE(0,*) ' difH, MAX, MIN ',MAXVAL(difH),MINVAL(difH)
CcnhDebugEnds
       slpxudif = 0.
       slpyudif = 0.
       slpxvdif = 0.
       slpyvdif = 0.
       slpxwdif = 0.
       slpywdif = 0.
       slpxgm = 0.
       slpygm = 0.
       ueddy = 0.
       veddy = 0.
       rich = 0.
C      Calcuate nearest offshore distance
C      Distance is in grid cells for now
       ioErr = 0
       WRITE(suffix,'(I10.10)') 0
       CALL READ_DUMP_2D ( DC, Nx, Ny,     'dc.', suffix, ioerr )
       IF ( ioErr .NE. 0 ) THEN
        DC  = 10.
        DO J = 1, Ny
        DO I = 1, Nx
         IF ( pMask(_I3(1,I,J)) .EQ. LAND ) GOTO 10
         XC=0.
         YC=0.
         DO IO=1,IB
          IP=I+IO
          IF ( IP .GT. Nx ) IP = MOD(IP,Nx)
          XC=XC+1
          DO JO=0,JB
           JP=J+JO
           IF ( JP .GT. Ny) JP = MOD(JP,Ny)
           YC=JO
           D=(XC**2+YC**2)**0.5
           IF ( pMask(_I3(1,IP,JP)) .EQ. LAND ) THEN
            DC(I,J)=MIN(DC(I,J),D)
           ENDIF
          ENDDO
         ENDDO
         XC=0.
         YC=0.
         DO IO=1,IB
          IP=I-IO
          IF ( IP .LT. 1  ) IP = Nx-IP
          XC=XC+1
          DO JO=0,JB
           JP=J-JO
           IF ( JP .LT. 1  ) JP = Ny-JP
           YC=JO
           D=(XC**2+YC**2)**0.5
           IF ( pMask(_I3(1,IP,JP)) .EQ. LAND ) THEN
            DC(I,J)=MIN(DC(I,J),D)
           ENDIF
          ENDDO
         ENDDO
         XC=0.
         DO IO=0,IB
          IP=I+IO
          IF ( IP .GT. Nx ) IP = MOD(IP,Nx)
          XC=IO
          YC=0.
          DO JO=1,JB
           JP=J-JO
           IF ( IP .LT. 1  ) IP = Nx-IP
           YC=YC+1
           D=(XC**2+YC**2)**0.5
           IF ( pMask(_I3(1,IP,JP)) .EQ. LAND ) THEN
            DC(I,J)=MIN(DC(I,J),D)
           ENDIF
          ENDDO
         ENDDO
         XC=0.
         DO IO=0,IB
          IP=I-IO
          IF ( IP .LT. 1  ) IP = Nx-IP
          XC=IO
          YC=0.
          DO JO=1,JB
           JP=J+JO
           IF ( JP .GT. Ny) JP = MOD(JP,Ny)
           YC=YC+1
           D=(XC**2+YC**2)**0.5
           IF ( pMask(_I3(1,IP,JP)) .EQ. LAND ) THEN
            DC(I,J)=MIN(DC(I,J),D)
           ENDIF
          ENDDO
         ENDDO
   10    CONTINUE
        ENDDO
        ENDDO
       ENDIF
       WHERE ( DC .GE. 10.            ) DC = 10
       WHERE ( pMask(_I3(1,:,:)) .EQ. LAND ) DC = 0
       ioErr = 0
       WRITE(suffix,'(I10.10)') 0
       CALL WRITE_DUMP_2D ( DC, Nx, Ny,     'dc.', suffix, ioerr )
      ENDIF
      
C     Compute mix layer base and density gradients
      IF (gmflg .or. khflg) THEN
       IF ( INT(currentTime/dslpcalcfreq) .NE.
     &      INT((currentTime-delT)/dslpcalcfreq) 
     &      .OR. LFIRST      ) THEN
        CALL EPARAM_SET_SIG_XYZ(
     I   T,S,
     O   sigx,sigy,sigz )
C       Set the level of the bottom of the mixed layer at sides of each box
        mlv = 0
        mlvmax = 0
        CALL DIAGS_CALC_MLD(
     I                     T, S )
        CALL DIAGS_GET_MLD (
     O                     mld, mlv )
C       If the average of the mixed layer level at two neighboring P-points 
C       is non-integer, say 2.5, add 0.5 
        mlvu = (mlv+Cshift(mlv,DIM=1,SHIFT=-1)+1)/2
        mlvv = (mlv+Cshift(mlv,DIM=2,SHIFT=-1)+1)/2
        mlvmax = maxval(mlv)
       ENDIF
      ENDIF

C     Compute Kh and Kv
      IF (khflg) THEN
        IF ( INT(currentTime/dkcalcfreq) .NE.
     &     INT((currentTime-delT)/dkcalcfreq) 
     &      .OR. LFIRST      ) THEN
          CALL EPARAM_GET_KH_KV(
     I        sigx,sigy,sigz,mlvu,mlvv,dc,
     O        difh,difcx,difcy,rich)
        ENDIF
      ENDIF
C     Compute slopes
      IF (gsflg .or. gmflg) THEN
        IF ( INT(currentTime/dslpcalcfreq) .NE.
     &      INT((currentTime-delT)/dslpcalcfreq) 
     &      .OR. LFIRST      ) THEN
          CALL EPARAM_SET_SLP(
     I        sigx,sigy,sigz,mlvu,mlvv,mlvmax,
     O        slpxgm,slpygm,slpxudif,slpyudif,slpxvdif,slpyvdif,slpxwdif,slpywdif)
        ENDIF
        IF (gmflg) THEN
          CALL EPARAM_SET_EDDY_VEL(
     I        difcx,difcy,slpxgm,slpygm,
     O        ueddy,veddy)
        ENDIF
      ENDIF
C     Compute total velocity
      utotal = u
      vtotal = v
      wtotal = w
      IF (gmflg) THEN
        CALL EPARAM_SET_TTL_VEL(
     I      u,v,ueddy,veddy,
     O      utotal,vtotal,wtotal)
      ENDIF
C     Dump state of mixing variables
      IF(DumpFlag .OR.
     &   DIFFERENT_MULTIPLE(30.D0*OneDay,currentTime,currentTime-delT) .OR.
     &   LFIRST )THEN
        iErr = 0.
        WRITE(suffix,'(I10.10)') nIter
        CALL WRITE_DUMP_3D( difh, Nx, Ny, Nz, 'Kh.', suffix, iErr)
        CALL WRITE_DUMP_2D( rich, Nx, Ny,     'RI.', suffix, iErr)
        CALL WRITE_DUMP_3D( slpxwdif, Nx, Ny, Nz, 'Slpxwd.', suffix, iErr)
        CALL WRITE_DUMP_3D( slpywdif, Nx, Ny, Nz, 'Slpywd.', suffix, iErr)
        CALL WRITE_DUMP_3D( slpxgm, Nx, Ny, Nz, 'Slpxgm.', suffix, iErr)
        CALL WRITE_DUMP_3D( slpygm, Nx, Ny, Nz, 'Slpygm.', suffix, iErr)
        CALL WRITE_DUMP_3D( sigx, Nx, Ny, Nz, 'SigmaX.', suffix, iErr)
        CALL WRITE_DUMP_3D( sigy, Nx, Ny, Nz, 'SigmaY.', suffix, iErr)
        CALL WRITE_DUMP_3D( sigz, Nx, Ny, Nz, 'SigmaZ.', suffix, iErr)
        CALL WRITE_DUMP_3D( ueddy, Nx, Ny, Nz, 'UEDDY.',  suffix, iErr)
        CALL WRITE_DUMP_3D( veddy, Nx, Ny, Nz, 'VEDDY.',  suffix, iErr)
        weddy = wtotal - w
        CALL WRITE_DUMP_3D( weddy, Nx, Ny, Nz, 'WEDDY.',  suffix, iErr)
      ENDIF
      LFIRST = .FALSE.
C
      RETURN
      END
C******************************************************************************
C     Procedure name: EPARAM_SET_KH_VERT_PROF
C           Function: Calculate exponential factors for the vertical profile of 
c                     Kh
C            Written: Summer 1996, Moto Nakamura, M.I.T.
C           Comments: 
C******************************************************************************
CStartofinterface
      SUBROUTINE EPARAM_SET_KH_VERT_PROF
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "MASKS.h"
#include "OPERATORS.h"
CEndOfInterface
C     ======== Local variables ========================= 
      INTEGER K
      REAL DmDl(Nz)
      REAL Wdepth(Nz)
      Wdepth(1) =  0.
      DO K=2,Nz
         Wdepth(k) = Wdepth(k-1) + delPs(k-1)/(RoNil*g)
      ENDDO
      DO K=1,Nz-1
        dmdl(k) = (Wdepth(k) + Wdepth(k+1))*0.5
      ENDDO
      DO K=1,Nz
        IF (verticalProfKh) THEN
          zFacUV(k) = exp(-dmdl(k)  /dscalekh)
          zFacW(k)  = exp(-Wdepth(k)/dscalekh)
        ELSE
          zFacUV(k) = 1.
          zFacW (k) = 1.
        ENDIF
      ENDDO
      RETURN
      END
C     Procedure name: EPARAM_SET_EDDY_VEL
C           Function: Diagnose the eddy-induced velocity for GM scheme
C            Written: Fall 1995, Moto Nakamura, M.I.T.
C           Comments: puts slope vector at midpoint of sides of each box
CStartofinterface
      SUBROUTINE EPARAM_SET_EDDY_VEL (
     I    difx,dify,slpx,slpy,
     O    ueddy,veddy)
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "OLDG.h"
#include "MASKS.h"
C     ======== Routine arguments ======================
      REAL difx  (_I3(Nz,Nx,Ny))
      REAL dify  (_I3(Nz,Nx,Ny))
      REAL slpx  (_I3(Nz,Nx,Ny))
      REAL slpy  (_I3(Nz,Nx,Ny))
      REAL ueddy   (_I3(Nz,Nx,Ny))
      REAL veddy   (_I3(Nz,Nx,Ny))
C     ======== Local variables ========================
      REAL fvectx (_I3(Nz,Nx,Ny))
      REAL fvecty (_I3(Nz,Nx,Ny))
      real dmdl(nz),hh(nz)
      integer k
      save dmdl
      save hh

      REAL Wdepth(Nz)
      LOGICAL firstflg
      DATA firstflg / .true. /
      SAVE firstflg
      IF( firstflg )THEN
        Wdepth(1) =  0.
        DO K=2,Nz
           Wdepth(k) = Wdepth(k-1) + delPs(k-1)/(RoNil*g)
        ENDDO
        DO K=1,Nz-1
           dmdl(k) = (Wdepth(k) + Wdepth(k+1))*0.5
        ENDDO
        DO K=1,Nz
           hh(k) = delPs(k)/(RoNil*g)
        ENDDO
        firstflg = .false.
      ENDIF


      ueddy = 0.
      veddy = 0.
      fvectx = 0.
      fvecty = 0.
c calculate the eddy-induced velocities.  slpx and slpy are given at  
c U- and V-points, respectively.  difx and dify are also given at  
c U- and V-points, respectively.  note difx and dify have negative sign 
c due to the sign convention of the diffusion terms in inc_*.F.  
c assume that F=K*del.(grad(sigma)/(dsigma/dz)) = 0 at the top and bottom
c and linearly interpolate from U- and V-points to midpoints of top and 
c bottom sides of each box.   
c we dont have to worry about zero values on land, since F is being 
c calculated at midpoints of sides of each box.  the negative sign is 
c to cancel the negative sign of difx and dify.  
      do k=2,nz-1
        fvectx(_I3(k,:,:)) = -umask(_I3(k,:,:))*wmask(_I3(k,:,:))*difx(_I3(k,:,:))*
     & (slpx(_I3(k-1,:,:))-hh(k-1)*(slpx(_I3(k-1,:,:))-slpx(_I3(k,:,:)))/
     & (dmdl(k)-dmdl(k-1))/2.)
        fvecty(_I3(k,:,:)) = -vmask(_I3(k,:,:))*wmask(_I3(k,:,:))*dify(_I3(k,:,:))*
     & (slpy(_I3(k-1,:,:))-hh(k-1)*(slpy(_I3(k-1,:,:))-slpy(_I3(k,:,:)))/
     & (dmdl(k)-dmdl(k-1))/2.)
      end do
      do k=1,nz-1
        ueddy(_I3(k,:,:)) = umask(_I3(k,:,:))*(fvectx(_I3(k,:,:))-fvectx(_I3(k+1,:,:)))/hh(k)
        veddy(_I3(k,:,:)) = vmask(_I3(k,:,:))*(fvecty(_I3(k,:,:))-fvecty(_I3(k+1,:,:)))/hh(k)
      end do
      RETURN
      END
C******************************************************************************
C     Procedure name: EPARAM_GET_KH_KV
C           Function: Calculate depth-averaged Ri for every side and 
C             compute Kh, Kx, and Ky for Redis scheme.   
C            Written: Fall 1995, Moto Nakamura, M.I.T.
C           Comments: 
CStartofinterface
      SUBROUTINE EPARAM_GET_KH_KV(
     I    sigx,sigy,sigz,kmixu,kmixv,dc,
     O    difh,difx,dify,rich)
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "MASKS.h"
#include "OPERATORS.h"
C     ======== Routine arguments ======================
      REAL sigx  (_I3(Nz,Nx,Ny))
      REAL sigy  (_I3(Nz,Nx,Ny))
      REAL sigz (_I3(Nz,Nx,Ny))
      REAL difh (_I3(Nz,Nx,Ny))
      REAL difx (_I3(Nz,Nx,Ny))
      REAL dify (_I3(Nz,Nx,Ny))
      REAL  DC(Nx,Ny)
      real hh(Nz),dsdxu(nx,ny),richu(nx,ny),rich(nx,ny)
      real dsdxv(nx,ny),richv(nx,ny),dsdyu(nx,ny),dsdzu(nx,ny)
      real dsdyv(nx,ny),dsdzv(nx,ny),htempy(nx,ny)
      real htempu(nx,ny),htempv(nx,ny),htempx(nx,ny),htempw(nx,ny)
      integer ntop,nbotm,k
      integer kmixu(nx,ny),kmixv(nx,ny)
      save hh
      PARAMETER (ntop=2)
CcnhDebuugStarts
      REAL xxx (Nx,Ny)
CcnhDebugEnds
C     ======== Local variables ========================

      real sigzref
      REAL DmDl(Nz)
      REAL Wdepth(Nz)
      LOGICAL firstflg
      DATA firstflg / .true. /
      SAVE firstflg
      REAL BW            ! Coastal boundary width
      DATA BW /3./
      SAVE BW
      IF( firstflg )THEN
        Wdepth(1) =  0.
        DO K=2,Nz
           Wdepth(k) = Wdepth(k-1) + delPs(k-1)/(RoNil*g)
        ENDDO
        DO K=1,Nz-1
           dmdl(k) = (Wdepth(k) + Wdepth(k+1))*0.5
        ENDDO
        do k=2,nz
          if (dmdl(k) .gt. dbotm .and. dmdl(k-1) .lt. dbotm) then
            nbotm = k-1
          else
            if (dmdl(k) .eq. dbotm) then
              nbotm = k
            endif
          endif
        enddo
        DO K=1,Nz
           hh(k) = delPs(k)/(RoNil*g)
        ENDDO
        firstflg = .false.
      ENDIF
      rich = 0.
      richu = 0.
      richv = 0.
      htempx = 0.
      htempy = 0.
      htempu = umask(_I3(1,:,:))*hh(1)
      htempv = vmask(_I3(1,:,:))*hh(1)
      htempw = pmask(_I3(1,:,:))*hh(1)
      dsdxu = 0.
      dsdyu = 0.
      dsdzu = 0.
      dsdxv = 0.
      dsdyv = 0.
      dsdzv = 0.
      difh = 0.
      difx = 0.
      dify = 0.
c use a typical value of dSigma/dz as the reference value 
      sigzref = -1.e-3
c sigx, sigy, sigz are given at U-, V-, and W-points.  interpolate them onto 
c P-points so that depth-weighted mean Kh can be calculated.  one can compute 
c the quantities in the top level, but, here, the top level is omitted.  
      do 1 k=ntop,nbotm
        where (umask(_I3(k,:,:)) .gt. 0. .and. kmixu .lt. k)
c          dsdzu = -g*(sigz(_I3(k,:,:))+sigz(_I3(k+1,:,:))+
c     & Cshift(sigz(k,:,:),DIM=1,SHIFT=-1)+Cshift(sigz(k+1,:,:),DIM=1,SHIFT=-1))/4./ronil
          dsdzu = -sigzref
          dsdxu = g*sigx(_I3(k,:,:))/ronil
          dsdyu = g*(sigy(_I3(k,:,:))+Cshift(sigy(_I3(k,:,:)),DIM=2,SHIFT=1)+
     & Cshift(sigy(_I3(k,:,:)),DIM=1,SHIFT=-1)+Cshift(Cshift(sigy(_I3(k,:,:)),DIM=2,SHIFT=1),DIM=1,SHIFT=-1))/
     & 4./ronil
        end where
CcnhDebugStarts
        xxx = 0.
        where (umask(_I3(k,:,:)) .gt. 0. .and. kmixu .lt. k)
         xxx = 1.
        end where
        write(0,*) '     xxx   != 0 ', COUNT(xxx  .NE.0.)
        write(0,*) '     dsdxu != 0 ', COUNT(dsdxu.NE.0.)
        write(0,*) ' xxx*dsdxu != 0 ', COUNT(xxx*dsdxu.NE.0.)
        write(0,*) '     dsdyu != 0 ', COUNT(dsdyu.NE.0.)
        write(0,*) ' xxx*dsdyu != 0 ', COUNT(xxx*dsdyu.NE.0.)
        write(0,*) ' dsdxu*dsdxu + dsdyu*dsdyu != 0 ', COUNT(dsdxu*dsdxu + dsdyu*dsdyu.NE.0.)
        where (umask(_I3(k,:,:)) .gt. 0. .and. kmixu .lt. k .and.
     &          dsdxu*dsdxu + dsdyu*dsdyu .EQ. 0. )
         xxx = 2.
        end where
        write(0,*) '     xxx   != 0 ', COUNT(xxx  .NE.0.)
        write(0,*) '     xxx   == 2 ', COUNT(xxx  .EQ.2.)
        write(0,*) '     xxx   == 2 ', MAXLOC(xxx)
CcnhDebugEnds
        where (umask(_I3(k,:,:)) .gt. 0. .and. kmixu .lt. k .AND.
     &         (dsdxu*dsdxu+dsdyu*dsdyu) .NE. 0. )
C         Ri is diagnostic only
          richu = richu + hh(k)*fCorW(_I3(1,:,:))*fCorW(_I3(1,:,:))*dsdzu/
     &            (dsdxu*dsdxu+dsdyu*dsdyu)
        end where
        where (umask(_I3(k,:,:)) .gt. 0. .and. kmixu .lt. k)
          difx(_I3(1,:,:)) = difx(_I3(1,:,:)) + 
     &         hh(k)*sqrt((dsdxu*dsdxu+dsdyu*dsdyu)/dsdzu)
          htempx = htempx + umask(_I3(k,:,:))*hh(k)
        end where
        where (vmask(_I3(k,:,:)) .gt. 0. .and. kmixv .lt. k)
c          dsdzv = -g*(sigz(_I3(k,:,:))+sigz(_I3(k+1,:,:))+
c     & Cshift(sigz(_I3(k,:,:)),DIM=2,SHIFT=-1)+Cshift(sigz(_I3(k+1,:,:)),DIM=2,SHIFT=-1))/4./ronil
          dsdzv = -sigzref
          dsdyv = g*sigy(_I3(k,:,:))/ronil
          dsdxv = g*(sigx(_I3(k,:,:))+Cshift(sigx(_I3(k,:,:)),DIM=1,SHIFT=1)+
     & Cshift(sigx(_I3(k,:,:)),DIM=2,SHIFT=-1)+Cshift(Cshift(sigx(_I3(k,:,:)),DIM=1,SHIFT=1),DIM=2,SHIFT=-1))/
     & 4./ronil
          dify(_I3(1,:,:)) = dify(_I3(1,:,:)) + 
     &         hh(k)*sqrt((dsdxv*dsdxv+dsdyv*dsdyv)/dsdzv)
          htempy = htempy + vmask(_I3(k,:,:))*hh(k)
        end where
        where (vmask(_I3(k,:,:)) .gt. 0. .and. kmixv .lt. k .AND.
     &         (dsdxv*dsdxv+dsdyv*dsdyv) .NE. 0. )
         richv = richv + hh(k)*fCorV(_I3(1,:,:))*fCorV(_I3(1,:,:))*dsdzv/
     &                     (dsdxv*dsdxv+dsdyv*dsdyv)
        end where
        htempu = htempu + hh(k)*umask(_I3(k,:,:))
        htempv = htempv + hh(k)*vmask(_I3(k,:,:))
        htempw = htempw + hh(k)*pmask(_I3(k,:,:))
 1    continue
      where (htempx .gt. 0.)
        richu = richu/htempx
        difx(_I3(1,:,:)) = coef*difx(_I3(1,:,:))/htempx
      end where
      where (htempy .gt. 0.)
        richv = richv/htempy
        dify(_I3(1,:,:)) = coef*dify(_I3(1,:,:))/htempy
      end where
c for those grid points with mixed layer depth reaching the bottom, use 
c the following formula; Kh = Kh(ref)*(depth/1000m)
      where (htempu .gt. 0. .and. htempx .eq. 0.)
        difx(_I3(1,:,:)) = difref*htempu/1000.
        richu = -1.
      end where
      where (htempv .gt. 0. .and. htempy .eq. 0.)
        dify(_I3(1,:,:)) = difref*htempv/1000.
        richv = -1.
      end where
c calculate Ri at the center only if all 4 surrounding values are 
c well-defined.  if not, put a flag value of 1.e90.  
      dsdxu = Cshift(richu,DIM=1,SHIFT=1)
      dsdxv = Cshift(richv,DIM=2,SHIFT=1)
      where (richu .lt. 0. .or. richv .lt. 0. .or. 
     &       dsdxu .lt. 0. .or. dsdxv .lt. 0.)
        rich = 1.D90
      else where
        rich = (richu+richv+dsdxu+dsdxv)/4.
      end where
      difh(_I3(1,:,:)) = (difx(_I3(1,:,:))+Cshift(difx(_I3(1,:,:)),DIM=1,SHIFT=1)+
     &      dify(_I3(1,:,:))+Cshift(dify(_I3(1,:,:)),DIM=2,SHIFT=1))/4. + difmin
      difx(_I3(1,:,:)) = umask(_I3(1,:,:))*(difx(_I3(1,:,:))+difmin)
      dify(_I3(1,:,:)) = vmask(_I3(1,:,:))*(dify(_I3(1,:,:))+difmin)
      where (difx(_I3(1,:,:)) .gt. difhmx)
        difx(_I3(1,:,:)) = difhmx
      end where
      where (dify(_I3(1,:,:)) .gt. difhmx)
        dify(_I3(1,:,:)) = difhmx
      end where
      where (difh(_I3(1,:,:)) .gt. difhmx)
        difh(_I3(1,:,:)) = difhmx
      end where
      where (rich .gt. 0.) 
        rich = log10(rich)
      end where
c for those grid points with depth less than or equal to 100m, apply
c the following formula; Kh = Kh(ref)*(depth/1000m)
      where (umask(_I3(1,:,:)) .gt. 0. .and. umask(_I3(5,:,:)) .eq. 0.) 
        difx(_I3(1,:,:)) = difref*htempu/1000. + difmin
      end where
      where ( umask(_I3(1,:,:)) .gt. 0. .and.
     &        dc.lt.bw .and. difx(_I3(1,:,:)) .gt. difref )
     & difx(_I3(1,:,:)) = difref*dc/5 + difmin
      where ( umask(_I3(1,:,:)) .gt. 0. .and.
     &        Cshift(dc,DIM=1,SHIFT=-1).lt.bw.and. difx(_I3(1,:,:)) .gt. difref )
     & difx(_I3(1,:,:)) = difref*Cshift(dc,DIM=1,SHIFT=-1)/5 + difmin
      where (vmask(_I3(1,:,:)) .gt. 0. .and. vmask(_I3(5,:,:)) .eq. 0.) 
        dify(_I3(1,:,:)) = difref*htempv/1000. + difmin
      end where
      where ( vmask(_I3(1,:,:)) .gt. 0. .and.
     &        dc.lt.bw .and. dify(_I3(1,:,:)) .gt. difref )
     & dify(_I3(1,:,:)) = difref*dc/5 + difmin
      where ( vmask(_I3(1,:,:)) .gt. 0. .and.
     &        Cshift(dc,DIM=2,SHIFT=-1).lt.bw.and. dify(_I3(1,:,:)) .gt. difref )
     & dify(_I3(1,:,:)) = difref*Cshift(dc,DIM=2,SHIFT=-1)/5 + difmin
      where (pmask(_I3(1,:,:)) .gt. 0. .and. pmask(_I3(5,:,:)) .eq. 0.) 
        difh(_I3(1,:,:)) = difref*htempw/1000. + difmin
      end where
      where ( pmask(_I3(1,:,:)) .gt. 0. .and.
     &        dc .lt. bw .and. difh(_I3(1,:,:)) .gt. difref )
       difh(_I3(1,:,:)) = difref*dc/5 + difmin
      end where
      do k=nz,1,-1
        difh(_I3(k,:,:)) = wmask(_I3(k,:,:))*difh(_I3(1,:,:))
      end do
      difx(_I3(1,:,:)) = -difx(_I3(1,:,:))
      dify(_I3(1,:,:)) = -dify(_I3(1,:,:))
      do k=2,nz
        difx(_I3(k,:,:)) = umask(_I3(k,:,:))*difx(_I3(1,:,:))
      end do
      do k=2,nz
        dify(_I3(k,:,:)) = vmask(_I3(k,:,:))*dify(_I3(1,:,:))
      end do
c set the vertical variation in Kh
      call EPARAM_SET_KH_XYZ(difh,difx,dify)
      END
C******************************************************************************
C     Procedure name: EPARAM_SET_KH_XYZ
C           Function: Calculate depth-dependent Kh
C            Written: Summer 1996, Moto Nakamura, M.I.T.
C           Comments: 
CStartofinterface
      SUBROUTINE EPARAM_SET_KH_XYZ(difh,difx,dify)
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "PARAMS.h"
#include "MASKS.h"
#include "OPERATORS.h"
#include "EPARAM.h"
C     ======== Routine arguments ======================
      REAL difh (_I3(Nz,Nx,Ny))
      REAL difx (_I3(Nz,Nx,Ny))
      REAL dify (_I3(Nz,Nx,Ny))
      REAL difhz (_I3(Nz,Nx,Ny))
      REAL difxz (_I3(Nz,Nx,Ny))
      REAL difyz (_I3(Nz,Nx,Ny))
      integer k
      DO K=1,Nz
         difhz(_I3(k,:,:)) = zfacw(k)*difh(_I3(k,:,:))
         difxz(_I3(k,:,:)) = zfacuv(k)*difx(_I3(k,:,:))
         difyz(_I3(k,:,:)) = zfacuv(k)*dify(_I3(k,:,:))
         where (difhz(_I3(k,:,:)) .lt. difmin)
           difhz(_I3(k,:,:)) =  difmin
         end where
         where (abs(difxz(_I3(k,:,:))) .lt. difmin)
           difxz(_I3(k,:,:)) = - difmin
         end where
         where (abs(difyz(_I3(k,:,:))) .lt. difmin)
           difyz(_I3(k,:,:)) = - difmin
         end where
         difh(_I3(k,:,:)) = wmask(_I3(k,:,:))*difhz(_I3(k,:,:))
         difx(_I3(k,:,:)) = umask(_I3(k,:,:))*difxz(_I3(k,:,:))
         dify(_I3(k,:,:)) = vmask(_I3(k,:,:))*difyz(_I3(k,:,:))
      ENDDO
      END
C******************************************************************************
C     Procedure name: EPARAM_SET_SIG_XYZ
C           Function: Calculate sigma referenced to a level immediately above each layer 
C                     and the in-situ density.  then calculate the horizontal and vertical 
C                     derivatives at U-, V-, and W-points. 
C            Written: Fall 1995, Moto Nakamura, M.I.T.
C           Comments: 
CStartofinterface
      SUBROUTINE EPARAM_SET_SIG_XYZ(
     I    Tpotem,Salt,
     O    sigx,sigy,sigz )
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "MASKS.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "EPARAM.h"
C     ======== Routine arguments ======================
      REAL Salt (_I3(Nz,Nx,Ny))
      REAL Tpotem (_I3(Nz,Nx,Ny))
      REAL sigx (_I3(Nz,Nx,Ny))
      REAL sigy (_I3(Nz,Nx,Ny))
      REAL sigz (_I3(Nz,Nx,Ny))
C     ======== Local variables ========================
      REAL sigma0 (_I3(Nz,Nx,Ny))
      REAL sigma1 (_I3(Nz,Nx,Ny))
      INTEGER k, LEV
      REAL dmdl(Nz)
      SAVE dmdl
c  MinSigStrat = minimum stratification, -1e-8 kg m-4

      REAL Wdepth(Nz)
      LOGICAL LFIRST
      DATA LFIRST / .true. /
      SAVE LFIRST
      IF( LFIRST )THEN
        Wdepth(1) =  0.
        DO K=2,Nz
           Wdepth(k) = Wdepth(k-1) + delPs(k-1)/(RoNil*g)
        ENDDO
        DO K=1,Nz-1
           dmdl(k) = (Wdepth(k) + Wdepth(k+1))*0.5
        ENDDO
        LFIRST = .false.
      ENDIF
CstartOfcnhChangeFromMotoOriginal
C     Switch to taking gradients of density anomaly as this is better
C     conditioned.
C     CALL RHO_ABS_CALC(
C    I    Tpotem,Salt,
C    O    Sigma0 )
C     
C
C     CALL RHO_ABS_ABOVE_CALC(
C    I    Tpotem,Salt,
C    O    Sigma1 )
      DO K=1,Nz
       LEV=K
       CALL UPDATE_RHO(Tpotem,Salt,LEV,'POLY3','LOCAL', sigma0 )
      ENDDO
      Sigma1(_I3(1,:,:)) = Sigma0(_I3(1,:,:))
      DO K=2,Nz
       LEV = K
       CALL UPDATE_RHO(Tpotem,Salt,LEV,'POLY3','ABOVE', sigma1 )
      ENDDO
CendOfcnhChangeFromMotoOriginal
c compute the horizontal gradient using the in-situ density at the 
c same level
      sigx = umask*pDdxOp*(sigma0-Cshift(sigma0,DIM=_I3X,SHIFT=-1))
      sigy = vmask*pDdyOp*(sigma0-Cshift(sigma0,DIM=_I3Y,SHIFT=-1))
c compute the vertical gradient using the in-situ density at the upper level 
c and the lower level potential density with reference to the upper level
      sigz(_I3(1,:,:)) = 0.
      do k=2,nz-1
        sigz(_I3(k,:,:)) = wmask(_I3(k,:,:))*(sigma0(_I3(k-1,:,:))-sigma1(_I3(k,:,:)))/
     &                (dmdl(k)-dmdl(k-1))
        where (sigz(_I3(k,:,:)) .gt. MinSigStrat)
          sigz(_I3(k,:,:)) = wmask(_I3(k,:,:))*MinSigStrat
        end where
c note dsigma/dz at the bottom is set equal to that at the top of the 
c bottom layer.  
        where (wmask(_I3(k,:,:)) .eq. 0. .and. wmask(_I3(k-1,:,:)) .ne. 0.)
          sigz(_I3(k,:,:)) = sigz(_I3(k-1,:,:))
        end where
      end do
      sigz(_I3(nz,:,:)) = sigz(_I3(nz-1,:,:))
      RETURN
      END
C******************************************************************************
C     Procedure name: EPARAM_SET_SLP
C           Function: calculate local isentropic slopes for Redis mixing 
C            scheme and G-M scheme at U-, V-, and W-points.   
C            Written: Fall 1995, Moto Nakamura, M.I.T.
C           Comments: 
CStartofinterface
      SUBROUTINE EPARAM_SET_SLP(
     I    sigx,sigy,sigz,kmixu,kmixv,kmixmax,
     O    slpxg,slpyg,slpxud,slpyud,slpxvd,slpyvd,slpxwd,slpywd )
      IMPLICIT NONE
c  calculate local isentropic slopes for Redis mixing scheme and G-M scheme 
c  at U-, V-, and W-points.   
C      ======== Global data ============================
#include "SIZE.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "MASKS.h"
#include "OPERATORS.h"
c slpmax1 = max absolute value of isentropic slope allowed for the 
c diffusion terms for levels with thickness 50m or more
c slpmax2 = max absolute value of isentropic slope allowed for the 
c eddy-induced velocity
c slpmax3 = max absolute value of isentropic slope allowed for the 
c diffusion terms for levels with thickness less than 50m
c kthin = lowest level whose depth is less than 50m.  one has to adjust 
c this parameter for the vertical resolution of runs
C     ======== Routine arguments ======================
      REAL sigx (_I3(Nz,Nx,Ny))
      REAL sigy (_I3(Nz,Nx,Ny))
      REAL sigz (_I3(Nz,Nx,Ny))
      REAL slpxud (_I3(Nz,Nx,Ny))
      REAL slpyud (_I3(Nz,Nx,Ny))
      REAL slpxvd (_I3(Nz,Nx,Ny))
      REAL slpyvd (_I3(Nz,Nx,Ny))
      REAL slpxwd (_I3(Nz,Nx,Ny))
      REAL slpywd (_I3(Nz,Nx,Ny))
      REAL slpxg (_I3(Nz,Nx,Ny))
      REAL slpyg (_I3(Nz,Nx,Ny))
      INTEGER kmixu(nx,ny)
      INTEGER kmixv(nx,ny)
      REAL slpxgmix(nx,ny)
      REAL slpygmix(nx,ny)
      INTEGER kmixmax
CEndofinterface
C     ======== Local variables ========================
      INTEGER K
      INTEGER kthin
      SAVE kthin
      real fixedsigz

      LOGICAL firstflg
      DATA firstflg / .true. /
      SAVE firstflg
      LOGICAL thset
      DATA thset /.FALSE./

C     Set kthin,  the lowest level that has thickness of less than dthin (50m)
      IF( firstflg )THEN
        kthin = 1
        DO k=Nz,2,-1
          IF( delps(k)/(RoNil*g).le.dthin .and. .not. thset )THEN
            kthin = K - 1
            thset = .TRUE.
          ENDIF
        ENDDO
        firstflg = .false.
      ENDIF
      fixedsigz = -5.e-3

c calculate x- and y-slopes at U- and V-points, respectively, 
c for the GM and Redis scheme.  to follow the notation used by Redi (1982), 
c kx and ky, multiply the slopes for Redis scheme by negative.  
      slpxg = umask*sigx/((sigz+Cshift(sigz,DIM=_I3Z,SHIFT=1)+Cshift(sigz,DIM=_I3X,SHIFT=-1)+
     & Cshift(Cshift(sigz,DIM=_I3X,SHIFT=-1),DIM=_I3Z,SHIFT=1)+small)/4.)
      slpyg = vmask*sigy/((sigz+Cshift(sigz,DIM=_I3Z,SHIFT=1)+Cshift(sigz,DIM=_I3Y,SHIFT=-1)+
     & Cshift(Cshift(sigz,DIM=_I3Y,SHIFT=-1),DIM=_I3Z,SHIFT=1)+small)/4.)
c calculate slopes in the mixed layer for GM-scheme.
c compute the slope at the base of the mixed layer and use the 
c value throughout the column. 
      slpxgmix = 0.
      slpygmix = 0.
      do k=1,kmixmax
        where (kmixu .eq. k)
          slpxgmix(:,:) = slpxg(_I3(k+1,:,:))
        end where
        where (kmixv .eq. k)
          slpygmix(:,:) = slpyg(_I3(k+1,:,:))
        end where
      end do
      do k=1,kmixmax
        where (kmixu .eq. k .or. kmixu .gt. k)
          slpxg(_I3(k,:,:)) = slpxgmix
        end where
        where (kmixv .eq. k .or. kmixv .gt. k)
          slpyg(_I3(k,:,:)) = slpygmix
        end where
      end do
      slpxud = -slpxg
      slpyvd = -slpyg
c calculate x-slope at V- and W-points for Redis scheme
      slpxvd = vmask*(slpxud+Cshift(slpxud,DIM=_I3X,SHIFT=1)+Cshift(slpxud,DIM=_I3Y,SHIFT=-1)+
     & Cshift(Cshift(slpxud,DIM=_I3Y,SHIFT=-1),DIM=_I3X,SHIFT=1)+small)/4.
      slpxwd = wmask*(slpxud+Cshift(slpxud,DIM=_I3X,SHIFT=1)+Cshift(slpxud,DIM=_I3Z,SHIFT=-1)+
     & Cshift(Cshift(slpxud,DIM=_I3Z,SHIFT=-1),DIM=_I3X,SHIFT=1)+small)/4.
c calculate y-slope at U- and W-points for Redis scheme
      slpyud = umask*(slpyvd+Cshift(slpyvd,DIM=_I3Y,SHIFT=1)+Cshift(slpyvd,DIM=_I3X,SHIFT=-1)+
     & Cshift(Cshift(slpyvd,DIM=_I3X,SHIFT=-1),DIM=_I3Y,SHIFT=1)+small)/4.
      slpywd = wmask*(slpyvd+Cshift(slpyvd,DIM=_I3Y,SHIFT=1)+Cshift(slpyvd,DIM=_I3Z,SHIFT=-1)+
     & Cshift(Cshift(slpyvd,DIM=_I3Z,SHIFT=-1),DIM=_I3Y,SHIFT=1)+small)/4.
c impose upper limit on slopes used for eddy-induced velocity
      where (abs(slpxg) .gt. slpmax2)
        slpxg = slpmax2*slpxg/abs(slpxg)
      end where
      where (abs(slpyg) .gt. slpmax2)
        slpyg = slpmax2*slpyg/abs(slpyg)
      end where
c impose upper limit on slopes used for diffusion terms
      where (abs(slpxud) .gt. slpmax1)
        slpxud = slpmax1*slpxud/abs(slpxud)
      end where
      where (abs(slpyud) .gt. slpmax1)
        slpyud = slpmax1*slpyud/abs(slpyud)
      end where
      where (abs(slpxvd) .gt. slpmax1)
        slpxvd = slpmax1*slpxvd/abs(slpxvd)
      end where
      where (abs(slpyvd) .gt. slpmax1)
        slpyvd = slpmax1*slpyvd/abs(slpyvd)
      end where
      where (abs(slpxwd) .gt. slpmax1)
        slpxwd = slpmax1*slpxwd/abs(slpxwd)
      end where
      where (abs(slpywd) .gt. slpmax1)
        slpywd = slpmax1*slpywd/abs(slpywd)
      end where
c impose smaller upper limit on slopes used for diffusion terms 
c in layers with depth less than 50 meters.  
      do k=1,kthin
        where (abs(slpxud(_I3(k,:,:))) .gt. slpmax3)
          slpxud(_I3(k,:,:)) = slpmax3*slpxud(_I3(k,:,:))/abs(slpxud(_I3(k,:,:)))
        end where
        where (abs(slpyud(_I3(k,:,:))) .gt. slpmax3)
          slpyud(_I3(k,:,:)) = slpmax3*slpyud(_I3(k,:,:))/abs(slpyud(_I3(k,:,:)))
        end where
        where (abs(slpxvd(_I3(k,:,:))) .gt. slpmax3)
          slpxvd(_I3(k,:,:)) = slpmax3*slpxvd(_I3(k,:,:))/abs(slpxvd(_I3(k,:,:)))
        end where
        where (abs(slpyvd(_I3(k,:,:))) .gt. slpmax3)
          slpyvd(_I3(k,:,:)) = slpmax3*slpyvd(_I3(k,:,:))/abs(slpyvd(_I3(k,:,:)))
        end where
c apply slpmax3 through the bottom of the lowest level that has 
c depth less than 50 meters
        where (abs(slpxwd(_I3(k+1,:,:))) .gt. slpmax3)
          slpxwd(_I3(k+1,:,:)) = slpmax3*slpxwd(_I3(k+1,:,:))/abs(slpxwd(_I3(k+1,:,:)))
        end where
        where (abs(slpywd(_I3(k+1,:,:))) .gt. slpmax3)
          slpywd(_I3(k+1,:,:)) = slpmax3*slpywd(_I3(k+1,:,:))/abs(slpywd(_I3(k+1,:,:)))
        end where
      end do

c calculate the slopes for the eddy-induced velocities using fixed sigz
      slpxg = umask*sigx/fixedsigz
      slpyg = vmask*sigy/fixedsigz
c calculate slopes in the mixed layer for GM-scheme.
c compute the slope at the base of the mixed layer and use the 
c value throughout the column. 
      slpxgmix = 0.
      slpygmix = 0.
      do k=1,kmixmax
        where (kmixu .eq. k)
          slpxgmix(:,:) = slpxg(_I3(k+1,:,:))
        end where
        where (kmixv .eq. k)
          slpygmix(:,:) = slpyg(_I3(k+1,:,:))
        end where
      end do
      do k=1,kmixmax
        where (kmixu .eq. k .or. kmixu .gt. k)
          slpxg(_I3(k,:,:)) = slpxgmix
        end where
        where (kmixv .eq. k .or. kmixv .gt. k)
          slpyg(_I3(k,:,:)) = slpygmix
        end where
      end do

c impose upper limit on slopes used for eddy-induced velocity
      where (abs(slpxg) .gt. slpmax2)
        slpxg = slpmax2*slpxg/abs(slpxg)
      end where
      where (abs(slpyg) .gt. slpmax2)
        slpyg = slpmax2*slpyg/abs(slpyg)
      end where

      END
C     Procedure name: eparam_set_ttl_vel
C           Function: diagnose the total advective velocity field from u, v, 
C                     ueddy, and veddy
C            Written: Fall 1995, Moto Nakamura, M.I.T.
CStartofinterface
      SUBROUTINE EPARAM_SET_TTL_VEL (
     I    u,v,ueddy,veddy,
     O    utotal,vtotal,wtotal)
      IMPLICIT NONE
C      ======== Global data ============================
#include "SIZE.h"
#include "OPERATORS.h"
#include "GRID.h"
#include "PARAMS.h"
#include "EPARAM.h"
#include "OLDG.h"
#include "MASKS.h"
C     ======== Routine arguments ======================
      REAL u  (_I3(Nz,Nx,Ny))
      REAL v  (_I3(Nz,Nx,Ny))
      REAL ueddy   (_I3(Nz,Nx,Ny))
      REAL veddy   (_I3(Nz,Nx,Ny))
      REAL utotal   (_I3(Nz,Nx,Ny))
      REAL vtotal   (_I3(Nz,Nx,Ny))
      REAL wtotal   (_I3(Nz,Nx,Ny))
C     ======== Local variables ========================
      REAL fluxY (_I3(Nz,Nx,Ny))
      REAL fluxX (_I3(Nz,Nx,Ny))
      REAL tmp(0:Nx+1,0:Ny+1)
      integer i,j,k
      utotal = u + ueddy
      vtotal = v + veddy
      wtotal = 0.
      fluxX = utotal*XA
      fluxY = vtotal*YA
Cdbg      DO 10 K = 1, Nz-1
Cdbg        WHERE (ZA(_I3(K+1,:,:)) .NE. 0. )
Cdbg          wtotal(_I3(K+1,:,:)) = -1.*
Cdbg     &    (Cshift(fluxX(_I3(K,:,:)),DIM=1,SHIFT=+1)-fluxX(_I3(K,:,:))
Cdbg     &    +Cshift(fluxY(_I3(K,:,:)),DIM=2,SHIFT=+1)-fluxY(_I3(K,:,:))
Cdbg     &    -wtotal(_I3(K,:,:))*ZA(_I3(K,:,:)))/ZA(_I3(K+1,:,:))
Cdbg        ENDWHERE
Cdbg 10   CONTINUE
      DO 10 K = Nz, 1, -1
       tmp(1:Nx,1:Ny) = uTotal(_I3(K,:,:))*XA(_I3(K,:,:))
       tmp(Nx+1,1:Ny) = tmp(1,1:Ny)
       DO J=1,Ny
        DO I=1,Nx
         wTotal(_I3(K,I,J)) = ( tmp(I+1,J)-tmp(I,J) )
        ENDDO
       ENDDO
       tmp(1:Nx,1:Ny) = vTotal(_I3(K,:,:))*YA(_I3(K,:,:))
       tmp(1:Nx,Ny+1) = tmp(1:Nx,1)
       DO J=1,Ny
        DO I=1,Nx
         wTotal(_I3(K,I,J)) = wTotal(_I3(K,I,J))+( tmp(I,J+1)-tmp(I,J) )
        ENDDO
       ENDDO
       IF ( K .NE. Nz ) THEN
        DO J=1,Ny
         DO I=1,Nx
          wTotal(_I3(K,I,J)) = wTotal(_I3(K,I,J)) + wTotal(_I3(K+1,I,J))*ZA(_I3(K+1,I,J))
         ENDDO
        ENDDO
       ENDIF
       DO J=1,Ny
        DO I=1,Nx
         IF ( ZA(_I3(K,I,J)) .NE. 0. ) THEN
          wTotal(_I3(K,I,J)) = wTotal(_I3(K,I,J))/ZA(_I3(K,I,J))
         ELSE
          wTotal(_I3(K,I,J)) = 0.
         ENDIF
        ENDDO
       ENDDO
 10   CONTINUE

      wtotal = wtotal*wmask
      RETURN
      END
