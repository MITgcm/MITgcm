C $Header: /u/gcmpack/MITgcm/pkg/kpp/Attic/kpp_init.F,v 1.1 2000/06/21 19:45:51 adcroft Exp $

#include "KPP_OPTIONS.h"

      SUBROUTINE KPP_INIT( myThid )
C     /==========================================================\
C     | SUBROUTINE KPP_INIT                                      |
C     | o Routine to initialize KPP variables and constants.     |
C     |==========================================================|
C     | Initialize KPP parameters and read in file data.kpp      |
C     \==========================================================/
      IMPLICIT NONE

C     === Global variables ===
#include "SIZE.h"
#include "EEPARAMS.h"
#include "PARAMS.h"
#include "GRID.h"
#include "KPP.h"
#include "KPP_PARAMS.h"

C     === Routine arguments ===
C     myThid -  Number of this instance of KPP_INIT
      INTEGER myThid

#ifdef ALLOW_KPP

C     === Local variables ===
C     msgBuf      - Informational/error meesage buffer
C     record      - Work variable for IO buffer
C     errIO       - IO error flag
C     iUnit       - Work variable for IO unit number
C     i,j,k,bi,bj - Loop counters
C     zehat       - zeta * ustar**3
C     zeta        - Stability parameter d/l

      CHARACTER*(MAX_LEN_MBUF) msgBuf
      CHARACTER*(MAX_LEN_PREC) record
      INTEGER errIO
      INTEGER IL, iUnit
      INTEGER i, j, k, bi, bj
      INTEGER  ILNBLNK
      EXTERNAL ILNBLNK
      _RS zehat
      _RS zeta
      _RS usta

C--   KPP vertical mixing parameters
      NAMELIST /KPP_PARM01/
     & kpp_freq,
     & KPPmixingMaps, KPPwriteState,
     & minKPPviscAz, maxKPPviscAz, minKPPghat, maxKPPghat,
     & minKPPdiffKzT, maxKPPdiffKzT, minKPPdiffKzS, maxKPPdiffKzS,
     & minKPPhbl,
     & epsln, phepsi, epsilon, vonk, dB_dz,
     & conc1, conam, concm, conc2, zetam,
     & conas, concs, conc3, zetas,
     & Ricr, cekman, cmonob, concv, hbf, Vtc,
     & zmin, zmax, umin, umax, 
     & num_v_smooth_Ri, num_v_smooth_BV,
     & num_z_smooth_sh, num_m_smooth_sh,
     & Riinfty, BVSQcon, difm0, difs0, dift0,
     & difmcon, difscon, diftcon,
     & cstar, cg

      _BEGIN_MASTER(myThid)

      WRITE(msgBuf,'(A)') ' KPP_INIT: opening data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)


      CALL OPEN_COPY_DATA_FILE(
     I                          'data.kpp', 'KPP_INIT',
     O                          iUnit,
     I                          myThid )

C--   set default KPP vertical mixing parameters
      kpp_freq      = deltaTClock
      KPPmixingMaps = .FALSE.
      KPPwriteState = .FALSE.
      minKPPghat    = 0.
      maxKPPghat    = 1.e10
      minKPPviscAz  = 0.
      DO k = 1,Nr
         maxKPPviscAz(k) = 100.
      ENDDO
      minKPPdiffKzT = 0.
      maxKPPdiffKzT = 100.
      minKPPdiffKzS = 0.
      maxKPPdiffKzS = 100.
      minKPPhbl     = delZ(1)*0.5

C-----------------------------------------------------------------------
C define some non-dimensional constants and
C the vertical mixing coefficients in m-k-s units
C-----------------------------------------------------------------------
 
      epsln   = 1.e-20
      phepsi  = 1.e-10
      epsilon = 0.1
      vonk    = 0.40
      dB_dz   = 5.2e-5
      conc1   = 5.0
      conam   = 1.257
      concm   = 8.380
      conc2   = 16.0
      zetam   = -0.2
      conas   = -28.86
      concs   = 98.96
      conc3   = 16.0
      zetas   = -1.0

c     parameters for subroutine "bldepth"

      Ricr    = 0.30
      cekman  = 0.7
      cmonob  = 1.0
      concv   = 1.8
      hbf     = 1.0

c     parameters and common arrays for subroutines
c     "kmixinit" and "wscale"

      zmin    = -4.e-7
      zmax    = 0.0
      umin    = 0.0
      umax    = .04

c     parameters for subroutine "Ri_iwmix"

      num_v_smooth_Ri = 0
      num_v_smooth_BV = 0
      num_z_smooth_sh = 0
      num_m_smooth_sh = 0
      Riinfty = 0.7
      BVSQcon = -0.2e-4

      difm0   = 0.005
      difs0   = 0.005
      dift0   = 0.005

      difmcon = 0.1
      difscon = 0.1
      diftcon = 0.1

C     parameters for subroutine "blmix"

      cstar   = 10.

C-----------------------------------------------------------------------

C--   Read settings from model parameter file "data.kpp".
      READ(UNIT=iUnit,NML=KPP_PARM01,IOSTAT=errIO)
      IF ( errIO .LT. 0 ) THEN
       WRITE(msgBuf,'(A)')
     &  'S/R INI_PARMS'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Error reading numerical model '
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'parameter file "data.kpp"'
       CALL PRINT_ERROR( msgBuf , 1)
       WRITE(msgBuf,'(A)')
     &  'Problem in namelist KPP_PARM01'
       CALL PRINT_ERROR( msgBuf , 1)
C      CALL MODELDATA_EXAMPLE( myThid )
       STOP 'ABNORMAL END: S/R KPP_INIT'
      ENDIF

      CLOSE(iUnit)

      WRITE(msgBuf,'(A)') ' KPP_INIT: finished reading data.kpp'
      CALL PRINT_MESSAGE( msgBuf, standardMessageUnit,
     &                    SQUEEZE_RIGHT , 1)

      _END_MASTER(myThid)

C--   Everyone else must wait for the parameters to be loaded
      _BARRIER

C-----------------------------------------------------------------------
C Initialize constants that depend on parameters in data.kpp
C-----------------------------------------------------------------------

      Vtc     = concv * sqrt(0.2/concs/epsilon) / vonk**2 / Ricr
      cg      = cstar * vonk * (concs * vonk * epsilon)**(1./3.)

c-----------------------------------------------------------------------
c construct the wm and ws lookup tables
c-----------------------------------------------------------------------

      deltaz = (zmax - zmin)/(nni + 1)
      deltau = (umax - umin)/(nnj + 1)
 
      do i = 0, nni + 1
         zehat = deltaz*i + zmin
         do j = 0, nnj + 1
            usta = deltau*j + umin
#ifdef KPP_TEST_DENOM
            zeta = zehat / max(phepsi,usta**3)
#else
            zeta = zehat/(usta**3 + epsln)
#endif
            if (zehat .ge. 0.) then
               wmt(i,j) = vonk*usta/(1. + conc1*zeta)
               wst(i,j) = wmt(i,j)
            else
               if (zeta .gt. zetam) then
                  wmt(i,j) = vonk*usta*(1. - conc2*zeta)**(1./4.)
               else
                  wmt(i,j) = vonk*(conam*usta**3 - concm*zehat)**(1./3.)
               endif
               if (zeta .gt. zetas) then
                  wst(i,j) = vonk*usta*(1. - conc3*zeta)**(1./2.)
               else
                  wst(i,j) = vonk*(conas*usta**3 - concs*zehat)**(1./3.)
               endif
            endif
         end do
      end do



C-----------------------------------------------------------------------
C     calculate mask pMask for pressure/tracer cells
C        (0 => land, 1 => water)
C     compute maximum number of wet levels in each column
C-----------------------------------------------------------------------

      do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)
             do j = 1-OLy, sNy+OLy
               do i = 1-OLx, sNx+OLx
                  nzmax(i,j,bi,bj) = 0
                  do k = 1, Nr
                     if (_hFacC(i,j,k,bi,bj).eq.0.) then
                        pMask (i,j,k,bi,bj) = 0.
                     else
                        pMask (i,j,k,bi,bj) = 1.
                     endif
                     nzmax(i,j,bi,bj) = nzmax(i,j,bi,bj)
     &                                + INT(pMask(i,j,k,bi,bj))
                  end do
               end do
            end do
          end do
      end do
      
C-----------------------------------------------------------------------
C     vertical grid
C-----------------------------------------------------------------------

#ifdef KPP_TEST_DENOM
      zgrid(0)  =  phepsi
      hwide(0)  =  phepsi
#else
      zgrid(0)  =  epsln
      hwide(0)  =  epsln
#endif
      zgrid(1)  = -delZ(1)*0.5
      hwide(1)  =  delZ(1)

      do k = 2, Nr
         zgrid(k) = zgrid(k-1) - (delZ(k-1)+delZ(k))*0.5
         hwide(k) = delZ(k)
      end do

      zgrid(Nrp1) = zgrid(Nr) * 100.

#ifdef KPP_TEST_DENOM
      hwide(Nrp1) = phepsi
#else
      hwide(Nrp1) = epsln
#endif

C-----------------------------------------------------------------------
C Initialize KPP variables KPPhbl, KPPghat, KPPviscAz,
C    KPPdiffKzT, and KPPdiffKzS
C-----------------------------------------------------------------------

      do bj = myByLo(myThid), myByHi(myThid)
         do bi = myBxLo(myThid), myBxHi(myThid)
            do j = 1-OLy, sNy+OLy
               do i = 1-OLx, sNx+OLx
                  KPPhbl(i,j,bi,bj) = 0.
               end do
            end do
            do k = 1, Nr
               do j = 1-OLy, sNy+OLy
                  do i = 1-OLx, sNx+OLx
                     KPPghat       (i,j,k,bi,bj) = 0.
                     KPPviscAz  (i,j,k,bi,bj) = viscAr
                     KPPdiffKzT (i,j,k,bi,bj) = diffKrT
                     KPPdiffKzS (i,j,k,bi,bj) = diffKrS
                  end do
               end do
            end do
          end do
      end do

#endif /* ALLOW_KPP */

      return
      end
