C $Id: CPP_MACROS.h,v 1.1 1998/05/25 20:21:06 cnh Exp $
C=====================================================================
C        Header file: CPP_MACROS.h                                   |
C           Function: Defines CPP macros which are used throughout   |
C                     the code to make global changes at compile     |
C                     time.                                          |
C           Comments:                                                |
C=====================================================================

C     /--------------------------------------------------------------\
C     | Index permutation                                            |
C     |==============================================================|
C     | o This macro is used to reorder the indices of               |
C     | three-dimensional arrays. Different orderings are selected by|
C     | using #define _IJK or #define _KIJ etc..                     |
C     \--------------------------------------------------------------/
C     /--------------------------------------------------------------\
C     | 1. Axis ordering i,j,k                                       |
C     \--------------------------------------------------------------/
#ifdef _IJK
# define _I3X 1
# define _I3Y 2
# define _I3Z 3
# define _I3(k,i,j) i,j,k
#endif
C     /--------------------------------------------------------------\
C     | 2. Axis ordering k,i,j                                       |
C     \--------------------------------------------------------------/
#ifdef _KIJ
# define _I3X 2
# define _I3Y 3
# define _I3Z 1
# define _I3(k,i,j) k,i,j
#endif

C     /--------------------------------------------------------------\
C     | Debugging                                                    |
C     |==============================================================|
C     | Use this macro to include or suppress special debug print    |
C     | statements.                                                  |
C     \--------------------------------------------------------------/
#ifdef _WRITE_DEBUG
# define _D(a) WRITE(0,*) a
#else
# define _D(a) 
#endif

C Double precision
#ifdef _ALL_DOUBLE
# define REAL Real*8
# define real Real*8
#else
# define REAL Real*8
# define real Real*4
#endif


C CPP Macros used to switch terms on and off
C ==========================================

C o Momentum equation terms
C _LPM Laplacian diffusion of momentum
C _BHM Biharmonic diffusion of momentum
C _ADM Advection of momentum
C _SPM Spherical polar metric terms
C _XZC XZ-plane coriolis terms

#ifdef _LAPLACIAN_DIFFUSION_OF_MOMENTUM
# define _LPM(a) a
#else
# define _LPM(a)
#endif

#ifdef _BIHARMONIC_DIFFUSION_OF_MOMENTUM
# define _BHM(a) a
#else
# define _BHM(a)
#endif

#ifdef _ADVECTION_OF_MOMENTUM
# define _ADM(a) a
#else
# define _ADM(a)
#endif

#ifdef _SPHERICAL_POLAR_METRIC_TERMS
# define _SPM(a) a
#else
# define _SPM(a)
#endif

#ifdef _XZ_CORIOLIS
# define _XZC(a) a
#else
# define _XZC(a)
#endif

C o Temerature equation terms
C _LPT Laplacian diffusion of temperature
C _BHT Biharmonic diffusion of temperature
C _ADT Advection of momentum
#ifdef _LAPLACIAN_DIFFUSION_OF_TEMPERATURE
# define _LPT(a) a
#else
# define _LPT(a)
#endif

#ifdef _BIHARMONIC_DIFFUSION_OF_TEMPERATURE
# define _BHT(a) a
#else
# define _BHT(a)
#endif 

#ifdef _ADVECTION_OF_TEMPERATURE
# define _ADT(a) a
#else
# define _ADT(a)
#endif

