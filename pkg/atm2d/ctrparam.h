C $Header: /u/gcmpack/MITgcm/pkg/atm2d/ctrparam.h,v 1.4 2010/08/24 13:52:54 jmc Exp $
C $Name:  $

C ==================================================
C
C ctrparam.h
C ----------
C
C  Purpose:     A header file contains cpp control parameters for the model
C
C  -------------------------------------------------
C
C  Usage:       1. (un)comment #define line; or
C               2. #define/#undef x [number]
C                 to set given cpp parameters x
C                 to be true or false
C
C  -------------------------------------------------
C
C  Author:      Chien Wang
C
C  Revision:
C       Date    By              Brief Description
C       ----    --              -----------------
C       052400  Chien Wang      created
C       121800  Chien Wang      add NYR_CHEM
C       062304  Chien Wang      rev for igsm2
C
C ==================================================

C === number of latitudinal grid points
C
#define N_LAT 46

C === number of vertical pressure layers
C
#define N_LEV 11

#define N_LON0 72

C === number of vertical  layers in duffusive ocean model
C
C --- Current options include 12 and 11
C
#define N_LEVOCEAN 11

C === defines 3D ocean
C
#define OCEAN_3D 1

C === coupling ocean co2 model
C
C
c#define CPL_OCEANCO2 1

C === coupling CLM
C
c#define CLM 1

#define NYR_CHEM 1

