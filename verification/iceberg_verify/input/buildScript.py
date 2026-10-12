#!/usr/bin/env python
# coding: utf-8
# Paul Summers, September 2025
# Script for generating MITgcm setup for idealized fjord
# Including forcing of plume, icebergs, BCs also enabled

import os
import numpy as np
import matplotlib.pyplot as plt
import pickle
import importlib
import shutil
import platform
import fileinput
import sys
import glob
import cmocean
from bisect import bisect_left

OSX = platform.system()

from scipy.interpolate import make_interp_spline

import sys
if OSX == 'Darwin':
    import gsw
    sys.path.append('main_scripts')
else:
    sys.path.append('/storage/home/hcoda1/2/psummers8/MITgcmSandbox/elizaScripts/main_scripts')

import run_config_funcs as rcf # type: ignore # import helpter functions

#Set up new folder
makeDirs = False
#Write input files, this lets us update the inputs with a full new run
writeFiles = True



if(makeDirs):
    setupNotes = open("setupReport.txt", "w") 

def find_closest_indices(sorted_A, sorted_B):
    closest_indices = []
    for a in sorted_A:
        pos = bisect_left(sorted_B, a)  # Find position in B where a would fit
        # Compare neighbors to find the closest
        if pos == 0:
            closest_indices.append(0)
        elif pos == len(sorted_B):
            closest_indices.append(len(sorted_B) - 1)
        else:
            before = pos - 1
            after = pos
            closest_indices.append(before if abs(sorted_B[before] - a) <= abs(sorted_B[after] - a) else after)
    return closest_indices

def setUpPrint(msg):
    print(msg)
    if(makeDirs):
        setupNotes.write(str(msg) + "\n")

# ## Main run configuration
email = 'INSERT@EXAMPLE.edu'
# set high level run configurations

briefSummaryOfExp = """
finModel is a smaller verion of a fjord for running on mac locally

Use icebergs and a subglacial plume

You should only have to build, then run.
"""


setUpPrint('====== Welcome to the mélange building script =====')

#========================================================================================
#main values to imput 

run_config = {}
grid_params = {}
run_config['ncpus_xy'] = [1,1] # cpu distribution in the x and y directions
run_config['run_name'] = 'bergDemoWARM'
run_config['ndays'] = 1 # simulation time (days)
run_config['test'] = False # if True, run_config['nyrs'] will be shortened to a few time steps

wallWidthInd = 2 #width of walls in units of dy
run_config['horiz_res_m'] = 500 # horizontal grid spacing (m)
run_config['Lx_m'] = 30000 # domain size in x (m)
run_config['Ly_m'] = 4000 + (2 * wallWidthInd * run_config['horiz_res_m']) # domain size in y (m) with walls (1 wall each side)
# NOTE: the number of grid points in x and y should be multiples of the number of cpus.
run_config['terminus_m'] = run_config['horiz_res_m']
terminus_index = int(run_config['terminus_m']/run_config['horiz_res_m'])
grid_params['Nr'] = 28 # num of z-grid points

run_config['make_icebergs'] = True # Do we make bergs? No if running from spin-up

setUpPrint(briefSummaryOfExp + "\nDirectory: %s \n\tmakeDirs: %s, writeFiles: %s" %(run_config['run_name'],makeDirs,writeFiles))
input("Confirm above is accurate before continuing...")

# Variables to adjust ======================
assign_deltaT = 0 # [C]
assign_plumeSGD = 500 #[m^3/s]
season_sw = 's'

# Offshore current =========================
oscStrength = 0.03 #[m/s] peak strength of offshore current
lengthOffShoreLength = 10e3 #width of offshore region [m]
indexOSC = int(lengthOffShoreLength/run_config['horiz_res_m'])

# Iceberg configuration =========================
iceBergDepth = 400 # max iceberg depth [meters], used for ICEBERG package
iceExtent = 8000 # [meters] of extent of ice
iceCoverage = 60 # % of ice cover in melange, stay under 90% ideally
doMelt = 1 # do we actually calculate melt (0/1 = no/yes)
doBlock = 1 # do we actually calculate melt (0/1 = no/yes)
# Set median drafts to align with output from melange1D
forceDraft = False
#========================================================================================
# The rest of this should take care of it self mostly

#run_config['evolve_salt'] = False
run_config['use_GMRedi'] = False # should be set to false for eddy permitting resolutions
run_config['periodic_forcing'] = True # you have to do this manually

MITgcm_release = 'MITgcm-checkpoint68z' #Sept 2024 release
#MITgcm_code_dir = os.path.join(group_home_dir, 'shared/mitgcm_releases', MITgcm_release)

# you probably don't need to touch this
run_config['use_MPI'] = False # for multi-processing
run_config['lf'] = '\r\n' # linebreak characters 
if OSX == 'Darwin':
    run_config['exps_dir'] = os.path.join('../experiments') 
else:
    run_config['exps_dir'] = os.path.join('/storage/home/hcoda1/2/psummers8/MITgcmSandbox/experiments') 
run_config['run_dir'] = os.path.join(run_config['exps_dir'], run_config['run_name'])
setUpPrint('run_config is %s' %run_config)

#========================================================================================
# Generate new experiment directory and copy over defaults

# create experimentary directory on SCRATCH and copy over default configuration
# NOTE: this step does not overwrite existing directories. 
if(makeDirs):
    run_subdir_list = ['build', 'code', 'input', 'results']
    for subdir in run_subdir_list:
        run_config['%s_dir'% subdir] = os.path.join(run_config['run_dir'], subdir)
        os.makedirs(run_config['%s_dir'% subdir], exist_ok=True)
     
# copy over defaults
    if OSX == 'Darwin':
        default_dirs = os.listdir('../DEFAULT_Berg/')
    else:
        default_dirs = os.listdir('/storage/home/hcoda1/2/psummers8/MITgcmSandbox/DEFAULT_Berg/')
    for dir00 in default_dirs:
        if dir00.startswith('.'):
            continue
            
        if OSX == 'Darwin':
            default_dir = '../DEFAULT_Berg/%s/'%dir00
        else:
            default_dir = '/storage/home/hcoda1/2/psummers8/MITgcmSandbox/DEFAULT_Berg/%s/'%dir00    
        default_files = os.listdir(default_dir)
        dst_dir = os.path.join(run_config['run_dir'], dir00)
        
        for file in default_files:
    
            if file.startswith('.'):
                continue
            else:
                src_fpath = os.path.join(default_dir, file)
                shutil.copy2(src_fpath, dst_dir)
                #print(src_fpath, '>', dst_dir)
    setUpPrint('run directory and subdirectories:')
    setUpPrint(run_config['run_dir'])
    setUpPrint(os.listdir(run_config['run_dir']))

    # create new analysis sub-dir in your home directory
    # if OSX == 'Darwin':
    #     analysis_dir = '/Users/psummers8/Documents/MITgcm/MITgcm/analysis/%s'%run_config['run_name']
    # else:
    #     analysis_dir = '/storage/home/hcoda1/2/psummers8/MITgcmSandbox/analysis/%s'%run_config['run_name']
    # os.makedirs(analysis_dir, exist_ok=True)
    
secsInDay = 24*60*60
secsInYear = 365*secsInDay


#========================================================================================
# set domain size
setUpPrint('====== Domain Size and Parameters =====')
domain_params = {}
domain_params['Lx'] = run_config['Lx_m'] # domain size in x (m)
domain_params['Ly'] = run_config['Ly_m'] # domain size in y (m)
domain_params['L_sponge'] = 6000 # width of eastern sponge layer (m)
domain_params['H'] = 600 # max domain depth (m)

# NOTE: the only thing you may need to change here is the number of z-grid pointsm, which was set above)

grid_params['nSx'] = 1 # num of tiles per processor in x-direction
grid_params['nSy'] = 1 # num of tiles per processor in y-direction
grid_params['nTx'] = 1 # num of threads per processor in x-direction
grid_params['nTy'] = 1 # num of threads per processor in y-direction
grid_params['OLx'] = 3 # num of overlapping x-gridpoints per tile
grid_params['OLy'] = 3 # num of overlapping y-gridpoints per tile
#grid_params['Nr'] = 70 # num of z-grid points (set above)

grid_params['nPx'] = run_config['ncpus_xy'][0] #num of processors in x-direction
grid_params['nPy'] = run_config['ncpus_xy'][1] #num of processors in x-direction

# grid_params['nSx'] = domain_params['Lx']/(run_config['horiz_res_m']) # num of x points in sub grid
# grid_params['nSy'] = domain_params['Ly']/(run_config['horiz_res_m']) # num of y points in sub grid

# grid_params['Nx'] = grid_params['sNx'] * grid_params['nSx'] * grid_params['nPx']
# grid_params['Ny'] = grid_params['sNy'] * grid_params['nSy'] * grid_params['nPy']

grid_params['Nx'] = domain_params['Lx']/(run_config['horiz_res_m']) # num of x points
grid_params['Ny'] = domain_params['Ly']/(run_config['horiz_res_m']) # num of y points

setUpPrint("Nx: %s" %grid_params['Nx'])
setUpPrint("Ny: %s" %grid_params['Ny'])

grid_params['sNx'] = grid_params['Nx']/grid_params['nPx']#num of x-gridpoints per tile
grid_params['sNy'] = grid_params['Ny']/grid_params['nPy'] #num of y-gridpoints per tile

setUpPrint("sNx: %s" %grid_params['sNx'])
setUpPrint("sNy: %s" %grid_params['sNy'])

# NOTE: sNx and sNy should be whole numbers/integers. As long we keep the horizontal resolution,
# domain dimesions, and number of cpus to be multiples of five, we should be ok. 

for key, param  in grid_params.items():
    assert param%1==0, "grid parameter needs to be an integer"
    grid_params[key] = int(param)

setUpPrint('Grid parameters')    
setUpPrint(grid_params)

# grid_params cont'd
grid_params['usingCartesianGrid'] = True
grid_params['usingSphericalPolarGrid'] = False 

# horizontal grid spacing
grid_params['delX'] = (domain_params['Lx']/grid_params['Nx'])*np.ones(grid_params['Nx'])
grid_params['delY'] = (domain_params['Ly']/grid_params['Ny'])*np.ones(grid_params['Ny'])


 # dz = domain_params['H']/grid_params['Nr']*np.ones(grid_params['Nr']);

# I'm smitten with myself for how well this works to make a smooth dz profile
dz_tmp = np.linspace(1,7,grid_params['Nr'])
dz = dz_tmp/np.sum(dz_tmp)*domain_params['H'] 
sum_z = np.cumsum(dz)
setUpPrint("dz: \n %s" %dz)
setUpPrint("z: \n %s"  %sum_z)
np.save(run_config['run_dir']+'/input/dz',dz)
grid_params['delZ'] = dz
# grid_params['hFacMinDr'] = dz.min()

#========================================================================================
#Physical parameters

params01 = {} 

# physical constants
g = 9.81 # acc. due to gravity (m/s**2)
Omega = 2*np.pi*366/365/86400 # planetary rotation rate 
Rp = 6400*1000 # planetary radius (m)
# lat_min = -70 # latitude at southern boundary (degrees)
#f0 = 2*Omega*np.sin(np.deg2rad(lat_min)) # coriolis param (1/s)
#beta = (2*Omega*np.cos(np.deg2rad(lat_min))/Rp) # beta param


# momentum scheme
params01['vectorInvariantMomentum'] = True

#Note: here and elsewhere, we need to be explicit about floats vs ints. E.g., use 12.0 to represent float and
# 12 for int

# viscosity parameters
#params01['viscA4'] = 0.0000 # Biharmonic viscosity?
params01['viscAz'] = 1.0e-4 # Vertical viscosity
params01['viscAh'] = 1.0e-3 # Vertical viscosity, this limits our timestep a lot
params01['viscC2smag'] = 2.2 # ??? viscosity

# advection and time stepping
params01['tempAdvScheme'] = 33 # needs to be int
params01['saltAdvScheme'] = 33 # needs to be int
#params01['tempStepping'] = True
params01['saltStepping'] = True
params01['staggerTimeStep'] = True

# diffusivity
#params01['diffK4T'] = 0.0e4 # ?? temp diffusion
params01['diffKhT'] = 0.0 #1.0e-5 # Horizontal temp diffusion
params01['diffKhS'] = 0.0 #1.0e-5 # Horz salt diffusion
params01['diffKzT'] = 0.0 #1.0e-5 # Vertical temp diffusion
params01['diffKzS'] = 0.0 #1.0e-5 # Vert salt diffusion
#params01['diffK4S'] = 0.0e4 # ?? salt diffusion


# equation of state
params01['eosType'] = 'JMD95Z'
params01['Tref'] = np.ones(grid_params['Nr'])*0. #ref temp
params01['Sref'] = np.ones(grid_params['Nr'])*34. #ref salt

# boundary conditions

params01['no_slip_sides'] = False
params01['no_slip_bottom'] = False
params01['rigidLid'] = False
params01['implicitFreeSurface'] = True
# params01['implicSurfPress'] = 1.0
# params01['implicDiv2DFlow'] = 1.0
params01['selectAddFluid'] = 1
# params01['useRealFreshWaterFlux'] = True #we add fluid above, so I think this is un-needed
params01['exactConserv'] = True
params01['implicitViscosity'] = True
params01['implicitDiffusion'] = True

# physical parameters
params01['f0'] = 1.37e-4
params01['beta'] = 0.0e-13
params01['gravity'] = g

# misc
params01['hFacMin'] = 0.05
params01['nonHydrostatic'] = True
params01['readBinaryPrec'] = 64
params01['useSmag3D'] = True
params01['smag3D_coeff'] = 1e-4

# ## Numeric solvers and I/O controls

# numeric solver parameters 

params02 = {}
params02['cg2dMaxIters'] = 300
params02['cg2dTargetResidual'] = 1e-13
params02['cg3dMaxIters'] = 20
params02['cg3dTargetResidual'] = 1e-8

# time stepping parameters 
params03 = {}
params03['dumpInitAndLast'] = False  #Reduce number of dumped files, no state vars saved
params03['nIter0'] = 0
#params03['endTime'] = 864000.0 # This is set elsewhere
deltaT = 25
params03['abEps'] = 0.1

#if run_config['testing']:
    
params03['chkptFreq'] = 0.0
params03['pChkptFreq'] = 86400.0
params03['taveFreq'] = 0.0
params03['dumpFreq'] = 0.0
params03['taveFreq'] = 0.0
params03['monitorFreq'] = 21600.0 # 6 hours
params03['monitorSelect'] = 1

# Force with yearly cycle
nt = 48
daysOfCycle = 365
# ForcingValue = np.sin(2*np.pi * np.arange(nt)/nt) # This sets temp variations at BCs
ForcingValue = assign_deltaT * np.ones(nt) # This sets temp variations at BCs

params03['periodicExternalForcing'] = True
params03['ExternForcingPeriod'] = daysOfCycle*86400/nt
params03['ExternForcingCycle'] = daysOfCycle*86400 

if run_config['test']:
    nTimeSteps = 10
else:
    nTimeSteps = np.ceil(run_config['ndays']*secsInDay/deltaT)

simTimeAct = nTimeSteps*deltaT

params03['endTime'] = int(simTimeAct) #int(params03['nIter0']*deltaT+simTimeAct)
params03['deltaT'] = np.round(deltaT)
grid_params['Nt'] = nTimeSteps

# gather params for data file 
params04 = {} #<-- using params04 to be consistent with ordering in Andrew's code
params04['usingCartesianGrid'] = grid_params['usingCartesianGrid']
params04['usingSphericalPolarGrid'] = grid_params['usingSphericalPolarGrid']
params04['delX']  = grid_params['delX']
params04['delY'] = grid_params['delY']
params04['delZ'] = dz


# get data fnames param
params05 = {}
params05['bathyFile'] ='bathymetry.bin'
params05['hydrogThetaFile'] = 'T.init'
params05['hydrogSaltFile'] = 'S.init'

if(makeDirs):
    data_params = [params01, params02, params03, params04, params05]
    #data file
    rcf.write_data(run_config, data_params, group_name='data', lf=run_config['lf'])
    #SIZE file
    rcf.createSIZEh(run_config, grid_params)

#========================================================================================
# Stability Check 
u_char = .5 #[m/s]
S_adv = 2 * (u_char * deltaT)/run_config['horiz_res_m']  # < 0.5 (Courant–Friedrichs–Lewy)
S_in = params01['f0'] * deltaT # < 0.5 (adams bashforth II)
# S_lh = 8 * params01['viscAh'] * deltaT /(run_config['horiz_res_m']**2) # < 0.6 
S_lv = 4 * params01['viscAz'] * deltaT /(dz[0]**2) # < 0.6 

S_adv_brg = 2 * (u_char * deltaT)/(run_config['horiz_res_m']*(1 - iceCoverage/100))  # < 0.5 (Courant–Friedrichs–Lewy)
S_in = params01['f0'] * deltaT # < 0.5 (adams bashforth II)
S_lv_brg = 4 * params01['viscAz'] * deltaT /((dz[0]*(1 - iceCoverage/100))**2) # < 0.6
setUpPrint('====== Stability Check =====')
setUpPrint("S_adv: <0.5 , S_in: <0.5 , S_lv: <0.6")
setUpPrint("S_adv: %.04f, S_in: %.04f, S_lv: %.04f" % (S_adv, S_in, S_lv))
setUpPrint("S_adv: %.04f, S_in: %.04f, S_lv: %.04f" % (S_adv_brg, S_in, S_lv_brg))

#========================================================================================
# Diagnostics

# adjust output frequency
if run_config['test']:
    run_config['inst_freq'] = 1 # multiples of timestep
    run_config['tavg_freq'] = 1 # multiples of timestep
    
else:
    run_config['inst_freq'] = 1 # multiples of hours (must be at least every day for coupling to get iceberg melt rates)
    run_config['tavg_freq'] = 1 # multiples of hours 


#---------specify time averaged fields------#
# NOTE: many more options available see mitgcm docs
diag_fields_avg = [['THETA','SALT','UVEL','WVEL','VVEL'],
                    ['BRGfwFlx','BRGhtFlx','BRGmltRt','BRG_TauX','BRG_TauY','BRGhFacC'],
                    ['icefrntW','icefrntT','icefrntS','icefrntA','icefrntR'],
                    ['TRAC01','TRAC02']
                    ]
diag_fields_max = 0
diag_fields_avg_name = ['dynDiag','BRGFlx','plumeDiag','ptraceDiag']

numdiags_avg = len(diag_fields_avg)
numdiags_avg_total = 0
for i in range(len(diag_fields_avg)):
    numdiags_avg_total += len(diag_fields_avg[i])
diag_phase_avg = 0.0

if run_config['test'] == True:
    diag_freq_inst = -run_config['inst_freq']*deltaT # negative values indicate snapshots at given interval
    diag_freq_avg = run_config['tavg_freq']*deltaT # positive values indicate time average over specified interval
else:
    diag_freq_inst = -run_config['inst_freq']*3600
    diag_freq_avg = run_config['tavg_freq']*3600
    
    
diag_params01 = {}
diag_params01['diag_mnc'] = False #<---you would need to modify this if you want netcdf output

for ii in range(numdiags_avg):  
    n = ii+1
    if len(diag_fields_avg[ii]) > diag_fields_max:
        diag_fields_max = len(diag_fields_avg[ii])
    diag_params01['fields(1:%i,%s)'%(len(diag_fields_avg[ii]),n)] ="','".join(diag_fields_avg[ii])
    diag_params01['fileName(%s)'%n] = diag_fields_avg_name[ii]
    diag_params01['frequency(%s)'%n] = diag_freq_avg
    diag_params01['timePhase(%s)'%n] = diag_phase_avg

    
#--------specify instanteous fields (i.e. snapshots)--------#
#removed for now to reduce file sizes, can add by just uncommenting
# diag_fields_inst = [['THETA','SALT','UVEL','WVEL','VVEL']]
# diag_fields_names = ['dynDiag']
# numdiags_inst = len(diag_fields_inst)
# diag_phase_inst = 0.0

# for ii in range(numdiags_inst):
#     n = numdiags_avg+ii+1
#     if len(diag_fields_inst[ii]) > diag_fields_max:
#         diag_fields_max = len(diag_fields_inst[ii])
#     diag_params01['fields(1:%i,%s)'%(len(diag_fields_inst[ii]),n)] = "','".join(diag_fields_inst[ii])
#     diag_params01['fileName(%s)'%n] = diag_fields_names[ii] + '_inst'
#     diag_params01['frequency(%s)'%n] = diag_freq_inst
#     diag_params01['timePhase(%s)'%n] = diag_phase_inst

setUpPrint('Diagnostic Settings')
setUpPrint(diag_params01)

Ndiags = n

diag_params02={}
diag_params = [diag_params01, diag_params02]


#========================================================================================
# Boundary Conditions
setUpPrint('====== Boundary Conditions =====')


#Tracer Mask, which locations have tracers, and which one, included in input mass
nTracers = 2 #increase here if we'd like to have more tracers
# MUST MATCH PTRACERS_SIZE.h

obcs_params01 = {}
obcs_params02 = {}
obcs_params03 = {}

obcs_params01['OB_singleIeast'] = -1
obcs_params01['OB_Jsouth(%i:%i)'%(grid_params['Nx']-indexOSC+1,grid_params['Nx'])] = np.ones(indexOSC,dtype=int)
obcs_params01['OB_Jnorth(%i:%i)'%(grid_params['Nx']-indexOSC+1,grid_params['Nx'])] = -1*np.ones(indexOSC,dtype=int)
obcs_params01['useOBCSsponge'] = True
obcs_params01['useOBCSprescribe']= True
#East
obcs_params01['OBEsFile']='EBCs.bin'
obcs_params01['OBEtFile']='EBCt.bin'
obcs_params01 ['OBEvFile']='EBCv.bin'
obcs_params01 ['OBEptrFile']="EBCptr.bin','EBCptr.bin" #must match nTracers if you want to zero them all at BCs
#North
obcs_params01['OBNsFile']='NsBCs.bin'  
obcs_params01['OBNtFile']='NsBCt.bin'  
obcs_params01 ['OBNvFile']='NsBCv.bin'
obcs_params01 ['OBNptrFile']="NsBCptr.bin','NsBCptr.bin" #must match nTracers if you want to zero them all at BCs
#South
obcs_params01['OBSsFile']='NsBCs.bin'
obcs_params01['OBStFile']='NsBCt.bin'
obcs_params01 ['OBSvFile']='NsBCv.bin'
obcs_params01 ['OBSptrFile']="NsBCptr.bin','NsBCptr.bin" #must match nTracers if you want to zero them all at BCs

obcs_params03['spongeThickness'] = int(domain_params['L_sponge'] / run_config['horiz_res_m']) #grid cells
obcs_params03['Urelaxobcsinner'] = 86400.0
obcs_params03['Urelaxobcsbound'] = 3600.0
obcs_params03['Vrelaxobcsinner'] = 86400.0
obcs_params03['Vrelaxobcsbound'] = 3600.0
obcs_params = [obcs_params01, obcs_params02, obcs_params03]
if(makeDirs):
    rcf.write_data(run_config, diag_params, group_name='diagnostics')
    
    ## create DIAGNOSTICS_SIZE.h
    Nlevels = grid_params['Nr']
    rcf.createDIAGSIZEh(run_config, Ndiags, Nlevels)

    # create eedata
    rcf.create_eedata(run_config, grid_params['nTx'], grid_params['nTy'])

    #create data.obcs
    rcf.write_data(run_config, obcs_params, group_name='obcs')

#========================================================================================
#Domain initialization and saving
setUpPrint('====== Domain Initialization =====')

def write_bin(fname, data):
    setUpPrint(fname + " " + str(np.shape(data)))
    if(writeFiles):
        data.astype(">f8").tofile(run_config['run_dir']+'/input/'+fname)
    else:
        setUpPrint('Not saving')
#Similar params as fed into MITgcm, but redeclared here
gravity = 9.81
sbeta = 8.0e-4
talpha = 0.4e-4
rho0 = 999.8
T0 = 1
S0 = 34

x = np.zeros([grid_params['Ny'], grid_params['Nx']])
x[:, 0] = run_config['horiz_res_m'] / 2

for i in np.arange(1, grid_params['Nx']):
    x[:,i] = x[:, i - 1] + run_config['horiz_res_m']

y = np.zeros([grid_params['Ny'], grid_params['Nx']])
y[0, :] = run_config['horiz_res_m'] / 2

for j in np.arange(1, grid_params['Ny']):
    y[j,:] = y[j-1, :] + run_config['horiz_res_m']

z = -np.cumsum(dz)


# Topography
fjordEnd = int(grid_params['Nx'] - indexOSC)


d = np.zeros([grid_params['Ny'], grid_params['Nx']]) - domain_params['H']
setUpPrint('fjord end: %i' %fjordEnd)
d[:wallWidthInd, 1:fjordEnd] = 0  # walls of fjord
d[-wallWidthInd:, 1:fjordEnd] = 0
d[: , :(terminus_index+1)] = 0 #cap west side


plt.figure(figsize=(10, 4))
plt.plot(x[5,:],d[5,:])
cp = plt.pcolormesh(x,y,d)
ax = plt.gca()
cbar = plt.colorbar(cp,orientation="horizontal",fraction=0.06)
ax.set_aspect('equal')
if(writeFiles):
    plt.savefig("%sbathymetry" % (run_config['run_dir']+'/input/'))
plt.show()
plt.close()

write_bin("bathymetry.bin", d)

# Temp/Salt/Vel  initial/boundaries nt is for time varying BCs
from scipy import interpolate
t2 = np.zeros([grid_params['Nr'],grid_params['Ny'],grid_params['Nx']])
s2 = np.zeros([grid_params['Nr'],grid_params['Ny'],grid_params['Nx']])
S2 = np.zeros([nt,grid_params['Nr'],grid_params['Ny']])
T2 = np.zeros([nt,grid_params['Nr'],grid_params['Ny']])
S_ns = np.zeros([nt,grid_params['Nr'],(grid_params['Nx'])])
T_ns = np.zeros([nt,grid_params['Nr'],(grid_params['Nx'])])
V_ns = np.zeros([nt,grid_params['Nr'],(grid_params['Nx'])])
W_ns = np.zeros([nt,grid_params['Nr'],(grid_params['Nx'])])
Ptr_ns = np.zeros([nt,grid_params['Nr'],(grid_params['Nx'])])

if(season_sw == 's'):
    ## Sermilik Summer Data
    ## 10.1029/2018GL077000 is paper 
    ## https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.nodc:0171277 has data
    data_tmp=np.load('shelfProfile2015.npz')
elif(season_sw == 'w'):
    ## Sermilik Winter
    ## Access DOI: DATASET | Published 2021 | doi:10.18739/A2M03XZ2K
    data_tmp=np.load('shelfProfile2010.npz')

# Can have bad values, toss them, set deepest measure equal to deeped valid measure for interpolator to work
s_tmp = data_tmp['S']
s_tmp[abs(s_tmp)>40] = np.nan
t_tmp = data_tmp['T']
t_tmp[abs(t_tmp)>10] = np.nan
z_tmp = 1*data_tmp['z']

# Smooth the data over a window of 5 meters
window_size = 5
weights = np.ones(window_size) / window_size
s_smth = np.convolve(s_tmp, weights, mode='same')
t_smth = np.convolve(t_tmp, weights, mode='same')

#Fill the bottom of these profiles with a valid value for interpolator
s_smth[-1] = s_tmp[~np.isnan(s_tmp)][-1]
t_smth[-1] = t_tmp[~np.isnan(t_tmp)][-1]
## Sermilik Winter like 2 layer
# t_avg = 1
# t_del = 4
# s_avg = 34
# s_del = 1.5
# pyclineDepth = 175
# pyclineThickness = 30
# z_tmp =  np.arange(0,600,20); #must be increasing, so do depth as positive, see negs later for z[:]
# t_tmp =  t_del / np.pi * np.arctan(2 * (z_tmp - pyclineDepth)/pyclineThickness) + t_avg
# s_tmp =  s_del / np.pi * np.arctan(2 * (z_tmp - pyclineDepth)/pyclineThickness) + s_avg

# # Sermilik Winter like 
# z_tmp =  np.asarray([   0,  100,  200,  250,  300,  500,  600]); #must be increasing, so do depth as positive, see negs later for z[:]
# t_tmp =  np.asarray([-2.2,   -2,   -1,    0,  1.8,  1.9,  2.0]);
# s_tmp =  np.asarray([-1.2,   -1,  -.5, 0.25,  0.8,  0.9,  1.0]) + 33;

# # Old Default
# z_tmp =  np.asarray([  0,  10,   50,  100,  200,  300, 500]); #must be increasing, so do depth as positive, see negs later for z[:]
# t_tmp =  np.asarray([  1,   1,  1.5,  1.8,  2.1,  2.3, 2.6]);
# s_tmp =  np.asarray([ 33,33.2, 33.8, 34.0, 34.3, 34.4,34.6]);
t_int = interpolate.PchipInterpolator(z_tmp[~np.isnan(s_smth)], t_smth[~np.isnan(s_smth)],extrapolate=False)
s_int = interpolate.PchipInterpolator(z_tmp[~np.isnan(s_smth)], s_smth[~np.isnan(s_smth)],extrapolate=False)
for j in np.arange(0,grid_params['Ny']):
    for i in np.arange(0, grid_params['Nx']):
        t2[:, j, i] = t_int(-1 * z[:])
        s2[:, j, i] = s_int(-1 * z[:])


#East BC
for j in np.arange(0,grid_params['Ny']):
    for k in range(nt):
        T2[k,:,j] = t_int(-1 * z[:]) 
        S2[k,:,j] = s_int(-1 * z[:])

#BC for V at East side
Ve = np.zeros([nt,grid_params['Nr'],grid_params['Ny']])
Ve[:,:,:] = 0 #[m/s]
Ptr_e = np.zeros([nt,grid_params['Nr'],grid_params['Ny']])

## N/S BCs
#coastal flow location and width
mean = 3 + fjordEnd
std_dev = 3

for i in np.arange(fjordEnd,grid_params['Nx']):
    for k in range(nt):
        T_ns[k,:,i] = t_int(-1 * z[:])
        S_ns[k,:,i] = s_int(-1 * z[:])
        V_ns[k,:,i] = -1 * oscStrength * np.exp(-0.5 * ((i - mean) / std_dev) ** 2) #[m/s] along coast flow south

plt.figure()
plt.plot(V_ns[0,0,:],label='along coast current')
plt.legend()
plt.ylabel('Along shore current')
plt.xlabel('Along fjord grid count')
plt.show()
plt.close()

## Seasonal Variation in Temp
# ForcingValue = ForcingValue[::-1] #flipping forcing for melange building
T_ns = T_ns + ForcingValue[:, None, None]
T2 = T2 + ForcingValue[:, None, None]
T_ns[-1,:,:] = T_ns[0,:,:]
T2[-1,:,:] = T2[0,:,:]

sampleTForcing = T2[:,int(grid_params['Nr']/2),0]
setUpPrint('Sample BC Temp change, mid depth:')
setUpPrint(sampleTForcing)

write_bin("T.init", t2)
write_bin("S.init", s2)
write_bin("EBCs.bin", S2)
write_bin("EBCt.bin", T2)
write_bin("EBCv.bin", Ve)
write_bin("NsBCs.bin", S_ns)
write_bin("NsBCt.bin", T_ns)
write_bin("NsBCv.bin", V_ns)
write_bin("NsBCW.bin", W_ns)
write_bin("NsBCptr.bin", Ptr_ns)
write_bin("EBCptr.bin", Ptr_e)

if OSX == 'Darwin':  #only work on Mac for now, can't get gsw installed on PACE
    d_tmp = data_tmp['density']
    pressure = -1 * np.ones(np.shape(t2[:,0,0])) * 1020 * 9.81 * z /(1e4)
    CT = gsw.CT_from_t(s2[:,0,0], t2[:,0,0], 0)
    density = gsw.rho(s2[:,0,0], CT, 0) 
    density = density - np.mean(density) #in-stu density less mean
    d_tmp = d_tmp - np.nanmean(d_tmp)
plt.figure()
plt.plot(s2[:,1,1] - 34, z, 'b', label="Sref - 34")
plt.plot(t2[:,1,1], z, 'r', label="Tref")
if OSX == 'Darwin':
    plt.plot(CT, z, 'r--',label="$\\theta$ref")
    plt.plot(density, z, label="∆ Density",color='xkcd:pumpkin')
    plt.scatter(d_tmp, -z_tmp,color='xkcd:pumpkin')
plt.scatter(s_tmp - 34,-z_tmp,color='b')
plt.scatter(t_tmp,-z_tmp,color='r')
plt.legend()
if(writeFiles):
    plt.savefig("%sinitialTS" % (run_config['run_dir']+'/input/'))
plt.show()
plt.close()


#=======================================================================================
# Plume
setUpPrint('====== Plume =====')
runoffVel = np.zeros([nt,grid_params['Ny'],grid_params['Nx']])
runoffRad = np.zeros([nt,grid_params['Ny'],grid_params['Nx']])
plumeMask = np.zeros([grid_params['Ny'],grid_params['Nx']])

## Total runoff (m^3/s)
## Seasonal Peak
# runoff = -2*assign_plumeSGD * np.sin(2*np.pi * np.arange(nt)/nt) - assign_plumeSGD
# runoff[runoff <  10 ] = 10
# ## linear ramp
# runoff = 1 + assign_plumeSGD * np.arange(nt)/float(nt)
# runoff[-1] = runoff[0] #wrapping periodic BCs to ensure no shock
# runoff = runoff[::-1] #flipping around for building melange
## Constant
runoff = assign_plumeSGD * np.ones(nt)

setUpPrint('Runoff is:')
setUpPrint(runoff)
# velocity (m/s) of subglacial runoff
wsg = 1 

# ice front location
icefront=(terminus_index+1) # adjacent to wall at western end of domain, simulate wall of ice

# plume location
plume_loc = int(np.round(grid_params['Ny']/2))
setUpPrint('Plume Location: %i discharge: %f' %(plume_loc, np.nanmax(runoff)))
## Define plume-type mask 
# 1 = ice but no plume (melting only)
# 2 = sheet plume (Jenkins)
# 3 = half-conical plume (Morton/Slater)
# 4 = both sheet plume and half-conical plume (NOT YET IMPLEMENTED)
# 5 = detaching conical plume (Goldberg)
# POSITIVE values indicate ice front is orientated north-south
# NEGATIVE values indicate ice front is orientated east-west

# Create virtual ice wall
plumeMask[wallWidthInd:-wallWidthInd,icefront] = 1 
# Located 1 cell in from western boundary (need solid barrier behind), and extending across the fjord with (fjord walls either side)

# Specify discharge location
plumeMask[plume_loc,icefront] = 2 # runoff emerges from centre of grounding line (2 is line, 3 is semi-cone)

# specify a runoff velocity of 1 m/s
runoffVel[:,plume_loc,icefront] = wsg

# calculate channel radius
if(plumeMask[plume_loc,icefront] == 3):
    runoffRad[:,plume_loc,icefront] = np.sqrt(2 * runoff / (np.pi * wsg))
elif(plumeMask[plume_loc,icefront] == 2):
    runoffRad[:,plume_loc,icefront] = runoff/ (run_config['horiz_res_m'] * wsg)
else:
    runoffRad[:,plume_loc,icefront] = 0

tracerMask = np.zeros([nTracers,grid_params['Ny'],grid_params['Nx']])
tracerMask[0,plume_loc,icefront] = 1

# Write files
write_bin("runoffVel.bin", runoffVel)
write_bin("runoffRad.bin", runoffRad)
write_bin("plumeMask.bin", plumeMask)
write_bin("pTracerMask.bin", tracerMask)

print(plumeMask[:,1])
plt.figure()
cp = plt.pcolormesh(x,y,plumeMask)
plt.title('Plume Mask')
plt.colorbar(cp)
if(writeFiles):
    plt.savefig("%splumeMask" % (run_config['run_dir']+'/input/'))
plt.show()
plt.close()

plt.figure()
time = np.arange(nt)*params03['ExternForcingPeriod']/86400
if(plumeMask[plume_loc,icefront] == 3):
    plt.plot(time,runoffRad[:,plume_loc,icefront]**2 * np.pi * wsg,'SGD',linewidth=3, linestyle='--')
elif(plumeMask[plume_loc,icefront] == 2):
    plt.plot(time,runoffRad[:,plume_loc,icefront] * run_config['horiz_res_m'] * wsg,label='SGD',linewidth=3, linestyle='--')
ax1=plt.gca()
# ax2=ax1.twinx()
# ax2.plot(time,sampleTForcing,label='T mid-depth',color='r')
sampleTForcing
ax1.set_xlabel('Time [days]')
ax1.set_ylabel('SGD [$m^3/s$]')
# ax2.set_ylabel('Temp [C]')
ax1.legend()
# ax2.legend()
if(writeFiles):
    plt.savefig("%splumeForcing" % (run_config['run_dir']+'/input/'))
plt.show()
plt.close()

## Boundary conditions

# pre-allocate
EBCu = np.zeros([nt,grid_params['Nr'],grid_params['Ny']])
setUpPrint('fjordMouthVelocity is:')
# Apply barotropic velocity to balance input of runoff
if np.max(runoff) > 0:
    for i in range(nt):
        fjordMouthCrossSection = -np.sum(d[:,-1]) * run_config['horiz_res_m']
        fjordMouthVelocity = runoff[i]/fjordMouthCrossSection
        # Out-of-domain velocity is positive at eastern boundary
        EBCu[i,:,:] = fjordMouthVelocity
setUpPrint(EBCu[:,0,0])
write_bin("EBCu.bin", EBCu)

if(run_config['make_icebergs']):
    #=======================================================================================
    # Make Bergs, now all in python
    setUpPrint('====== Making mélange =====')
    #Make Masks

    hfacThreshold = .9

    nz = grid_params['Nr']
    ny = grid_params['Ny']
    nx = grid_params['Nx']

    deltaY = run_config['horiz_res_m']
    deltaX = run_config['horiz_res_m']

    bergMask = np.zeros([ny,nx])
    driftMask = np.zeros([ny,nx])
    meltMask = np.zeros([ny,nx])
    barrierMask = np.zeros([ny,nx])
    bergConc = np.zeros([ny,nx])
    bergMaskNums = np.zeros([ny,nx])
    numBergsPerCell = np.zeros([ny,nx],dtype=np.int64)

    # Berg parameters
    bergType = 1 # 1 = block 2 = cone (not implemented)
    alpha = 1.9 * 2 # slope of inverse power law size frequency distribution
    scaling = 1 # 1 = Sulak 2017 2 = Barker 2004
    maxBergDepth = iceBergDepth # (m) - set to zero if 'prescribing' max iceberg width, set at top here
    minBergDepth= 40 # (m)
    maxBergWidth = 0 # (m) - set to zero if 'prescribing' max iceberg depth
    minBergWidth = 40 # (m)

    iceStart = (terminus_index+1)
    iceExtentIndex = int(np.round((iceExtent)/run_config['horiz_res_m']))

    # Iceberg mask
    bergMask[wallWidthInd:-wallWidthInd,iceStart:iceExtentIndex] = 1 # icebergs in inner 5 km, all oriented east-west

    # Drift mask, No drift for Melange experiments, but can toggle on here if you want
    # driftMask[1:-1,1:iceExtentIndex] = 1 # calculate effect of iceberg drift on melt rates 

    # Melt mask, only let bergs melt in this region (make melt water, these don't change size)
    meltMask[wallWidthInd:-wallWidthInd,iceStart:iceExtentIndex] = doMelt # Allow focus on blocking effect only

    # Barrier mask
    barrierMask[wallWidthInd:-wallWidthInd,iceStart:iceExtentIndex] = doBlock # make icebergs a physical barrier to water flow
    barrierMask[plume_loc,icefront] = 0 #Plume code struggles with hFac adjustments

    # Iceberg concentration (# of each surface cell that is filled in plan view)
    # bergConc[1:-1,iceStart:iceExtentIndex] = np.linspace(iceCoverage,10,(iceExtentIndex-1)) # iceberg concentration set at top
    bergConc[wallWidthInd:-wallWidthInd,iceStart:iceExtentIndex] = iceCoverage # iceberg concentration set at top

    # print(bergConc[1:-1,1:iceExtentIndex])

    desiredBergArea = np.sum(bergConc/100.0*deltaX*deltaY)
    bergMaskArea = np.sum(bergMask*deltaX*deltaY)
    setUpPrint('Area where bergs live: ' + str(bergMaskArea) + ' m^2')
    setUpPrint('Desired berg area: ' + str(desiredBergArea) + ' m^2')
    setUpPrint('Ratio: ' + str(desiredBergArea/bergMaskArea*100) + '%')

    if(scaling == 1): # then use Sulak17 volume-area scaling volume = 6.0*area^1.30
        # assumes volume = L*W*D and W = L/1.62 (Dowdeswell et al 1992)
        if(maxBergWidth==0):
            maxBergWidth = 0.0642449*maxBergDepth**(5/3)
            # minBergWidth = 0.0642449*minBergDepth**(5/3)
        elif(maxBergDepth==0):
            maxBergDepth = 5.19155*maxBergWidth**(5/3)
            minBergDepth = 5.19155*minBergWidth**(5/3)
    elif(scaling == 2): # Then use Barker04 width-depth relationship
        # Depth = 2.91*Width^0.71
        if(maxBergWidth==0):
            maxBergWidth = (100*10**(58/71)*maxBergDepth**(100/71)) / (291*291**(29/71))
            #minBergWidth = (100*10**(58/71)*minBergDepth**(100/71)) / (291*291**(29/71))        
        elif(maxBergDepth==0):
            maxBergDepth = 2.91*maxBergWidth^0.71
            minBergDepth = 2.91*minBergWidth^0.71

    numberOfBergs = 50 #low start, immediately doubled by scheme below, so guess low, high guesses (300%+) can cause to fail
    bergTopArea = 0
    areaResidual = 1
    # Generate the Inverse Power Law cumulative distribution function
    # over the range minBergWidth-maxBergWidth with a slope of alpha.
    setUpPrint('Making bergs, this can take a few loops...')
    loop_count = 1

    np.random.seed(2)
    setUpPrint('random seed set, not really random anymore')

    while(np.abs(areaResidual) > .005 ): # Create random power dist of bergs, ensure correct surface area
        numberOfBergs = round(numberOfBergs * (1 + areaResidual))  
        setUpPrint('\tnumberOfBergs: ' + str(numberOfBergs))
        x_width = np.arange(minBergWidth, maxBergWidth, (maxBergWidth-minBergWidth)/(numberOfBergs*1e2))
        inversePowerLawPDF_width = ((alpha-1) / minBergWidth) * (x_width/minBergWidth) ** (-alpha)
            # Get the CDF numerically
        inversePowerLawCDF_width = np.cumsum(inversePowerLawPDF_width)
            # Normalize
        inversePowerLawCDF_width = inversePowerLawCDF_width / inversePowerLawCDF_width[-1]
            
            # Generate number_of_bergs uniformly distributed random numbers.
        uniformlyDistributedRandomNumbers = np.random.uniform(0,1,numberOfBergs)
        
        inversePowerLawDistNumbers_width = np.zeros(uniformlyDistributedRandomNumbers.size);

        nearestIndex_width = [0] * uniformlyDistributedRandomNumbers.size

        nearestIndex_width = find_closest_indices(uniformlyDistributedRandomNumbers,inversePowerLawCDF_width)


        inversePowerLawDistNumbers_width = x_width[nearestIndex_width];
        inversePowerLawDistNumbers_length = inversePowerLawDistNumbers_width/1.12 # Widths are bigger 

        randScale = np.random.normal(6,1.22,numberOfBergs)
        randPower = np.random.normal(0.3,0.016,numberOfBergs)
        inversePowerLawDistNumbers_depth = randScale * (inversePowerLawDistNumbers_width*inversePowerLawDistNumbers_length) ** randPower * (920/1025)
        inversePowerLawDistNumbers_depth[inversePowerLawDistNumbers_depth < 5.123] = 5.123 #doest like round numbers, cap low end of bergs

        tooWide = np.count_nonzero(inversePowerLawDistNumbers_width > deltaX * np.sqrt(hfacThreshold))  #disallow completely full cells
        tooLong = np.count_nonzero(inversePowerLawDistNumbers_length > deltaX * np.sqrt(hfacThreshold))
        inversePowerLawDistNumbers_width[inversePowerLawDistNumbers_width > deltaX * np.sqrt(hfacThreshold)] = deltaX * np.sqrt(hfacThreshold) - .01  # Max width is grid cell (assumed square)
        inversePowerLawDistNumbers_length[inversePowerLawDistNumbers_length > deltaX * np.sqrt(hfacThreshold)] = deltaX * np.sqrt(hfacThreshold) - .01 # Max length is grid cell (assumed square)
        if(tooLong + tooWide > 0):
            setUpPrint('\t\tBergs clipped: %i for width, %i for length' % (tooWide, tooLong))
        
        bergTopArea = sum(inversePowerLawDistNumbers_width*inversePowerLawDistNumbers_length)
        areaResidual = (desiredBergArea - bergTopArea)/desiredBergArea
        setUpPrint('\t\t%.2f %% Bergs' % (bergTopArea/bergMaskArea*100))
        setUpPrint('\t\tareaResidual %.2f %%' % (areaResidual * 100))
        loop_count += 1
    setUpPrint('====== Success! Found our bergs =====')
    setUpPrint('Width min/mean/max: %f/%f/%f [m]' % (np.min(inversePowerLawDistNumbers_width),np.mean(inversePowerLawDistNumbers_width),np.max(inversePowerLawDistNumbers_width)))
    setUpPrint('Depth min/mean/max: %f/%f/%f [m]' % (np.min(inversePowerLawDistNumbers_depth),np.mean(inversePowerLawDistNumbers_depth),np.max(inversePowerLawDistNumbers_depth)))
    setUpPrint('Total Berg Area %f' % bergTopArea)
    setUpPrint('Total Berg fract: %.2f %%' % (bergTopArea/bergMaskArea*100))

    # Now we sort these berg into cell, randomly 
    bergMaski = 0  #Bad name, but this is the count of cells that will recieve bergs
    bergDict = {}

    for j in range(ny):
        for i in range(nx):
            if(bergMask[j,i] == 1):
                # print('i,j, bergmask',i,j,bergMask[j,i])
                bergMaski = 1 + bergMaski #Needs to start at 1, as non-bergs will be 0
                bergMaskNums[j,i] = bergMaski #Assign Mask Nums, not random as we'll randomly place bergs in cells
                bergDict[bergMaski] = [j,i] #This lets us do 1-D loops for the whole grid
    setUpPrint('%i cells with bergs' % bergMaski)

    # Sort my bergs
    sorted_indices = np.argsort(-inversePowerLawDistNumbers_depth) # Sort backwards to get descending from big to small bergs 
    sorted_depth = inversePowerLawDistNumbers_depth[sorted_indices]
    sorted_width = inversePowerLawDistNumbers_width[sorted_indices]
    sorted_length = inversePowerLawDistNumbers_length[sorted_indices]
    assignedCell = np.random.randint(0,bergMaski,[numberOfBergs]) # In this script, every berg has a home

    # Array for bergs
    bergsPerCellLimit = 500
    icebergs_depths = np.zeros([bergMaski,bergsPerCellLimit])
    icebergs_widths = np.zeros([bergMaski,bergsPerCellLimit])
    icebergs_length = np.zeros([bergMaski,bergsPerCellLimit])  #careful, not plural as to length match

    np.random.seed(2)
    assignedCell = np.random.randint(0,bergMaski,[numberOfBergs]) #every Berg has a spot

    icebergs_per_cell = np.zeros([bergMaski],dtype=np.int16)
    icebergs_area_per_cell = np.zeros([bergMaski])

    for i in range(numberOfBergs): 
        j = assignedCell[i]
        # print('looking at mask number',j,'at berg',i)
        # print('Berg number', icebergs_per_cell[j],'in this cell')
        # print('Width, Length',sorted_width[i], sorted_length[i])
        bergArea = sorted_width[i] * sorted_length[i]
        loopLimiter = 0
        while(bergArea > (deltaX * deltaY  * (bergConc[bergDict[j+1][0],bergDict[j+1][1]]/100) - icebergs_area_per_cell[j])): #if above 'full', pick random new cell, accept with decreasing probability
            j_old = j
            j = np.random.randint(0,bergMaski)
            loopLimiter += 1
            if((bergArea + icebergs_area_per_cell[j])/(deltaX * deltaY) < hfacThreshold - .01): #only consider accepting if under 95
                odds = np.abs(np.random.normal(0,.5,1))  #randomly accepts those that are big in overfull cells, but at decreasing frequency
                overFull = ((bergArea + icebergs_area_per_cell[j])/(deltaX * deltaY)*100 - bergConc[bergDict[j+1][0],bergDict[j+1][1]])
                if(odds > overFull):
                    # print('accepting overfull')
                    assignedCell[i] = j  #if we it a shuffling critera
                    break
            if(loopLimiter > bergMaski*20): #eventually we have to force some in
                indexesAllowed = np.where((deltaX * deltaY * hfacThreshold - icebergs_area_per_cell)  > bergArea)
                # print(indexesAllowed)
                randi = np.random.randint(0,len(indexesAllowed[0]))
                j = indexesAllowed[0][randi]
                assignedCell[i] = j  #if we it a shuffling critera, must line up for calculation below
                setUpPrint('\t Randomly missed, will force into cell with room: %i' % j)
                if((np.min(icebergs_area_per_cell) + bergArea)/(deltaX * deltaY) > hfacThreshold):
                    setUpPrint('WARNING cell very full: %.2f%%' %((np.min(icebergs_area_per_cell) + bergArea)*100/(deltaX * deltaY)))
                break
        
        icebergs_depths[j,icebergs_per_cell[j]] = sorted_depth[i]
        icebergs_widths[j,icebergs_per_cell[j]] = sorted_width[i]
        icebergs_length[j,icebergs_per_cell[j]] = sorted_length[i]
        icebergs_per_cell[j] += 1
        # icebergs_area_per_cell[j] = np.sum(icebergs_widths[j,:]*icebergs_length[j,:])
        icebergs_area_per_cell[j] += bergArea
    setUpPrint('Bergs per cell and filled faction at surface for spot check')     
    setUpPrint(icebergs_per_cell)
    setUpPrint(np.round(icebergs_area_per_cell/(deltaX*deltaY),2))
    setUpPrint('Max fill is: %.2f%%' % (np.nanmax(icebergs_area_per_cell/(deltaX*deltaY))*100))

    # All bergs now sorted 
    openFrac = np.zeros([nz,ny,nx])
    SA = np.zeros([nz,ny,nx])
    SA[:,:,:] = np.nan

    #This loop knows where all bergs are already, different from searching for all bergs across entire grid
    for i in range(bergMaski):
        bergCount = icebergs_per_cell[i]
        numBergsPerCell[bergDict[i+1][0],bergDict[i+1][1]] = bergCount
        if(bergCount > 0):
            lengths = icebergs_length[i,icebergs_length[i,:] > 0] #return only non-zeros
            widths = icebergs_widths[i,icebergs_widths[i,:] > 0] #return only non-zeros
            depths = icebergs_depths[i,icebergs_depths[i,:] > 0] #return only non-zeros
            for k in range(nz):
                cellVolume = deltaX*deltaY*dz[k]
                d_bot = sum_z[k] #bottom of depth bin
                d_top = sum_z[k] - dz[k]
                volume1 = dz[k] * lengths[depths > d_bot] * widths[depths > d_bot]
                SA1 = dz[k]*2*(lengths[depths > d_bot] + widths[depths > d_bot])
                partialFill = (depths < d_bot) & (depths > d_top)
                #partial fill
                volume2 = (depths[partialFill] - d_top) * lengths[partialFill] * widths[partialFill]
                #partial sides
                SA2 = (depths[partialFill] - d_top)*2*(lengths[partialFill] + widths[partialFill]) 
                #bottom
                SA3 = lengths[partialFill] * widths[partialFill]
                #print(np.sum(volume1), np.sum(volume2))
                openFrac[k,bergDict[i+1][0],bergDict[i+1][1]] = 1-((np.sum(volume1) + np.sum(volume2))/cellVolume)
                SA[k,bergDict[i+1][0],bergDict[i+1][1]] = np.sum(SA1) + np.sum(SA2) + np.sum(SA3)
        elif(bergCount == 0):
            openFrac[:,bergDict[i+1][0],bergDict[i+1][1]] = 1
            SA[:,bergDict[i+1][0],bergDict[i+1][1]] = 0

    icebergs_depths2D = np.zeros([bergsPerCellLimit,grid_params['Ny'],grid_params['Nx']])
    icebergs_widths2D = np.zeros([bergsPerCellLimit,grid_params['Ny'],grid_params['Nx']])
    icebergs_length2D = np.zeros([bergsPerCellLimit,grid_params['Ny'],grid_params['Nx']])

    for k in range(bergMaski):
        j = bergDict[k+1][0]
        i = bergDict[k+1][1]
        icebergs_depths2D[:,j,i] = icebergs_depths[k,:]
        icebergs_widths2D[:,j,i] = icebergs_widths[k,:]
        icebergs_length2D[:,j,i] = icebergs_length[k,:]

    if(forceDraft): #need a reference melange to do this
        mX=np.load(run_config['run_dir']+'/input/melangeX.npy')
        mH=np.load(run_config['run_dir']+'/input/melangeH.npy')
        x_fd = np.arange(deltaX/2,deltaX*(nx+.5),deltaX)
        depthHelper = icebergs_depths2D.copy()
        depthHelper[depthHelper == 0] = np.nan
        depthMedian = np.nanmedian(depthHelper,axis=[0,1])
        del depthHelper
        for i in range(nx):
            for j in range(ny):
                if(~np.isnan(depthMedian[i])):
                    icebergs_depths2D[:,j,i] = icebergs_depths2D[:,j,i]/depthMedian[i]*np.interp(x,mX,mH,0,0)[i]
                    # pass

        for i in range(nx):
            for j in range(ny):
                numberOfBergs = len(icebergs_length2D[icebergs_length2D[:,j,i] > 0])
                if(numberOfBergs > 0):
                    lengths = icebergs_length2D[icebergs_length2D[:,j,i] > 0,j,i] #return only non-zeros
                    widths = icebergs_widths2D[icebergs_widths2D[:,j,i] > 0,j,i] #return only non-zeros
                    depths = icebergs_depths2D[icebergs_depths2D[:,j,i] > 0,j,i] #return only non-zeros
                    for k in range(nz):
                        cellVolume = deltaX*deltaY*dz[k]
                        d_bot = sum_z[k] #bottom of depth bin
                        d_top = sum_z[k] - dz[k]
                        volume1 = dz[k] * lengths[depths > d_bot] * widths[depths > d_bot]
                        SA1 = dz[k]*2*(lengths[depths > d_bot] + widths[depths > d_bot])
                        partialFill = (depths < d_bot) & (depths > d_top)
                        #partial fill
                        volume2 = (depths[partialFill] - d_top) * lengths[partialFill] * widths[partialFill]
                        #partial sides
                        SA2 = (depths[partialFill] - d_top)*2*(lengths[partialFill] + widths[partialFill]) 
                        #bottom
                        SA3 = lengths[partialFill] * widths[partialFill]
                        #print(np.sum(volume1), np.sum(volume2))
                        openFrac[k,j,i] = 1-((np.sum(volume1) + np.sum(volume2))/cellVolume)
                        SA[k,j,i] = np.sum(SA1) + np.sum(SA2) + np.sum(SA3)
                elif(bergCount == 0):
                    openFrac[:,j,i] = 1
                    SA[:,j,i] = 0

    # Tracers for all Bergs, just set to 1 everywhere
    brgTracerMask = np.zeros([nTracers,grid_params['Ny'],grid_params['Nx']])
    brgTracerMask[1,:,:] = 1.0 

    # Plots for reference on whats happening berg-wise
    fig = plt.figure()
    plt.subplot(2,2,1)
    for i in range(bergMaski):
        plt.plot(openFrac[:,bergDict[i+1][0],bergDict[i+1][1]],-sum_z,alpha=.5,color='xkcd:gray',linewidth=.5)
    plt.plot(np.mean(openFrac[:,bergMask==1],1),-sum_z,alpha=1,color='xkcd:black',linewidth=1,linestyle='--',label='Average Bergs')
    plt.plot([0,1],[-maxBergDepth,-maxBergDepth],color = 'xkcd:red',linestyle=':', label='Target Max Depth')
    plt.plot([1-np.max(bergConc)/100,1-np.max(bergConc)/100],[-domain_params['H'],0],color = 'xkcd:gray',linestyle=':',label='Target Max Berg Conc')
    plt.xlabel('Open Fraction of Cells')
    plt.ylabel('Depth [m]')
    # plt.legend()  #not quite room so off for now

    plt.subplot(2,2,2)
    plt.hist(inversePowerLawDistNumbers_depth,bins = 50)
    plt.ylabel('Count')
    plt.xlabel('Depth [m]')

    plt.subplot(2,2,3)
    plt.hist(inversePowerLawDistNumbers_width,bins = 50)
    plt.ylabel('Count')
    plt.xlabel('Width [m]')

    plt.subplot(2,2,4)
    plt.hist(inversePowerLawDistNumbers_length,bins = 50)
    plt.ylabel('Count')
    plt.xlabel('Length [m]')
    plt.suptitle('Iceberg Stats')
    fig.tight_layout()
    if(writeFiles):
        plt.savefig(run_config['run_dir']+'/input/bergStatistics.png', format='png', dpi=200)
    plt.show()

    fig = plt.figure()
    pltHelper = 1-openFrac[0,:,:]
    pltHelper[bergMask == 0] = np.nan
    plt.subplot(211)
    pc = plt.pcolormesh(pltHelper,cmap='cmo.ice_r')
    cbar = plt.colorbar(pc)
    plt.suptitle('Iceberg Cover and Residual')
    plt.ylabel('Cell across fjord')
    cbar.set_label('iceberg cover')

    plt.subplot(212)
    pc = plt.pcolormesh(pltHelper - bergConc/100,cmap='cmo.ice')
    cbar = plt.colorbar(pc)
    pc_min = np.nanmin(pltHelper) * 100
    pc_max = np.nanmax(pltHelper) * 100
    plt.xlabel("Cell along fjord, Ice coverage is %.2f%% - %.2f%%" % (pc_min, pc_max))
    plt.ylabel('Cell across fjord')
    cbar.set_label('cover resid')
    fig.tight_layout()
    if(writeFiles):
        plt.savefig(run_config['run_dir']+'/input/bergMap.png', format='png', dpi=200)
    plt.show()

    fig = plt.figure()
    pc = plt.pcolormesh(barrierMask,cmap='cmo.ice_r')
    cbar = plt.colorbar(pc)
    plt.suptitle('Barrier Mask')
    plt.ylabel('Cell across fjord')
    plt.xlabel("Cell along fjord")
    fig.tight_layout()
    if(writeFiles):
        plt.savefig(run_config['run_dir']+'/input/barrierMask.png', format='png', dpi=200)
    plt.show()

    fig = plt.figure()
    pc = plt.pcolormesh(meltMask,cmap='cmo.ice_r')
    cbar = plt.colorbar(pc)
    plt.suptitle('Melt Mask')
    plt.ylabel('Cell across fjord')
    plt.xlabel("Cell along fjord")
    fig.tight_layout()
    if(writeFiles):
        plt.savefig(run_config['run_dir']+'/input/meltMask.png', format='png', dpi=200)
    plt.show()

    plt.figure()
    varphi = 1-openFrac
    varphi[varphi == 1] = np.nan
    pc = plt.pcolormesh(x[0,:],-sum_z,np.nanmean(varphi,axis=1),cmap='gray_r')
    cbar = plt.colorbar(pc)
    depthHelper = icebergs_depths2D.copy()
    depthHelper[depthHelper == 0] = np.nan
    plt.plot(x[0,:],np.nanmedian(-depthHelper,axis=[0,1]),color='xkcd:red')
    plt.suptitle('$\\varphi$')
    plt.ylabel('Depth [m]')
    plt.xlabel('Along fjord [m]')
    cbar.set_label('ice fraction')
    if(writeFiles):
        plt.savefig(run_config['run_dir']+'/input/draftPlot.png', format='png', dpi=200)
    plt.show()
    ## write iceberg txt files
    # setUpPrint('Saving text files for bergs...')
    # if(writeFiles):
    #     for i in range(bergMaski):
    #         with open(run_config['run_dir'] + '/input/iceberg_depth_%05i.txt' % (i+1) , 'w') as file_handler:
    #             for item in icebergs_depths[i,icebergs_depths[i,:] > 0]:
    #                 file_handler.write("{}\n".format(item))
    #         with open(run_config['run_dir']+'/input/iceberg_width_%05i.txt' % (i+1) , 'w') as file_handler:
    #             for item in icebergs_widths[i,icebergs_widths[i,:] > 0]:
    #                 file_handler.write("{}\n".format(item))
    #         with open(run_config['run_dir'] + '/input/iceberg_length_%05i.txt' % (i+1) , 'w') as file_handler:
    #             for item in icebergs_length[i,icebergs_length[i,:] > 0]:
    #                 file_handler.write("{}\n".format(item))

    # write global files
    write_bin('bergMask.bin',bergMask)
    write_bin('bergMaskNums.bin',bergMaskNums)
    write_bin('numBergsPerCell.bin',numBergsPerCell)
    write_bin('openFrac.bin',openFrac)
    write_bin('totalBergArea.bin',SA)
    write_bin('meltMask.bin',meltMask)
    write_bin('driftMask.bin',driftMask)
    write_bin('barrierMask.bin',barrierMask)
    write_bin('icebergs_depths.bin',icebergs_depths2D)
    write_bin('icebergs_widths.bin',icebergs_widths2D)
    write_bin('icebergs_length.bin',icebergs_length2D)
    write_bin('brg_tracerMask.bin',brgTracerMask)

    setUpPrint('Berg setup is done.')
#========================================================================================
#Update files in INPUT directory
setUpPrint('====== Cleaning up input/ files =====')
# a bit of a hack to adjust default values just for this exp

def replaceAll(file,searchExp,replaceExp):
    for line in fileinput.input(file, inplace=1):
        if searchExp in line:
            line = line.replace(searchExp,replaceExp)
        sys.stdout.write(line)

#turn on ICEBERG if off
if(makeDirs):
    replaceAll(run_config['run_dir'] + '/input/data.pkg','ICEBERG=.FALSE.', 'ICEBERG=.TRUE.') 



#========================================================================================
# PACE (GaTech) 
setUpPrint('====== sbatch script and settings =====')

cluster_params = {}
cluster_params['cluster_name'] = 'PACE'
cluster_params['opt_file'] = 'darwin_amd64_gfortran' #<-- may need to update this at some point
cluster_params['mvapich2_ver'] = '2.3.7' #'4.0.3' 4.1.2'
cluster_params['mvapich2_inc_dir'] = '/usr/local/pace-apps/spack/packages/linux-rhel9-x86_64_v3/gcc-12.3.0/mvapich2-2.3.7-1-qv3gjagtbx5e3rlbdy6iy2sfczryftyt/' 
cluster_params['netcdf_dir'] = ''
cluster_params['use_mpi'] = True

cluster_params['email'] = email

cluster_params['exps_dir'] = run_config['run_dir']
cluster_params['run_dir'] = os.path.join(cluster_params['exps_dir'], run_config['run_name'])
cluster_params['cpus_per_node'] = 10 

#extra run commands for the sbatch script
extraList = ["# ~/.conda/envs/MITgcm/bin/python RunModelMpi.py"]

run_config['extraCommands'] = "".join(extraList)
     

# ## Estimate wall clock time
ncpus = run_config['ncpus_xy'][0]*run_config['ncpus_xy'][1]
setUpPrint('===== Wall Clock Time =====')
estTime = int(grid_params['Ny']) * int(grid_params['Nx']) * int(grid_params['Nr']) * int(grid_params['Nt']) *2e-7
setUpPrint('Estimated run time is %.2f hours for one CPU' % (estTime/60))
setUpPrint('Estimated run time is %.2f hours for %i CPUs\n' % (estTime/60/ncpus*1.2,ncpus))

comptime_hrs = estTime/60/ncpus*1.2 

if(makeDirs):
    print(os.getcwd())
    #some coupling files
    os.makedirs("%s/couplingResults" %run_config['run_dir'], exist_ok=True)
    os.makedirs("%s/figs" %run_config['run_dir'], exist_ok=True)
    os.system("cp ../experiments/melangeModelExample.py %s/melangeModel.py" %run_config['run_dir'])
    os.system("ln -s ../makeBuild.sh %s" %run_config['run_dir'])
    os.system("ln -s ../makeRun.sh %s" %run_config['run_dir'])
    
    if os.path.isfile(run_config['run_dir']+'/input/setupReport.txt'):   
        os.remove(run_config['run_dir']+'/input/setupReport.txt')
        setUpPrint('previous setupReport.txt deleted in '+ run_config['run_dir']+'/input/')
    shutil.move('setupReport.txt', run_config['run_dir']+'/input')
    shutil.copy('finModel.py', run_config['run_dir']+'/input/buildScript.py')
    replaceAll(run_config['run_dir']+'/input/buildScript.py','makeDirs = False', 'makeDirs = False') 
    rcf.createSBATCHfile_Sherlock(run_config, cluster_params, walltime_hrs=1.2*comptime_hrs, email=email, mem_GB=1)
    setupNotes.close()
    print('Done! Remember to build before you run the script, building on MPI time is very inefficient')
elif(writeFiles):
    print("Done! You shouldn't have to rebuild as we only changed run time options here")
else:
    print('Nothing was saved, I hope you liked the pretty plots at least')
