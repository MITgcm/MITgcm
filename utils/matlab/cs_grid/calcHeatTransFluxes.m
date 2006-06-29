function HT = calcHeatTransFluxes(varargin);

% HT = calcHeatTransFluxes(od,ad,sd,og,ag,time,blkFile,[optional]);
% HT = calcHeatTransFluxes(od,ad,sd,og,ag,time,blkFile,'grav',9.81);
%
% Input:
%   The incoming data must be in a structured array format (which is the
%   format that comes from rdmnc) having the following fields:
%       od  [Ocean tave data]  T,Ttave
%       ad  [Atmos tave data]  T,Ttave
%       sd  [Aim tave data  ]  T,TSRtave,OLRtave,SSRtave,SLRtave,
%                              EVAPtave,SHFtave,EnFxPrtave
%       og  [Ocean grid data]  drF,HFacC,rA
%       ag  [Atmos grid data]  drF,HFacC
%   Other input parameters:
%       time     (vec)  Time levels to analyze ([] for all, miminum 3)
%       blkFile  (str)  Broken line file (eg 'isoLat_cs32_59.mat')
%   Optional parameters:
%       'grav'   (num, default 9.81)  Acceleration due to gravity
%       'LhVap'  (num, default 2501)  Latent heat of vaporization
%       'CpO'    (num, default 3994)  Specific heat capacity of water
%       'RhoO'   (num, default 1035)  Density of sea water
%       'CpA'    (num, default 1004)  Specific heat capacity of water
%
% Output:
%   HT is a structured array with the oceanic, atmpspheric, and total heat
%   transport at time(2:end-1) in PW, the calculated adjustments, heat
%   content (and its change) in the bands, and appropriate axis
%   information.
%
% Description:
%   Calculate heat transport from fluxes. Using a heat content budget
%   equation that equates the change in heat content of a zonal band with
%   the fluxes in and out of the top and bottom, find the poleward heat
%   transport by integrating from the north pole (HT = 0) southward.  The
%   adjustment to the flux fields required for the heat transport at the
%   south pole to be 0 is also tabulated.  In the atmosphere this
%   adjustment is associated with the frictional damping not coded to
%   increase the temperature.
% 
%   This function is built to be used with cubed-sphere output from the
%   Aim-ocean coupled model of the MITgcm.  The zonal bands are defined by
%   a set of broken lines, the information for which is contain in the
%   broken line file "blkFile".
%
% Original Author:  Daniel Enderton
%
% Usage:
% 
%   >> og = rdmnc('/GridOcn/grid.*');
%   >> ag = rdmnc('/GridAtm/grid.*');
%   >> od = rdmnc('/DataOcn/tave.*');
%   >> ad = rdmnc('/DataAtm/tave.*');
%   >> sd = rdmnc('/DataAtm/aim_tave.*');
%   
%   >> LhVap = 2500;
%   >> aHeat = 1004./9.81;  % aCp/g
%   >> oHeat = 3994.*1035;  % oCp*oRho
%   
%   >> HT = CalcHT_Fluxes(od,ad,sd,og,ag,[],LhVap,aHeat,oHeat,blkFile)
%
%   >> plot(HT.ylatHT,HT.HTocn(:,1),'g',...
%           HT.ylatHT,HT.HTatm(:,1),'b',...
%           HT.ylatHT,HT.HTtot(:,1),'r'); grid on;

StorageAtm = 1;
StorageOcn = 1;

% Default constants (can be overriden).
LhVap = 2501;
grav = 9.81;
CpO = 3994;
RhoO = 1035;
CpA = 1004;

% Read input parameters.
od = varargin{1};
ad = varargin{2};
sd = varargin{3};
og = varargin{4};
ag = varargin{5};
time = varargin{6};
blkFile = varargin{7};
for ii = 8:2:length(varargin)
    temp = varargin{ii+1}; eval([varargin{ii},' = temp;']);
end

aHeat = CpA./grav;
oHeat = RhoO.*CpO;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare / reform  incoming data                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Determine time indecies.
if isequal(od.T,ad.T) & isequal(od.T,sd.T)
    if isempty(time), time = od.T; i_time = 1:length(time);  
    else [dump,i_time] = ismember(time,od.T); end
else
    error('Time axes must match for ocean, atmospheric, and aim data!');
end

% Grid information.
nt = length(time);
nc = size(og.HFacC,2);
onr = size(og.HFacC,3);
anr = size(ag.HFacC,3);

odrf = og.drF;
adrf = ag.drF;
ac  = reshape(og.rA ,[6*nc*nc,1]);
ohc = reshape(og.HFacC(1:6*nc,1:nc,1:onr),[6*nc*nc,onr]);
ahc = reshape(ag.HFacC(1:6*nc,1:nc,1:anr),[6*nc*nc,anr]);

% Temperature fields.
if StorageOcn
    ot = reshape(od.Ttave(1:6*nc,1:nc,1:onr,i_time),[6*nc*nc,onr,nt]);
end
if StorageAtm
    at = reshape(ad.Ttave(1:6*nc,1:nc,1:anr,i_time),[6*nc*nc,anr,nt]);
end

% Aim fields.
toaSW  = reshape(   sd.TSRtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
toaLW  = reshape(   sd.OLRtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
srfSW  = reshape(   sd.SSRtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
srfLW  = reshape(   sd.SLRtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
srfEvp = reshape(  sd.EVAPtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
srfSH  = reshape(   sd.SHFtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);
srfEFP = reshape(sd.EnFxPrtave(1:6*nc,1:nc,i_time),[6*nc*nc,nt]);

% Load broken line information.  Determine latitude axis of a broken line
% by the mean of the latitude of the edges along the broken line.  These
% are the latitude coordinates for the poleward heat transport calculations
% and the surface flux / heat contents values are the mean between two
% broken lines.
load(blkFile);
ydim=length(bkl_Ylat);
YlatAv=sum(bkl_Ysg,1)./(1+bkl_Npts');
ylatHT = [-90,YlatAv,90];
ylatSF = ( ylatHT(2:ydim+2) + ylatHT(1:ydim+1) )./2;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Make heat transport calculations                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Top of atmosphere and surface fluxes.
Ft = toaSW-toaLW;
Fs = srfSW-srfLW-LhVap.*srfEvp-srfSH+srfEFP;

% Loop over bands and calculate (per band) (1) surface area, (2) top of
% atmpsphere and surface fluxes, and (3) oceanic and atmospheric heat
% content.  There are nbkl broken lines and nbkl+1 bands between broken
% lines.  The variable "bkl_Zon" gives the band number (nbkl+1 total) for a
% given index between 0 and nbkl, that is, nbkl+1 total zones.
AreaZon = NaN.*zeros([ydim+1,1]);
Ft_Zon = NaN.*zeros([ydim+1,nt]);
Fs_Zon = NaN.*zeros([ydim+1,nt]);
HA = NaN.*zeros([ydim+1,nt]);
HO = NaN.*zeros([ydim+1,nt]);
dHAdt = NaN.*zeros([ydim+1,nt-2]);
dHOdt = NaN.*zeros([ydim+1,nt-2]);
for izon = 1:ydim+1
    ii = find(bkl_Zon == izon-1);
    AreaZon(izon,1) = sum(ac(ii));
    for it = 1:length(time)
        % ??? HFacC for surface fluxes? How to deal with runoff / land?
        Ft_Zon(izon,it) = sum(Ft(ii,it).*ac(ii));
        Fs_Zon(izon,it) = sum(Fs(ii,it).*ac(ii));
        if StorageAtm
            HA(izon,it) = sum(aHeat.*((at(ii,:,it).*ahc(ii,:))*adrf).*ac(ii));
        end
        HO(izon,it) = sum(oHeat.*((ot(ii,:,it).*ohc(ii,:))*odrf).*ac(ii));
    end
end

% Calculate change in heat content (this means that the heat transport is
% calculated for time(2:end-1) only, hence subsetting fluxes as well).
for it = 2:length(time)-1
    if StorageAtm
        dHAdt(:,it-1) = (HA(:,it+1)-HA(:,it-1))./(time(it+1)-time(it-1));
    end
    dHOdt(:,it-1) = (HO(:,it+1)-HO(:,it-1))./(time(it+1)-time(it-1));
end
Ft_Zon = Ft_Zon(:,2:end-1);
Fs_Zon = Fs_Zon(:,2:end-1);

% Compute offsets.
% ??? However small, why is there an oceanic offset?
ocnadj_Zon = Fs_Zon - dHOdt;
atmadj_Zon = Ft_Zon - Fs_Zon - dHAdt;
for it = 1:size(ocnadj_Zon,2)
    ocnadj(it) = sum(ocnadj_Zon(:,it))./sum(AreaZon);
    atmadj(it) = sum(atmadj_Zon(:,it))./sum(AreaZon);
end

% Using a heat content budget equation that equates the change in heat
% content of a band with the fluxes in and out of the top and bottom, find
% the poleward heat transport by integrating from the north pole (HT = 0)
% southward.  The above computed adjustment is required for the heat
% transport at the south pole to be 0.
HTocn = zeros(size(Ft_Zon)+[1,0]);
HTatm = zeros(size(Ft_Zon)+[1,0]);
for ilat = ydim+2:-1:2
    HTocn(ilat-1,:) =   HTocn(ilat,:)    ...
                      - Fs_Zon(ilat-1,:) ...
                      + dHOdt(ilat-1,:)  ...
                      + ocnadj.*AreaZon(ilat-1);
    HTatm(ilat-1,:) =   HTatm(ilat,:)    ...
                      - Ft_Zon(ilat-1,:) ...
                      + Fs_Zon(ilat-1,:) ...
                      + dHAdt(ilat-1,:)  ...
                      + atmadj.*AreaZon(ilat-1);
end
HTtot = HTocn + HTatm;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             Assign outputs                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

HT.time = time(2:end-1);
HT.ylatSF = ylatSF;
HT.ylatHT = ylatHT;
HT.AreaZon = AreaZon;
HT.Fs = Fs_Zon;
HT.Ft = Ft_Zon;
HT.HTtot = HTtot*1e-15;
HT.HTocn = HTocn*1e-15;
HT.HTatm = HTatm*1e-15;
HT.HO = HO(:,2:end-1);
HT.HA = HA(:,2:end-1);
HT.dHOdt = dHOdt;
HT.dHAdt = dHAdt;
HT.ocnadj = ocnadj;
HT.atmadj = atmadj;
