% m-file: mit_loadglobaldata.m
% read all the data files for the 4x4 global run and take time averages
% of it
precision = mit_getparm('data','readBinaryPrec');
if isempty(precision); precision = 32.; end
if precision == 32
  acc = 'real*4';
elseif precision == 64;
  acc = 'real*8';
else
  error('readBinaryPrec contains unknown precision')
end

nx = grd.nx;
ny = grd.ny;
nz = grd.nz;

% load restoring fields and compute annual mean
thetaClimFile = mit_getparm('data','thetaClimFile');
if isempty(thetaClimFile) | thetaClimFile == ' ';
  tdmean = repmat(NaN,size(grd.hfacc(:,:,1)));
else
  tdmean = squeeze(mean(mit_readfield(thetaClimFile,[nx ny 12],acc),3));
end
saltClimFile = mit_getparm('data','saltClimFile');
if isempty(saltClimFile) | saltClimFile == ' ';
  sdmean = repmat(NaN,size(grd.hfacc(:,:,1)));
else
  sdmean = squeeze(mean(mit_readfield(saltClimFile,[nx ny 12],acc),3));
end
EmPmRFile = mit_getparm('data','EmPmRFile');
if isempty(EmPmRFile) | EmPmRFile == ' ';
  empr = repmat(NaN,nx,ny);
else
  empr = squeeze(mean(mit_readfield(EmPmRFile,[nx ny 12],acc),3));
end
empr = change(empr,'==',0,NaN);
surfQFile = mit_getparm('data','surfQFile');
if isempty(surfQFile) | surfQFile == ' ';
  qnet = repmat(NaN,nx,ny);
else
  qnet = squeeze(mean(mit_readfield(surfQFile,[nx ny 12],acc),3));
end
qnet = change(qnet,'==',0,NaN);
thetaFile = mit_getparm('data','hydrogThetaFile');
if isempty(thetaFile) | thetaFile == ' ';
  tdatamean = repmat(NaN,size(grd.hfacc));
else
  tdatamean = squeeze(mean(mit_readfield(thetaFile,[nx ny nz 12],acc),4));
end
msgbin = dir('sal.bin');
saltFile = mit_getparm('data','hydrogSaltFile');
if isempty(saltFile) | saltFile == ' ';
  sdatamean = repmat(NaN,size(grd.hfacc));
else
  sdatamean = squeeze(mean(mit_readfield(saltFile,[nx ny nz 12],acc),4));
end

if ~strcmp(grd.buoyancy,'OCEANIC');
  sdatamean = sdatamean(:,:,end:-1:1);
  tdatamean = tdatamean(:,:,end:-1:1);
end
