function [fldzon,mskzon,ylat,areazon] = calcZonalAvgCube(fld,ny,yc,ar,hc);

% [fldzon,mskzon,ylat,areazon] = calcZonalAvgCube(fld,ny,yc,ar,hc);
%
% Input arguements:  
%   fld     Data to zonally average ([6*n,n,nr,nt], [6*n,n,nr], [6*n,n])
%   ny      Number of evenly spaces latitude lines to zonally average onto
%   yc      Cell centered latitude
%   ar      Cell area
%   hc      HFacC
%
% Output arguement:
%   fldzon  Zonally averaged field ([ny,nr,nt], [ny,nr], [ny])
%   mskzon  Mask for zonal slice
%   ylat    Latitude coordinates for zonal average data
%   areazon Area associated with zonal band
%
% Description:
%   Calculates zonal average of "fld".
%
% Original author:  JMC
% Modifications: Daniel Enderton

% Dimension information.
nBas = 0;
dims = size(fld);
%if length(dims) > 4,
%    error(['To many dimensions (Max 4):  ',mat2str(dims)]);
%end
nc = dims(2); nr=1; nt=1; nDim=4;
if length(dims) > 2,
  if length(size(hc)) == 3, nr=size(hc,3); end
  if dims(3) ~= nr, nr=1; nDim=3; end
  nt=prod(dims(3:end))/nr;
end

% Reshape data.
if nr == 1, hc = hc(:,:,1); end % Account for nr = 1
hc = reshape(hc, [6*nc*nc,nr]);
yc = reshape(yc, [6*nc*nc,1] );
ar = reshape(ar, [6*nc*nc,1] );
fld= reshape(fld,[6*nc*nc,nr,nt]);

% Define latitude axis (with regular spacing) to average to.
dy=180/ny; ylat=-90+([1:ny]-0.5)*dy;

% Define minimum area fraction (relative to full latitude band):
frcZmn=0.05;                   % ??? Why

% Compute zonal line weights.
nMax = 6*nc*3;           % Max number of cells near zonal line
ijzav = zeros(ny,nMax);  % Index of cells near band line (within dy)
alzav = zeros(ny,nMax);  % Area weights for cells near band line
npzav = zeros(ny,1);     % Number of cells close to band line
for ij = 1:6*nc*nc                % Loop over all cells
    del=(yc(ij)-ylat(1))/dy;  % Latitude difference from bottom
    jz=1+floor(del);              % Number of zonal lines away (round up)
    del=rem(del,1);               % Fracional distance from upper line
    if jz < 1
        n = npzav(jz+1) + 1;   % Contribution from cell NORTH of line
        ijzav(jz+1,n) = ij;
        alzav(jz+1,n) = 1;     % Weight = 1 because all area accounted for
        npzav(jz+1) = n; 
    elseif jz >= ny
        n = npzav(jz) + 1;     % Contribution from cell NORTH of line
        ijzav(jz,n) = ij;
        alzav(jz,n) = 1;       % Weight = 1 because all area accounted for
        npzav(jz) = n;
    else
        n = npzav(jz) + 1;     % Contribution from cell SOUTH of line
        ijzav(jz,n) = ij;
        alzav(jz,n) = 1 - del;
        npzav(jz) = n;
        n = npzav(jz+1) + 1;   % Contribution from cell NORTH of line
        ijzav(jz+1,n) = ij;
        alzav(jz+1,n) = del;
        npzav(jz+1) = n;
    end
end
[NbMx,jM] = max(npzav);  % Reduce size
ijzav = ijzav(:,1:NbMx);
alzav = alzav(:,1:NbMx);

mskC = ceil(hc); % Cell centered mask:  1 = any land, 0 = no land

% Basin stuff:
% if nBas > 0
%     % standard 3. Sector definition:
%     % mskB=rdda([rac,'maskC_bas.bin'],[6*nc*nc nBas],1,'real*4','b');
%     % with Mediter in Indian Ocean Sector :
%     mskB = rdda([rac,'maskC_MdI.bin'],[6*nc*nc nBas],1,'real*4','b');
% elseif nBas < 0
%     landFrc = rdda([rac,'landFrc_cs32.zero.bin'],[6*nc*nc 1],1,'real*8','b');
%     mskB = zeros(6*nc*nc,abs(nBas));
%     mskB(:,1) = landFrc;
%     if nBas == -2, mskB(:,2) = 1 - landFrc; end
% end

% Basin stuff:
if nBas < 0, mskzon = zeros(ny,1+abs(nBas));
else,        mskzon = zeros(ny,nr,1+abs(nBas)); end
for nb = 1:1+abs(nBas)
    if nb == 1, vv1=ar; else vv1=ar.*mskB(:,nb-1); end
    if nBas < 0,
        var=vv1;
        for j=1:ny,
            ijLoc=ijzav(j,1:npzav(j));
            vvLoc=alzav(j,1:npzav(j))';
            mskzon(j,nb)=sum(vvLoc.*var(ijLoc));
        end
    else
        for k=1:nr,
            var=vv1.*hc(:,k); 
            for j=1:ny,
                ijLoc=ijzav(j,1:npzav(j));
                vvLoc=alzav(j,1:npzav(j))';
                mskzon(j,k,nb)=sum(vvLoc.*var(ijLoc));
            end
        end
    end
end

% Area associated with zonal lines:
areazon=zeros(ny,1);
for j=1:ny,
    ijLoc = ijzav(j,1:npzav(j));
    vvLoc = alzav(j,1:npzav(j))';
    areazon(j) = sum(vvLoc.*ar(ijLoc));
end

% Compute zonal average:
fldzon = zeros(ny,nr,1+abs(nBas),nt);
for it = 1:nt,
    fld1t = fld(:,:,it);
    for nb = 1:1+abs(nBas),
        if nb == 1, vv1 = ar;
        else        vv1 = ar.*mskB(:,nb-1); end
        for k = 1:nr,
            if nBas < 0, mskLoc=mskzon(:,nb);
            else         mskLoc=mskzon(:,k,nb); end
            var = vv1.*hc(:,k);
            var = var.*fld1t(:,k);
            for j = 1:ny
                if mskLoc(j) > frcZmn*areazon(j),
                    ijLoc=ijzav(j,1:npzav(j));
                    vvLoc=alzav(j,1:npzav(j))';
                    fldzon(j,k,nb,it)=sum(vvLoc.*var(ijLoc))/mskLoc(j);
                else
                    fldzon(j,k,nb,it)=NaN;
                end
            end
        end
    end
end
if length(dims) > nDim,
  fldzon=reshape(fldzon,[ny nr 1+abs(nBas) dims(nDim:end)]);
end
fldzon = squeeze(fldzon);
