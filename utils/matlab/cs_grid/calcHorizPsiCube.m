function [psiH,xv,yv,psiH_cubeC] = calcHorizPsiCube(d,g,rstar,psiLineF);

% [psiH,xv,yv,psiH_cubeC] = calcHorizPsiCube(d,g,rstar,psiLineF);
%
% Input arguements:  
%   The incoming field data (d) and grid data (g) must be in a structured
%   array format (which is the format that comes from rdmnc):
%       d  [Field data]  hUtave,hVtave (rstar=1) OR uVeltave,vVeltave (=0)
%       g  [Grid data ]  drF,dxG,dyG,HFacW,HFacS
%   Other input parameters:
%       rstar    (str)  0 or 1 if you are using r* coordinates or not
%       psiLineF (str)  File ('psiLine_N2S_cs32')
%
% Output fields:
%   psiH        Barotropic streamfunction, vector points (eg [6146,nt])
%   xv          Longitude of vector points (eg [6146,nt])
%   yv          Latitude of vector points (eg [6146,nt])
%   psiH_cubeC  'psiH' interpolated to cell center points (eg [192,32,nt])
%
% Description:
%   Caculates barotropic stream function (psi).  Data should be in
%   z-coordinates and the output is the volume transport psi [10^6 m^3/s =
%   Sv].  If the rstar parameters is on, hu and hv are used, if off, the
%   hfacw*.u and hfacs*.v are used (the multiplication being done inside
%   the function).
%
% Original Author:  Jean-Michel Campin
% Modifications:  Daniel Enderton

nc = size(g.XC,2);
nr = length(g.drF);
nt = size(d.uVeltave,4);

xv = reshape(g.XG(1:6*nc,1:nc),[6*nc*nc,1]);
yv = reshape(g.YG(1:6*nc,1:nc),[6*nc*nc,1]);
xv(end+1)=xv(1);  yv(end+1)=yv(1+2*nc);
xv(end+1)=xv(1+3*nc);  yv(end+1)=yv(1);

dxg = reshape(g.dxG(1:6*nc,1:nc),[6*nc*nc,1])*1.e-6;
dyg = reshape(g.dyG(1:6*nc,1:nc),[6*nc*nc,1])*1.e-6;
delR = reshape(g.drF,[1,nr]);
hw = reshape(g.HFacW(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
hs = reshape(g.HFacS(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
if rstar
    hu = reshape(d.hUtave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    hv = reshape(d.hVtave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
else
    hu = reshape(d.uVeltave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    hv = reshape(d.vVeltave(1:6*nc,1:nc,1:nr,1:nt),[6*nc*nc,nr,nt]);
    for it = 1:nt
        hu(:,:,it) = hw.*hu(:,:,it);
        hv(:,:,it) = hs.*hv(:,:,it);
    end
end

% Load: 'psi_N','psi_C','psi_P','psiUV','psi_T'
load(psiLineF);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  Calculate horizontal stream function                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compute depth integrated volume transport through faces.
utrs = NaN.*zeros([6*nc*nc,nt]);
vtrs = NaN.*zeros([6*nc*nc,nt]);
for it = 1:nt
    utrs(:,it) = sum((dyg*delR).*hu(:,:,it),2);
    vtrs(:,it) = sum((dxg*delR).*hv(:,:,it),2);
end

% Compute barotropic stream function.  A little description of what the
% variables are and how the computation is done would be nice.
psiH = zeros(6*nc*nc+2,nt);
for it = 1:nt
    psiNx = size(psi_C,1);
    psiNy = size(psi_C,2);
    %nPt2 = ;
    ufac = rem(psi_T,2); % Mask for u transport.
    vfac = fix(psi_T/2); % Mask for v transport.
    for j = 2:psiNy
        for i = 1:psi_N(j)
            i0 = psi_P(i,j);
            i1 = psi_C(i,j);
            i2 = psiUV(i,j);
            psiH(i1,it) =   psiH(i0,it) ...
                          + ufac(i,j)*utrs(i2,it) ...
                          + vfac(i,j)*vtrs(i2,it);
        end
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Interpolate to grid tracer (cell center) points.            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

psiH_cubeC = NaN.*zeros([6*nc,nc,nt]);
for it = 1:nt
    psiH_extra = psiH(end-1:end,it);
    temp  = reshape(psiH(1:end-2,it),[6*nc,nc]);

    [nx ny nt]=size(temp);
    temp=permute( reshape(temp,[nx/6 6 ny]),[1 3 2]);
    temp(end+1,:,:)=NaN;
    temp(:,end+1,:)=NaN;
    temp(end,:,[1 3 5])=temp(1,:,[2 4 6]);
    temp(:,end,[2 4 6])=temp(:,1,[3 5 1]);
    temp(:,end,[1 3 5])=squeeze(temp(1,end:-1:1,[3 5 1]));
    temp(end,:,[2 4 6])=squeeze(temp(end:-1:1,1,[4 6 2]));
    temp(1,end,[1 3 5]) = psiH_extra(1);
    temp(end,1,[2 4 6]) = psiH_extra(2);

    temp = 0.25 .* (temp(1:nc  ,1:nc  ,:) + ...
                          temp(2:nc+1,1:nc  ,:) + ...
                          temp(1:nc  ,2:nc+1,:) + ...
                          temp(2:nc+1,2:nc+1,:));

    psiH_cubeC(:,:,it) = reshape(permute(temp,[1 3 2]),[6*nc,nc]);
end
