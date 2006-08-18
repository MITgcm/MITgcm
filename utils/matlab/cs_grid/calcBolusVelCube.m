function [ub,vb]=calcBolusVelCube(d,g);

% [ub,vb] = calcEulerPsiCube(d,g);
%
% Input arguements:  
%   The incoming field data (d) and grid data (g) must be in a structured
%   array format (which is the format that comes from rdmnc):
%       d  [Field data]  Kwx,Kwy
%       g  [Grid data ]  drF,rA,dxC,dyC,dxG,dyG,HFacW,HFacS
%
% Original author:  Jean-Michel Campin
% Modifications:  Daniel Enderton
%
% JMCs old comments:
%-- GM-Bolus transp. : 
%   from Kwx & Kwy (Skew-Flx => /2), 
%    compute Volume Stream function psiX,psiY above uVel.vVel 
%     (at interface between 2 levels), units=m^3/s :
%     psiX=(rAc*kwx)_i / dXc ; psiY=(rAc*kwy)_j / dYc ;
%   and then the bolus velocity (m/s):
%     ub = d_k(psiX)/dYg/drF ; vb = d_k(psiY)/dXg/drF ;
%---------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Prepare / reform  incoming data                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nc = size(g.XC,2);
nr = length(g.drF);
nt = size(d.GM_Kwx,4);

dr  = g.drF;
hw = reshape(g.HFacW(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
hs = reshape(g.HFacS(1:6*nc,1:nc,1:nr),[6*nc*nc,nr]);
ra  = reshape(g.rA(1:6*nc,1:nc) ,[6*nc*nc,1]);
dxc = reshape(g.dxC(1:6*nc,1:nc),[6*nc*nc,1]);
dyc = reshape(g.dyC(1:6*nc,1:nc),[6*nc*nc,1]);
dxg = reshape(g.dxG(1:6*nc,1:nc),[6*nc*nc,1]);
dyg = reshape(g.dyG(1:6*nc,1:nc),[6*nc*nc,1]);
kwx_all = reshape(d.GM_Kwx,[6*nc*nc,nr,nt]);
kwy_all = reshape(d.GM_Kwy,[6*nc*nc,nr,nt]);

rAu=dxc.*dyg;
rAv=dyc.*dxg;
%--- recip_hFac & mask :
hw_recip=1./hw; hw_recip(find(hw==0))=0; 
hs_recip=1./hs; hs_recip(find(hs==0))=0; 
hw=ceil(hw); hw=min(1,hw);
hs=ceil(hs); hs=min(1,hs);

for it = 1:nt
    kwx = kwx_all(:,:,it);
    kwy = kwy_all(:,:,it);

    %-- K*ra + add 1 overlap :
    kwx = 0.5*(ra*ones(1,nr)).*kwx;
    kwy = 0.5*(ra*ones(1,nr)).*kwy;
    kwx = reshape(kwx,[6*nc,nc,nr]);
    kwy = reshape(kwy,[6*nc,nc,nr]);
    v6X = split_C_cub(kwx,1);
    v6Y = split_C_cub(kwy,1);
    k6x = v6X(:,[2:nc+1],:,:);
    k6y = v6Y([2:nc+1],:,:,:);

    %----------------- 
    v6X = zeros(nc,nc,nr,6);
    v6Y = zeros(nc,nc,nr,6);

    v6X([1:nc],:,:,:) = k6x([2:nc+1],:,:,:) + k6x([1:nc],:,:,:);
    v6Y(:,[1:nc],:,:) = k6y(:,[2:nc+1],:,:) + k6y(:,[1:nc],:,:);

    v6X = v6X/2;
    v6Y = v6Y/2;

    psiX = zeros(6*nc,nc,nr+1);
    psiY = zeros(6*nc,nc,nr+1);

    for n = 1:6
        is = 1+nc*(n-1);ie=nc*n;
        psiX([is:ie],[1:nc],[1:nr]) = v6X([1:nc],[1:nc],[1:nr],n);
        psiY([is:ie],[1:nc],[1:nr])=v6Y([1:nc],[1:nc],[1:nr],n);
    end

    psiX = reshape(psiX,6*nc*nc,nr+1);
    psiY = reshape(psiY,6*nc*nc,nr+1);

    psiX(:,[1:nr]) = hw.*psiX(:,[1:nr]);
    psiY(:,[1:nr]) = hs.*psiY(:,[1:nr]);
    ub = psiX(:,[2:nr+1]) - psiX(:,[1:nr]);
    vb = psiY(:,[2:nr+1]) - psiY(:,[1:nr]);

    dr = reshape(dr,[1,length(dr)]);
    ub = reshape(hw_recip.*ub./(rAu*dr),[6*nc,nc,nr]);
    vb = reshape(hs_recip.*vb./(rAv*dr),[6*nc,nc,nr]);
    
    ub_all(:,:,:,it) = ub;
    vb_all(:,:,:,it) = vb;
end

ub = ub_all;
vb = vb_all;
