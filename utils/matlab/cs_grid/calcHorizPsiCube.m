function [psiH,xv,yv,psiH_cubeC] = calcHorizPsiCube(d,g);
%function [psiH]=calc_psiH_CS(u3d,v3d,hFacW,hFacS);
% [psi,mskZ,ylat]=calc_PsiH_CS(u3d,v3d,[hFacW,hFacS]);

%- Units: dx,dy /1e6 ; delR [m] ; psiH in Sv [10^12 m3/s]

rstar = 0;

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
    hu = hu(:,:,1);
    hv = hv(:,:,1);
end

%- load : 'psi_N','psi_C','psi_P','psiUV','psi_T' :
psiLineF='psiLine_N2S_cs32';
load(psiLineF);


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                  Calculate horizontal stream function                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compute depth integrated volume transport through faces.
utrs=(dyg*delR).*hu; utrs=sum(utrs,2);
vtrs=(dxg*delR).*hv; vtrs=sum(vtrs,2);

% Compute barotropic stream function.  A little description of what the
% variables are and how the computation is done would be nice.
psiNx = size(psi_C,1);
psiNy = size(psi_C,2);
nPt2 = 6*nc*nc+2;
psiH = zeros(nPt2,1);
ufac = rem(psi_T,2); % Mask for u transport.
vfac = fix(psi_T/2); % Mask for v transport.
for j = 2:psiNy
    for i = 1:psi_N(j)
        i0 = psi_P(i,j);
        i1 = psi_C(i,j);
        i2 = psiUV(i,j);
        psiH(i1) = psiH(i0)+ufac(i,j)*utrs(i2)+vfac(i,j)*vtrs(i2);
    end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             Interpolate to grid tracer (cell center) points.            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

psiH_extra = psiH(end-1:end);
psiH_cubeC  = reshape(psiH(1:end-2),[6*nc,nc]);

[nx ny nt]=size(psiH_cubeC);
psiH_cubeC=permute( reshape(psiH_cubeC,[nx/6 6 ny]),[1 3 2]);
psiH_cubeC(end+1,:,:)=NaN;
psiH_cubeC(:,end+1,:)=NaN;
psiH_cubeC(end,:,[1 3 5])=psiH_cubeC(1,:,[2 4 6]);
psiH_cubeC(:,end,[2 4 6])=psiH_cubeC(:,1,[3 5 1]);
psiH_cubeC(:,end,[1 3 5])=squeeze(psiH_cubeC(1,end:-1:1,[3 5 1]));
psiH_cubeC(end,:,[2 4 6])=squeeze(psiH_cubeC(end:-1:1,1,[4 6 2]));
psiH_cubeC(1,end,[1 3 5]) = psiH_extra(1);
psiH_cubeC(end,1,[2 4 6]) = psiH_extra(2);

psiH_cubeC = 0.25 .* (psiH_cubeC(1:nc  ,1:nc  ,:) + ...
                      psiH_cubeC(2:nc+1,1:nc  ,:) + ...
                      psiH_cubeC(1:nc  ,2:nc+1,:) + ...
                      psiH_cubeC(2:nc+1,2:nc+1,:));

psiH_cubeC = reshape(permute(psiH_cubeC,[1 3 2]),[6*nc,nc]);