function [ub,vb]=calc_Bolus_CS(rAc,dXc,dYc,dXg,dYg,delR,kwx,kwy,hFacW,hFacS);
% [ub,vb]=calc_Bolus_CS(rAc,dXc,dYc,dXg,dYg,delR,kwx,kwy,hFacW,hFacS);
%-- GM-Bolus transp. : 
%   from Kwx & Kwy (Skew-Flx => /2), 
%    compute Volume Stream function psiX,psiY above uVel.vVel 
%     (at interface between 2 levels), units=m^3/s :
%     psiX=(rAc*kwx)_i / dXc ; psiY=(rAc*kwy)_j / dYc ;
%   and then the bolus velocity (m/s):
%     ub = d_k(psiX)/dYg/drF ; vb = d_k(psiY)/dXg/drF ;
%---------------------------------------------------------------------
% set_axis
[ncx,nc]=size(rAc); nr=length(delR);
nPg=6*nc*nc; ncp=nc+1;

rAu=dXc.*dYg; rAu=reshape(rAu,nPg,1);
rAv=dYc.*dXg; rAv=reshape(rAv,nPg,1);

%-- K*rAc + add 1 overlap :
tmp=reshape(rAc,nPg,1)*ones(1,nr);
kwx=reshape(kwx,nPg,nr);
kwy=reshape(kwy,nPg,nr);
tmp=0.5*tmp;
kwx=tmp.*kwx;
kwy=tmp.*kwy;
kwx=reshape(kwx,ncx,nc,nr);
kwy=reshape(kwy,ncx,nc,nr);
v6X=split_C_cub(kwx,1);
v6Y=split_C_cub(kwy,1);
k6x=v6X(:,[2:ncp],:,:);
k6y=v6Y([2:ncp],:,:,:);

%--- recip_hFac & mask :
hFacW=reshape(hFacW,nPg,nr);
hFacS=reshape(hFacS,nPg,nr);
rhFcW=1./hFacW; rhFcW(find(hFacW==0))=0; 
rhFcS=1./hFacS; rhFcS(find(hFacS==0))=0; 
hFacW=ceil(hFacW); hFacW=min(1,hFacW);
hFacS=ceil(hFacS); hFacS=min(1,hFacS);

%----------------- 

 v6X=zeros(nc,nc,nr,6);
 v6X([1:nc],:,:,:)=k6x([2:ncp],:,:,:)+k6x([1:nc],:,:,:);
 v6X=v6X/2;
 psiX=zeros(ncx,nc,nr+1); 
for n=1:6
 is=1+nc*(n-1);ie=nc*n;
 psiX([is:ie],[1:nc],[1:nr])=v6X([1:nc],[1:nc],[1:nr],n);
end
 psiX=reshape(psiX,nPg,nr+1); 
 psiX(:,[1:nr])=hFacW.*psiX(:,[1:nr]);
 ub=zeros(nPg,nr);
 ub=psiX(:,[2:nr+1])-psiX(:,[1:nr]);
 delR = reshape(delR,[1,length(delR)]);
 tmp=rAu*delR;
 ub=ub./tmp; ub=rhFcW.*ub;
 ub=reshape(ub,ncx,nc,nr);
 
%----------------- 

 v6Y=zeros(nc,nc,nr,6);
 v6Y(:,[1:nc],:,:)=k6y(:,[2:ncp],:,:)+k6y(:,[1:nc],:,:);
 v6Y=v6Y/2;
 psiY=zeros(ncx,nc,nr+1);
for n=1:6
 is=1+nc*(n-1);ie=nc*n;
 psiY([is:ie],[1:nc],[1:nr])=v6Y([1:nc],[1:nc],[1:nr],n);
end
 psiY=reshape(psiY,nPg,nr+1); 
 psiY(:,[1:nr])=hFacS.*psiY(:,[1:nr]);
 vb=zeros(nPg,nr);
 vb=psiY(:,[2:nr+1])-psiY(:,[1:nr]);
 tmp=rAv*delR;
 vb=vb./tmp; vb=rhFcS.*vb;
 vb=reshape(vb,ncx,nc,nr);
 
%----------------- 
return
