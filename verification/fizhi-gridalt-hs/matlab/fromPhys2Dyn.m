function [vDy,klev,dpD]=fromPhys2Dyn(vPh,dpP,inpPres,delR);
% [vDy,klev]=fromPhys2Dyn(vPh,dpP,Dp,delR); (1rst call)
% [vDy,klev]=fromPhys2Dyn(vPh,dpP,klev);    (next calls)
%
%  vPh :: 3.D input field on Physics grid, to put on Dynamics grid.
%  dpP :: delta.P field (3D) for Physics grid.
%  Dp  :: Total air-column thickness (2.D, pressure units)
% delR :: Dynamics delta.P (vector) (pressure units)
% klev :: index of the Dynamics level for each physics grid level (integer array, 3.D)
%  vDy :: 3.D output field on Dynamics grid.
%  dpD :: delta.P field (3D) for dynamics grid (~hFacC*delR) [usefull for checking]
%
%- 1rst call:
% Dp=rdmds('Depth'); delR (from file "data"); dpP from diagnostics output ('DPPHYS  ')
% [vDy,klev]=fromPhys2Dyn(vPh,dpP,Dp,delR);
%- next calls:
% [vDy]=fromPhys2Dyn(vPh,dpP,klev);
% $Header: /u/gcmpack/MITgcm/verification/fizhi-gridalt-hs/matlab/fromPhys2Dyn.m,v 1.1 2005/07/07 23:51:52 jmc Exp $

if nargin < 4,
 first=0;
 klev=inpPres;
 if size(klev) ~= size(vPh),
   fprintf(' Mismatch in size vPh:');fprintf(' %i',size(vPh));
   fprintf(' & klev:');fprintf(' %i',size(klev)); 
   fprintf('\n => stop'); vDy=0; klev=0; return
 end
 nr=max(max(max(klev)));
else
 first=1;
 Dp=inpPres;
 nr=length(delR);
end

 ncx=size(dpP,1); nc=size(dpP,2); nP=size(dpP,3); nPg=ncx*nc;
 epsil=1.e-6;

%- find corresponding levels:
if first == 1,
 fprintf(' Compute klev: ');
 TimeT0=clock;
 var=Dp./sum(dpP,3);
 for k=1:nP, dpP(:,:,k)=var.*dpP(:,:,k); end
 pp1=cumsum(dpP,3); pp1=reshape(pp1,nPg,nP);
 pDw=zeros(1,nr+1); pDw(nr:-1:1)=cumsum(delR(nr:-1:1));
 klev=ones(nPg,nP);
 for ij=1:nPg,
  for k=1:nP,
   [K]=find(pDw>pp1(ij,k)-epsil);
   klev(ij,k)=max(K);
  end
 end
 klev=reshape(klev,ncx,nc,nP);
 TimeT1=clock;
 fprintf(' <- done (Time= %8.3f s)\n',etime(TimeT1,TimeT0));
else
 if size(klev) ~= size(vPh),
   fprintf(' Mismatch in size vPh:');fprintf(' %i',size(vPh));
   fprintf(' & klev:');fprintf(' %i',size(klev)); 
   fprintf('\n => stop'); vDy=0; klev=0; return
 end
end

 fprintf(' Average on Dyn.Grid: ');
 TimeT2=clock;
 vDy=zeros(ncx,nc,nr);
 dpD=zeros(ncx,nc,nr);
 for k=1:nr,
   var=dpP; var(find(klev~=k))=0;
   dpDyn=sum(var,3);
   dpD(:,:,k)=dpDyn;
   dpDyn(find(dpDyn==0))=-1;
   var=var.*vPh;
   vDyn=sum(var,3)./dpDyn;
   vDyn(find(dpDyn==-1))=0;
   vDy(:,:,k)=vDyn;
 end
 TimeT3=clock;
 fprintf(' <- done (Time= %8.3f s)\n',etime(TimeT3,TimeT2));

return
