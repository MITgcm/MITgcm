%- this is an exemple for how to use "fromPhys2Dyn.m"
%  after running mitgcmuv in directory "build"
%- note: all pressure variables need to be in the same units; 

dir0='../build/';
Dp=rdmds([dir0,'Depth']); Dp=Dp/100 ; %<- convert to [mb]
hFacC=rdmds([dir0,'hFacC']);
dp=rdmds([dir0,'fizhidt']); 

delR=100*ones(1,10); %<- converted to [mb]
nP=size(dp,3); nr=length(delR);
%- old version only:
%  convert to mb & reverse level index in dp to match Physics levels order:
% var=dp/100; dp(:,:,[1:nP])=var(:,:,[nP:-1:1]);

%- load the field to interpolate to the Dynamics grid:
it=5; vPh=rdmds([dir0,'fizhi_T'],it);

%--Do the mapping: Physics.grid -> Dynamics.grid
%- 1rst time:
 [vDy,klev,dpDyn]=fromPhys2Dyn(vPh,dp,Dp,delR);
%- next time:
%[vDy,klev,dpDyn]=fromPhys2Dyn(vPh,dp,klev);

%- check versus hFacC:
for k=1:nr,
 dpDyn(:,:,k)=dpDyn(:,:,k)/delR(k);
end
dd=dpDyn-hFacC;
fprintf(' Check / hFac : max(|Diff|)= %e \n',max(max(max(abs(dd)))) );

