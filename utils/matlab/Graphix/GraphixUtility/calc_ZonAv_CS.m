function [fldZb,mskZb,ylat]=...
    calc_ZonAv_CS(fld,kpr,kwr,nBas,xcs,ycs,xcg,ycg,arc,datadir,hFacC);
if (nargin < 4), nBas=0; end
krd=1;

%kpr=1 ; kwr=1; krd=1 ; %- comput weight + zonAv_mask + zonAv(fld)
%kpr=2 ; kwr=1; krd=1 ; %- read weight but comput zonAv_mask + zonAv(fld);
%kpr=3 ; kwr=1; krd=1 ; %- read weight & zonAv_mask but comput zonAv(fld);
%kpr=3 ; kwr=1; krd=0 ; %- read nothing except field, comput zonAv(fld);
%nBas=0: Global ZonAv only ; nBas=-1: atmos-> Glob+Land ; =-2: idem +Ocean ; 
%nBas>0 : ocean: Glob+(nbas)bassins

%nBas=-2;
%kpr=3 ; kwr=1; krd=1;
% dim3=0;

% Rac <- bassin mask , cs_grid & ZonAv weight
% dir <- hFacC & zonAv_mask
Rac = '/Users/enderton/Research/CoupledGFDExperiments/cs_grid/';
dir = datadir;

%fprintf('calc_ZonAv_CS:');
dims=size(fld);
if length(dims) > 4,
    fprintf('error: too many dimensions!\n');
    dims;
    return
end
if length(dims) < 4, nti=1 ; else nti=dims(4); end
if length(dims) < 3, nr=1 ;  else nr=dims(3);  end
fld=reshape(fld,dims(1),dims(2),nr,nti);
%fprintf('assume 3rd dim = nr = %i \n',nr);

%- define latitude Axis (with regular spacing) to average to:
ny=64 ; yyMx=90;
yyMn=-yyMx ; ddy=(yyMx-yyMn)/ny;
ylat=yyMn+([1:ny]-0.5)*ddy;

%- define minimum area fraction (relative to full latitude band):
frcZmn=0.05;
sufx02=['_Zav_',int2str(ny)];

%------------------------------------
rac=Rac ;
[ncx nc]=size(arc); nPg=ncx*nc;

if kpr == 1,
%- compute weight used for zonal average
% (no bassin, no mask at this level)

    x6c=reshape(xcs,nPg,1);
    y6c=reshape(ycs,nPg,1);

%---
    nMax=ncx*3;
    ijzav=zeros(ny,nMax);
    alzav=zeros(ny,nMax);
    npzav=zeros(ny,1);


    for ij=1:nPg,
        yloc=y6c(ij);
        del=(yloc-ylat(1))/ddy;
        jz=1+floor(del); del=rem(del,1);
        if jz < 1,
            jj=jz+1;
            n=npzav(jj)+1; 
            ijzav(jj,n)=ij;
            alzav(jj,n)=1;
            npzav(jj)=n; 
        elseif jz >= ny,
            jj=jz;
            n=npzav(jj)+1; 
            ijzav(jj,n)=ij;
            alzav(jj,n)=1;
            npzav(jj)=n; 
        else
            jj=jz;
            n=npzav(jj)+1; 
            ijzav(jj,n)=ij;
            alzav(jj,n)=1-del;
            npzav(jj)=n; 
            jj=jz+1;
            n=npzav(jj)+1; 
            ijzav(jj,n)=ij;
            alzav(jj,n)=del;
            npzav(jj)=n; 
        end
    end

%-- reduce size:
    [NbMx,jM]=max(npzav);
    %fprintf('Total Nb: %i \n',sum(npzav));
    %fprintf('Max Nb for lat(j=%i)=%3.2f : %i \n',jM,ylat(jM),NbMx);

    ijZav=ijzav(:,1:NbMx);
    alZav=alzav(:,1:NbMx);
    npZav=npzav;
    if kwr == 1,
        namfil=['zonAvWeigth_cs32_',int2str(ny)];
        save(namfil,'npZav','alZav','ijZav');
        %fprintf(['save npZav alZav & ijZav to file: ',namfil,'.mat \n']);
    end
else
    if krd > 0,
        namfil=['zonAvWeigth_cs32_',int2str(ny)];
        fprintf(['read npZav alZav & ijZav from file: ',namfil,'\n']);
        load([Rac,namfil]);
    end
end

if kpr > 0 & krd > 0,
    rac=dir;
%if nBas < 0, hFacC=ones(ncx,nc,dim3); else hFacC=rdmds([rac,'hFacC']); end
    %nr_hfac=size(hFacC,3);
    if nBas < 0, hFacC=ones(ncx,nc,nr); end%else hFacC=rdmds([rac,'hFacC']); end
    mskC=min(1,hFacC); mskC=ceil(mskC);
    %hFacC=reshape(hFacC,nPg,nr_hfac); mskC=reshape(mskC,nPg,nr_hfac);
    if nr == 1, hFacC = hFacC(:,:,1); mskC = mskC(:,:,1); end % Cludge by Daniel.
    hFacC=reshape(hFacC,nPg,nr); mskC=reshape(mskC,nPg,nr);
    a6c=reshape(arc,nPg,1);
    if nBas > 0,
%- standard 3. Sector definition:
  %mskB=rdda([Rac,'maskC_bas.bin'],[nPg nBas],1,'real*4','b');
%- with Mediter in Indian Ocean Sector :
        mskB=rdda([Rac,'maskC_MdI.bin'],[nPg nBas],1,'real*4','b');
    elseif nBas < 0,

  %landFrc=rdda([dir,'landFrc.cpl_FM.bin'],[nPg 1],1,'real*8','b');
        landFrc=rdda([dir,'landFrc_cs32.zero.bin'],[nPg 1],1,'real*8','b');

        mskB=zeros(nPg,abs(nBas));
        mskB(:,1)=landFrc;
        if nBas==-2, mskB(:,2)=1-landFrc ; end
    end
end

if kpr == 1 | kpr == 2,
    if nBas < 0, mskZb=zeros(ny,1+abs(nBas)); else mskZb=zeros(ny,nr,1+abs(nBas)); end
    for nb=1:1+abs(nBas),
        if nb == 1, vv1=a6c ; else vv1=a6c.*mskB(:,nb-1); end
        if nBas < 0,
            var=vv1;
            for j=1:ny,
                ijLoc=ijZav(j,1:npZav(j));
                vvLoc=alZav(j,1:npZav(j))';
                mskZb(j,nb)=sum(vvLoc.*var(ijLoc));
            end
        else
            for k=1:nr,
                var=vv1.*hFacC(:,k); 
                for j=1:ny,
                    ijLoc=ijZav(j,1:npZav(j));
                    vvLoc=alZav(j,1:npZav(j))';
                    mskZb(j,k,nb)=sum(vvLoc.*var(ijLoc));
                end
            end
        end
    end
 
    if kwr == 1,
        if nBas < 0, namH='landFrc'; else namH='hFacC'; end
        namfil=[rac,namH,sufx02];
        fid=fopen(namfil,'w','b'); fwrite(fid,mskZb,'real*8'); fclose(fid);
        %fprintf([' -> write mskZb to file: ',namfil,'\n']);
    end
    
elseif krd == 1,
    if nBas < 0, 
        namH='landFrc';
        namfil=[rac,namH,sufx02];
        mskZb=rdda(namfil,[ny 1+abs(nBas)],1,'real*8','b');
    else 
        namH='hFacC';
        namfil=[rac,namH,sufx02];
        mskZb=rdda(namfil,[ny nr 1+abs(nBas)],1,'real*8','b');
    end
    %fprintf([' <- read mskZb from file: ',namfil,'\n']);
end

if kpr > 0 ,
%- area Zon.Av:
    arZ=zeros(ny,1);
    for j=1:ny,
        ijLoc=ijZav(j,1:npZav(j)); vvLoc=alZav(j,1:npZav(j))';
        arZ(j)=sum(vvLoc.*a6c(ijLoc));
    end
    fldZb=zeros(ny,nr,1+abs(nBas),nti);
    for nt=1:nti,
        fld1t=reshape(fld(:,:,:,nt),nPg,nr);
        for nb=1:1+abs(nBas),
            if nb == 1, vv1=a6c ; else vv1=a6c.*mskB(:,nb-1); end
            for k=1:nr,
                if nBas < 0, mskLoc=mskZb(:,nb); else mskLoc=mskZb(:,k,nb); end
                var=vv1.*hFacC(:,k);
                var=var.*fld1t(:,k);
                for j=1:ny,
                    if mskLoc(j) > frcZmn*arZ(j),
                        ijLoc=ijZav(j,1:npZav(j));
                        vvLoc=alZav(j,1:npZav(j))';
                        fldZb(j,k,nb,nt)=sum(vvLoc.*var(ijLoc))/mskLoc(j);
                    else
                        fldZb(j,k,nb,nt)=NaN;
                    end
                end
            end
        end
    end
end

return
