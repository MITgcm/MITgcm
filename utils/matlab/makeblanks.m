% Code to make blanks for MITgcm based on model grid
% Optimizes sNx and sNy by minimizing the overlap area per core and maximizing the number of available cores used
%
% Need to change variables:
%	targetnodes: sets the number of nodes available for computation
%	corespernode: sets the number of cores on each node
%	OLx, OLy: overlap areas based on advection scheme
%
%
% Alex Andriatis
% aandriat@ucsd.edu
% 10-03-2020

targetnodes = 1 % How many nodes you need to fit into;
corespernode = 36 % Number of cores per node;

OLx = 4; % overlap area set in SIZE.h
OLy = 4;

maskInC = rdmds('maskInC'); % Can also use a mask from surface layer of hFacC
[nx ny] = size(maskInC)

% Get the integer factors of nx
k=1:nx;
dnx=k(rem(nx,k)==0);
dnx=dnx(dnx>10); % Remove potential sNx < 10
disp('Integer factors of nx are:');
disp(dnx);

% Get the integer factors of ny
k=1:ny;
dny=k(rem(ny,k)==0);
dny=dny(dny>10); % Remove potential sNy < 10
disp('Integer factors of ny are:');
disp(dny)

% Allocate matrices for variables
[DNY,DNX] = meshgrid(dny,dnx);
NBLANKS = NaN(size(DNY)); % Number of blanks created
NCORES = NBLANKS; % Number of cores used
NNODES = NBLANKS; % Number of nodes used
NOL = NBLANKS; % Overlap area per node
BLNKS = cell(size(DNY)); % Blank core numbers

% Loop through all combinations of snx and sny
for m=1:length(dnx)
  for n=1:length(dny)
    snx = dnx(m); sny = dny(n);
    npx = nx/snx; npy = ny/sny;
    count = 0;prcnm = 0;mblnks=0;
    for j = 1:npy
      for i = 1:npx
        prcnm = prcnm+1;
        tmp = maskInC((i-1)*snx+1:i*snx,(j-1)*sny+1:j*sny);
        if sum(tmp(:)) == 0 %TILE IS BLANK
          count = count + 1;
          mblnks(count) = prcnm;
        end
      end
    end
    ncores = npx*npy-count;
    nnodes = ncores/corespernode;

    NBLANKS(m,n) = count;
    NCORES(m,n) = ncores;
    NNODES(m,n) = nnodes;
    NOL(m,n) = OLx*2*sny + OLy*2*snx; % Overlap area per node
    BLNKS{m,n} = mblnks;
  end
end

% Remove all combinations using more than targetnodes nodes
I = find(NNODES<targetnodes);

% Find combinations with the min overlap area
minoverlap = min(NOL(I));
II = find(NOL(I)==minoverlap);
I = I(II);
 
% Find combinations using the most nubmer of cores
maxcores = max(NCORES(I));
II = find(NCORES(I)==maxcores);
I=I(II);

fprintf('For %i nodes and %i cores per node, the best sizes for a %ix%i grid are: \n', targetnodes, corespernode, nx, ny);
for i=1:length(I);
  ind=I(i);
  snx=DNX(ind); sny=DNY(ind); nblank=NBLANKS(ind); ncores=NCORES(ind); nnodes=NNODES(ind); blanks=BLNKS{ind};
  fprintf('sNx = %i, sNy = %i, nPx = %i, nPy = %i, resulting in %i blanks, and a total of %i cores on %0.2f nodes \n',snx,sny,nx/snx,ny/sny,nblank,ncores,nnodes);
end

% Unless it matters, choose the first of the best combinations
I=I(1);
blnks = BLNKS{I};
NB = NBLANKS(I);

% Write blank list
if NB > 0
NR = floor((NB-10)/10);
  fidm=fopen(['blanks.txt'],'w','b');
  if (10+NR*10)<NB
    fprintf(fidm,' blankList(1:%i)= ',NB);
    for i = 1:NB
      fprintf(fidm, '%i, ',blnks(i));
    end
  else
    fprintf(fidm,' blankList(1:%i)= %i, %i, %i, %i, %i, %i, %i, %i, %i, %i, \n',...
    NB,blnks(1),blnks(2),blnks(3),blnks(4),blnks(5),blnks(6),blnks(7),...
    blnks(8),blnks(9),blnks(10));
     for i =  1:NR
       fprintf(fidm,'       %i, %i, %i, %i, %i, %i, %i, %i, %i, %i, \n',...
       blnks(1+i*10),blnks(2+i*10),blnks(3+i*10),blnks(4+i*10),blnks(5+i*10), ...
       blnks(6+i*10),blnks(7+i*10),blnks(8+i*10),blnks(9+i*10),blnks(10+i*10));
     end
     if (10+NR*10)<NB
       for i = (10+NR*10+1) : NB
         fprintf(fidm,'       %i,\n',blnks(i));
       end
     end
  end
  fprintf(fidm,'\n \n \n \n\n');
  fclose(fidm);
end

