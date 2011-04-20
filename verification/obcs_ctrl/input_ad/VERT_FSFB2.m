function [c2, Psi, G, N2, Pmid] = VERT_FSFB2(N2,Pmid)
%function [c2, Psi, G, N2, Pmid] = VERT_FSFB2(N2,Pmid)
%
%    VERT_FSFB.m
%
%     Gabriel A. Vecchi - May 12, 1998
%%%%%%%%%%%%%%%%
% 
%    Solves the discretized wave projection problem
%    given the vertical profiles of Temperature, Salinity, Pressure 
%    and the depth inteval length.
%
%  Uses the seawater function sw_bfrq to calculate N2.
%%%%%%%%%%%%%%%%
%
%    Arguments:
%       T = temperature vector at same depths as salinity and pressure.
%       S = salinity vector at same depths as temperature and pressure.
%       P = pressure vector at same depths as temperature and salinity.
%       Dz = length of depth interval in meters.
%%%%%%%%%%%%%%%%
%
%    Returns:
%	c2 = vector of square of the wavespeed.
%       Psi = matrix of eigenvectors (horizontal velocity structure functions).
%       G  =  matrix of integral of eigenvectors (vertical velocity structure functions).
%       N2 = Brunt-Vaisla frequency calculated at the midpoint pressures.
%       Pmid = midpoint pressures.
%%%%%%%%%%%%%%%%

%  Find N2 - get a M-1 sized vector, at the equator.
%[N2,crap,Pmid] = sw_bfrq(S,T,P,0);

for i = 1:length(N2)
   if N2(i) < 0
      N2(i) = min(abs(N2));
   end;
end;

% bdc: needs equally-spaced depths!
Dz= median(diff(Pmid));

% add a point for the surface
M = length(N2)+1;

%  Fill in D - the differential operator matrix.
%  Surface (repeat N2 from midpoint depth)
D(1,1) = -2/N2(1);
D(1,2) = 2/N2(1);
%  Interior
for i = 2 : M-1,
D(i,i-1) = 1/N2(i-1);
D(i,i) = -1/N2(i-1)-1/N2(i);
D(i,i+1) = 1/N2(i);
end
%  Bottom
D(M,M-1) = 2/N2(M-1);
D(M,M) = -2/N2(M-1);
D=-D./(Dz*Dz);
%bdc: no need for A?
% A = eye(M);

% Calculate generalized eigenvalue problem
% bdc: eigs gets top M-1
%[Psi,lambda] = eigs(D,[],M-1);
% use eig:
[Psi,lambda] = eig(D);

% Calculate square of the wavespeed.
c2 = sum(lambda);
c2=1./c2;

Psi = fliplr(Psi);
c2 = fliplr(c2);
for i=1:size(Psi,2)
  Psi(:,i) = Psi(:,i)/Psi(1,i);
end

% normalize?
G = INTEGRATOR(M,Dz)*Psi;

function [INT] = INTEGRATOR(M,Del)
%function [INT] = INTEGRATOR(M,Del)
%
%    INTEGRATOR.m
%
%     Gabriel A. Vecchi - June 7, 1998
%%%%%%%%%%%%%%%%
%    Generates and integration matrix.
%    Integrates from first point to each point.
%%%%%%%%%%%%%%%%

INT = tril(ones(M));
INT = INT - 0.5*(eye(M));
INT(:,1) = INT(:,1) - 0.5;
INT = INT*Del;


