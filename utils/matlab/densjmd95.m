function rho = densjmd95(s,t,p);
%function rho = densjmd95(S,Theta,P);

% DENSJMD95    Density of sea water
%=========================================================================

% USAGE:  dens = densjmd95(S,Theta,P)
%
% DESCRIPTION:
%    Density of Sea Water using Jackett and McDougall 1995 (JAOT 12) 
%    polynomial (modified UNESCO polynomial).
%
% INPUT:  (all must have same dimensions)
%   S     = salinity    [psu      (PSS-78)]
%   Theta = potential temperature [degree C (IPTS-68)]
%   P     = pressure    [dbar]
%       (P may have dims 1x1, mx1, 1xn or mxn for S(mxn) )
%
% OUTPUT:
%   dens = density  [kg/m^3] 
% 
% AUTHOR:  Martin Losch 2002-08-09  (mlosch@mit.edu)
%
% check value
% S     = 35.5 PSU
% Theta = 3 degC
% P     = 3000 dbar
% rho   = 1041.83267 kg/m^3
%

% Jackett and McDougall, 1995, JAOT 12(4), pp. 381-388

% created by mlosch on 2002-08-09
  
%----------------------
% CHECK INPUT ARGUMENTS
%----------------------
  if nargin ~=3
    error('densjmd95.m: Must pass 3 parameters')
  end 
  if ndims(s) > 2
    dims = size(s);
    dimt = size(t);
    dimp = size(p);
    if length(dims) ~= length(dimt) | length(dims) ~= length(dimp) ...
	  length(dimt) ~= length(dimp)
      error(['for more than two dimensions, S, Theta, and P must have the' ...
	     ' same number of dimensions'])
    else
      for k=length(dims)
	if dims(k)~=dimt(k) | dims(k)~=dimp(k) | dimt(k)~=dimp(k)
	  error(['for more than two dimensions, S, Theta, and P must have' ...
		 ' the same dimensions'])
	end
      end
    end
  else
    % CHECK S,T,P dimensions and verify consistent
    [ms,ns] = size(s);
    [mt,nt] = size(t);
    [mp,np] = size(p);
    
    % CHECK THAT S & T HAVE SAME SHAPE
    if (ms~=mt) | (ns~=nt)
      error('check_stp: S & T must have same dimensions')
    end %if
    
    % CHECK OPTIONAL SHAPES FOR P
    if     mp==1  & np==1      % P is a scalar.  Fill to size of S
      p = p(1)*ones(ms,ns);
    elseif np==ns & mp==1      % P is row vector with same cols as S
      p = p( ones(1,ms), : ); %   Copy down each column.
    elseif mp==ms & np==1      % P is column vector
      p = p( :, ones(1,ns) ); %   Copy across each row
    elseif mp==ms & np==ns     % P is a matrix size(S)
			       % shape ok 
    else
      error('check_stp: P has wrong dimensions')
    end %if
    [mp,np] = size(p);
    % IF ALL ROW VECTORS ARE PASSED THEN LET US PRESERVE SHAPE ON RETURN.
    Transpose = 0;
    if mp == 1  % row vector
      p       =  p(:);
      t       =  t(:);
      s       =  s(:);   
      Transpose = 1;
    end 
    %***check_stp
  end
  
  % convert pressure to bar
  p = .1*p;
    
  % coefficients nonlinear equation of state in pressure coordinates for
  % 1. density of fresh water at p = 0
  eosJMDCFw(1) =  999.842594;
  eosJMDCFw(2) =    6.793952e-02;
  eosJMDCFw(3) = -  9.095290e-03;
  eosJMDCFw(4) =    1.001685e-04;
  eosJMDCFw(5) = -  1.120083e-06;
  eosJMDCFw(6) =    6.536332e-09;
  % 2. density of sea water at p = 0
  eosJMDCSw(1) =    8.244930e-01;
  eosJMDCSw(2) = -  4.089900e-03;
  eosJMDCSw(3) =    7.643800e-05 ;
  eosJMDCSw(4) = -  8.246700e-07;
  eosJMDCSw(5) =    5.387500e-09;
  eosJMDCSw(6) = -  5.724660e-03;
  eosJMDCSw(7) =    1.022700e-04;
  eosJMDCSw(8) = -  1.654600e-06;
  eosJMDCSw(9) =    4.831400e-04;

  t2 = t.*t;
  t3 = t2.*t;
  t4 = t3.*t;
  
  is = find(s(:) < 0 );
  if ~isempty(is)
    warning('found negative salinity values, reset them to NaN');
    s(is) = NaN;
  end
  s3o2 = s.*sqrt(s);
            
  % density of freshwater at the surface
  rho =   eosJMDCFw(1) ...
	+ eosJMDCFw(2)*t ...
	+ eosJMDCFw(3)*t2 ...
	+ eosJMDCFw(4)*t3 ...
	+ eosJMDCFw(5)*t4 ...
	+ eosJMDCFw(6)*t4.*t;
  % density of sea water at the surface
  rho =  rho ...
	 + s.*( ...
	     eosJMDCSw(1) ...
	     + eosJMDCSw(2)*t ...
	     + eosJMDCSw(3)*t2 ...
	     + eosJMDCSw(4)*t3 ...
	     + eosJMDCSw(5)*t4 ...
	     ) ...
         + s3o2.*( ...
	     eosJMDCSw(6) ...
	     + eosJMDCSw(7)*t ...
	     + eosJMDCSw(8)*t2 ...
	     ) ...
	 + eosJMDCSw(9)*s.*s;

  rho = rho./(1 - p./bulkmodjmd95(s,t,p));
  
  if ndims(s) < 3 & Transpose
    rho = rho';
  end %if
  
  return
  
function bulkmod = bulkmodjmd95(s,t,p)
%function bulkmod = bulkmodjmd95(s,t,p)
  
  dummy = 0;
  % coefficients in pressure coordinates for
  % 3. secant bulk modulus K of fresh water at p = 0
  eosJMDCKFw(1) =   1.965933e+04;
  eosJMDCKFw(2) =   1.444304e+02;
  eosJMDCKFw(3) = - 1.706103e+00;
  eosJMDCKFw(4) =   9.648704e-03;
  eosJMDCKFw(5) = - 4.190253e-05;
  % 4. secant bulk modulus K of sea water at p = 0
  eosJMDCKSw(1) =   5.284855e+01;
  eosJMDCKSw(2) = - 3.101089e-01;
  eosJMDCKSw(3) =   6.283263e-03;
  eosJMDCKSw(4) = - 5.084188e-05;
  eosJMDCKSw(5) =   3.886640e-01;
  eosJMDCKSw(6) =   9.085835e-03;
  eosJMDCKSw(7) = - 4.619924e-04;
  % 5. secant bulk modulus K of sea water at p
  eosJMDCKP( 1) =   3.186519e+00;
  eosJMDCKP( 2) =   2.212276e-02;
  eosJMDCKP( 3) = - 2.984642e-04;
  eosJMDCKP( 4) =   1.956415e-06;
  eosJMDCKP( 5) =   6.704388e-03;
  eosJMDCKP( 6) = - 1.847318e-04;
  eosJMDCKP( 7) =   2.059331e-07;
  eosJMDCKP( 8) =   1.480266e-04;
  eosJMDCKP( 9) =   2.102898e-04;
  eosJMDCKP(10) = - 1.202016e-05;
  eosJMDCKP(11) =   1.394680e-07;
  eosJMDCKP(12) = - 2.040237e-06;
  eosJMDCKP(13) =   6.128773e-08;
  eosJMDCKP(14) =   6.207323e-10;

  t2 = t.*t;
  t3 = t2.*t;
  t4 = t3.*t;

  is = find(s(:) < 0 );
  if ~isempty(is)
    warning('found negative salinity values, reset them to NaN');
    s(is) = NaN;
  end
  s3o2 = s.*sqrt(s);
  %p = pressure(i,j,k,bi,bj)*SItoBar
  p2 = p.*p;
  % secant bulk modulus of fresh water at the surface
  bulkmod =   eosJMDCKFw(1) ...
	    + eosJMDCKFw(2)*t ...
	    + eosJMDCKFw(3)*t2 ...
	    + eosJMDCKFw(4)*t3 ...
	    + eosJMDCKFw(5)*t4;
  % secant bulk modulus of sea water at the surface
  bulkmod = bulkmod ...
	    + s.*(   eosJMDCKSw(1) ...
		     + eosJMDCKSw(2)*t ...
		     + eosJMDCKSw(3)*t2 ...
		     + eosJMDCKSw(4)*t3 ...
		     ) ...
	    + s3o2.*(   eosJMDCKSw(5) ...
			+ eosJMDCKSw(6)*t ...
			+ eosJMDCKSw(7)*t2 ...
			);
  % secant bulk modulus of sea water at pressure p
  bulkmod = bulkmod ...
	    + p.*(   eosJMDCKP(1) ...
		     + eosJMDCKP(2)*t ...
		     + eosJMDCKP(3)*t2 ...
		     + eosJMDCKP(4)*t3 ...
		     ) ...
	    + p.*s.*(   eosJMDCKP(5) ...
			+ eosJMDCKP(6)*t ...
			+ eosJMDCKP(7)*t2 ...
			) ...
	    + p.*s3o2*eosJMDCKP(8) ...
	    + p2.*(   eosJMDCKP(9) ...
		      + eosJMDCKP(10)*t ...
		      + eosJMDCKP(11)*t2 ...
		      ) ...
	    + p2.*s.*(   eosJMDCKP(12) ...
			 + eosJMDCKP(13)*t ...
			 + eosJMDCKP(14)*t2 ...
			 );

      return

