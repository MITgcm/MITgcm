function rho = densmdjwf(s,t,p);
%function rho = densmdjwf(S,Theta,P);

% DENSMDJWF    Density of sea water
%=========================================================================

% USAGE:  dens = densmdjwf(S,Theta,P)
%
% DESCRIPTION:
%    Density of Sea Water using the McDougall et al. 2003 (JAOT 20)
%    polynomial.
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

% McDougall et al., 2003, JAOT 20(5), pp. 730-741

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
  
  % coefficients nonlinear equation of state in pressure coordinates for
  eosMDJWFnum(12) =  9.99843699e+02;
  eosMDJWFnum( 1) =  7.35212840e+00;
  eosMDJWFnum( 2) = -5.45928211e-02;
  eosMDJWFnum( 3) =  3.98476704e-04;
  eosMDJWFnum( 4) =  2.96938239e+00;
  eosMDJWFnum( 5) = -7.23268813e-03;
  eosMDJWFnum( 6) =  2.12382341e-03;
  eosMDJWFnum( 7) =  1.04004591e-02;
  eosMDJWFnum( 8) =  1.03970529e-07;
  eosMDJWFnum( 9) =  5.18761880e-06;
  eosMDJWFnum(10) = -3.24041825e-08;
  eosMDJWFnum(11) = -1.23869360e-11;
  
  eosMDJWFden(13) =  1.00000000e+00;
  eosMDJWFden( 1) =  7.28606739e-03;
  eosMDJWFden( 2) = -4.60835542e-05; 
  eosMDJWFden( 3) =  3.68390573e-07;
  eosMDJWFden( 4) =  1.80809186e-10;
  eosMDJWFden( 5) =  2.14691708e-03;
  eosMDJWFden( 6) = -9.27062484e-06;
  eosMDJWFden( 7) = -1.78343643e-10;
  eosMDJWFden( 8) =  4.76534122e-06;
  eosMDJWFden( 9) =  1.63410736e-09;
  eosMDJWFden(10) =  5.30848875e-06;
  eosMDJWFden(11) = -3.03175128e-16;
  eosMDJWFden(12) = -1.27934137e-17;

  p1 = p;
    
  t1 = t;
  t2 = t.*t;
  
  s1=s;
  is = find(s1(:) < 0 );
  if ~isempty(is)
    warning('found negative salinity values, reset them to NaN');
    s1(is) = NaN;
  end
  sp5 = sqrt(s1);
  p1t1=p1.*t1;
  %
  num = eosMDJWFnum(12) ...
	+ t1.*(eosMDJWFnum(1) ... 
        +     t1.*(eosMDJWFnum(2) + eosMDJWFnum(3).*t1) )  ...
	+ s1.*(eosMDJWFnum(4) ...
        +     eosMDJWFnum(5).*t1  + eosMDJWFnum(6).*s1) ...
	+ p1.*(eosMDJWFnum(7) + eosMDJWFnum(8).*t2 ...
        +     eosMDJWFnum(9).*s1 ...
	+     p1.*(eosMDJWFnum(10) + eosMDJWFnum(11).*t2) );
  den = eosMDJWFden(13) ...
	+ t1.*(eosMDJWFden(1) ...
	+     t1.*(eosMDJWFden(2) ...
	+         t1.*(eosMDJWFden(3) + t1.*eosMDJWFden(4) ) ) ) ...
	+ s1.*(eosMDJWFden(5) ...
	+     t1.*(eosMDJWFden(6) ... 
	+         eosMDJWFden(7).*t2) ... 
	+     sp5.*(eosMDJWFden(8) + eosMDJWFden(9).*t2) ) ... 
	+ p1.*(eosMDJWFden(10) ...
	+     p1t1.*(eosMDJWFden(11).*t2 + eosMDJWFden(12).*p1) );
  
  epsln = 0;
  denom = 1.0./(epsln+den) ;

  
  rho = num.*denom;

  return
  
