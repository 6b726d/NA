unit Class_RPolynomials;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Class_Matrix;

type
  CRPolynomials = class
    function remainder(dividend: double; divisor: double): double;
    function Execute(Roots: dvector): string;
    function Coefficients(Roots: dvector): dvector;
    function Polynomial(Roots: dvector): string;

  end;

implementation

function CRPolynomials.remainder(dividend: double; divisor: double): double;
begin
  remainder:= dividend - (divisor * Round(dividend/divisor));
end;

function CRPolynomials.Execute(Roots: dvector): string;
begin
  Execute:= Polynomial(Roots);
end;

function CRPolynomials.Coefficients(Roots: dvector): dvector;
var Coeffs: dvector;
    r,c: integer;
    n: integer;
begin
  n:= Length(Roots);
  SetLength(Coeffs,n+1);
  Coeffs[0]:= -Roots[0];
  Coeffs[1]:= 1;
  for r:=2 to n do
  begin
    Coeffs[r]:= 1;
    for c:=r-2 downto 0 do
    begin
      Coeffs[c+1]:= Coeffs[c] - (Roots[r-1]*Coeffs[c+1]);
    end;
    Coeffs[0]:= Coeffs[0]*(-Roots[r-1]);
    if (remainder(r,2)) = 1 then Coeffs[r]:= -Coeffs[r];
  end;

  Coefficients:= Coeffs;
end;

function CRPolynomials.Polynomial(Roots: dvector): string;
var SFunction: string;
    Coeffs: dvector;
    i: integer;
    n: integer;
begin
  Coeffs:= Coefficients(Roots);
  n:= Length(Coeffs);
  SFunction:= FloatToStr(Coeffs[0]) + ' + (' + FloatToStr(Coeffs[1]) + '*x)';
  for i:=2 to n-1 do
  begin
    SFunction:= SFunction + ' + (' + FloatToStr(Coeffs[i]) + '*(x^' + IntToStr(i) + '))';
  end;

  Polynomial:= SFunction;
end;

end.

