unit Class_LPolynomials;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, Class_Matrix;

type
  CLPolynomials = class
    CC_Lagrange: string;
    function Execute(XPoints: dvector; YPoints: dvector): string;
    function NumberLagrange(XPoints: dvector; i: integer): string;
    private
      nx: integer;
      ny: integer;
      function Lagrange(XPoints: dvector; YPoints: dvector): string;
    public
      constructor create;

  end;

implementation

constructor CLPolynomials.create;
begin
  CC_Lagrange:= '--ValidPolynomial--';
  nx:= 0;
  ny:= 0;
end;

function CLPolynomials.Execute(XPoints: dvector; YPoints: dvector): string;
begin
  Execute:= Lagrange(XPoints,YPoints);
end;

function CLPolynomials.NumberLagrange(XPoints: dvector; i: integer): string;
var lx: string;
    xi,xj: Real;
    j: integer;
    df: Real;
begin
  nx:= Length(XPoints);
  for j:=0 to nx-1 do
  begin
    if i<>j then
    begin
      xi:= XPoints[i];
      xj:= xPoints[j];
      df:= xi - xj;
      if df=0 then CC_Lagrange:= '--InvalidPolynomial--';
      lx:= lx + ' * (x-' + FloatToStr(xj) + ')/' + FloatToStr(df);
    end;
  end;

  NumberLagrange:= lx;

end;

function CLPolynomials.Lagrange(XPoints: dvector; YPoints: dvector): string;
var SFunction: string;
    li: string;
    yi: Real;
    i: integer;
begin
  ny:= Length(YPoints);

  yi:= YPoints[0];
  li:= NumberLagrange(XPoints,0);

  SFunction:= '(' + FloatToStr(yi) + li + ')';

  for i:=1 to ny-1 do
  begin
    yi:= YPoints[i];
    li:= NumberLagrange(XPoints,i);
    SFunction:= SFunction + ' + ' + '(' + FloatToStr(yi) + li + ')';
  end;

  Lagrange:= SFunction;

end;

end.

