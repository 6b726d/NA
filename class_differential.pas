unit Class_Differential;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  CDifferential = class
    ErrorAllowed: Double;
    function f(x: Double; y: Double): Double;
    function Execute(df:String; x0:Double; y0:Double; xn: Double; m: Integer): Double;
    private
      function Euler(df:String; x0:Double; y0:Double; xn: Double): Double;
      function EulerHeun(df:String; x0:Double; y0:Double; xn: Double): Double;
      function RungeKutta(df:String; x0:Double; y0:Double; xn: Double): Double;
      function RKDormandPrince(df:String; x0:Double; y0:Double; xn: Double): Double;
      function RKFehlberg(df:String; x0:Double; y0:Double; xn: Double): Double;
      function RKCashKarp(df:String; x0:Double; y0:Double; xn: Double): Double;
    public
      constructor Create;
      destructor Destroy; override;

  end;

implementation

uses ParseMath;

var
  Parse: TParseMath;

constructor CDifferential.Create;
begin
  Parse:= TParseMath.create();
  Parse.AddVariable('x',0);
  Parse.AddVariable('y',0);
end;

destructor CDifferential.Destroy;
begin
  Parse.destroy;
end;

function CDifferential.Execute(df: String; x0: Double; y0: Double; xn: Double; m: Integer): Double;
begin
  if m=0 then Execute:= Euler(df,x0,y0,xn)
  else if m=1 then Execute:= EulerHeun(df,x0,y0,xn)
  else if m=2 then Execute:= RungeKutta(df,x0,y0,xn)
  else if m=3 then Execute:= RKDormandPrince(df,x0,y0,xn)
  else if m=4 then Execute:= RKFehlberg(df,x0,y0,xn)
  else if m=5 then Execute:= RKCashKarp(df,x0,y0,xn)
  else Execute:= RKDormandPrince(df,x0,y0,xn);

end;

function CDifferential.f(x: Double; y: Double): Double;
begin
  Parse.NewValue('x',x);
  Parse.NewValue('y',y);
  f:= Parse.Evaluate();
end;

function CDifferential.Euler(df:String; x0:Double; y0:Double; xn: Double): Double;
var e: Double;
    h: Double;
    n: Integer;
    xi,yi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    yi:= yi + (h*f(xi,yi));
    xi:= xi + h;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  Euler:= RoundTo(yi,-G_Decimal);

end;

function CDifferential.EulerHeun(df:String; x0:Double; y0:Double; xn: Double): Double;
var e: Double;
    h: Double;
    n: Integer;
    xi,yi,xs,ys: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    ys:= yi + (h*f(xi,yi));
    xs:= xi + h;
    yi:= yi + ((h/2)*(f(xi,yi)+f(xs,ys)));
    xi:= xs;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  EulerHeun:= RoundTo(yi,-G_Decimal);

end;

function CDifferential.RungeKutta(df:String; x0:Double; y0:Double; xn: Double): Double;
var yn: Double;
    e: Double;
    h: Double;
    n: Integer;
    k1,k2,k3,k4: Double;
    xi,yi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    k1:= h*f(xi,yi);
    k2:= h*f(xi+(h/2),yi+(k1/2));
    k3:= h*f(xi+(h/2),yi+(k2/2));
    k4:= h*f(xi+h,yi+k3);
    yi:= yi + ((1/6)*(k1+(2*k2)+(2*k3)+k4));
    xi:= xi + h;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  RungeKutta:= RoundTo(yi,-G_Decimal);

end;

function CDifferential.RKDormandPrince(df:String; x0:Double; y0:Double; xn: Double): Double;
var e: Double;
    h: Double;
    n: Integer;
    k1,k2,k3,k4,k5,k6,k7: Double;
    yi4,yi5: Double;
    xi,yi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    k1:= h*f(xi,yi);
    k2:= h*f(xi+(h/5),yi+(k1/5));
    k3:= h*f(xi+((3/10)*h),yi+((3/40)*k1)+((9/40)*k2));
    k4:= h*f(xi+((4/5)*h),yi+((44/45)*k1)-((56/15)*k2)+((32/9)*k3));
    k5:= h*f(xi+((8/9)*h),yi+((19372/6561)*k1)-((25360/2187)*k2)+((64448/6561)*k3)-((212/729)*k4));
    k6:= h*f(xi+h,yi+((9017/3168)*k1)-((355/33)*k2)+((46732/5247)*k3)+((49/176)*k4)-((5103/18656)*k5));
    yi4:= yi+ ((35/384)*k1)+((500/1113)*k3)+((125/192)*k4)-((2187/6784)*k5)+((11/84)*k6);
    k7:= h*f(xi+h,yi4);
    yi5:= yi + ((5179/57600)*k1)+((7571/16695)*k3)+((393/640)*k4)-((92097/339200)*k5)+((187/2100)*k6)+((1/40)*k7);
    yi:= yi5;//yi4
    xi:= xi + h;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  RKDormandPrince:= RoundTo(yi,-G_Decimal);

end;

function CDifferential.RKFehlberg(df:String; x0:Double; y0:Double; xn: Double): Double;
var e: Double;
    h: Double;
    n: Integer;
    k1,k2,k3,k4,k5,k6: Double;
    yi4,yi5: Double;
    xi,yi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    k1:= h*f(xi,yi);
    k2:= h*f(xi+(h/4),yi+(k1/4));
    k3:= h*f(xi+((3/8)*h),yi+((3/32)*k1)+((9/32)*k2));
    k4:= h*f(xi+((12/13)*h),yi+((1932/2197)*k1)-((7200/2197)*k2)+((7296/2197)*k3));
    k5:= h*f(xi+h,yi+((439/216)*k1)-(8*k2)+((3680/513)*k3)-((845/4104)*k4));
    k6:= h*f(xi+(h/2),yi-((8/27)*k1)+(2*k2)-((3544/2565)*k3)+((1859/4104)*k4)-((11/40)*k5));
    yi4:= yi + ((25/216)*k1)+((1408/2565)*k3)+((2197/4104)*k4)-(k5/5);
    yi5:= yi + ((16/135)*k1)+((6656/12825)*k3)+((28561/56430)*k4)-((9/50)*k5)+((2/55)*k6);
    yi:= yi5;//yi4
    xi:= xi + h;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  RKFehlberg:= RoundTo(yi,-G_Decimal);

end;

function CDifferential.RKCashKarp(df:String; x0:Double; y0:Double; xn: Double): Double;
var e: Double;
    h: Double;
    n: Integer;
    k1,k2,k3,k4,k5,k6: Double;
    yi4,yi5: Double;
    xi,yi: Double;
    i: Integer;
begin

  e:= ErrorAllowed;

  if xn<x0 then
  begin
    n:= Round((x0-xn)/e);
    h:= (x0-xn)/n;
    h:= -h;
  end
  else
  begin
    n:= Round((xn-x0)/e);
    h:= (xn-x0)/n;
  end;

  Parse.Expression:= df;

  SetLength(G_DValues,n+1,2);

  xi:= x0;
  yi:= y0;

  G_DValues[0,0]:= xi;
  G_Dvalues[0,1]:= yi;

  for i:=1 to n do
  begin
    k1:= h*f(xi,yi);
    k2:= h*f(xi+(h/5),yi+(k1/5));
    k3:= h*f(xi+((3/10)*h),yi+((3/40)*k1)+((9/40)*k2));
    k4:= h*f(xi+((3/5)*h),yi+((3/10)*k1)-((9/10)*k2)+((6/5)*k3));
    k5:= h*f(xi+h,yi-((11/54)*k1)+((5/2)*k2)-((70/27)*k3)+((35/27)*k4));
    k6:= h*f(xi+((7/8)*h),yi+((1631/55296)*k1)+((175/512)*k2)+((575/13824)*k3)+((44275/110592)*k4)+((253/4096)*k5));
    yi4:= yi + ((2825/27648)*k1)+((18575/48384)*k3)+((13525/55296)*k4)+((277/14336)*k5)+(k6/4);
    yi5:= yi + ((37/378)*k1)+((250/621)*k3)+((125/594)*k4)+((512/1771)*k6);
    yi:= yi5;//yi4
    xi:= xi + h;
    G_DValues[i,0]:= xi;
    G_Dvalues[i,1]:= yi;
  end;

  G_ConvergenceCriteria:= '--Converge--';//_

  RKCashKarp:= RoundTo(yi,-G_Decimal);

end;

end.

