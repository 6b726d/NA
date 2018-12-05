unit Class_Root;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

type
  CRoot = class
    ErrorAllowed: Double;
    xi: Double;
    SRoot: String;
    function f(x: Double): Double;
    function Execute(fx:String; a:Double; b:Double; m: Integer; tf: Boolean): Double;
    private
      function Bisection(fx:String; a:Double; b:Double): Double;
      function FalsePosition(fx:String; a:Double; b:Double): Double;
      function Secant(fx:String; a:Double; b:Double): Double;
      function CCSecant(fx:String; a:Double; b:Double): Double;
      function RDefault(fx:String; a:Double; b:Double): Double;
      function ZRoot(fx:String; a:Double; b:Double; m: Integer): Double;
      function ARoot(fx:String; a:Double; b:Double; m: Integer): Double;
    public
      constructor Create;
      destructor Destroy; override;

  end;

implementation

uses ParseMath;

var
  Parse: TParseMath;

constructor CRoot.Create;
begin
  Parse:= TParseMath.create();
  Parse.AddVariable('x',0);
end;

destructor CRoot.Destroy;
begin
  Parse.destroy;
end;

function CRoot.Execute(fx: String; a: Double; b: Double; m: Integer; tf:Boolean): Double;
begin
  if tf=true then
  begin
    Execute:= ARoot(fx,a,b,m);
  end
  else
  begin
    Execute:= ZRoot(fx,a,b,m);
  end;

end;

function CRoot.f(x: Double): Double;
begin
  Parse.NewValue('x',x);
  f:= Parse.Evaluate();
end;

function CRoot.Bisection(fx:String; a:Double; b:Double): Double;
var Error: Double;
    xn: Double;
begin
   if (a+b)=0 then b:=b+0.1;
   Parse.Expression:= fx;
   xi:= Infinity;
   if f(a)*f(b) >= 0 then
   begin
     Bisection:= NaN;
     G_ConvergenceCriteria:= '--NoBolzano--';
   end
   else
   begin
   repeat
     xn:= xi;
     xi:= (a + b) / 2;
     if f(xi) * f(a) < 0 then
        b:= xi
     else
        a:= xi;
     Error:= abs(xi - xn) ;
   until (Error <= ErrorAllowed) ;
   G_ConvergenceCriteria:= '--Converge--';
   Bisection:= RoundTo(xi,-G_Decimal);
   end;
end;

function CRoot.FalsePosition(fx:String; a:Double; b:Double): Double;
var Error: Double;
    xn: Double;
begin
   Parse.Expression:= fx;
   xi:= Infinity;
   if f(a)*f(b) >= 0 then
   begin
     FalsePosition:= NaN;
     G_ConvergenceCriteria:= '--NoBolzano--';
   end
   else
   begin
   repeat
     xn:= xi;
     xi:= b - (f(b)*((b-a)/(f(b)-f(a))));
     if f(xi) * f(a) < 0 then
        b:= xi
     else
        a:= xi;
     Error:= abs(xi - xn) ;
   until (Error <= ErrorAllowed) ;
   G_ConvergenceCriteria:= '--Converge--';
   FalsePosition:= RoundTo(xi,-G_Decimal);
   end;
end;

function CRoot.Secant(fx:String; a:Double; b:Double): Double;
var Error: Double;
    xn: Double;
    h: Double;
begin
   Parse.Expression:= fx;
   h:= ErrorAllowed/10;
   if (a+b)=0 then xi:= 0.1
   else xi:= (a+b)/2;
   Error:= xi;
   repeat
     xn:= xi;
     xi:= xn - ((2*h*f(xn))/(f(xn+h)-f(xn-h)));
     Error:= abs(xi - xn) ;
   until (Error <= ErrorAllowed);
   if (a<xi) and (xi<b) then
   begin
     Secant:= RoundTo(xi,-G_Decimal);
     G_ConvergenceCriteria:= '--Converge--';//_
   end
   else
   begin
     Secant:= RoundTo(xi,-G_Decimal);
     G_ConvergenceCriteria:= '--Outside The Interval--';//_
   end;
end;

function CRoot.CCSecant(fx:String; a:Double; b:Double): Double;
var Error: Double;
    xn: Double;
    h: Double;
    PError: Double;//
begin
   Parse.Expression:= fx;
   h:= ErrorAllowed/10;
   if (a+b)=0 then xi:= 0.1
   else xi:= (a+b)/2;
   Error:= xi;
   PError:= xi+1;
   repeat
     xn:= xi;
     xi:= xn - ((2*h*f(xn))/(f(xn+h)-f(xn-h)));
     PError:= Error;
     Error:= abs(xi - xn) ;
   until (Error<=ErrorAllowed) or (Error>PError) ;
   //until (Error <= ErrorAllowed);
   if (Error>PError) then
   begin
     CCSecant:= NaN;
     G_ConvergenceCriteria:= '--NoConverge--';//_
   end
   else if (a<xi) and (xi<b) then
   begin
     CCSecant:= RoundTo(xi,-G_Decimal);
     G_ConvergenceCriteria:= '--Converge--';//_
   end
   else
   begin
     CCSecant:= RoundTo(xi,-G_Decimal);
     G_ConvergenceCriteria:= '--Outside The Interval--';//_
   end;
   //CCSecant:= RoundTo(xi,-G_Decimal);
end;

function CRoot.RDefault(fx:String; a:Double; b:Double): Double;
var Error: Double;
    xn: Double;
    h: Double;
begin
   Parse.Expression:= fx;
   h:= ErrorAllowed/10;
   if (a+b)=0 then xi:= 0.1
   else xi:= (a+b)/2;
   if f(a)*f(b) >= 0 then
   begin
     RDefault:= NaN;
     G_ConvergenceCriteria:= '--NoConverge--';//NoBolzano
   end
   else
   begin
   repeat
     xn:= xi;
     xi:= xn - ((2*h*f(xn))/(f(xn+h)-f(xn-h)));
     Error:= abs(xi - xn) ;
   until (Error <= ErrorAllowed) ;
   G_ConvergenceCriteria:= '--Converge--';
   RDefault:= RoundTo(xi,-G_Decimal);
   end;
end;

function CRoot.ZRoot(fx: String; a: Double; b: Double; m: Integer): Double;
begin
  if m=0 then ZRoot:= Bisection(fx,a,b)
  else if m=1 then ZRoot:= FalsePosition(fx,a,b)
  else if m=2 then ZRoot:= Secant(fx,a,b)
  else if m=3 then ZRoot:= CCSecant(fx,a,b)
  else ZRoot:= RDefault(fx,a,b);
end;

function CRoot.ARoot(fx: String; a:Double; b:Double; m:Integer): Double;
var ar: String;          //raices
    r: Double;          //raiz
    i: Integer;
    ai,bi: Double;
    h: Double;
    n: Integer;
begin
  n:=100;
  h:= (b-a)/n;
  ai:= a;
  bi:= a+h;
  ar:= '[';
  //Evitar raices repetidas
  for i:=0 to n-1 do
  begin
    r:= ZRoot(fx,ai,bi,m);
    if (not isNaN(r)) then
    begin
      if (ai<r) and (r<bi) then ar:= ar + FloatToStr(r) + ',';
    end;
    ai:= ai+h;
    bi:= bi+h;
  end;
  //Cuando no es vacio
  if Length(ar)<>1 then ar:= Copy(ar,1,Length(ar)-1);
  ar:= ar + ']';
  SRoot:= ar; //Raices
  G_ConvergenceCriteria:= '--Converge--';//_
  ARoot:= 0;  //No se usa

end;

end.

