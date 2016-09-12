unit constraints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringArray = array of string;

  TConstraint = class
  private
    fName: string;
  public
    constructor Create(const AName: string);

    property Name: string read fName;
  end;

  TClockConstraint = class(TConstraint)
  private
    fPeriod: double;
    fPin: string;
  public
    constructor Create(const AName, APin: string; APeriod: double);

    property Pin: string read fPin;
    property Period: double read fPeriod;
  end;

function GetClockPins: TStringArray;

function AddClock(const AName, APin: string; APeriod: double): TConstraint;

procedure LoadConstraints(const AFilename: string);

implementation

var
  _Constraints: TStringList;

function GetClockPins: TStringArray;
var
  i,cnt: longint;
begin
  cnt:=0;
  for i:=0 to _Constraints.Count-1 do
    if _Constraints.Objects[i] is TClockConstraint then
      inc(cnt);

  setlength(result, cnt);

  cnt:=0;
  for i:=0 to _Constraints.Count-1 do
    if _Constraints.Objects[i] is TClockConstraint then
    begin
      result[cnt]:=TClockConstraint(_Constraints.Objects[i]).Pin;
      inc(cnt);
    end;
end;

function AddClock(const AName, APin: string; APeriod: double): TConstraint;
begin
  result:=TClockConstraint.Create(AName, APin, APeriod);
  _Constraints.AddObject(AName, result);
end;

procedure LoadConstraints(const AFilename: string);
begin
  AddClock('Clock_500', 'Clk', 2e-9);
end;

constructor TConstraint.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
end;

constructor TClockConstraint.Create(const AName, APin: string; APeriod: double);
begin
  inherited Create(AName);
  fPin:=APin;
  fPeriod:=APeriod;
end;

initialization
  _Constraints:= TStringList.Create;
  _Constraints.OwnsObjects:=true;
  _Constraints.CaseSensitive:=true;
  _Constraints.Duplicates:=dupIgnore;
  _Constraints.Sorted:=true;
finalization
  _Constraints.Free;

end.

