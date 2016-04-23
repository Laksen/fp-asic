unit constraints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
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
  public
    constructor Create(const AName: string; APeriod: double);

    property Period: double read fPeriod;
  end;

function AddClock(const AName: string; APeriod: double): TConstraint;

procedure LoadConstraints(const AFilename: string);

implementation

var
  _Constraints: TStringList;

function AddClock(const AName: string; APeriod: double): TConstraint;
begin
  result:=TClockConstraint.Create(AName, APeriod);
  _Constraints.AddObject(AName, result);
end;

procedure LoadConstraints(const AFilename: string);
begin

end;

constructor TConstraint.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
end;

constructor TClockConstraint.Create(const AName: string; APeriod: double);
begin
  inherited Create(AName);
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

