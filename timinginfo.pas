unit timinginfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDimension = array of double;
  TDimensionType = (
    // Set1
    dtInputNetTransition,

    dtConstrainedPinTransition,
    // Set2
    dtTotalOutputNetCapacitance,
    dtOutputNetCapacitance,
    dtOutputNetWireCapacitance,
    dtOutputNetPinCapacitance,

    dtRelatedPinTransition,
    //  Set3
    dtRelatedOutTotalOutputNetCapacitance,
    dtRelatedOutOutputNetLength,
    dtRelatedOutOutputNetWireCapacitance,
    dtRelatedOutOutputNetPinCapacitance);

  TTimingTable = class
  private
    fDimensions: array of record Indicies: TDimension; DimType: TDimensionType; end;
    fName: string;

    function GetDimensionCount: longint;
    function GetDimensionIndices(AIndex: longint): TDimension;
    function GetDimensionType(AIndex: longint): TDimensionType;
    procedure SetDimensionIndices(AIndex: longint; AValue: TDimension);
    procedure SetDimensionType(AIndex: longint; AValue: TDimensionType);
  public
    function Clone: TTimingTable;

    constructor Create(const AName: string; ADimensions: LongInt);

    property Name: string read fName;

    property DimensionCount: longint read GetDimensionCount;
    property DimensionIndices[AIndex: longint]: TDimension read GetDimensionIndices write SetDimensionIndices;
    property DimensionType[AIndex: longint]: TDimensionType read GetDimensionType write SetDimensionType;
  end;

  TTimingInfo = class

  end;

procedure AddTable(ATable: TTimingTable);
function GetTable(const AName: string): TTimingTable;

implementation

var
  Timings: TStringList;

procedure AddTable(ATable: TTimingTable);
begin
  if ATable=nil then exit;
  Timings.AddObject(ATable.Name, ATable);
end;

function GetTable(const AName: string): TTimingTable;
var
  idx: Integer;
begin
  idx:=Timings.IndexOf(AName);
  if idx<0 then
    result:=nil
  else
    result:=TTimingTable(Timings.Objects[idx]);
end;

function TTimingTable.GetDimensionCount: longint;
begin
  result:=Length(fDimensions);
end;

function TTimingTable.GetDimensionIndices(AIndex: longint): TDimension;
begin
  result:=fDimensions[AIndex].Indicies;
end;

function TTimingTable.GetDimensionType(AIndex: longint): TDimensionType;
begin
  result:=fDimensions[AIndex].DimType;
end;

procedure TTimingTable.SetDimensionIndices(AIndex: longint; AValue: TDimension);
begin
  fDimensions[AIndex].Indicies:=copy(AValue);
end;

procedure TTimingTable.SetDimensionType(AIndex: longint; AValue: TDimensionType);
begin
  fDimensions[AIndex].DimType:=AValue;
end;

function TTimingTable.Clone: TTimingTable;
begin
  result:=TTimingTable.Create(Name+inttostr(Timings.Count), 0);
  result.fDimensions:=Copy(fDimensions);
end;

constructor TTimingTable.Create(const AName: string; ADimensions: LongInt);
begin
  inherited Create;
  fName:=AName;
  SetLength(fDimensions, ADimensions);
end;

initialization
  Timings:=TStringList.Create;
  Timings.CaseSensitive:=true;
  Timings.Sorted:=true;
  Timings.Duplicates:=dupIgnore;
  Timings.OwnsObjects:=true;
finalization
  Timings.Free;

end.

