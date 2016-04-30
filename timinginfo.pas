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

    fData: array of double;

    function GetDimensionCount: longint;
    function GetDimensionIndices(AIndex: longint): TDimension;
    function GetDimensionType(AIndex: longint): TDimensionType;
    function GetRawData(AIndex: longint): double;
    procedure SetDimensionIndices(AIndex: longint; AValue: TDimension);
    procedure SetDimensionType(AIndex: longint; AValue: TDimensionType);

    function GetIndex(AValue: double; ADimension: longint; out AScale: double): longint;
    procedure SetRawData(AIndex: longint; AValue: double);
  public
    function GetData(AIdx1: double): double;
    function GetData(AIdx1, AIdx2: double): double;
    function GetData(AIdx1, AIdx2, AIdx3: double): double;

    function Clone: TTimingTable;

    constructor Create(const AName: string; ADimensions: LongInt);

    property Name: string read fName;

    property Data[AIndex: longint]: double read GetRawData write SetRawData;

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

function TTimingTable.GetRawData(AIndex: longint): double;
begin
  result:=fData[AIndex];
end;

procedure TTimingTable.SetDimensionIndices(AIndex: longint; AValue: TDimension);
begin
  fDimensions[AIndex].Indicies:=copy(AValue);
end;

procedure TTimingTable.SetDimensionType(AIndex: longint; AValue: TDimensionType);
begin
  fDimensions[AIndex].DimType:=AValue;
end;

function TTimingTable.GetIndex(AValue: double; ADimension: longint; out AScale: double): longint;
var
  idc: TDimension;
  i: longint;
begin
  idc:=fDimensions[ADimension].Indicies;

  AScale:=0;

  if avalue<=idc[0] then
    exit(0)
  else if AValue>=idc[high(idc)] then
    begin
      AScale:=1;
      exit(high(idc)-1);
    end;

  for i:=0 to high(idc)-1 do
  begin
    if (idc[i]<=avalue) and (avalue<idc[i+1]) then
    begin
      // value=t*(idc[i+1]-idc[i])+idc[i]
      AScale:=(AValue-idc[i])/(idc[i+1]-idc[i]);
      exit(i);
    end;
  end;
end;

procedure TTimingTable.SetRawData(AIndex: longint; AValue: double);
begin
  fData[AIndex]:=AValue;
end;

function Lerp(A,B, T: double): double;
begin
  result:=(b-a)*t+a;
end;

function TTimingTable.GetData(AIdx1: double): double;
var
  sc: double;
  idx0: LongInt;
begin
  idx0:=GetIndex(AIdx1, 0, sc);

  result:=Lerp(fData[idx0], fData[idx0+1], sc);
end;

function TTimingTable.GetData(AIdx1, AIdx2: double): double;
var
  sc1, sc2,
  res0, res1: double;
  idx0, idx1: LongInt;
begin
  idx0:=GetIndex(AIdx1, 0, sc1);
  idx1:=GetIndex(AIdx2, 1, sc2);

  res0:=lerp(fData[idx0*length(fDimensions[1].Indicies)+idx1], fData[(idx0+1)*length(fDimensions[1].Indicies)+idx1], sc1);
  res1:=lerp(fData[idx0*length(fDimensions[1].Indicies)+idx1+1], fData[(idx0+1)*length(fDimensions[1].Indicies)+idx1+1], sc1);

  result:=Lerp(res0,res1,sc2);
end;

function TTimingTable.GetData(AIdx1, AIdx2, AIdx3: double): double;
begin
  raise Exception.Create('3D interpolation not supported');
  result:=0;
  //result:=fData[(GetIndex(AIdx1, 0)*length(fDimensions[1].Indicies)+GetIndex(AIdx2, 1))*length(fDimensions[2])+GetIndex(AIdx3, 2)];
end;

function TTimingTable.Clone: TTimingTable;
var
  i, cnt: longint;
begin
  result:=TTimingTable.Create(Name+inttostr(Timings.Count), 0);
  result.fDimensions:=Copy(fDimensions);

  cnt:=1;
  for i:=0 to high(fDimensions) do
    cnt:=cnt*length(fDimensions[i].Indicies);

  setlength(result.fData, cnt);
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

