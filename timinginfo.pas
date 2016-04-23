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

    function GetDimensionCount: longint;
    function GetDimensionIndices(AIndex: longint): TDimension;
    function GetDimensionType(AIndex: longint): TDimensionType;
    procedure SetDimensionIndices(AIndex: longint; AValue: TDimension);
    procedure SetDimensionType(AIndex: longint; AValue: TDimensionType);
  public
    function Clone: TTimingTable;

    constructor Create(ADimensions: LongInt);

    property DimensionCount: longint read GetDimensionCount;
    property DimensionIndices[AIndex: longint]: TDimension read GetDimensionIndices write SetDimensionIndices;
    property DimensionType[AIndex: longint]: TDimensionType read GetDimensionType write SetDimensionType;
  end;

  TTimingInfo = class

  end;

implementation

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
  result:=TTimingTable.Create(0);
  result.fDimensions:=Copy(fDimensions);
end;

constructor TTimingTable.Create(ADimensions: LongInt);
begin
  inherited Create;
  SetLength(fDimensions, ADimensions);
end;

end.

