unit cells;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TUnit = int64;

  TCoordinate = record
    X,Y: int64;
  end;

  TPoly = record
    Points: array of TCoordinate;
    Layer: longint;
  end;

  TCell = class
  private
    fName: string;
  public
    constructor Create(const AName: string);

    property Name: string read fName;
  end;

function FindCell(const AName: string): TCell;
procedure RegisterCell(ACell: TCell);

implementation

var
  CellDB: TStringList;

function FindCell(const AName: string): TCell;
var
  idx: Integer;
begin
  if CellDB.Find(AName, idx) then
    result:=TCell(CellDB.Objects[idx])
  else
    raise exception.CreateFmt('Failed to find cell: "%s"', [AName]);
end;

procedure RegisterCell(ACell: TCell);
begin
  CellDB.AddObject(ACell.Name, ACell);
end;

constructor TCell.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
end;

initialization
  CellDB:=TStringList.Create;
  CellDB.CaseSensitive:=true;
  CellDB.Sorted:=true;
  CellDB.Duplicates:=dupIgnore;
  CellDB.OwnsObjects:=true;
finalization
  CellDB.Free;

end.

