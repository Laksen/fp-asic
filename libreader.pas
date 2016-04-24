unit libreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LoadLib(const AFilename: string);

implementation

uses
  math,
  cells,
  libparser;

procedure Error(const AStr: string);
begin
  raise exception.Create(AStr);
end;

procedure ExpectSub(AEntry: TEntry; const AName: string);
begin
  if AEntry.Name<>AName then Error('Expected "'+AName+'" sub-entry');
  if not (AEntry is TSubEntry) then Error('Expected "'+AName+'" sub-entry');
end;

function Equals(AValue: TEntry; const AStr: string): boolean;
begin
  result:=false;
  if AValue=nil then exit;
  result:=AValue.GetValue=AStr;
end;

function GetDefault(AEntry: TEntry; ADefault: double): double;
begin
  if AEntry=nil then
    exit(ADefault)
  else
    result:=AEntry.GetValue;
end;

procedure Parse(AEntry: TEntry);
var
  ent, cell, pin: TEntry;
  lu_tables: TEntries;
  ci: TCell;
  cpin: PPin;
  fcap, rcap, cap: Double;
begin
  ExpectSub(AEntry, 'library');

  ent:=AEntry.GetSingle('delay_model');
  if ent=nil then error('Expected "delay_model"');
  if ent.GetValue<>'table_lookup' then error('Only table_lookup delay supported');

  lu_tables:=AEntry.GetMultiple('lu_table_template');

  for cell in AEntry.GetMultiple('cell') do
  begin
    if not HasCell(cell.GetValue) then continue;

    ci:=FindCell(cell.GetValue);

    for pin in cell.GetMultiple('pin') do
    begin
      cpin:=ci.FindPin(pin.GetValue);
      if cpin=nil then continue;

      if Equals(pin.GetSingle('clock'),'true') then cpin^.PinClass:=pcClock;
      if Equals(pin.GetSingle('is_pad'),'true') then cpin^.PinClass:=pcPad;

      // Capacitance
      fcap:=GetDefault(pin.GetSingle('fall_capacitance'), 0);
      rcap:=GetDefault(pin.GetSingle('rise_capacitance'), 0);
      cap:=GetDefault(pin.GetSingle('capacitance'), max(fcap,rcap));

      cpin^.Physical.Capacitance:=cap;
    end;
  end;
end;

procedure LoadLib(const AFilename: string);
var
  fs: TFileStream;
  p: TParser;
  ent: TEntry;
begin
  fs:=TFileStream.Create(AFilename, fmOpenRead);
  try
    p:=TParser.Create(fs);
    try
      ent:=p.ENTRY;
      try
        Parse(ent);
      finally
        ent.Free;
      end;
    finally
      p.Free;
    end;
  finally
    fs.Free;
  end;
end;

end.

