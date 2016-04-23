unit libreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure LoadLib(const AFilename: string);

implementation

uses
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

procedure Parse(AEntry: TEntry);
var
  ent, cell, pin: TEntry;
  lu_tables: TEntries;
  ci: TCell;
  cpin: PPin;
begin
  ExpectSub(AEntry, 'library');

  ent:=AEntry.GetSingle('delay_model');
  if ent=nil then error('Expected "delay_model"');
  if ent.GetValue<>'table_lookup' then error('Only table_lookup delay supported');

  lu_tables:=AEntry.GetMultiple('lu_table_template');

  for cell in AEntry.GetMultiple('cell') do
  begin
    ci:=FindCell(cell.GetValue);

    if ci=nil then continue;

    for pin in cell.GetMultiple('pin') do
    begin
      cpin:=ci.FindPin(pin.GetValue);
      if cpin=nil then continue;

      if Equals(pin.GetSingle('clock'),'true') then cpin^.PinClass:=pcClock;
      if Equals(pin.GetSingle('is_pad'),'true') then cpin^.PinClass:=pcPad;
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

initialization
  LoadLib('../osu035/osu035_stdcells.lib');

end.

