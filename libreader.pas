unit libreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

procedure LoadLib(const AFilename: string);

implementation

uses
  math,
  cells,
  timinginfo,
  libparser;

const
  TimingTypeLut: array[TDimensionType] of string = (
    'input_net_transition',
    'constrained_pin_transition',
    'total_output_net_capacitance',
    'output_net_capacitance',
    'output_net_wire_capacitance',
    'output_net_pin_capacitance',
    'related_pin_transition',
    'related_out_total_output_net_capacitance',
    'related_out_output_net_length',
    'related_out_output_net_wire_capacitance',
    'related_out_output_net_pin_capacitance'
  );

function GetTiming(const AStr: string): TDimensionType;
var
  i: TDimensionType;
begin
  result:=dtInputNetTransition;
  for i:=low(TDimensionType) to high(TDimensionType) do
    if TimingTypeLut[i]=AStr then
      exit(i);
end;

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

function ParseIndex(const AStr: string): TDimension;
var
  c: Char;
  i, cnt: longint;
  s: String;
begin
  setlength(result,0);

  s:=trim(AStr);
  if s='' then exit();

  cnt:=0;
  for c in AStr do
    if c=',' then
      inc(cnt);

  setlength(result, cnt+1);
  for i:=0 to cnt-1 do
    result[i]:=strtofloat(trim(Copy2SymbDel(s,',')));
end;

function GetTiming(ATable: TEntry): TTimingTable;
var
  e: TEntry;
  st: string;
  s: Variant;
  c: Char;
  i: longint;
begin
  result:=GetTable(ATable.GetValue);
  if result<>nil then
  begin
    result:=Result.Clone;
    AddTable(result);

    if ATable.GetSingle('index_1')<>nil then Result.DimensionIndices[0]:=ParseIndex(ATable.GetSingle('index_1').GetValue);
    if ATable.GetSingle('index_2')<>nil then Result.DimensionIndices[1]:=ParseIndex(ATable.GetSingle('index_2').GetValue);
    if ATable.GetSingle('index_3')<>nil then Result.DimensionIndices[2]:=ParseIndex(ATable.GetSingle('index_3').GetValue);

    e:=ATable.GetSingle('values');

    st:='';
    for s in e.GetValues do
      if st='' then
        st:=s
      else
        st:=st+','+s;

    i:=0;

    while st<>'' do
    begin
      Result.Data[i]:=strtofloat(trim(Copy2SymbDel(st,',')));
      inc(i);
    end;
  end;
end;

function MakeTable(ATable: TEntry): TTimingTable;
var
  i, dims: longint;
  dim: array[0..2] of string;
  idx: array[0..2] of TDimension;
begin
  if ATable.GetSingle('variable_3')<>nil then
  begin
    dims:=3;
    dim[0]:=ATable.GetSingle('variable_1').GetValue;
    dim[1]:=ATable.GetSingle('variable_2').GetValue;
    dim[2]:=ATable.GetSingle('variable_3').GetValue;

    idx[0]:=ParseIndex(ATable.GetSingle('index_1').GetValue);
    idx[1]:=ParseIndex(ATable.GetSingle('index_2').GetValue);
    idx[2]:=ParseIndex(ATable.GetSingle('index_3').GetValue);
  end
  else if ATable.GetSingle('variable_2')<>nil then
  begin
    dims:=2;
    dim[0]:=ATable.GetSingle('variable_1').GetValue;
    dim[1]:=ATable.GetSingle('variable_2').GetValue;

    idx[0]:=ParseIndex(ATable.GetSingle('index_1').GetValue);
    idx[1]:=ParseIndex(ATable.GetSingle('index_2').GetValue);
  end
  else if ATable.GetSingle('variable_2')<>nil then
  begin
    dims:=1;
    dim[0]:=ATable.GetSingle('variable_1').GetValue;

    idx[0]:=ParseIndex(ATable.GetSingle('index_1').GetValue);
  end
  else
    exit(nil);

  result:=TTimingTable.Create(ATable.GetValue, dims);

  for i:=0 to dims-1 do
  begin
    result.DimensionIndices[i]:=idx[i];
    result.DimensionType[i]:=GetTiming(dim[i]);
  end;
end;

procedure Parse(AEntry: TEntry);
var
  ent, cell, pin, timing, t, rt, ft, lu_table: TEntry;
  ci: TCell;
  cpin: PPin;
  fcap, rcap, cap: Double;
  idx: longint;
  rel: string;
begin
  ExpectSub(AEntry, 'library');

  ent:=AEntry.GetSingle('delay_model');
  if ent=nil then error('Expected "delay_model"');
  if ent.GetValue<>'table_lookup' then error('Only table_lookup delay supported');

  for lu_table in AEntry.GetMultiple('lu_table_template') do
  begin
    AddTable(MakeTable(lu_table));
  end;

  for cell in AEntry.GetMultiple('cell') do
  begin
    if not HasCell(cell.GetValue) then continue;

    ci:=FindCell(cell.GetValue);

    if assigned(cell.GetSingle('ff')) then
      ci.DFF:=true;

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

      // Timing
      for timing in pin.GetMultiple('timing') do
      begin
        t:=timing.GetSingle('related_pin');
        if t=nil then continue;

        rel:=t.GetValue;

        rt:=timing.GetSingle('rise_transition');
        ft:=timing.GetSingle('fall_transition');
        if (rt<>nil) and (ft<>nil) then
        begin
          idx:=length(cpin^.Physical.TransitionTimes);

          setlength(cpin^.Physical.TransitionTimes, idx+1);
          cpin^.Physical.TransitionTimes[idx].RelativePin:=rel;
          cpin^.Physical.TransitionTimes[idx].FallTransition:=GetTiming(ft);
          cpin^.Physical.TransitionTimes[idx].RiseTransition:=GetTiming(rt);
        end;
      end;

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

initialization
  DecimalSeparator:='.';

end.

