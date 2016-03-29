unit blifreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, strutils;

type
  TConnection = record
    Pin,
    Net: string;
  end;

  TSubCircuit = class
  private
    fName: string;
    fConnections: array of TConnection;
    function GetConnection(AIndex: longint): TConnection;
    function GetCount: longint;
  protected
    procedure AddConnection(APinNet: string);
    procedure AddConnection(const APin, ANet: string);
  public
    constructor Create(const AName: string);

    property Name: string read fName;
    property Count: longint read GetCount;
    property Connection[AIndex: longint]: TConnection read GetConnection;
  end;

  TBlifReader = class
  private
    FGNDName,
    FVDDName,
    fName: string;
    fConnections,
    fNets,
    fInputs,
    fOutputs: TStringList;
    fSubCircuits: TObjectList;
    procedure AddStr(AList: TStringList; var AStr: string);
    function GetCount: longint;
    function GetSubCircuit(AIndex: longint): TSubCircuit;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(AStream: TStream);

    property Name: string read fName;

    property Connections: TStringList read fConnections;
    property Nets: TStringList read fNets;
    property Inputs: TStringList read fInputs;
    property Outputs: TStringList read fOutputs;

    property Count: longint read GetCount;
    property SubCircuit[AIndex: longint]: TSubCircuit read GetSubCircuit;

    property VDDName: string read FVDDName write FVDDName;
    property GNDName: string read FGNDName write FGNDName;
  end;

implementation

function TBlifReader.GetCount: longint;
begin
  result:=fSubCircuits.Count;
end;

function TBlifReader.GetSubCircuit(AIndex: longint): TSubCircuit;
begin
  result:=TSubCircuit(fSubCircuits[AIndex]);
end;

constructor TBlifReader.Create;
begin
  inherited Create;
  FVDDName:='vdd';
  FGNDName:='gnd';
  fName:='';

  fConnections:=TStringList.Create;
  fConnections.CaseSensitive:=true;
  fConnections.Sorted:=true;
  fConnections.Duplicates:=dupIgnore;

  fNets:=TStringList.Create;
  fNets.CaseSensitive:=true;
  fNets.Sorted:=true;
  fNets.Duplicates:=dupIgnore;

  fOutputs:=TStringList.Create;
  fOutputs.CaseSensitive:=true;
  fOutputs.Sorted:=true;
  fOutputs.Duplicates:=dupIgnore;

  fInputs:=TStringList.Create;
  fInputs.CaseSensitive:=true;
  fInputs.Sorted:=true;
  fInputs.Duplicates:=dupIgnore;

  fSubCircuits:=TObjectList.Create(true);
end;

destructor TBlifReader.Destroy;
begin
  fSubCircuits.Free;
  fConnections.Free;
  fOutputs.free;
  fInputs.free;
  fNets.free;
  inherited Destroy;
end;

procedure TBlifReader.AddStr(AList: TStringList; var AStr: string);
var
  s: String;
begin
  s:=trim(AStr);

  if s='$true' then s:=FVDDName
  else if s='$false' then s:=FGNDName;

  if s<>'' then
    AList.Add(s);
end;

procedure TBlifReader.LoadFromStream(AStream: TStream);
var
  st: TStringList;
  s, nr, pin, t: String;
  i: longint;
  ckt: TSubCircuit;
begin
  st:=TStringList.Create;
  try
    st.LoadFromStream(AStream);

    for i:=0 to st.Count-1 do
    begin
      s:=trim(st[i]);
      if pos('#',s)=1 then continue;

      if pos('.model',s)=1 then
      begin
        delete(s, 1,7);
        fName:=s;
      end
      else if pos('.inputs',s)=1 then
      begin
        delete(s,1,8);

        while pos(' ', s)>0 do
        begin
          t:=Copy2SpaceDel(s);
          AddStr(fInputs, t);
        end;
        AddStr(fInputs, s);
      end
      else if pos('.outputs',s)=1 then
      begin
        delete(s,1,9);

        while pos(' ', s)>0 do
        begin
          t:=Copy2SpaceDel(s);
          AddStr(fOutputs, t);
        end;
        AddStr(fOutputs, s);
      end
      else if pos('.names',s)=1 then
      begin
        delete(s,1,7);
        s:=trim(s);

        if pos(' ',s)>0 then
        begin
          if st[i+1]='1 1' then
          begin
            pin:=trim(Copy2SpaceDel(s));

            if pin='$true' then
              pin:=FVDDName
            else if pin='$false' then
              pin:=FGNDName;

            fConnections.Add(pin+'='+trim(s));
          end;
        end;
      end
      else if pos('.subckt',s)=1 then
      begin
        delete(s,1,8);

        ckt:=TSubCircuit.Create(Copy2SpaceDel(s));

        while pos(' ', s)>0 do
        begin
          nr:=trim(Copy2SpaceDel(s));

          if nr<>'' then
          begin
            pin:=Copy2SymbDel(nr, '=');

            AddStr(fNets, nr);
            ckt.AddConnection(pin, nr);
          end;
        end;

        fSubCircuits.Add(ckt);
      end;
    end;
  finally
    st.free;
  end;
end;

function TSubCircuit.GetConnection(AIndex: longint): TConnection;
begin
  result:=fConnections[AIndex];
end;

function TSubCircuit.GetCount: longint;
begin
  result:=length(fConnections);
end;

procedure TSubCircuit.AddConnection(APinNet: string);
var
  pin: String;
begin
  pin:=trim(Copy2SymbDel(APinNet,'='));
  AddConnection(pin, trim(APinNet));
end;

procedure TSubCircuit.AddConnection(const APin, ANet: string);
begin
  setlength(fConnections, high(fConnections)+2);
  with fConnections[high(fConnections)] do
    begin
      Net:=ANet;
      Pin:=APin;
    end;
end;

constructor TSubCircuit.Create(const AName: string);
begin
  inherited Create;
  fName:=AName;
end;

end.

