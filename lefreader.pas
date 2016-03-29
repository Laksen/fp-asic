unit lefreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

type
  TLEFReader = class
  public
    procedure LoadFromStream(AStream: TStream);

    constructor Create;
    destructor Destroy; override;
  end;

procedure LoadLEF(const AFilename: string);

implementation

uses
  cells;

procedure LoadLEF(const AFilename: string);
var
  lr: TLEFReader;
  s: TFileStream;
begin
  lr:=TLEFReader.Create;
  try
    s:=TFileStream.Create(AFilename, fmOpenRead);
    try
      lr.LoadFromStream(s);
    finally
      s.Free;
    end;
  finally
    lr.Free;
  end;
end;

procedure TLEFReader.LoadFromStream(AStream: TStream);
var
  st: TStringList;
  i: longint;
  s, name: string;
  layer: TLayer;

  procedure Next;
  begin
    repeat
      s:=trim(st[i]);
      inc(i);
    until pos('#', s)<>1;
  end;

begin
  st:=TStringList.Create;
  try
    st.LoadFromStream(AStream);

    i:=0;
    while i<st.count do
    begin
      if pos('#', s)=1 then
      begin
        next;
        continue;
      end;

      if pos('LAYER ', s)=1 then
      begin
        delete(s,1,6);
        name:=trim(s);

        layer:=GetLayer(name);

        Next;
        while pos('END ', s)<>1 do
        begin
          if pos('TYPE',s)=1 then
          begin
            delete(s,1,5); s:=trim(s);
            s:=TrimRightSet(s,[';',' ']);

            case s of
              'MASTERSLICE': layer.LayerType:=ltPoly;
              'CUT': layer.LayerType:=ltCut;
              'ROUTING': layer.LayerType:=ltRouting;
            end;

            Next;
          end
          else if pos('SPACING',s)=1 then
          begin
            delete(s,1,8); s:=trim(s);
            s:=TrimRightSet(s,[';',' ']);

            DefaultFormatSettings.DecimalSeparator:='.';
            layer.Spacing:=StrToFloat(s);

            Next;
          end
          else if pos('WIDTH',s)=1 then
          begin
            delete(s,1,6); s:=trim(s);
            s:=TrimRightSet(s,[';',' ']);

            DefaultFormatSettings.DecimalSeparator:='.';
            layer.Width:=StrToFloat(s);

            Next;
          end
          else if pos('PITCH',s)=1 then
          begin
            delete(s,1,6); s:=trim(s);
            s:=TrimRightSet(s,[';',' ']);

            DefaultFormatSettings.DecimalSeparator:='.';
            layer.Pitch:=StrToFloat(s);

            Next;
          end
          else
            Next;
        end;
        Next;
      end
      else if pos('VIA ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('VIARULE ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('SITE ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('MACRO ', s)=1 then
      begin
        while pos('END ', s)<>1 do
        begin
          if pos('PORT ', s)=1 then
          begin
            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else if pos('PIN ', s)=1 then
          begin
            if pos('PORT ', s)=1 then
            begin
              while pos('END ', s)<>1 do
                Next;
              Next;
            end;

            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else if pos('OBS ', s)=1 then
          begin
            while pos('END ', s)<>1 do
              Next;
            Next;
          end
          else
            Next;
        end;
        Next;
      end
      else
        Next;
    end;
  finally
    st.Free;
  end;
end;

constructor TLEFReader.Create;
begin
  inherited Create;
end;

destructor TLEFReader.Destroy;
begin
  inherited Destroy;
end;

end.

