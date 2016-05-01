{
  Naive LEF parser

  Should be updated to do real parsing at some point
}
unit lefreader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

type
  TLEFReader = class
  private
    function ValueToMeter(const AValue: double): double;
  public
    procedure LoadFromStream(AStream: TStream);

    constructor Create;
    destructor Destroy; override;
  end;

procedure LoadLEF(const AFilename: string);

implementation

uses
  cells, geometry;

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

function TLEFReader.ValueToMeter(const AValue: double): double;
begin
  result:=AValue*1e-6;
end;

procedure TLEFReader.LoadFromStream(AStream: TStream);
var
  st: TStringList;
  i, layeridx: longint;
  s, name, cls: string;
  layer: TLayer;
  via: TVia;
  x, y: double;
  start, stop: TCoordinate;
  cell: TCell;
  dir: TPinDirection;
  clas: TPinClass;
  shape: TPinShape;

  procedure Next;
  begin
    repeat
      s:=trim(st[i]);
      inc(i);
    until pos('#', s)<>1;
  end;

begin
  layeridx:=0;
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
        layer.Index:=layeridx;
        inc(layeridx);

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
        delete(s,1,4);
        s:=trim(s);
        name:=trim(Copy2SpaceDel(s));

        via:=TVia.Create(name);
        RegisterCell(via);

        Next;
        while pos('END ', s)<>1 do
        begin
          if pos('LAYER ',s)=1 then
          begin
            delete(s,1,6);
            s:=trim(s);

            layer:=GetLayer(trim(Copy2SymbDel(s, ';')));

            Next;
          end
          else if pos('RECT ',s)=1 then
          begin
            delete(s,1,5);
            s:=trim(s);

            x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
            y:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);

            start:=GetCoord(ValueToMeter(x),ValueToMeter(y));

            x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
            y:=strtofloat(trim(Copy2SymbDel(s, ';')));

            stop:=GetCoord(ValueToMeter(x),ValueToMeter(y));

            via.AddPoly(GetLayerRect(layer, start, stop));

            Next;
          end
          else
            Next;
        end;
        Next;
      end
      else if pos('VIARULE ', s)=1 then
      begin
        while pos('END ', s)<>1 do
          Next;
        Next;
      end
      else if pos('SITE', s)=1 then
      begin
        delete(s, 1,5); s:=trim(s);

        cls:='';
        while pos('END ', s)<>1 do
        begin
          if pos('CLASS',s)=1 then
          begin
            delete(s,1,6); s:=trim(s);

            cls:=trim(Copy2SymbDel(s,';')); s:=trim(s);

            next;
          end
          else if pos('SIZE',s)=1 then
          begin
            Delete(s,1,5);
            s:=trim(s);

            x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
            delete(s,1,2); s:=trim(s);
            y:=strtofloat(trim(Copy2SymbDel(s,';'))); s:=trim(s);

            if cls = 'CORE' then
              CoreSize:=GetCoord(ValueToMeter(x),ValueToMeter(y))
            else if cls = 'IO' then
              PadSize:=GetCoord(ValueToMeter(x),ValueToMeter(y));

            next;
          end
          else
            Next;
        end;
        Next;
      end
      else if pos('MACRO ', s)=1 then
      begin
        delete(s,1,5);
        s:=trim(s);

        cell:=FindCell(s);

        while pos('END ', s)<>1 do
        begin
          if pos('FOREIGN ', s)=1 then
          begin
            Next;
          end
          else if pos('CLASS ', s)=1 then
          begin
            delete(s, 1,6); s:=trim(s);

            if pos('PAD',s)=1 then cell.CellClass:=ccPad
            else if pos('CORE',s)=1 then cell.CellClass:=ccCore
            else if pos('ENDCAP',s)=1 then
            begin
              delete(s,1,6); s:=trim(s);

              if pos('TOPLEFT',s)=1 then cell.CellClass:=ccEndCapTL
              else if pos('TOPRIGHT',s)=1 then cell.CellClass:=ccEndCapTR
              else if pos('BOTTOMRIGHT',s)=1 then cell.CellClass:=ccEndCapBR
              else if pos('BOTTOMLEFT',s)=1 then cell.CellClass:=ccEndCapBL;
            end;

            Next;
          end
          else if pos('SITE ', s)=1 then
          begin
            Next;
          end
          else if pos('SIZE ', s)=1 then
          begin
            Delete(s,1,5);
            s:=trim(s);

            x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
            delete(s,1,2); s:=trim(s);
            y:=strtofloat(trim(Copy2SymbDel(s,';'))); s:=trim(s);

            cell.Size:=GetCoord(ValueToMeter(x),ValueToMeter(y));

            Next;
          end
          else if pos('PIN ', s)=1 then
          begin
            delete(s,1,4);
            name:=trim(s);

            Next;

            dir:=pdUnknown;
            clas:=pcNone;
            shape:=psDefault;

            while pos('END ', s)<>1 do
            begin
              if pos('DIRECTION', s)=1 then
              begin
                delete(s,1,9);
                s:=Uppercase(trim(copy2symb(s,';')));

                case s of
                  'INPUT': dir:=pdIn;
                  'OUTPUT': dir:=pdOut;
                  'INOUT': dir:=pdInOut;
                end;
              end
              else if pos('USE', s)=1 then
              begin
                delete(s,1,3);
                s:=Uppercase(trim(copy2symb(s,';')));

                case s of
                  'POWER': clas:=pcPower;
                end;
              end
              else if pos('SHAPE', s)=1 then
              begin
                delete(s,1,5);
                s:=Uppercase(trim(copy2symb(s,';')));

                case s of
                  'ABUTMENT': shape:=psAbutment;
                end;
              end
              else if pos('PORT', s)=1 then
              begin
                next;
                while pos('END', s)<>1 do
                begin
                  if pos('LAYER ',s)=1 then
                  begin
                    delete(s,1,6);
                    s:=trim(s);

                    layer:=GetLayer(trim(Copy2SymbDel(s, ';')));

                    Next;
                  end
                  else if pos('RECT ',s)=1 then
                  begin
                    delete(s,1,5);
                    s:=trim(s);

                    x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
                    y:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);

                    start:=GetCoord(ValueToMeter(x),ValueToMeter(y));

                    x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
                    y:=strtofloat(trim(Copy2SymbDel(s, ';')));

                    stop:=GetCoord(ValueToMeter(x),ValueToMeter(y));

                    cell.AddPin(name, GetLayerRect(layer, start, stop), dir, clas, shape);

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

            Next;
          end
          else if pos('OBS', s)=1 then
          begin
            next;
            while pos('END', s)<>1 do
            begin
              if pos('LAYER ',s)=1 then
              begin
                delete(s,1,6);
                s:=trim(s);

                layer:=GetLayer(trim(Copy2SymbDel(s, ';')));

                Next;
              end
              else if pos('RECT ',s)=1 then
              begin
                delete(s,1,5);
                s:=trim(s);

                x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
                y:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);

                start:=GetCoord(ValueToMeter(x),ValueToMeter(y));

                x:=strtofloat(Copy2SpaceDel(s)); s:=trim(s);
                y:=strtofloat(trim(Copy2SymbDel(s, ';')));

                stop:=GetCoord(ValueToMeter(x),ValueToMeter(y));

                cell.AddObstruction(GetLayerRect(layer, start, stop));

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

