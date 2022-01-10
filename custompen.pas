{ <Makes a custom pen>

  Copyright (C) <Version 1.0.1.1 08.01.2022> <Bernd Hübner>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit CustomPen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, Dialogs, Controls, rs_mbstylemanager;

type
  TChangeEvent = procedure of object;

type
 TCustomPenStyle = (cpsSolid,cpsDash,cpsDot,cpsDashDot,cpsDashDotDot,cpsNull);

 type
  TCustomEndCap = (cepENDCAP_ROUND,cepENDCAP_SQUARE,cepENDCAP_FLAT);


type
 TPenValues = record
  Color    : TColor;                  //Linienfarbe
  PenStyle : TCustomPenStyle;         //benutzerdefinierter Stil
  PenWidth : integer;                 //Liniendicke
  EndCap   : integer;                 //Style Linienende
  Join     : integer;                 //Ecken Style
  LB       : TLogBrush;               //LogBrush
  Style    : LongWord;                //PenStyle
  Dashes   : array [0..5] of DWord;   //aray für Striche
  Length   : DWord;                   //array Länge
  LL       : integer;                 //Länge Linien
  LS       : integer;                 //Länge Lücken
 end;

type
 { TCustomPen }
 TCustomPen = class (TPersistent)
  private
    FMargin: integer;
   FOnChange : TChangeEvent;
   FEndCap   : TCustomEndCap;
   procedure SetColor(AValue: TColor);
   procedure SetEndCap(AValue: TCustomEndCap);
   procedure SetLength(AValue: integer);
   procedure SetMargin(AValue: integer);
   procedure SetPenStyle(AValue: TCustomPenStyle);
   procedure SetPenWidth(AValue: integer);
   procedure SetSpace(AValue: integer);
  public
   PenValues : TPenValues;
   constructor Create;
   function  CreatePen : HPen;

   property Style        : TCustomPenStyle read PenValues.PenStyle  write SetPenStyle;
   property PenWidth     : integer         read PenValues.PenWidth  write SetPenWidth;
   property Color        : TColor          read PenValues.Color     write SetColor;
   property LinesLength  : integer         read PenValues.LL        write SetLength;
   property LinesSpace   : integer         read PenValues.LS        write SetSpace;
   property EndCap       : TCustomEndCap   read FEndCap             write SetEndCap;
   //The edge distance in the target component
   property Margin       : integer         read FMargin             write SetMargin;

   property OnChange : TChangeEvent read FOnChange write FOnChange;

 end;
implementation

{ TCustomPen }


constructor TCustomPen.Create;
begin
 PenValues.Color      := clMaroon;
 FEndCap              := cepENDCAP_FLAT;
 PenValues.EndCap     := PS_ENDCAP_FLAT;
 PenValues.Join       := PS_JOIN_MITER;
 PenValues.PenStyle   := cpsSolid;
 PenValues.PenWidth   := 2;
 PenValues.Length     := 4;
 PenValues.LL         := 10;
 PenValues.LS         :=  3;
 PenValues.LB.lbStyle := BS_SOLID;
 PenValues.LB.lbHatch := 0;
end;

function TCustomPen.CreatePen: HPen;
begin

 if PenValues.PenStyle = cpsSolid then
  begin
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_Solid + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,0, nil);
  end;

 if PenValues.PenStyle = cpsDash then
  begin
   PenValues.Length      := 4;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDot then
  begin
   PenValues.Length      := 4;
   PenValues.Dashes[0]   := round(PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDashDot then
  begin
   PenValues.Length      := 4;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDashDotDot then
  begin
   PenValues.Length      := 6;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[4]   := round(PenValues.PenWidth);
   PenValues.Dashes[5]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsNull  then
  begin
   PenValues.Style := PS_GEOMETRIC or PS_Null;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,0, nil);
  end;

 if Result = 0 then showmessage(rs_penerror);
end;


procedure TCustomPen.SetPenWidth(AValue: integer);
begin
  if aValue < 1 then Exit;
  if aValue > 25 then Exit;
  if PenValues.PenWidth = aValue then Exit;
  PenValues.PenWidth:=AValue;
  if Assigned(OnChange) then OnChange;
end;

procedure TCustomPen.SetColor(AValue: TColor);
begin
  if PenValues.Color = aValue then Exit;
  PenValues.Color :=AValue;
  if Assigned(OnChange) then OnChange;
end;

procedure TCustomPen.SetEndCap(AValue: TCustomEndCap);
begin
 if FEndCap = AValue then Exit;
 FEndCap := aValue;
 if aValue = cepENDCAP_SQUARE then PenValues.EndCap := PS_ENDCAP_SQUARE;
 if aValue = cepENDCAP_ROUND  then PenValues.EndCap := PS_ENDCAP_ROUND;
 if aValue = cepENDCAP_FLAT   then PenValues.EndCap := PS_ENDCAP_FLAT;
 if Assigned(OnChange) then OnChange;

end;

procedure TCustomPen.SetLength(AValue: integer);
begin
  if PenValues.LL=AValue then Exit;
  PenValues.LL:=AValue;
  if Assigned(OnChange) then OnChange;
end;

procedure TCustomPen.SetMargin(AValue: integer); //The edge distance in the target component
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
end;

procedure TCustomPen.SetSpace(AValue: integer);
begin
  if PenValues.LS=AValue then Exit;
  PenValues.LS:=AValue;
  if Assigned(OnChange) then OnChange;
end;

procedure TCustomPen.SetPenStyle(AValue: TCustomPenStyle);
begin
  if PenValues.PenStyle=AValue then Exit;
  PenValues.PenStyle:=AValue;
  //PenValues.LL := FLength;
  //PenValues.LS := FSpace;
  if Assigned(OnChange) then OnChange;
end;


end.

