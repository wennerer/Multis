{ <infmultis a part of the multis package>

  Copyright (C) <01.05.2022> <Bernd Hübner>

  This library is free software; you can redistribute it and/or modify it under the
  terms of the GNU Library General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version with the following modification:

  As a special exception, the copyright holders of this library give you permission
  to link this library with independent modules to produce an executable,
  regardless of the license terms of these independent modules,and to copy and
  distribute the resulting executable under terms of your choice, provided that you
  also meet, for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this
  exception to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit infmultis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Graphics, IntfGraphics, LCLType, LCLIntf,
  GraphType;



procedure BmpToAlphaBmp(var AlphaBmp :TBitmap;BlendValue : byte);
procedure Gradient_Bmp(var aBmp: TBitmap; aStart, aStop: TColor;aCourse: integer);
procedure Gradient_Bmp(var aBmp: TBitmap; aWidth, aHeight: integer);
procedure Alternate_Bmp(var Bmp: TBitmap; aStart, aStop: TColor);
procedure Blend_Bmp(var BackBmp: TBitmap; const StartBmp,EndBmp: TBitmap; aFrac: double);

implementation



procedure BmpToAlphaBmp(var AlphaBmp :TBitmap;BlendValue : byte);
var Image1 : TLazIntfImage;
    Image2 : TLazIntfImage;
    Bmp    : TBitmap;
    valR,valG,valB: byte;
    {$IFDEF LCLQt5 or LCLQt}
    valTmp : byte;
    {$ENDIF}
    x,y                   : integer;
    P                     : TPoint;
begin
 try
  // for Windows must be set AlphaBmp.PixelFormat:= pf32bit!
  Bmp       := TBitmap.Create;
  Bmp.Assign(AlphaBmp);
  Image1:= Bmp.CreateIntfImage;
  Image2:= TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
  Image2.SetSize(Bmp.Width,Bmp.Height);
  {$IFDEF LINUX}
   for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;
       {$IFDEF LCLQt5 or LCLQt}
        valTmp := valR;
        valR   := valB;
        valB   := valTmp;
       {$ENDIF}
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valB;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valR;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=BlendValue;

      end;//for x
    end;//for y
    AlphaBmp.LoadFromIntfImage(Image2);
  {$ENDIF}
  {$IFDEF WINDOWS}
  if BlendValue = 0 then BlendValue:=1; //or set Bmp to transparency
  for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;

       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valR;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valB;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:= BlendValue;

      end;//for x
    end;//for y
    AlphaBmp.LoadFromIntfImage(Image2);
   {$ENDIF}
 finally
  Image1.Free;
  Image2.Free;
  Bmp.Free;
 end;
end;


procedure Gradient_Bmp(var aBmp: TBitmap; aStart, aStop: TColor;
  aCourse: integer);
var lv:integer;
    StartR,StartG,StartB : integer;
    StopR,StopG,StopB    : integer;
    delR,delG,delB       : integer;
    fR,fG,fb : double;
    valR,valG,valB       : integer;
    greater,radius: integer;
    tmpBmp               : TBitmap;

const gbHorizontal     = 0;
      gbVertical       = 1;
      gbSpread         = 2;
      gbRadiant        = 3;
      gbAlternate      = 4;


 function System_ToRGB(clSys:TColor):TColor;
  var FPCol :  TFPColor;
  begin
   FPCol:=TColorToFPColor(ColorToRGB(clSys));
   result :=FPColorToTColor(FPCol);
  end;

begin
 if aBmp.Width = 0 then exit; //this needs under windows
 aStart:=System_ToRGB(aStart);
 aStop:=System_ToRGB(aStop);

 if aStart=clBlack then aStart:=RGB(1,0,0);
 if aStop=clBlack then aStop:=rgb(1,0,0);

 StartR:=getRvalue(aStart);
 StartG:=getGvalue(aStart);
 StartB:=getBvalue(aStart);

 StopR:=getRvalue(aStop);
 StopG:=getGvalue(aStop);
 StopB:=getBvalue(aStop);

 delR:= StartR-StopR;
 delG:= StartG-StopG;
 delB:= StartB-StopB;

  if aCourse = gbSpread then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
  else
   greater := round(aBmp.Width/2);

  radius:= round(sqrt(sqr((aBmp.Width/2))+sqr((aBmp.Height/2))));
  fR := delR /radius;
  fG := delG /radius;
  fB := delB /radius;
  for Lv:=radius downto 0 do
   begin
    valR:= StopR+round(lv*fR);
    valG:= StopG+round(lv*fG);
    valB:= StopB+round(lv*fB);
    if valR <0 then valR:=0;
    if valG <0 then valG:=0;
    if valB <0 then valB:=0;
    if valR >255 then valR:=255;
    if valG >255 then valG:=255;
    if valB >255 then valB:=255;
    aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Pen.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Ellipse((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //gbSpread


 if aCourse = gbRadiant then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
   else
   greater := round(aBmp.Width/2);

   fR := delR /greater;
   fG := delG /greater;
   fB := delB /greater;
   for Lv:=greater downto 0 do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //gbRadiant


 if aCourse = gbVertical then
 begin
  fR := delR /aBmp.Height;
  fG := delG /aBmp.Height;
  fB := delB /aBmp.Height;

 for Lv:=0 to aBmp.Height do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(0,lv,aBmp.Width,lv+1);
  end;
 end;//gbVertical

 if aCourse = gbHorizontal then
 begin
  fR := delR /aBmp.Width;
  fG := delG /aBmp.Width;
  fB := delB /aBmp.Width;

 for Lv:=0 to aBmp.Width do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(lv,0,lv+1,aBmp.Height);
  end;
 end;//gbHorizontal

 if aCourse = gbAlternate  then
 begin
  try
   tmpBmp               := TBitmap.Create;
   tmpBmp.SetSize(2, 2);
   tmpBmp.Canvas.Pixels[0, 0] := aStart;
   tmpBmp.Canvas.Pixels[0, 1] := aStop;
   tmpBmp.Canvas.Pixels[1, 0] := aStop;
   tmpBmp.Canvas.Pixels[1, 1] := aStart;

   aBmp.Canvas.Brush.Bitmap := nil;
   aBmp.Canvas.Brush.Bitmap := tmpBmp ;
   aBmp.Canvas.FillRect(0, 0,aBmp.Width,aBmp.Height) ;

  finally
   //tmpBmp.Free;
    FreeAndNil(tmpBmp);
  end;
 end;//gbAlternate

end;

procedure Gradient_Bmp(var aBmp: TBitmap; aWidth, aHeight: integer);
var tmpBmp               : TBitmap;
begin
 try
   tmpBmp     := TBitmap.Create;
   tmpBmp.Assign(aBmp);

   aBmp.Canvas.Brush.Bitmap := nil;
   aBmp.SetSize(aWidth,aHeight);
   aBmp.Canvas.Brush.Bitmap := tmpBmp ;
   aBmp.Canvas.FillRect(0, 0,aBmp.Width,aBmp.Height) ;

  finally
   tmpBmp.Free;
  end;
end;

procedure Alternate_Bmp(var Bmp: TBitmap; aStart, aStop: TColor);
var lv               : integer;
 function System_ToRGB(clSys:TColor):TColor;
  var FPCol :  TFPColor;
  begin
   FPCol:=TColorToFPColor(ColorToRGB(clSys));
   result :=FPColorToTColor(FPCol);
  end;

begin
 aStart:=System_ToRGB(aStart);
 aStop:=System_ToRGB(aStop);

 if aStart=clBlack then aStart:=RGB(1,0,0);
 if aStop=clBlack then aStop:=rgb(1,0,0);

 Bmp.Canvas.Brush.Color:= aStart;
 Bmp.Canvas.FillRect(0,0,Bmp.Width,Bmp.Height);
 Bmp.Canvas.Pen.Color:= aStop;
 for lv := 0 to Bmp.Width do
  begin
   if odd(lv) then Bmp.Canvas.Line(lv,0,lv,Bmp.Height);
  end;
 for lv := 0 to Bmp.Height do
  begin
   if odd(lv) then Bmp.Canvas.Line(0,lv,Bmp.Width,lv);
  end;
end;


procedure Blend_Bmp(var BackBmp: TBitmap; const StartBmp,EndBmp: TBitmap; aFrac: double);
var
  x, y     : Integer;
  r,g, b   : Word;
  BackImg  : TLazIntfImage;
  StartImg : TLazIntfImage;
  EndImg   : TLazIntfImage;
begin

 BackImg  := BackBmp.CreateIntfImage;
 StartImg := StartBmp.CreateIntfImage;
 EndImg   := EndBmp.CreateIntfImage;
 try
    for y := 0 to StartBmp.Height-1 do
      for x := 0 to StartBmp.Width - 1 do
      begin
        r := round((1.0-aFrac) * StartImg.Colors[x, y].Red   + aFrac * EndImg.Colors[x, y].Red);
        g := round((1.0-aFrac) * StartImg.Colors[x, y].Green + aFrac * EndImg.Colors[x, y].Green);
        b := round((1.0-aFrac) * StartImg.Colors[x, y].Blue  + aFrac * EndImg.Colors[x, y].Blue);
        BackImg.Colors[x, y] := FPColor(r, g, b);
      end;

    BackBmp.LoadFromIntfImage(BackImg);

  finally
    BackImg.Free;
    StartImg.Free;
    EndImg.Free;
  end;
end;

end.

