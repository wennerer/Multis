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
  GraphType, Math, LazCanvas, FPCanvas;



procedure BmpToAlphaBmp(var AlphaBmp :TBitmap;BlendValue : byte);
procedure Gradient_Bmp(var aBmp: TBitmap; aStart, aStop: TColor;aCourse: integer);
procedure Gradient_Bmp(var aBmp: TBitmap; aWidth, aHeight: integer);
procedure Alternate_Bmp(var Bmp: TBitmap; aStart, aStop: TColor);
procedure Blend_Bmp(var BackBmp: TBitmap; const StartBmp,EndBmp: TBitmap; aFrac: double);
procedure Mask_Bmp(var aBmp : TBitmap; aOrd : integer;aRadius : integer);
function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
procedure RotateImage(Img: TFPCustomImage; Angle: Double);
procedure BlendImages(Img1, Img2: TFPCustomImage; AFactor: Double);
procedure AlphaImages(Img1: TFPCustomImage; AFactor: Double);
procedure ChangeColor(Img: TLazIntfImage;aColor : TColor);
procedure ChangeBorderColor(Img: TLazIntfImage;aColor : TColor);
Procedure StretchDrawImgToImg(SourceImg, DestImg: TLazIntfImage; DestWidth, DestHeight: integer);
function System_ToRGB(clSys:TColor):TColor;

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

   (*tmpBmp.SetSize(2,2);
   tmpBmp.Canvas.Brush.Color:= aStart;
   tmpBmp.Canvas.FillRect(0,0,1,1);
   tmpBmp.Canvas.FillRect(1,1,2,2);
   tmpBmp.Canvas.Brush.Color:= aStop;
   tmpBmp.Canvas.FillRect(1,0,2,1);
   tmpBmp.Canvas.FillRect(0,1,1,2);*)

   (*tmpBmp.SetSize(6,6);
   tmpBmp.Canvas.Brush.Color:= aStart;
   tmpBmp.Canvas.FillRect(0,0,3,3);
   tmpBmp.Canvas.FillRect(3,3,6,6);
   tmpBmp.Canvas.Brush.Color:= aStop;
   tmpBmp.Canvas.FillRect(3,0,6,3);
   tmpBmp.Canvas.FillRect(0,3,3,6);*)

   (*tmpBmp.SetSize(4,4);
   tmpBmp.Canvas.Brush.Color:= aStart;
   tmpBmp.Canvas.FillRect(0,0,2,2);
   tmpBmp.Canvas.FillRect(2,2,4,4);
   tmpBmp.Canvas.Brush.Color:= aStop;
   tmpBmp.Canvas.FillRect(2,0,4,2);
   tmpBmp.Canvas.FillRect(0,2,2,4);*)

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

procedure Mask_Bmp(var aBmp: TBitmap; aOrd: integer;aRadius : integer);
var bmp1,bmp2,bmp3,bmp4 : TBitmap;
    aRect  : TRect;
begin
 try
  aRect := rect(0,0,aBmp.Width,aBmp.Height);

  bmp1 := TBitmap.Create;
  bmp1.SetSize(aBmp.Width,aBmp.Height);
  bmp1.Canvas.Draw(0,0,aBmp);


  bmp2 := TBitmap.Create;
  bmp2.SetSize(aBmp.Width,aBmp.Height);
  bmp2.TransparentColor:=clblack;
  bmp2.Transparent:= true;
  bmp2.Canvas.Brush.Color:=clwhite;
  bmp2.Canvas.FillRect(0,0,aBmp.Width,aBmp.Height);
  bmp2.Canvas.Brush.Color:=clBlack;
  case aOrd of
   0      : bmp2.Canvas.Rectangle(aRect);
   1      : bmp2.Canvas.RoundRect(aRect,aRadius,aRadius);
   2      : bmp2.Canvas.Ellipse(aRect);
   3      : bmp2.Canvas.Ellipse(aRect);
  end;

  bmp3 := TBitmap.Create;
  bmp3.SetSize(aBmp.Width,aBmp.Height);
  bmp3.Canvas.Brush.Color:=clwhite;
  bmp3.Canvas.FillRect(0,0,aBmp.Width,aBmp.Height);
  bmp3.Canvas.Brush.Color:=clBlack;
  case aOrd of
   0      : bmp3.Canvas.Rectangle(aRect);
   1      : bmp3.Canvas.RoundRect(aRect,aRadius,aRadius);
   2      : bmp3.Canvas.Ellipse(aRect);
   3      : bmp3.Canvas.Ellipse(aRect);
  end;

  bmp4 := TBitmap.Create;
  bmp4.SetSize(aBmp.Width,aBmp.Height);
  bmp4.Transparent:= true;
  bmp4.TransparentColor:= clBlack;
  bmp4.Canvas.Brush.Color:=clBlack;
  bmp4.Canvas.FillRect(0,0,aBmp.Width,aBmp.Height);
  bmp4.Canvas.copymode:=cmSrcCopy;
  bmp4.Canvas.Draw(0,0,bmp1);
  bmp4.Canvas.Draw(0,0,bmp2);
  bmp4.Canvas.copymode:=cmSrcInvert;
  bmp4.Canvas.Draw(0,0,bmp3);

  aBmp.Transparent:= true;
  aBmp.TransparentColor:= clBlack;
  aBmp.Assign(bmp4);
  //aBmp.Canvas.Draw(0,0,bmp4);


 finally
  bmp1.Free;
  bmp2.Free;
  bmp3.Free;
  bmp4.Free;

 end;
end;

function RotatePoint(const APoint: TPoint; AAngle: Double): TPoint;
var
  sa, ca: Double;
begin
  SinCos(AAngle, sa, ca);
  Result.X := Round( ca * APoint.X + sa * APoint.Y);
  Result.Y := Round(-sa * APoint.X + ca * APoint.Y);
end;

procedure RotateImage(Img: TFPCustomImage; Angle: Double);
var
  Buffer: TFPCustomImage;
  x, y: Integer;
  C, P: TPoint;
begin
  Buffer := TFPMemoryImage.Create(Img.Width, Img.Height);
  C := Point(Img.Width div 2, Img.Height div 2);
  for y := 0 to Buffer.Height-1 do
    for x := 0 to Buffer.Width-1 do
    begin
      P := RotatePoint(Point(x, y) - C, Angle) + C;

      if (P.X >= 0) and (P.Y >= 0) and (P.X < Img.Width) and (P.Y < Img.Height) then
        Buffer.Colors[x, y] := Img.Colors[P.X, P.Y]
      else
        Buffer.Colors[x, y] := colTransparent;
    end;
  for y := 0 to Img.Height-1 do
   for x := 0 to Img.Width-1 do
      Img.Colors[x, y] := Buffer[x, y];
  Buffer.Free;
end;

procedure BlendImages(Img1, Img2: TFPCustomImage; AFactor: Double);
var
  x, y: Integer;
  r,g,b,a: Word;
  f1, f2: Double;
begin
  f1 := 1.0 - AFactor;
  f2 := AFactor;
  for y := 0 to Img1.Height-1 do
    for x := 0 to Img1.Width-1 do
    begin
      r := round(f1 * Img1.Colors[x, y].Red   + f2 * Img2.Colors[x, y].Red);
      g := round(f1 * Img1.Colors[x, y].Green + f2 * Img2.Colors[x, y].Green);
      b := round(f1 * Img1.Colors[x, y].Blue  + f2 * Img2.Colors[x, y].Blue);
      a := round(f1 * Img1.Colors[x, y].Alpha + f2 * Img2.Colors[x, y].Alpha);
      Img1.Colors[x, y] := FPColor(r, g, b, a);
    end;
end;

procedure AlphaImages(Img1: TFPCustomImage; AFactor: Double);
var
  x, y: Integer;
  r,g,b,a: Word;
  f1: Double;
begin
 f1 := AFactor;
 for y := 0 to Img1.Height-1 do
    for x := 0 to Img1.Width-1 do
    begin
      r := round(f1 * Img1.Colors[x, y].Red   );
      g := round(f1 * Img1.Colors[x, y].Green );
      b := round(f1 * Img1.Colors[x, y].Blue  );
      a := round(f1 * Img1.Colors[x, y].Alpha );
      Img1.Colors[x, y] := FPColor(r, g, b, a);
    end;
end;

procedure ChangeColor(Img: TLazIntfImage;aColor : TColor);
var Image1                 : TCustomBitmap;
    Image2                 : TLazIntfImage;
    valR,valG,valB,valA    : byte;
    valRNew,valGNew,valBNew: byte;
    valtemp1,valtemp2      : byte;
    x,y                    : integer;
    P                      : TPoint;
begin
 Image1 := TPortableNetworkGraphic.Create;
 Image1.SetSize(Img.Width,Img.Height);
 Image2:= Image1.CreateIntfImage;
 Image2.Assign(Img);
 {$IFDEF Windows}
  valtemp1:=0;
  valtemp2:=200;
  valRNew:=GetRValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getBvalue(aColor);
 {$ENDIF}
 {$IFDEF FreeBSD}
  valRNew:=GetBValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getRvalue(aColor);
  valtemp1:=200;
  valtemp2:=0;
 {$ENDIF}
 {$IFDEF Linux}
  valRNew:=GetBValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getRvalue(aColor);
  valtemp1:=200;
  valtemp2:=0;
 {$ENDIF}
 try

   for y := 0 to  Img.height - 1 do
    begin
     for x := 0 to Img.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Img.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbBlue;
       valA:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbReserved;
       //if valA <> 0 then
       if valR = valtemp2 then
        if valG = 0 then
         if valB = valtemp1 then
          if ValA = 255 then
           begin
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valRNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valGNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valBNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=255;
           end;
       if (ValA <> 255) and (valA <> 0) then
           begin
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valRNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valGNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valBNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=valA;
           end;

      end;//for x
    end;//for y
    Img.Assign(Image2);

 finally
  Image2.Free;
  Image1.Free;
 end;
end;

procedure ChangeBorderColor(Img: TLazIntfImage;aColor : TColor);
var Image1                 : TCustomBitmap;
    Image2                 : TLazIntfImage;
    valR,valG,valB,valA    : byte;
    valRNew,valGNew,valBNew: byte;
    valtemp1,valtemp2      : byte;
    x,y                    : integer;
    P                      : TPoint;
begin
 Image1 := TPortableNetworkGraphic.Create;
 Image1.SetSize(Img.Width,Img.Height);
 Image2:= Image1.CreateIntfImage;
 Image2.Assign(Img);
 {$IFDEF Windows}
  valtemp1:=0;
  valtemp2:=200;
  valRNew:=GetRValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getBvalue(aColor);
 {$ENDIF}
 {$IFDEF FreeBSD}
  valRNew:=GetBValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getRvalue(aColor);
  valtemp1:=200;
  valtemp2:=0;
 {$ENDIF}
 {$IFDEF LINUX}
  valRNew:=GetBValue(aColor);
  valGNew:=getGvalue(aColor);
  valBNew:=getRvalue(aColor);
  valtemp1:=200;
  valtemp2:=0;
 {$ENDIF}
 try

   for y := 0 to  Img.height - 1 do
    begin
     for x := 0 to Img.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Img.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbBlue;
       valA:= PRGBQuad(Img.GetDataLineStart(P.Y))[P.X].rgbReserved;
       if valA <> 0 then
           begin
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valRNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valGNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valBNew;
            PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=valA;
           end;


      end;//for x
    end;//for y
    Img.Assign(Image2);

 finally
  Image2.Free;
  Image1.Free;
 end;
end;


Procedure StretchDrawImgToImg(SourceImg, DestImg: TLazIntfImage; DestWidth, DestHeight: integer);
Var
  DestIntfImage: TLazIntfImage;
  DestCanvas: TLazCanvas;
Begin
  DestIntfImage := TLazIntfImage.Create(0, 0);
  DestIntfImage.Assign(DestImg);
  DestCanvas := TLazCanvas.Create(DestIntfImage);
  DestCanvas.Interpolation := TMitchelInterpolation.Create;//TFPSharpInterpolation.Create;
  DestCanvas.StretchDraw(0, 0, DestWidth, DestHeight, SourceImg);
  DestImg.Assign(DestIntfImage);
  DestCanvas.Interpolation.Free;
  DestCanvas.Free;
  DestIntfImage.Free;
End;

function System_ToRGB(clSys:TColor):TColor;
  var FPCol :  TFPColor;
  begin
   FPCol:=TColorToFPColor(ColorToRGB(clSys));
   result :=FPColorToTColor(FPCol);
  end;


end.

