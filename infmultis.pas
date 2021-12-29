unit infmultis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics, LCLType,
  GraphType;

   //FPImage,LCLIntf,



procedure BmpToAlphaBmp(var AlphaBmp :TBitmap;BlendValue : byte);

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
end.

