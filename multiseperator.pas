{ <A seperator between Controls>
  <Version 1.0.1.0>
  Copyright (C) <11.01.2022> <Bernd Hübner>
  Many thanks to the members of the German Lazarus Forum!
  for more information see https://www.lazarusforum.de/viewtopic.php?p=125633#p125633

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

unit MultiSeperator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,InfMultis, CustomPen,
  LCLType, LCLIntf, ExtCtrls, rs_mbstylemanager, LCLProc;

type
  TMSeperatorStyle = (mspRect,mspRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate, gcBitmap); //for background color

type
  TOrientation = (mspHorizontal,mspVertical);

type
  TLineDesign = (ldSingle,ldDouble);

type

  { TMSCustomPen }

  TMSCustomPen           = class (TCustomPen)

  private
    FOwner : TGraphicControl;
    FDesign: TLineDesign;
    procedure SetDesign(AValue: TLineDesign);
  published
   //The number of lines
   //Die Anzahl der Linien
   property Design : TLineDesign read FDesign write SetDesign default ldSingle;
   //The style of the line, cpsNull makes unvisible
   //Der Stil der Linie, cpsNull macht unsichtbar
   property Style default cpsNull;
   //The width of the line
   //Die Dicke der Linie
   property PenWidth default 1;
   //The color of the line
   //Die Farbe der Linie
   property Color default clWhite;
   //The lenght of the lines at dash,dashdot ...
   //Die Länge der Linien bei Strich-, Strichpunkt ...
   property LinesLength default 5;
   //The lenght of the space at dash,dashdot ...
   //Die Länge der Zwischenräume bei Strich-, Strichpunkt ...
   property LinesSpace default 5;
   //The shape of the line ends
   //Die Form der Linienenden
   property EndCap default cepEndCap_Flat;
   //The distance from the line to the border
   //Der Abstand der Linie zur Border
   property Margin default 4;

  end;


type

  { TMultiSeperator }

  TMultiSeperator = class (TGraphicControl)
  private
    FBorderColor: TColor;
    FBorderWidth: integer;
    FColorEnd           : TColor;
    FColorStart         : TColor;
    FCustomPen          : TMSCustomPen;
    FGradient           : TGradientCourse;
    FBackgrdVisible     : boolean;   //this is not a published property clNone
    FImage: TPicture;
    FOrientation        : TOrientation;
    FRRRadius           : integer;
    FSeperatorBounds    : TRect;
    FLoaded             : boolean;  //skips turning when csloading
    FStyle              : TMSeperatorStyle;


    procedure CalculateSeperator;
    procedure DrawDoubleLine;
    procedure DrawSeperator;
    procedure DrawSingleLine;
    procedure DrawTheBackground;
    procedure DrawABorder;
    procedure BorderChangingChange({%H-}Sender: TObject);
    procedure CustomPenCanged;
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer);

    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetCustomPen(AValue: TMSCustomPen);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetPicture(AValue: TPicture);
    procedure SetOrientation(AValue: TOrientation);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMSeperatorStyle);

  protected
    procedure BoundsChanged;override;
    procedure Loaded; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   Paint; override;
  published
   //The geometric shape of the seperator
   //Die geometrische Form des Seperators
   property Style      : TMSeperatorStyle read FStyle write SetStyle default mspRect;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //The orientation of the seperator
   //Die Orientierung des Seperators
   property Orientation : TOrientation read FOrientation  write SetOrientation default mspVertical;
   //The start color of the background ( for color gradient),clNone makes unvisibel
   //Die Startfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property ColorStart : TColor  read FColorStart      write SetColorStart default clMaroon;
   //The end color of the background ( for color gradient),clNone makes unvisibel
   //Die Endfarbe des Hintergrundes (für Farbverlauf),clNone macht unsichtbar
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default $002C2CAA;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The color of the border, clNone makes unvisible
   //Die Farbe des RahmensclNone macht unsichtbar
   property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
   //The whidth of the border
   //Die Dicke des Rahmens
   property BorderWidth : integer read FBorderWidth write SetBorderWidth default 1;
   //The Settings of the line in the seperator
   //Die Einstellungen der Linie im Seperator
   property LineSettings : TMSCustomPen read FCustomPen write SetCustomPen;
   //Contains the image displayed in the control, only active with gcBitmap
   //Beeinhaltet das Bild das im Control ausgegeben wird, nur bei gcBitmap
   property BackgrdImage : TPicture    read FImage write SetPicture;

   property Align;
   property Anchors;
   property Action;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property ShowHint;
   property Visible;
   property Enabled;

  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiseperator_icon.lrs}
  RegisterComponents('Multi',[TMultiSeperator]);
end;

{ TMSCustomPen }

procedure TMSCustomPen.SetDesign(AValue: TLineDesign);
begin
  if FDesign=AValue then Exit;
  FDesign:=AValue;
  (FOwner as TMultiSeperator).Invalidate;
end;

{ TSeperator }

constructor TMultiSeperator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Width  :=  10;
 Height := 200;
 FColorStart     := clMaroon;
 FColorEnd       := $002C2CAA;
 FGradient       := gcSpread;
 FBackgrdVisible := true;
 FOrientation    := mspVertical;
 FLoaded         := false;
 FStyle          := mspRect;
 FRRRadius       := 10;
 FBorderColor    := clNone;
 FBorderWidth    := 1;


 FCustomPen              := TMSCustomPen.Create;
 FCustomPen.Style        := cpsNull;
 FCustomPen.Color        := clWhite;
 FCustomPen.EndCap       := cepEndCap_Flat;
 FCustomPen.LinesLength  := 5;
 FCustomPen.LinesSpace   := 5;
 FCustomPen.PenWidth     := 1;
 FCustomPen.Margin       := 4;
 FCustomPen.Design       := ldSingle;
 FCustomPen.FOwner       := self;
 FCustomPen.OnChange     := @CustomPenCanged;

 FImage                  := TPicture.Create;

 BorderSpacing.OnChange:= @BorderChangingChange;
 //debugln('Create');
 CalculateSeperator;
end;

destructor TMultiSeperator.Destroy;
begin

 FCustomPen.Free;
 FImage.Free;
 inherited Destroy;
end;
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TMultiSeperator.BorderChangingChange(Sender: TObject);
begin
 ////debugln('BorderChangingChange');
 CalculateSeperator;
 invalidate;
end;

procedure TMultiSeperator.CustomPenCanged;
begin
 Invalidate;
end;

procedure TMultiSeperator.BoundsChanged;
begin
  inherited BoundsChanged; //debugln('BoundsChanged');
  CalculateSeperator;
  invalidate;
end;

procedure TMultiSeperator.Loaded;
begin
  inherited Loaded; //debugln('Loaded');
  FLoaded := true;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiSeperator.SetColorEnd(AValue: TColor);
begin
  if not (csLoading in ComponentState) then
  if FColorStart = clNone then showmessage(rs_clNoneColorStart);
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  if aValue = clNone then FBackgrdVisible := false;
  if (aValue <> clNone) and (FColorStart <> clNone) then FBackgrdVisible := true;
  invalidate;
end;

procedure TMultiSeperator.SetColorStart(AValue: TColor);
begin
  if not (csLoading in ComponentState) then
  if FColorEnd = clNone then showmessage(rs_clNoneColorEnd);
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  if aValue = clNone then FBackgrdVisible := false;
  if (aValue <> clNone) and (FColorEnd <> clNone) then FBackgrdVisible := true;
  invalidate;
end;

procedure TMultiSeperator.SetCustomPen(AValue: TMSCustomPen);
begin
  if FCustomPen=AValue then Exit;
  FCustomPen:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetPicture(AValue: TPicture);
begin
  if FImage=AValue then Exit;
  FImage:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetOrientation(AValue: TOrientation);
var w,h : integer;
begin
  if FOrientation=AValue then Exit; //debugln('SetOrientation');
  FOrientation:=AValue;
  if (csLoading in ComponentState) and not FLoaded then exit;
  w := width;
  h := height;

  w  := w xor h;
  h  := w xor h;
  w  := w xor h;

  setBounds(left,top,w,h);
end;

procedure TMultiSeperator.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetStyle(AValue: TMSeperatorStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  invalidate;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Calculat---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiSeperator.CalculateSeperator;
const XSpacer : integer = 0;
      YSpacer : integer = 0;
begin
 ////debugln('CalculateSeperator');
 if Orientation = mspVertical then YSpacer := BorderSpacing.InnerBorder
 else XSpacer := BorderSpacing.InnerBorder;

 FSeperatorBounds.Left   := XSpacer;
 FSeperatorBounds.Top    := YSpacer;
 FSeperatorBounds.right  := width - XSpacer;
 FSeperatorBounds.bottom := Height - YSpacer;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiSeperator.DrawTheBackground;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(Width,Height);

 if (FGradient <> gcBitmap) then
  Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient))
 else
  begin
   bkBmp.Assign(FImage.Bitmap);
   Gradient_Bmp(bkBmp,width,height);
  end;

 trBmp := TBitmap.Create;
 trBmp.SetSize(Width,Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,Width,Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mspRoundRect : trBmp.Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : trBmp.Canvas.Rectangle(0,0,width,height);
 end;


 mask := TBitmap.Create;
 mask.SetSize(Width,Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,Width,Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mspRoundRect : mask.Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : mask.Canvas.Rectangle(0,0,width,height);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(Width,Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(0,0,Dest);

 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;
end;

procedure TMultiSeperator.DrawSeperator;
var OldPen : HPEN;
begin
 OldPen := SelectObject(canvas.Handle,FCustomPen.CreatePen);

 if FCustomPen.FDesign = ldSingle then DrawSingleLine;
 if FCustomPen.FDesign = ldDouble then DrawDoubleLine;

 DeleteObject(SelectObject(canvas.Handle, OldPen));

end;

procedure TMultiSeperator.DrawSingleLine;
begin
 if Orientation =mspVertical then
  canvas.Line(FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Top+FCustomPen.Margin,
              FSeperatorBounds.Left + FSeperatorBounds.width div 2,FSeperatorBounds.Bottom-FCustomPen.Margin)
 else
  canvas.Line(FSeperatorBounds.Left+FCustomPen.Margin,FSeperatorBounds.Top + FSeperatorBounds.Height div 2,
              FSeperatorBounds.Right-FCustomPen.Margin,FSeperatorBounds.Top +  FSeperatorBounds.Height div 2);
end;

procedure TMultiSeperator.DrawDoubleLine;
var i : integer;
begin
if Orientation =mspVertical then
 begin
  i := (width - FCustomPen.PenWidth) div 3;
  canvas.Line(FSeperatorBounds.Left + i,FSeperatorBounds.Top+FCustomPen.Margin,
              FSeperatorBounds.Left + i,FSeperatorBounds.Bottom-FCustomPen.Margin);
  canvas.Line(FSeperatorBounds.Right -i ,FSeperatorBounds.Top+FCustomPen.Margin,
              FSeperatorBounds.Right -i,FSeperatorBounds.Bottom-FCustomPen.Margin);
 end
else
 begin
  i := (height - FCustomPen.PenWidth) div 3;
  canvas.Line(FSeperatorBounds.Left+FCustomPen.Margin,FSeperatorBounds.Top + i,
              FSeperatorBounds.Right-FCustomPen.Margin,FSeperatorBounds.Top + i);
  canvas.Line(FSeperatorBounds.Left+FCustomPen.Margin,FSeperatorBounds.Bottom-i,
              FSeperatorBounds.Right-FCustomPen.Margin,FSeperatorBounds.Bottom-i);
 end;
end;

procedure TMultiSeperator.DrawABorder;
begin
 Canvas.Brush.Style := bsClear;
 Canvas.Pen.Color   := FBorderColor;
 Canvas.Pen.Width   := FBorderWidth;
 case FStyle of
  mspRoundRect : Canvas.RoundRect(0,0,width,height,FRRRadius,FRRRadius);
  mspRect      : Canvas.Rectangle(0,0,width,height);
 end;
end;

procedure TMultiSeperator.Paint;
begin
 inherited Paint;
 if FBackgrdVisible then DrawTheBackground;

 DrawSeperator;
 if FBorderColor <> clNone then DrawABorder;

 ////debugln('paint');
end;

end.
