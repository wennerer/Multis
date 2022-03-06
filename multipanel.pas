{ <A panel for the multi components>
  <Version 1.0.0.0>
  Copyright (C) <23.01.2022> <Bernd Hübner>
  For some improvements see https://www.lazarusforum.de/viewtopic.php?f=29&t=14033

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

//doto's:
//radius bei border

unit MultiPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  infmultis, LMessages, LCLIntf, LCLType, LCLProc, dbugintf;

type
  TMPanelStyle = (mpsRect,mpsRoundRect,mpsEllipse);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TTrigger = (trClick,trHover);

type
  TDirection = (LeftTop_RightBottom,RightTop_LeftBottom,LeftBottom_RightTop);

type

  { TComp }    //compressed

  TComp = class(TPersistent)
   private
     FActive        : boolean;
     FLeft          : integer;
     FOwner         : TCustomPanel;
     FHeight        : integer;
     FTop           : integer;
     FWidth         : integer;
     procedure SetActive(AValue: boolean);
     procedure SetHeight(AValue: integer);
     procedure SetWidth(AValue: integer);
   public
    constructor create(aOwner: TCustomPanel);

   published
    property Active   : boolean read FActive write SetActive default true;
    property Width    : integer read FWidth write SetWidth default 50;
    property Height   : integer read FHeight write SetHeight default 50;


  end;

  { TStre }      //enlarged, streched

  TStre = class(TPersistent)
  private
    FLeft       : integer;
    FTop        : integer;
    FActive        : boolean;
    FOwner         : TCustomPanel;
    FHeight        : integer;
    FWidth         : integer;
    procedure SetActive(AValue: boolean);
    procedure SetHeight(AValue: integer);
    procedure SetWidth(AValue: integer);

   public
    constructor create(aOwner: TCustomPanel);

   published
    property Active : boolean read FActive write SetActive default false;
    property Width  : integer read FWidth write SetWidth default 110;
    property Height : integer read FHeight write SetHeight default 150;

  end;


type

 { TDDMenu }   //as DropDown

 TDDMenu = class(TPersistent)
  private
    FActive        : boolean;
    FCompressed    : TComp;
    FDirection: TDirection;
    FOwner         : TCustomPanel;
    FStretched     : TStre;
    FTrigger       : TTrigger;
   procedure SetActive(AValue: boolean);
   procedure SetDirection(AValue: TDirection);
   procedure SetTrigger(AValue: TTrigger);

  protected

  public
   constructor create(aOwner: TCustomPanel);
  published

   property Active : boolean read FActive write SetActive default false;

   property Compressed : TComp read FCompressed write FCompressed;

   property Stretched  : TStre read FStretched write FStretched;

   property Trigger  : TTrigger read FTrigger write SetTrigger default trHover;

   property Direction : TDirection read FDirection write SetDirection default LeftTop_RightBottom;
 end;

type

 { TBorder }

 TBorder = class(TPersistent)
   private
     FInnerColor: TColor;
     FInnerWidth: integer;
     FOuterWidth: integer;
     FOwner                : TCustomPanel;
     FOuterColor           : TColor;
     procedure SetInnerColor(AValue: TColor);
     procedure SetInnerWidth(AValue: integer);
     procedure SetOuterColor(AValue: TColor);
     procedure SetOuterWidth(AValue: integer);

   protected

   public
    constructor create(aOwner: TCustomPanel);
   published

    property OuterColor : TColor read FOuterColor write SetOuterColor default clNone;

    property OuterWidth : integer read FOuterWidth write SetOuterWidth default 1;

    property InnerColor : TColor read FInnerColor write SetInnerColor default clNone;

    property InnerWidth : integer read FInnerWidth write SetInnerWidth default 1;
 end;


type

  { TMultiPanel }

  TMultiPanel = class(TCustomPanel)
  private
    FBorder: TBorder;
    FColorEnd        : TColor;
    FColorStart      : TColor;
    FDDMenu          : TDDMenu;
    FGradient        : TGradientCourse;
    FRRRadius        : integer;
    FStyle           : TMPanelStyle;
    FChangeable      : boolean;  //flag for dropdownmenu
    FSwitch          : boolean;  //flag for dropdownmenu in designtime
    FTriggerNot      : boolean;
    FTimer           : TTimer;
    FSpeed           : integer; //Timer for dropdownmenu
    FStep            : integer; //for Dropdownmenu
    FRunThroughPaint : boolean;



    procedure MultiBkgrdBmp;
    procedure SetBorder(AValue: TBorder);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDropDownMenu(Sender: TPersistent; aValue: boolean);
    procedure SetSizeDropDownMenu(Sender:TPersistent);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMPanelStyle);
    procedure MultiPanelOnTimer({%H-}Sender : TObject);
    procedure LeftTopToRightBottom;
    procedure RightTopToLeftBottom;
    procedure LeftBottomToRightTop;
    //procedure BottomToTop;

  protected
    procedure DrawThePanel;
    procedure DrawABorder;
    procedure BoundsChanged;override;
    procedure Loaded; override;
  public
   FMultiBkgrdBmp         : TBitmap;
   procedure   ParentInputHandler({%H-}Sender: TObject; Msg: Cardinal);
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   MouseEnter; override;
   procedure   MouseLeave; override;
   procedure   Paint; override;
  published
   //The geometric shape of the panel
   //Die geometrische Form des Panels
   property Style      : TMPanelStyle read FStyle write SetStyle default mpsRect;
   //The start color of the panel ( for color gradient)
   //Die Startfarbe des Panels (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the panel ( for color gradient)
   //Die Endfarbe des Panels (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;

   property BorderSettings : TBorder read FBorder write SetBorder;

   property DropDownMenu : TDDMenu read FDDMenu write FDDMenu;


   property Width  default 400;
   property Height default 200;
   //property Color;
   //property ParentColor default true;


  end;

procedure Register;

implementation
uses multibutton, multiplexslider;

procedure Register;
begin
  {$I multipanel_icon.lrs}
  RegisterComponents('Multi',[TMultiPanel]);
end;


{ TMultiPanel }

constructor TMultiPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width          := 400;
  Height         := 200;
  FColorEnd      := clSilver;
  FColorStart    := clGray;
  FGradient      := gcSpread;
  FRRRadius      := 10;
  FStyle         := mpsRect;

  FBorder                := TBorder.create(self);
  FBorder.FOuterColor    := clNone;
  FBorder.FOuterWidth    := 1;
  FBorder.FInnerColor    := clNone;
  FBorder.FInnerWidth    := 1;

  FDDMenu                    := TDDMenu.create(self);
  FDDMenu.FActive            := false;
  FDDMenu.FTrigger           := trHover;
  FDDMenu.FDirection         := LeftTop_RightBottom;

  FDDMenu.FCompressed        := TComp.create(self);
  FDDMenu.FCompressed.FWidth :=  50;
  FDDMenu.FCompressed.FHeight:=  50;
  FDDMenu.FCompressed.FLeft  := Left;
  FDDMenu.FCompressed.FTop   := Top;
  FDDMenu.FCompressed.FActive:= true;
  FDDMenu.FCompressed.FLeft  := left;

  FDDMenu.FStretched         := TStre.create(self);
  FDDMenu.FStretched.FWidth  := 110;
  FDDMenu.FStretched.FHeight := 150;
  FDDMenu.FStretched.FLeft   := Left;
  FDDMenu.FStretched.FTop    := Top;
  FDDMenu.FStretched.FActive := false;

  FStep                      := 1;

  FSpeed                     := 5;
  FTimer                     := TTimer.Create(self);
  FTimer.Enabled             := false;
  FTimer.Interval            := FSpeed;
  FTimer.OnTimer             := @MultiPanelOnTimer;


  FMultiBkgrdBmp := TBitmap.Create;
  Application.AddOnUserInputHandler(@ParentInputHandler);
  FChangeable := true;
  FTriggerNot := false;
end;

destructor TMultiPanel.Destroy;
begin
  Application.RemoveOnUserInputHandler(@ParentInputHandler);
  FMultiBkgrdBmp.Free;
  FBorder.Free;
  FDDMenu.FCompressed.Free;
  FDDMenu.FStretched.Free;
  FDDMenu.Free;
  inherited Destroy;
end;

procedure TMultiPanel.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMultiPanel.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TMultiPanel.ParentInputHandler(Sender: TObject; Msg: Cardinal);
var x,y,h : integer;
    HotspotCompressed : TRect;
    HotspotStretched  : TRect;
    P : TPoint;
begin
 if not FDDMenu.FActive then exit;

 if not (csDesigning in ComponentState) then
  begin
   x := Mouse.CursorPos.X - parent.Left - left;
   h := GetSystemMetrics(SM_CYCAPTION);//high of the menu
   y := Mouse.CursorPos.Y - parent.Top - h - top;
   HotspotCompressed := Rect(0,0,FDDMenu.FCompressed.FWidth,FDDMenu.FCompressed.FHeight);
   HotspotStretched  := Rect(0,0,FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);
   P := Point(x,y);

   if FDDMenu.FTrigger = trClick then
    begin
     if ptinrect(HotspotCompressed,P) and (msg = LM_LBUTTONDOWN) and (FDDMenu.FStretched.FActive = false) then
      begin
       DropDownMenu.Stretched.Active:= true;
       exit;
      end;
     if ptinrect(HotspotStretched,P)  and (msg = LM_LBUTTONDOWN) then DropDownMenu.Compressed.Active:= true;
    end;

   if FDDMenu.FTrigger = trHover then
    begin
     //if ptinrect(HotspotCompressed,P) and (msg = LM_LBUTTONUp) then FTriggerNot := true;

     if not ptinrect(HotspotCompressed,P) then FTriggerNot := false;
     if (msg = LM_LBUTTONUp) then FTriggerNot := true;

     //strech
     if ptinrect(HotspotCompressed,P) and (DropDownMenu.Stretched.Active = false) then
      begin
       if FTriggerNot then exit;
       DropDownMenu.Stretched.Active:= true;
       exit;
      end;

      //compress
     if ptinrect(HotspotStretched,P)  and (msg = LM_LBUTTONDOWN) then
      DropDownMenu.Compressed.Active:= true;
     if not ptinrect(HotspotStretched,P) then
      DropDownMenu.Compressed.Active:= true;
    end;
  end;

end;


procedure TMultiPanel.BoundsChanged;
begin
  inherited BoundsChanged;
  if not assigned(FDDMenu) then exit;
  if not FDDMenu.FActive then exit;
  if not FChangeable then
   begin
    FChangeable := true;
    exit;
   end;
  //this is only for designtime
  if FDDMenu.FCompressed.FActive and not FSwitch then
   begin
    FDDMenu.FCompressed.FLeft:= left;
    FDDMenu.FCompressed.FTop := top;
   end;
  FSwitch := false;

  SetSizeDropDownMenu(self);

end;

procedure TMultiPanel.Loaded;
begin
 inherited Loaded;
 if not FRunThroughPaint then MultiBkgrdBmp;
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiPanel.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetBorder(AValue: TBorder);
begin
  if FBorder=AValue then Exit;
  FBorder:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetDropDownMenu(Sender:TPersistent;aValue:boolean);
begin
 if (Sender is TStre) then
  begin
   if aValue then
    FDDMenu.FCompressed.FActive := false
   else
    FDDMenu.FCompressed.FActive := true;
  end;//TStre

 if (Sender is TComp) then
  begin
   if aValue then
    FDDMenu.FStretched.FActive:= false
   else
    FDDMenu.FStretched.FActive:= true;
  end;//TComp

 if not (csDesigning in ComponentState) then
  begin
   FTimer.Enabled:= true;
   exit;
  end;

  if FDDMenu.FCompressed.FActive then FSwitch:= true;
  SetSizeDropDownMenu(Sender);

end;

procedure TMultiPanel.SetSizeDropDownMenu(Sender: TPersistent);  //only at design time
begin
  if not FDDMenu.FActive then exit;
  if not (csDesigning in ComponentState) then exit;

  if (Sender is TDDMenu) then  //is required when dropdownmenu.active is set
   begin
    if FDDMenu.FCompressed.FActive then Sender := FDDMenu.FCompressed;
    if FDDMenu.FStretched.FActive  then Sender := FDDMenu.FStretched;
   end;



  if (Sender is TStre) or (Sender is TComp) then  //set size with OI
  begin
   FChangeable := false;
   if FDDMenu.FCompressed.FActive then
   begin
    width := FDDMenu.FCompressed.FWidth;
    height:= FDDMenu.FCompressed.FHeight;
    if FDDMenu.FDirection = RightTop_LeftBottom then left := FDDMenu.FCompressed.FLeft;
    if FDDMenu.FDirection = LeftBottom_RightTop then top  := FDDMenu.FCompressed.FTop;
   end;//if compressed active
  if FDDMenu.FStretched.FActive then
   begin
    width := FDDMenu.FStretched.FWidth;
    height:= FDDMenu.FStretched.FHeight;
    if FDDMenu.FDirection = RightTop_LeftBottom then
     left := (FDDMenu.FCompressed.FLeft+FDDMenu.FCompressed.FWidth)-FDDMenu.FStretched.FWidth;
    if FDDMenu.FDirection = LeftBottom_RightTop then
     top := (FDDMenu.FCompressed.FTop+FDDMenu.FCompressed.FHeight)-FDDMenu.FStretched.FHeight;
   end; //if streched active
   exit;
  end;

  if FDDMenu.FCompressed.FActive then    //set Size with drag
   begin
    FDDMenu.FCompressed.FWidth  := width;
    FDDMenu.FCompressed.FHeight := height;
   end;//if compressed active
  if FDDMenu.FStretched.FActive then
   begin
    FDDMenu.FStretched.FWidth  := width;
    FDDMenu.FStretched.FHeight := height;
   end; //if streched active
end;

procedure TMultiPanel.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  invalidate;
end;

procedure TMultiPanel.SetStyle(AValue: TMPanelStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  invalidate;
end;

procedure TMultiPanel.MultiPanelOnTimer(Sender: TObject);
begin
 if FDDMenu.FDirection = LeftTop_RightBottom then LeftTopToRightBottom;
 if FDDMenu.FDirection = RightTop_LeftBottom then RightTopToLeftBottom;
 if FDDMenu.FDirection = LeftBottom_RightTop then LeftBottomToRightTop;
 //if FDDMenu.FDirection = BottomTop then BottomToTop;

end;

procedure TMultiPanel.LeftTopToRightBottom;
begin
  //that stretches
 if FDDMenu.FStretched.Active then
    begin
     if width < FDDMenu.FStretched.FWidth then width := width +FStep;
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FStep;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if width > FDDMenu.FCompressed.FWidth then width := width - FStep;
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FStep;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
      end;
    end;

end;

procedure TMultiPanel.RightTopToLeftBottom;
begin
  //that stretches
 if FDDMenu.FStretched.FActive then
    begin
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FStep;
     if width < FDDMenu.FStretched.FWidth then
      begin
       width := width + FStep;
       Left  := Left - FStep;
      end;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FStep;
     if width > FDDMenu.FCompressed.FWidth then
      begin
       width := width - FStep;
       Left  := Left + FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
end;


procedure TMultiPanel.LeftBottomToRightTop;
begin
 if FDDMenu.FStretched.Active then
    begin
     if width < FDDMenu.FStretched.FWidth then width := width +FStep;
     if Height < FDDMenu.FStretched.FHeight then
      begin
       Height := Height + FStep;
       Top    := Top - FStep;
      end;
     if (width >= FDDMenu.FStretched.FWidth) and (Height >=FDDMenu.FStretched.FHeight) then
      begin
       width := FDDMenu.FStretched.FWidth;
       height:= FDDMenu.FStretched.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
  //that pulls together
 if FDDMenu.FCompressed.Active then
    begin
     if width > FDDMenu.FCompressed.FWidth then width := width - FStep;
     if Height > FDDMenu.FCompressed.FHeight then
      begin
       Height := Height - FStep;
       Top    := Top + FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
end;

(*
procedure TMultiPanel.BottomToTop;
begin
  //that stretches
 if DropDownMenu.FStretched.Active then
    begin
     if Height < DropDownMenu.FStretched.Height then
      begin
       Height := Height + FCount;
       Top := Top - FCount;
      end;
     if (Height >=DropDownMenu.FStretched.Height) then
      begin
       height:= DropDownMenu.FStretched.Height;
       FTimer.Enabled:= false;
      end;
    end;
  //that pulls together
 if DropDownMenu.FCompressed.Active then
    begin
     if Height > DropDownMenu.FCompressed.Height then
      begin
       Height := Height - FCount;
       Top := Top + FCount;
      end;
     if (Height <=DropDownMenu.FCompressed.Height) then
      begin
       height:= DropDownMenu.FCompressed.Height;
       FTimer.Enabled:= false;
      end;
    end;
end;
 *)
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiPanel.DrawThePanel;
var   bkBmp        : TBitmap;
      trBmp        : TBitmap;
      mask         : TBitmap;
      Dest         : TBitmap;

begin

   bkBmp := TBitmap.Create;
   bkBmp.SetSize(Width,Height);
   FMultiBkgrdBmp.SetSize(width,height);

   if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clGray,clSilver,ord(gcVertical)); //otherwise flickers

   Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));


   trBmp := TBitmap.Create;
   trBmp.SetSize(Width,Height);
   trBmp.TransparentColor:=clblack;
   trBmp.Transparent:= true;
   trBmp.Canvas.Brush.Color:=clwhite;
   trBmp.Canvas.FillRect(0,0,Width,Height);
   trBmp.Canvas.Brush.Color:=clBlack;
   case FStyle of
    mpsRoundRect : trBmp.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : trBmp.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : trBmp.Canvas.Ellipse(0,0,Width,height);
   end;

   mask := TBitmap.Create;
   mask.SetSize(Width,Height);
   mask.Canvas.Brush.Color:=clwhite;
   mask.Canvas.FillRect(0,0,Width,Height);
   mask.Canvas.Brush.Color:=clBlack;
   case FStyle of
    mpsRoundRect : mask.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : mask.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : mask.Canvas.Ellipse(0,0,Width,height);
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
   FMultiBkgrdBmp.Canvas.Draw(0,0,Dest);

   bkBmp.Free;
   trBmp.Free;
   mask.Free;
   Dest.Free;
end;

procedure TMultiPanel.MultiBkgrdBmp;
var   bkBmp        : TBitmap;
      trBmp        : TBitmap;
      mask         : TBitmap;
      Dest         : TBitmap;

begin

   bkBmp := TBitmap.Create;
   bkBmp.SetSize(FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);
   FMultiBkgrdBmp.SetSize(FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);

   if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clGray,clSilver,ord(gcVertical)); //otherwise flickers

   Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));


   trBmp := TBitmap.Create;
   trBmp.SetSize(FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);
   trBmp.TransparentColor:=clblack;
   trBmp.Transparent:= true;
   trBmp.Canvas.Brush.Color:=clwhite;
   trBmp.Canvas.FillRect(0,0,Width,Height);
   trBmp.Canvas.Brush.Color:=clBlack;
   case FStyle of
    mpsRoundRect : trBmp.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : trBmp.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : trBmp.Canvas.Ellipse(0,0,Width,height);
   end;

   mask := TBitmap.Create;
   mask.SetSize(FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);
   mask.Canvas.Brush.Color:=clwhite;
   mask.Canvas.FillRect(0,0,Width,Height);
   mask.Canvas.Brush.Color:=clBlack;
   case FStyle of
    mpsRoundRect : mask.Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : mask.Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : mask.Canvas.Ellipse(0,0,Width,height);
   end;

   Dest       := TBitmap.Create;
   Dest.SetSize(FDDMenu.FStretched.FWidth,FDDMenu.FStretched.FHeight);
   Dest.Transparent:= true;
   Dest.TransparentColor:= clBlack;
   Dest.Canvas.Brush.Color:=clBlack;
   Dest.Canvas.FillRect(0,0,100,100);
   Dest.Canvas.copymode:=cmSrcCopy;
   Dest.Canvas.Draw(0,0,bkBmp);
   Dest.Canvas.Draw(0,0,trBmp);
   Dest.Canvas.copymode:=cmSrcInvert;
   Dest.Canvas.Draw(0,0,mask);

   FMultiBkgrdBmp.Canvas.Draw(0,0,Dest);

   bkBmp.Free;
   trBmp.Free;
   mask.Free;
   Dest.Free;
end;

procedure TMultiPanel.DrawABorder;
var i : integer;
begin
 Canvas.Brush.Style := bsClear;
 if FBorder.FOuterColor <> clNone then
  begin
   Canvas.Pen.Color   := FBorder.FOuterColor;
   Canvas.Pen.Width   := FBorder.FOuterWidth;
   case FStyle of
    mpsRoundRect : Canvas.RoundRect(0,0,Width,height,FRRRadius,FRRRadius);
    mpsRect      : Canvas.Rectangle(0,0,Width,height);
    mpsEllipse   : Canvas.Ellipse(0,0,Width,height);
   end;
  end;
 if FBorder.FInnerColor <> clNone then
  begin
   Canvas.Pen.Color   := FBorder.FInnerColor;
   Canvas.Pen.Width   := FBorder.FInnerWidth;
   i := FBorder.FOuterWidth + (FBorder.FInnerWidth div 2);
   case FStyle of
    mpsRoundRect : Canvas.RoundRect(0+i,0+i,Width-i,height-i,FRRRadius,FRRRadius);
    mpsRect      : Canvas.Rectangle(0+i,0+i,Width-i,height-i);
    mpsEllipse   : Canvas.Ellipse(0+i,0+i,Width-i,height-i);
   end;
  end;
end;

procedure TMultiPanel.Paint;
var lv : integer;
begin
  if parent.Color = clDefault then color:=clForm else ParentColor:=true;
  //inherited Paint;
  DrawThePanel;
  DrawABorder;


  FRunThroughPaint := true;
  //update all child windows
   for lv := 0 to pred(ControlCount) do
     begin
      if Controls[lv] is TMultiButton then (Controls[lv] as TMultiButton).Invalidate;
      if Controls[lv] is TMultiplexSlider then (Controls[lv] as TMultiplexSlider).Invalidate;
     end;
end;

{$Include mp_dropdownmenu.inc}
{$Include mp_border.inc}
end.
