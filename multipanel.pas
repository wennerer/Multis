{ <A panel for the multi components>
  <Version 1.0.0.0>
  Copyright (C) <07.03.2022> <Bernd Hübner>
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

unit MultiPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  infmultis, LMessages, LCLIntf, LCLType, LCLProc, dbugintf;

type
  TClickEvent = procedure(Sender: TObject) of object;
type
  TMouseMoveEvent = procedure(Sender: TObject;Shift: TShiftState;
                              X,Y: Integer) of Object;
type
  TMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: Integer) of Object;
type
  TMouseEnterLeave = procedure(Sender: TObject) of object;
type
  TNotifyEvent = procedure(Sender: TObject) of object;
type
  TKeyEvent = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
type
  TKeyPressEvent = procedure(Sender: TObject; var Key: char) of Object;

type
  TMPanelStyle = (mpsRect,mpsRoundRect,mpsEllipse);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type
  TTrigger = (trClick,trHover);

type
  TDirection = (LeftTop_RightBottom,RightTop_LeftBottom,LeftBottom_RightTop,RightBottom_LeftTop);

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
    //makes the selection the starting value
    //macht die Auswahl zum Startwert
    property Active   : boolean read FActive write SetActive default true;
    //the width of the compressed panel
    //die Breite des komprimierten Panels
    property Width    : integer read FWidth write SetWidth default 50;
    //the height of the compressed panel
    //die Höhe des komprimierten Panels
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
    //makes the selection the starting value
    //macht die Auswahl zum Startwert
    property Active : boolean read FActive write SetActive default false;
    //the width of the stretched panel
    //die Breite des gedehnten Panels
    property Width  : integer read FWidth write SetWidth default 110;
    //the height of the stretched panel
    //die Höhe des gedehnten Panels
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
    FSpeed: integer;
    FStep: integer;
    FStretched     : TStre;
    FTrigger       : TTrigger;

   procedure SetActive(AValue: boolean);
   procedure SetDirection(AValue: TDirection);
   procedure SetSpeed(AValue: integer);
   procedure SetStep(AValue: integer);
   procedure SetTrigger(AValue: TTrigger);

  protected

  public
   constructor create(aOwner: TCustomPanel);
  published
   //activates the dropdown function
   //Aktiviert die DropDown-Funktion
   property Active : boolean read FActive write SetActive default false;
   //properties of the compressed panel
   //Eigenschaften des komprimierten Panels
   property Compressed : TComp read FCompressed write FCompressed;
   //properties of the streched Panel
   //Eigenschaften des gedehnten Panels
   property Stretched  : TStre read FStretched write FStretched;
   //Trigger
   //Auslöser
   property Trigger  : TTrigger read FTrigger write SetTrigger default trHover;
   //the fold-out direction
   //die Ausklapprichtung
   property Direction : TDirection read FDirection write SetDirection default LeftTop_RightBottom;
   //the drawing speed (timer intervall)
   //die Zeichengeschwindigkeit (Timer Intervall)
   property Speed : integer read FSpeed write SetSpeed default 3; //Timer for dropdownmenu
   //the drawing steps (pixels)
   //der Zeichenschritt(Pixel)
   property Step : integer read FStep write SetStep default 2; //for Dropdownmenu
 end;

type

 { TBorder }

 TBorder = class(TPersistent)
   private
     FBetween     : integer;
     FInnerColor  : TColor;
     FInnerWidth  : integer;
     FOuterWidth  : integer;
     FOwner       : TCustomPanel;
     FOuterColor  : TColor;

     procedure SetBetween(AValue: integer);
     procedure SetInnerColor(AValue: TColor);
     procedure SetInnerWidth(AValue: integer);
     procedure SetOuterColor(AValue: TColor);
     procedure SetOuterWidth(AValue: integer);

   protected

   public
    constructor create(aOwner: TCustomPanel);
   published
    //the color of the outerborder
    //die Farbe des äußeren Randes
    property OuterColor : TColor read FOuterColor write SetOuterColor default clNone;
    //the width of the outerborder
    //die dicke des äußeren Randes
    property OuterWidth : integer read FOuterWidth write SetOuterWidth default 1;
    //the color of the innerborder
    //die Farbe des inneren Randes
    property InnerColor : TColor read FInnerColor write SetInnerColor default clNone;
    //the width of the innerborder
    //die dicke des inneren Randes
    property InnerWidth : integer read FInnerWidth write SetInnerWidth default 1;
    //the space between inner- and outerborder
    //der Raum zwischen Innen- und Außenrand
    property Between : integer read FBetween write SetBetween default 1;
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
    FOnClick: TClickEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TMouseEnterLeave;
    FOnMouseLeave: TMouseEnterLeave;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FRRRadius        : integer;
    FStyle           : TMPanelStyle;
    FChangeable      : boolean;  //flag for dropdownmenu
    FSwitch          : boolean;  //flag for dropdownmenu in designtime
    FTriggerNot      : boolean;
    FTimer           : TTimer;
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
    procedure RightBottomToLeftTop;

  protected
    procedure DrawThePanel;
    procedure DrawABorder;
    procedure BoundsChanged;override;
    procedure Loaded; override;
    procedure KeyPress(var Key: char);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
    procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
    procedure DoExit;  override;
    procedure DoEnter; override;
  public
   FMultiBkgrdBmp         : TBitmap;
   procedure ParentInputHandler({%H-}Sender: TObject; Msg: Cardinal);
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure Paint; override;
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
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 40;
   //the properties of the border
   //die Eigenschaften des Randes
   property BorderSettings : TBorder read FBorder write SetBorder;
   //the properties of the dropdownmenu
   //Die Eigenschaften der DropDownfunktion
   property DropDownMenu : TDDMenu read FDDMenu write FDDMenu;


   property Width  default 250;
   property Height default 150;

   property DragMode;
   property DragKind;
   property DragCursor;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;
   property Align;
   property Anchors;
   property Action;
   property BidiMode;
   property BorderSpacing;
   property Constraints;
   property HelpType;

   property OnClick : TClickEvent read FOnClick     write FOnClick;
   property OnMouseMove : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnEnter : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress :TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;

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
  Width          := 250;
  Height         := 150;
  FColorEnd      := clSilver;
  FColorStart    := clGray;
  FGradient      := gcSpread;
  FRRRadius      := 40;
  FStyle         := mpsRect;

  FBorder                := TBorder.create(self);
  FBorder.FOuterColor    := clNone;
  FBorder.FOuterWidth    := 1;
  FBorder.FInnerColor    := clNone;
  FBorder.FInnerWidth    := 1;
  FBorder.FBetween       := 1;

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
  FDDMenu.FStep              := 2;
  FDDMenu.FSpeed             := 3;

  FTimer                     := TTimer.Create(self);
  FTimer.Enabled             := false;
  FTimer.Interval            := FDDMenu.FSpeed;
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
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TMultiPanel.MouseLeave;
begin
  inherited MouseLeave;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
end;

procedure TMultiPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
end;

procedure TMultiPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
end;

procedure TMultiPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
  if Assigned(OnClick) then OnClick(self);
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

procedure TMultiPanel.KeyPress(var Key: char);
begin
 inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiPanel.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
   if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiPanel.DoExit;
begin
  inherited DoExit;
  if Assigned(OnExit) then OnExit(self);
end;

procedure TMultiPanel.DoEnter;
begin
  inherited DoEnter;
  if Assigned(OnEnter) then OnEnter(self);
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
  //FSwitch:= true;
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
    if FDDMenu.FDirection = RightBottom_LeftTop then
     begin
      FChangeable := false;
      left := FDDMenu.FCompressed.FLeft;
      FChangeable := false;
      top  := FDDMenu.FCompressed.FTop;
     end;
   end;//if compressed active
  if FDDMenu.FStretched.FActive then
   begin
    width := FDDMenu.FStretched.FWidth;
    height:= FDDMenu.FStretched.FHeight;
    if FDDMenu.FDirection = RightTop_LeftBottom then
     left := (FDDMenu.FCompressed.FLeft+FDDMenu.FCompressed.FWidth)-FDDMenu.FStretched.FWidth;
    if FDDMenu.FDirection = LeftBottom_RightTop then
     top := (FDDMenu.FCompressed.FTop+FDDMenu.FCompressed.FHeight)-FDDMenu.FStretched.FHeight;
    if FDDMenu.FDirection = RightBottom_LeftTop then
     begin
      left := (FDDMenu.FCompressed.FLeft+FDDMenu.FCompressed.FWidth)-FDDMenu.FStretched.FWidth;
      FChangeable:= false;
      top := (FDDMenu.FCompressed.FTop+FDDMenu.FCompressed.FHeight)-FDDMenu.FStretched.FHeight;
     end;
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
 if FDDMenu.FDirection = RightBottom_LeftTop then RightBottomToLeftTop;

end;

procedure TMultiPanel.LeftTopToRightBottom;
begin
  //that stretches
 if FDDMenu.FStretched.Active then
    begin
     if width < FDDMenu.FStretched.FWidth then width := width +FDDMenu.FStep;
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FDDMenu.FStep;
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
     if width > FDDMenu.FCompressed.FWidth then width := width - FDDMenu.FStep;
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FDDMenu.FStep;
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
     if Height < FDDMenu.FStretched.FHeight then Height := Height + FDDMenu.FStep;
     if width < FDDMenu.FStretched.FWidth then
      begin
       width := width + FDDMenu.FStep;
       Left  := Left - FDDMenu.FStep;
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
     if Height > FDDMenu.FCompressed.FHeight then Height := Height - FDDMenu.FStep;
     if width > FDDMenu.FCompressed.FWidth then
      begin
       width := width - FDDMenu.FStep;
       Left  := Left + FDDMenu.FStep;
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
     if width < FDDMenu.FStretched.FWidth then width := width +FDDMenu.FStep;
     if Height < FDDMenu.FStretched.FHeight then
      begin
       Height := Height + FDDMenu.FStep;
       Top    := Top - FDDMenu.FStep;
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
     if width > FDDMenu.FCompressed.FWidth then width := width - FDDMenu.FStep;
     if Height > FDDMenu.FCompressed.FHeight then
      begin
       Height := Height - FDDMenu.FStep;
       Top    := Top + FDDMenu.FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
end;


procedure TMultiPanel.RightBottomToLeftTop;
begin
  //that stretches
 if FDDMenu.FStretched.FActive then
    begin
     if Height < FDDMenu.FStretched.FHeight then
      begin
       Height := Height + FDDMenu.FStep;
       Top    := Top - FDDMenu.FStep;
      end;
     if width < FDDMenu.FStretched.FWidth then
      begin
       width := width + FDDMenu.FStep;
       Left  := Left - FDDMenu.FStep;
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
     if Height > FDDMenu.FCompressed.FHeight then
      begin
       Height := Height - FDDMenu.FStep;
       Top    := Top + FDDMenu.FStep;
      end;
     if width > FDDMenu.FCompressed.FWidth then
      begin
       width := width - FDDMenu.FStep;
       Left  := Left + FDDMenu.FStep;
      end;
     if (width <= FDDMenu.FCompressed.FWidth) and (Height <=FDDMenu.FCompressed.FHeight) then
      begin
       width := FDDMenu.FCompressed.FWidth;
       height:= FDDMenu.FCompressed.FHeight;
       FTimer.Enabled:= false;
      end;
    end;
end;

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
   i := FBorder.FBetween;
   case FStyle of
    mpsRoundRect : Canvas.RoundRect(0+i,0+i,Width-i,height-i,FRRRadius- round(i*1.5),FRRRadius- round(i*1.5));
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
