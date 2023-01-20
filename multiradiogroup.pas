{ <A RadioGroup in the multi design>
  <Version 1.0.0.2>
  Copyright (C) <21.01.2023> <Bernd Hübner>

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



unit MultiRadioGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Contnrs, LResources, Forms, Controls, Graphics,
  Dialogs, infmultis, LCLProc, LCLIntf, LMessages, LCLType, ImgList,
  GraphPropEdits, PropEdits, LCLVersion;

type
  TChangeEvent = procedure(const aIndex: integer) of object;

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
  TMRadioStyle = (mssRect,mssRoundRect);

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color


type
  TMultiRadioGroup = class; //TCustomControl
  TMRadioButton     = class; //TCollectionItem

type

  { TMRadioButtons }

  TMRadioButtons = class(TCollection)
  private
   FMultiRadioGroup : TMultiRadioGroup;
   function GetRadioButton(Index: Integer): TMRadioButton;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetRadioButton(Index: Integer; AValue: TMRadioButton);
  protected
   function GetOwner: TPersistent; override;
  public
   constructor Create(aCollection: TMultiRadioGroup; aItemClass: TCollectionItemClass);
   procedure FontIsChanged(aHeight : integer);
   procedure SetAllNotSelected(aIndex : integer);
   procedure TriggerAutoSize;
   property Items[Index: Integer]: TMRadioButton read GetRadioButton write SetRadioButton; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;


 type
  TMRadioButton = class(TCollectionItem)
   private
     FButtonColor          : TColor;
     FButtonSelColor       : TColor;
     FDisabledAlpBV        : integer;
     FDisabledColor        : TColor;
     FEnabled              : boolean;
     FHoverColor           : TColor;
     FImageIndex           : TImageIndex;
     FImageLeft            : integer;
     FImageList            : TCustomImageList;
     FImageListChangeLink  : TChangeLink;
     FImageTop             : integer;
     FImageWidth           : integer;
     FRadioButtons         : TCollection;
     FCaptionChange        : boolean;
     FCaptionWordbreak     : boolean;
     FCapLeft              : integer;
     FCaption              : TCaption;
     FCapTop               : integer;
     FColor                : TColor;
     FFont                 : TFont;
     FHeight               : integer;
     FParentFont           : boolean;
     FSelected             : Boolean;
     FTextStyle            : TTextStyle;
     FVisible              : Boolean;
     FWidth                : integer;
     procedure SetButtonColor(AValue: TColor);
     procedure SetButtonSelColor(AValue: TColor);
     procedure SetCapLeft(AValue: integer);
     procedure SetAlignment(AValue: TAlignment);
     procedure SetCaption(AValue: TCaption);
     procedure SetCaptionWordbreak(AValue: boolean);
     procedure SetCapTop(AValue: integer);
     procedure SetColor(AValue: TColor);
     procedure SetDisabledAlpBV(AValue: integer);
     procedure SetDisabledColor(AValue: TColor);
     procedure SetEnabled(AValue: boolean);
     procedure SetFont(AValue: TFont);
     procedure SetHoverColor(AValue: TColor);
     procedure ImagesChanged({%H-}Sender: TObject);
     procedure SetImageIndex(AValue: TImageIndex);
     procedure SetImageLeft(AValue: integer);
     procedure SetImageList(AValue: TCustomImageList);
     procedure SetImageTop(AValue: integer);
     procedure SetImageWidth(AValue: integer);
     procedure SetLayout(AValue: TTextLayout);
     procedure SetParentFont(AValue: boolean);
     procedure SetSelected(AValue: Boolean);
     procedure SetTextStyle(AValue: TTextStyle);
     procedure SetVisible(AValue: Boolean);
     function GetDisplayName: string; override;
     procedure SetDisplayName(const Value: string); override;
   protected
     function GetOwner: TPersistent; override;
     procedure RadioButtonFontChanged({%H-}Sender : TObject);
     procedure SetAllNotSelected(aIndex : integer);
   public
    FHotspot: TRect;
    FHover  : boolean;
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
    property Visible  : Boolean read FVisible write SetVisible default true;
    property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
    property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;
    property DisplayName : string read GetDisplayName write SetDisplayName;
   published
    //The text that the user writes in the radiobutton
    //Der Text den der Benutzer in den Radiobutton schreibt
    property Caption  : TCaption read FCaption write SetCaption;
    //The background colour of the RadioButton
    //Die Hintergrundfarbe des RadioButtons
    property Color    : TColor read FColor write SetColor default clNone;
    //The color of the radiobutton
    //Die Farbe des Radiobuttons
    property ButtonColor : TColor read FButtonColor write SetButtonColor default clWhite;
    //The color of the selected radiobutton
    //Die Farbe des selektierten Radiobuttons
    property ButtonSelColor : TColor read FButtonSelColor write SetButtonSelColor default clBlack;
    //Determines if a radio button is selected
    //Bestimmt ob ein Radiobutton ausgewählt ist
    property Selected : Boolean read FSelected write SetSelected;
    //The color of a hoverevent
    //Die Farbe eines Hoverereignisses
    property HoverColor : TColor read FHoverColor write SetHoverColor default clSilver;
    //The font to be used for text display the caption.
    //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
    property Font: TFont read FFont write SetFont;
    // Uses the font from the Parent when enabled
    // Verwendet die Schriftart aus dem Parent, wenn aktiviert
    property ParentFont : boolean read FParentFont write SetParentFont default true;
    //Alignment of the text in the caption (left, center, right)
    //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
    property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taLeftJustify;
    //Alignment of the text in the caption (top, center, bottom)
    //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
    property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
    //Allows a line break in the caption
    //Ermöglicht einen Zeilenumbruch in der Caption
    property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default false;
    //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
    //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
    property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 5;
    //The vertical distance of the text in the text rectangle (only effective with tlTop)
    //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
    property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
    //Determines whether the control reacts on mouse or keyboard input.
    //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
    property Enabled : boolean read FEnabled write SetEnabled default true;
    //A list for including images
    //Eine Liste zum Einfügen von Bildern
    property Images  :  TCustomImageList  read FImageList write SetImageList default nil;
    //The Index of a Image in a ImageList
    //Der Index eines Bildes in einer ImageList
    property ImageIndex : TImageIndex read FImageIndex write SetImageIndex default -1;
    //The unique width of all images in the list.
    //Die einmalige Breite aller Bilder in der Liste.
    property ImageWidth : integer read FImageWidth write SetImageWidth default 0;
    //The coordinate of the left edge of a Image
    //Die Koordinate der linken Ecke des Bildes
    property ImageLeft  : integer read FImageLeft write SetImageLeft default 0;
    //The coordinate of the top edge of a Image
    //Die Koordinate der oberen Ecke des Bildes
    property ImageTop   : integer read FImageTop write SetImageTop default 0;


   end;






type

  { TMultiRadioGroup }

  TMultiRadioGroup = class(TCustomControl)
  private
    FAligningImages: Boolean;
    FAutoSize: boolean;
    FCaption                : TCaption;
    FDisabledAlpBV          : integer;
    FDisabledColor          : TColor;
    FEnabled                : boolean;
    FOnChange               : TChangeEvent;
    FFont                   : TFont;
    FOnEnter                : TNotifyEvent;
    FOnExit                 : TNotifyEvent;
    FOnKeyDown              : TKeyEvent;
    FOnKeyPress             : TKeyPressEvent;
    FOnKeyUp                : TKeyEvent;
    FOnMouseDown            : TMouseEvent;
    FOnMouseEnter           : TMouseEnterLeave;
    FOnMouseLeave           : TMouseEnterLeave;
    FOnMouseUp              : TMouseEvent;
    FRadioButtons           : TMRadioButtons;
    FColorEnd               : TColor;
    FColorStart             : TColor;
    FFocusAlBlVal           : byte;
    FFocusColor             : TColor;
    FFocusedOn              : boolean;
    FFocusFrameWidth        : integer;
    FForegroundFocusOn      : boolean;
    FGradient               : TGradientCourse;
    FRRRadius               : integer;
    FStyle                  : TMRadioStyle;
    FRadioGroupBounds       : TRect;
    FOnMouseMove            : TMouseMoveEvent;

    function CreateRadioButtons: TMRadioButtons;
    function GetRadioButton: TMRadioButtons;
    function GetTextHeight(AText: String; AFont: TFont): Integer;
    function GetTextWidth(AText: String; AFont: TFont): Integer;
    function IsRadioButtonsStored: Boolean;
    procedure SetCaption(AValue: TCaption);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetDisabledAlpBV(AValue: integer);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetEnabled(AValue: boolean);reintroduce;
    procedure SetFocusAlBlVal(AValue: byte);
    procedure SetFocusColor(AValue: TColor);
    procedure SetFocusedOn(AValue: boolean);
    procedure SetFocusFrameWidth(AValue: integer);
    procedure SetFont(AValue: TFont);
    procedure SetForegroundFocusOn(AValue: boolean);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetRadioButton(AValue: TMRadioButtons);
    procedure SetRRRadius(AValue: integer);
    procedure SetStyle(AValue: TMRadioStyle);


  protected
   procedure BoundsChanged;override;
   procedure KeyPress(var Key: char);override;
   procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
   procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
   procedure CNKeyDown    (var Message: TLMKeyDown);    message CN_KEYDOWN;
   procedure DoExit;  override;
   procedure DoEnter; override;
   procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
                                const AXProportion, AYProportion: Double);override;
   procedure CalculateRadioGroup(var aRect: TRect);
   function CalculateSpace(aCaptionHeight, aTRH : integer) : integer;
   function CalculateTextRect(aCaptionHeight, aTRH, aSpace, alv: integer): TRect;
   function CalculateButtonRect(aTeRect : TRect; aTRH : integer):TRect;
   function CalculateSelectedRect(aButRect : TRect; aTRH : integer): TRect;
   function CalculateHotspot(aTeRect : TRect): TRect;
   procedure DrawRadioGroup;
   procedure DrawRadioButtons;
   procedure SetAutoSize(Value: Boolean);override;
   procedure CalculatePreferredSize(var PreferredWidth,PreferredHeight: integer;WithThemeSpace: Boolean); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure RadioButtonFontIsChanged(aHeight : integer);
   procedure SetAllNotSelected(aIndex : integer);
   procedure TriggerAutoSize;
   procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double); override;
  {$IF LCL_FullVersion >= 2010000}
   procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
  {$IFEND}
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;

   //The colour of the control when enable := false
   //Die Farbe des Controlls wenn enable := false
   property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
   //How translucent is the DisabledColor (0=transparent, 255=opaque).
   //Wie transparent die DisabledColor ist (0=transparent, 255=undurchsichtig).
   property DisabledAlphaBValue : integer read FDisabledAlpBV write SetDisabledAlpBV;
   //Aligns the images when they are to the right of the caption
   //Richtet die Images aus wenn sie rechts von der Caption sind
   property AligningImages : Boolean read FAligningImages write FAligningImages;
  published
   //The headline of the radio group
   //Die Überschrift der Radiogroup
   property Caption : TCaption read FCaption write SetCaption;
   //The font to be used for text display the caption.
   //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
   property Font: TFont read FFont write SetFont;
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //How translucent the focusframe is (0=transparent, 255=opaque).
   //Wie transparent der Fokusrahmen ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //The color of the Fokusframe/Foregroundfocus when the Control has the focus
   //Die Farbe des Fokusrahmens/Foregroundfocus wenn das Control den Fokus hat
   property FocusColor : TColor read FFocusColor write SetFocusColor default clOlive;
   //Indicates when the RadioGroup has focus
   //Zeigt an wenn die RadioGroup den Fokus besitzt
   property FocusFrameOn : boolean read FFocusedOn write SetFocusedOn default true;
   //Indicates when the RadioGroup has focus
   //Zeigt an wenn die RadioGroup den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //The geometric shape of the RadioGroup
   //Die geometrische Form des RadioGroups
   property Style      : TMRadioStyle read FStyle write SetStyle default mssRoundRect;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;
   //The start color of the RadioGroup ( for color gradient)
   //Die Startfarbe des RadioGroups (für Farbverlauf)
   property ColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the RadioGroup ( for color gradient)
   //Die Endfarbe des RadioGroups (für Farbverlauf)
   property ColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //Corner diameter if the geometric shape is RoundRect
   //Eckendurchmesser wenn geometrische Form ist RoundRect
   property RndRctRadius : integer    read FRRRadius   write SetRRRadius default 10;
   //Determines whether the control reacts on mouse or keyboard input.
   //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
   property Enabled : boolean read FEnabled write SetEnabled default true;
   //Allows the user to navigate to this control, by pressing the Tab key
   //Ermöglicht dem Benutzer das Navigieren zu diesem Steuerelement durch Drücken der Tabulatortaste
   property TabStop default TRUE;
   //Opens the editor to add radio buttons
   //Öffnet den Editor um Radiobuttons hinzuzufügen
   property RadioButtons : TMRadioButtons read GetRadioButton write SetRadioButton stored IsRadioButtonsStored;
   //Allows automatic adjustment of the size for the control, according to its content
   //Ermöglicht die automatische Anpassung der Größe der Kontrolle an ihren Inhalt
   property AutoSize : boolean read FAutoSize write SetAutoSize default false;

   property DragMode;
   property DragKind;
   property DragCursor;
   property Align;
   property Anchors;
   property Action;
   property BidiMode;
   property BorderSpacing;
   property Constraints;
   property HelpType;
   property TabOrder;
   property Visible;

   property OnChange         : TChangeEvent read FOnChange write FOnChange;
   property OnMouseMove      : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown      : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp        : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter     : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave     : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnEnter          : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit           : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress       : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown        : TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp          : TKeyEvent read FOnKeyUp write FOnKeyUp;
  end;

procedure Register;

implementation
{xxxxxxxxxxxxxxxxx TImageIndexPropertyEditor xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}
type
  TMRadioButtonImageIndexPropertyEditor = class(TImageIndexPropertyEditor)
  protected
    function GetImageList: TCustomImageList; override;
  end;

function TMRadioButtonImageIndexPropertyEditor.GetImageList: TCustomImagelist;
begin
  Result := TMRadioButton(GetComponent(0)).Images;
end;


procedure Register;
begin
  {$I multiradiogroup_icon.lrs}
  RegisterComponents('Multi',[TMultiRadioGroup]);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TMRadioButton, 'ImageIndex', TMRadioButtonImageIndexPropertyEditor);
end;

{ TMultiRadioGroup }

constructor TMultiRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  width                 := 210;
  height                := 120;
  FFocusAlBlVal         := 125;
  FFocusFrameWidth      :=   5;
  FFocusedOn            := true;
  FFocusColor           := clOlive;
  FForegroundFocusOn    := false;
  FStyle                := mssRoundRect;
  FGradient             := gcSpread;
  FRRRadius             := 10;
  FColorStart           := clGray;
  FColorEnd             := clSilver;
  FFont                 := TFont.Create;
  FEnabled              := true;
  FDisabledColor        := $D2D2D2;
  FDisabledAlpBV        := 180;
  FAutoSize             := false;
  TabStop               := TRUE;
  FAligningImages       := true;

  FRadioButtons := CreateRadioButtons;  //TCollection
  FRadioButtons.Add;

end;

destructor TMultiRadioGroup.Destroy;
begin
  FFont.Free;
  FRadioButtons.Free;
  inherited Destroy;
end;

procedure TMultiRadioGroup.Loaded;
begin
  inherited Loaded;
  if not FEnabled then FFocusedOn := false;
  CalculateRadioGroup(FRadioGroupBounds);
end;

procedure TMultiRadioGroup.MouseEnter;
begin
  inherited MouseEnter;
  if not FEnabled then exit;
  if Assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TMultiRadioGroup.MouseLeave;
begin
  inherited MouseLeave;
  if not FEnabled then exit;
  if Assigned(OnMouseLeave) then OnMouseLeave(self);
end;

procedure TMultiRadioGroup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not FEnabled then exit;
  if parent.Visible then setfocus;
  if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
end;


procedure TMultiRadioGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var lv                    : integer;
begin
  inherited MouseMove(Shift, X, Y);
  if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
  if not FEnabled then exit;
  for lv := 0 to pred(RadioButtons.Count) do
   begin
    RadioButtons.Items[lv].FHover:= false;
    if RadioButtons.Items[lv].FEnabled then
     if PtInRect(RadioButtons.Items[lv].FHotspot,Point(x,y)) then
      RadioButtons.Items[lv].FHover:= true;
   end;

  Invalidate;
end;

procedure TMultiRadioGroup.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var lv                    : integer;
begin
 inherited MouseUp(Button, Shift, X, Y);
 if not FEnabled then exit;
 if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
 if parent.Visible then setfocus;
  for lv := 0 to pred(RadioButtons.Count) do
   if RadioButtons.Items[lv].FEnabled then
    if PtInRect(RadioButtons.Items[lv].FHotspot,Point(x,y)) then
     begin
      RadioButtons.Items[lv].Selected:= true;
      if Assigned(OnChange) then OnChange(RadioButtons.Items[lv].Index);
     end;
 Invalidate;
end;

function TMultiRadioGroup.CreateRadioButtons: TMRadioButtons;
begin
   result := TMRadioButtons.Create(Self, TMRadioButton);
end;

procedure TMultiRadioGroup.BoundsChanged;
begin
  inherited BoundsChanged;
  CalculateRadioGroup(FRadioGroupBounds);
  Invalidate;
end;

procedure TMultiRadioGroup.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiRadioGroup.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiRadioGroup.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiRadioGroup.CNKeyDown(var Message: TLMKeyDown);
var lv : integer;
begin
  if not FEnabled then exit;
  with Message do begin
    Result := 1;
    case CharCode of
        VK_UP    : begin
                    for lv := 0 to pred(RadioButtons.Count) do
                     if RadioButtons.Items[lv].Selected = true then
                      if lv > 0 then
                       begin
                        RadioButtons.Items[lv-1].Selected := true;
                        if Assigned(OnChange) then OnChange(RadioButtons.Items[lv-1].Index);
                        break;
                      end;
                   end;
        VK_DOWN  : begin
                    for lv := 0 to pred(RadioButtons.Count) do
                     if RadioButtons.Items[lv].Selected = true then
                      if lv < pred(RadioButtons.Count) then
                       begin
                        RadioButtons.Items[lv+1].Selected := true;
                        if Assigned(OnChange) then OnChange(RadioButtons.Items[lv+1].Index);
                        break;
                      end;
                   end;

      else begin
        Result := 0;
      end;
    end;
  end;

  inherited;
end;

procedure TMultiRadioGroup.DoExit;
begin
  inherited DoExit;
  if Assigned(OnExit) then OnExit(self);
end;

procedure TMultiRadioGroup.DoEnter;
begin
  inherited DoEnter;
  if Assigned(OnEnter) then OnEnter(self);
end;

procedure TMultiRadioGroup.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
var lv : integer;
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
   begin
    FFocusFrameWidth:= round(FFocusFrameWidth*AXProportion);
    FRRRadius       := round(FRRRadius*AXProportion);

    if assigned(RadioButtons) then
     begin
      for lv := 0 to pred(RadioButtons.Count) do
       begin
        RadioButtons.Items[lv].FImageLeft      := round(RadioButtons.Items[lv].FImageLeft*AXProportion);
        RadioButtons.Items[lv].FImageTop       := round(RadioButtons.Items[lv].FImageTop *AYProportion);
        RadioButtons.Items[lv].FCapLeft        := round(RadioButtons.Items[lv].FCapLeft *AXProportion);
        RadioButtons.Items[lv].FCapTop         := round(RadioButtons.Items[lv].FCapTop * AYProportion);
       end;//count
    end;//assigned
   Invalidate;
  end;//AMode
end;


procedure TMultiRadioGroup.RadioButtonFontIsChanged(aHeight: integer);
var lv : integer;
begin
 for lv := 0 to pred(RadioButtons.Count) do
  begin
   RadioButtons.Items[lv].Font.Height:=aHeight;
   if RadioButtons.Items[lv].Font.Height <> 0 then RadioButtons.Items[lv].ParentFont:= false;
  end;
 if not (csLoading in ComponentState) then TriggerAutoSize;
end;

procedure TMultiRadioGroup.SetAllNotSelected(aIndex: integer);
var lv : integer;
begin
 for lv := 0 to pred(RadioButtons.Count) do
  if lv <> aIndex then
   RadioButtons.Items[lv].Selected:= false;
 Invalidate;
end;

procedure TMultiRadioGroup.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
 inherited ScaleFontsPPI(AToPPI, AProportion);
 DoScaleFontPPI(Font, AToPPI, AProportion);

end;

{$IF LCL_FullVersion >= 2010000}
procedure TMultiRadioGroup.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
 inherited FixDesignFontsPPI(ADesignTimePPI);
 DoFixDesignFontPPI(Font, ADesignTimePPI);
end;
{$IFEND}

//XXXXXXXXXXXXXXXXXXXXXXXXXX--- Setter MultiRadioGroup---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiRadioGroup.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  if not FEnabled then FFocusedOn:= false else FFocusedOn := true;
  Invalidate;
end;

procedure TMultiRadioGroup.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetRadioButton(AValue: TMRadioButtons);
begin
 FRadioButtons.Assign(Avalue);
end;

function TMultiRadioGroup.GetRadioButton: TMRadioButtons;
begin
 result := FRadioButtons;
end;

function TMultiRadioGroup.IsRadioButtonsStored: Boolean;
begin
 result := RadioButtons.Enabled;
end;

procedure TMultiRadioGroup.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if not (csLoading in ComponentState) then TriggerAutoSize;
  Invalidate;
end;

procedure TMultiRadioGroup.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetDisabledAlpBV(AValue: integer);
begin
  if FDisabledAlpBV=AValue then Exit;
  FDisabledAlpBV:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetDisabledColor(AValue: TColor);
begin
  if FDisabledColor=AValue then Exit;
  FDisabledColor:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusColor(AValue: TColor);
begin
  if FFocusColor=AValue then Exit;
  FFocusColor:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusedOn(AValue: boolean);
begin
  if FFocusedOn=AValue then Exit;
  FFocusedOn:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetFocusFrameWidth(AValue: integer);
begin
  if FFocusFrameWidth=AValue then Exit;
  FFocusFrameWidth:=AValue;
  CalculateRadioGroup(FRadioGroupBounds);
  Invalidate;
end;

procedure TMultiRadioGroup.SetFont(AValue: TFont);
begin
  if FFont=AValue then Exit;
  FFont.Assign(aValue);
  Invalidate;
end;

procedure TMultiRadioGroup.SetForegroundFocusOn(AValue: boolean);
begin
  if FForegroundFocusOn=AValue then Exit;
  FForegroundFocusOn:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetRRRadius(AValue: integer);
begin
  if FRRRadius=AValue then Exit;
  FRRRadius:=AValue;
  Invalidate;
end;

procedure TMultiRadioGroup.SetStyle(AValue: TMRadioStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  Invalidate;
end;


procedure TMultiRadioGroup.SetAutoSize(Value: Boolean);
begin
 inherited SetAutoSize(Value);
 FAutoSize := Value;
 if FAutoSize then TriggerAutoSize;
 if FAutoSize = false then InvalidatePreferredSize;
end;

procedure TMultiRadioGroup.TriggerAutoSize;
begin
 InvalidatePreferredSize;
 if Assigned(Parent) and Parent.AutoSize then
  Parent.AdjustSize;

 AdjustSize;

end;

procedure TMultiRadioGroup.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var lv                    : integer;
    TeRect                : TRect;
    ButRect               : TRect;
    CaptionHeight         : integer;
    MaxCaptionWidth       : integer;
    MaxImageWidth         : integer;
    Space                 : integer;
    TRH                   : integer;
    tempW                 : integer;
    FAutoWidth            : integer;
    ImW                   : integer;

begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);

 if not assigned(RadioButtons) then exit;

  FAutoWidth := GetTextWidth(FCaption,FFont)+(2*FocusFrameWidth)+10;
  CaptionHeight := GetTextHeight(FCaption,FFont);
  MaxCaptionWidth := 0;
  MaxImageWidth   := 0;

  for lv := 0 to pred(RadioButtons.Count) do
  begin
   RadioButtons.Items[lv].CaptionAlignment:= taLeftJustify;


   if not RadioButtons.Items[lv].FParentFont then
     Canvas.Font.Assign(RadioButtons.Items[lv].FFont)
   else
    Canvas.Font.Assign(FFont);

   if GetTextWidth(RadioButtons.Items[lv].FCaption,Canvas.Font) > MaxCaptionWidth then
     MaxCaptionWidth := GetTextWidth(RadioButtons.Items[lv].FCaption,Canvas.Font);

   Imw     := 0;
   TRH     := GetTextHeight(RadioButtons.Items[lv].FCaption,Canvas.Font);
   Space   := CalculateSpace(CaptionHeight,TRH);
   TeRect  := CalculateTextRect(CaptionHeight,TRH,Space,lv);
   ButRect := CalculateButtonRect(TeRect,TRH);

   if (RadioButtons.Items[lv].FImageList <> nil) and (RadioButtons.Items[lv].FImageIndex > -1) and
     (RadioButtons.Items[lv].FImageIndex < RadioButtons.Items[lv].FImageList.Count) then
    begin

      ImW := RadioButtons.Items[lv].Images.Width;

      if (RadioButtons.Items[lv].ImageLeft <= 10) then
       begin
        //Image is on the leftside
         RadioButtons.Items[lv].FCapLeft  := RadioButtons.Items[lv].ImageLeft+ImW+5;
         RadioButtons.Items[lv].ImageLeft := 5;
       end
      else
       begin
        //Image is on the rightside
         RadioButtons.Items[lv].ImageLeft := GetTextWidth(RadioButtons.Items[lv].FCaption,Canvas.Font)+10;
         RadioButtons.Items[lv].FCapLeft:= 5;


       //max Imagewidth
         if FAligningImages then
          if RadioButtons.Items[lv].Images.Width > MaxImageWidth then
           MaxImageWidth := RadioButtons.Items[lv].Images.Width;
       end;
    end;


   tempW := (2*FocusFrameWidth)+35+ButRect.Width+GetTextWidth(RadioButtons.Items[lv].FCaption,Canvas.Font)+ImW;
    if tempW > FAutoWidth then FAutoWidth := tempW;
  end;//Count

  if FAligningImages then
   begin
    for lv := 0 to pred(RadioButtons.Count) do
     RadioButtons.Items[lv].ImageLeft := MaxCaptionWidth +15;
    FAutoWidth := (2*FocusFrameWidth)+35+ButRect.Width+MaxCaptionWidth +10+MaxImageWidth;

   end;

  PreferredWidth  := FAutoWidth;
  PreferredHeight := (2* FocusFrameWidth)+CaptionHeight+(RadioButtons.Count*TeRect.Height);
end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Calculate---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

function TMultiRadioGroup.GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextWidth(AText);
 finally
  bmp.Free;
 end;
end ;

function TMultiRadioGroup.GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextHeight(AText);
 finally
  bmp.Free;
 end;
end ;

procedure TMultiRadioGroup.CalculateRadioGroup(var aRect: TRect);
begin
 aRect :=  rect(FFocusFrameWidth,FFocusFrameWidth,width-FFocusFrameWidth,height-FFocusFrameWidth);
end;

function TMultiRadioGroup.CalculateSpace(aCaptionHeight, aTRH: integer
  ): integer;
begin
 Result := (Height-((FocusFrameWidth*2)+aCaptionHeight+(RadioButtons.Count*aTRH))) div (RadioButtons.Count+1);
end;

function TMultiRadioGroup.CalculateTextRect(aCaptionHeight, aTRH, aSpace, alv: integer): TRect;
begin
 Result := rect(10+FocusFrameWidth +(aTRH-2),aCaptionHeight+(aSpace*(alv+1))+(alv*aTRH)+FocusFrameWidth,
                 Width-FocusFrameWidth,aCaptionHeight+(aSpace*(alv+1))+aTRH+(alv*aTRH)+FocusFrameWidth);
end;

function TMultiRadioGroup.CalculateButtonRect(aTeRect: TRect; aTRH: integer
  ): TRect;
begin
 Result := rect(10+FocusFrameWidth,aTeRect.Top+2,10+FocusFrameWidth+(aTRH-4),aTeRect.Bottom-2);
end;

function TMultiRadioGroup.CalculateSelectedRect(aButRect: TRect; aTRH: integer
  ): TRect;
begin
 Result := rect(aButRect.Left+round(aTRH * 0.2),aButRect.Top+round(aTRH * 0.2),
                aButRect.Right-round(aTRH * 0.2),aButRect.Bottom-round(aTRH * 0.2));
end;

function TMultiRadioGroup.CalculateHotspot(aTeRect: TRect): TRect;
begin
  Result := rect(FocusFrameWidth,aTeRect.Top,aTeRect.Right,aTeRect.Bottom);
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiRadioGroup.DrawRadioGroup;
var bkBmp        : TBitmap;
    trBmp        : TBitmap;
    mask         : TBitmap;
    Dest         : TBitmap;
begin
 bkBmp := TBitmap.Create;
 bkBmp.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);

 if FGradient = gcAlternate then Gradient_Bmp(bkBmp,clSilver,clGray,ord(gcVertical)); //otherwise flickers
 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : trBmp.Canvas.RoundRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : trBmp.Canvas.Rectangle(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 end;

 mask := TBitmap.Create;
 mask.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 mask.Canvas.Brush.Color:=clBlack;
 case FStyle of
  mssRoundRect : mask.Canvas.RoundRect(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height,FRRRadius,FRRRadius);
  mssRect      : mask.Canvas.Rectangle(0,0,FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 end;

 Dest       := TBitmap.Create;
 Dest.SetSize(FRadioGroupBounds.Width,FRadioGroupBounds.Height);
 Dest.Transparent:= true;
 Dest.TransparentColor:= clBlack;
 Dest.Canvas.Brush.Color:=clBlack;
 Dest.Canvas.FillRect(0,0,100,100);
 Dest.Canvas.copymode:=cmSrcCopy;
 Dest.Canvas.Draw(0,0,bkBmp);
 Dest.Canvas.Draw(0,0,trBmp);
 Dest.Canvas.copymode:=cmSrcInvert;
 Dest.Canvas.Draw(0,0,mask);

 canvas.Draw(FRadioGroupBounds.Left,FRadioGroupBounds.Top,Dest);

 bkBmp.Free;
 trBmp.Free;
 mask.Free;
 Dest.Free;

end;

procedure TMultiRadioGroup.DrawRadioButtons;
var lv                    : integer;
    TeRect                : TRect;
    ButRect               : TRect;
    SelRect               : TRect;
    CaptionHeight         : integer;
    Space                 : integer;
    TRH                   : integer;
    tmpBmp                : TBitmap;

begin
 if not assigned(RadioButtons) then exit;

 CaptionHeight := GetTextHeight(FCaption,FFont);

 for lv := 0 to pred(RadioButtons.Count) do
  begin
 //the font in the radiobutton
   if not RadioButtons.Items[lv].FParentFont then
    Canvas.Font.Assign(RadioButtons.Items[lv].FFont)
   else
    Canvas.Font.Assign(FFont);

   TRH     := GetTextHeight(RadioButtons.Items[lv].FCaption,Canvas.Font);
   Space   := CalculateSpace(CaptionHeight,TRH);
   TeRect  := CalculateTextRect(CaptionHeight,TRH,Space,lv);
   ButRect := CalculateButtonRect(TeRect,TRH);
   SelRect := CalculateSelectedRect(ButRect,TRH);
   RadioButtons.Items[lv].FHotspot := CalculateHotspot(TeRect);

  //the background of the Radiobuttons
   if RadioButtons.Items[lv].FColor <> clNone then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FColor;
      Canvas.FillRect(RadioButtons.Items[lv].FHotspot);
     end;

  //the hover over the radiobuttons
    if RadioButtons.Items[lv].FHover then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FHoverColor;
      Canvas.FillRect(RadioButtons.Items[lv].FHotspot);
     end;

  //the radiobutton
    canvas.Brush.Color:= RadioButtons.Items[lv].FButtonColor;
    canvas.Ellipse(ButRect);

  //the selection in the radiobutton
    if RadioButtons.Items[lv].Selected then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FButtonSelColor;
      canvas.Ellipse(SelRect);
     end;

  //the ItemCaption of a new radiobutton
    if not RadioButtons.Items[lv].FCaptionChange then
     RadioButtons.Items[lv].FCaption := 'Radiobutton ' + inttostr(RadioButtons.Items[lv].Index+1);

  //the ItemCaption in the radiobutton
    canvas.TextRect(TeRect,TeRect.Left+RadioButtons.Items[lv].FCapLeft,TeRect.Top+RadioButtons.Items[lv].FCapTop,
                    RadioButtons.Items[lv].FCaption,RadioButtons.Items[lv].FTextStyle);

  //Draw the Image
     if (RadioButtons.Items[lv].FImageList <> nil) and (RadioButtons.Items[lv].FImageIndex > -1) and
     (RadioButtons.Items[lv].FImageIndex < RadioButtons.Items[lv].FImageList.Count) then
      RadioButtons.Items[lv].FImageList.ResolutionForPPI[RadioButtons.Items[lv].FImageWidth,
          Font.PixelsPerInch,GetCanvasScaleFactor].Draw(Canvas,TeRect.Left+RadioButtons.Items[lv].FImageLeft,
          TeRect.Top+RadioButtons.Items[lv].FImageTop,RadioButtons.Items[lv].FImageIndex);

  //not Enable
   if not RadioButtons.Items[lv].FEnabled then
    begin
     try
      tmpBmp             := TBitmap.Create;
      {$IFDEF WINDOWS}
       tmpBmp.PixelFormat := pf32bit;
      {$ENDIF}
      tmpBmp.SetSize(RadioButtons.Items[lv].FHotspot.width,RadioButtons.Items[lv].FHotspot.height);
      tmpBmp.Canvas.Brush.Color:= RadioButtons.Items[lv].FDisabledColor;
      tmpBmp.Canvas.FillRect(0,0,tmpBmp.Width,tmpBmp.Height);

      BmpToAlphaBmp(tmpBmp,RadioButtons.Items[lv].FDisabledAlpBV);
      canvas.Draw(RadioButtons.Items[lv].FHotspot.Left,RadioButtons.Items[lv].FHotspot.Top,tmpBmp);
     finally
      tmpBmp.Free;
     end;//not Enable
   end;
  end;//Count

end;


procedure TMultiRadioGroup.Paint;
var tmpBmp     : TBitmap;

begin
  inherited Paint;
  //draw the Focusframe
 if (Focused=true) and (FFocusedOn = true) then
  begin
   try
    tmpBmp             := TBitmap.Create;
    {$IFDEF WINDOWS}
     tmpBmp.PixelFormat := pf32bit;
    {$ENDIF}
    tmpBmp.SetSize(width,height);
    tmpBmp.Canvas.Brush.Color:= FFocusColor;
    tmpBmp.Canvas.FillRect(0,0,width,height);

    BmpToAlphaBmp(tmpBmp,FFocusAlBlVal);
    canvas.Draw(0,0,tmpBmp);


   finally
    tmpBmp.Free;
   end;
 end;

 DrawRadioGroup;

 canvas.Brush.Style:= bsClear;
 canvas.Font.Assign(FFont);
 canvas.TextOut(FocusFrameWidth+5,FocusFrameWidth,FCaption);
 canvas.Brush.Style:= bsSolid;

 DrawRadioButtons;

 //Enable
 if not FEnabled then
 begin
  try
   tmpBmp             := TBitmap.Create;
   {$IFDEF WINDOWS}
    tmpBmp.PixelFormat := pf32bit;
   {$ENDIF}
   tmpBmp.SetSize(width,height);
   tmpBmp.Canvas.Brush.Color:= FDisabledColor;
   tmpBmp.Canvas.FillRect(0,0,width,height);

   BmpToAlphaBmp(tmpBmp,FDisabledAlpBV);
   canvas.Draw(0,0,tmpBmp);
  finally
   tmpBmp.Free;
  end;
 end;



end;



{$Include mrg_radiobuttonitem.inc}

end.
