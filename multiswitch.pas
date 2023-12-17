{ <TMultiSwitch is a toggle component>
  <Version 0.0.0.1>
  Copyright (C) <10.12.2023> <Bernd Hübner>
  Many thanks to the members of the German Lazarus Forum!
  For some improvements see https://www.lazarusforum.de/viewtopic.php?p=137567#p137567
  The images in the resource are from Roland Hahn. Vielen Dank!
  See: https://www.lazarusforum.de/viewtopic.php?p=128092#p128092

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



unit MultiSwitch;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, LResources, Forms, Controls, Graphics, Dialogs,
  IntfGraphics, LCLIntf, GraphType, PropEdits, infmultis, ExtCtrls, LMessages,
  LCLType, StdCtrls, LCLVersion, LCLProc;

type
  TDirection = (fsRight,fsLeft);

type
  TClickEvent      = procedure(Sender: TObject) of object;
type
  TMouseMoveEvent  = procedure(Sender: TObject;Shift: TShiftState;X,Y: Integer) of Object;
type
  TMouseEvent      = procedure(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer) of Object;
type
  TMouseEnterLeave = procedure(Sender: TObject) of object;
type
  TNotifyEvent     = procedure(Sender: TObject) of object;
type
  TKeyEvent        = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of Object;
type
  TKeyPressEvent   = procedure(Sender: TObject; var Key: char) of Object;
type
  TChangeEvent     = procedure(Sender: TObject) of object;
type
  TDirectionEvent  = procedure(Sender: TObject;aDirection : TDirection) of object;



type
  TSwitchMode = (fsClick,fsSlide);

type
  TRollImage = class (TPersistent)
   private
     aDirection : TDirection;
     ImageIndex : integer;
  end;


type

  { TThumbnail }

  TThumbnail = class(TCustomControl)
  private
    FAColor  : TColor;
    FOnClick : TClickEvent;
    FImage   : TCustomBitmap;
    procedure SetAColor(AValue: TColor);

  protected

  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure Paint; override;

   property aColor  : TColor read FAColor write SetAColor;
   property OnClick : TClickEvent read FOnClick     write FOnClick;
  end;

type

    { TPropertyImageSelector }

    TPropertyImageSelector = class (TPropertyEditor)
    private
     PEForm       : TCustomForm;
     FThumbnail   : array [0..39] of TThumbnail;
     FRadioButton : array [0..1] of TRadioButton;
     FButton      : array [0..1] of TButton;
     FRollImage   : TRollImage;
     FFirst       : boolean;
     procedure ButtonsClick(Sender: TObject);
     procedure RadioButtons({%H-}Sender: TObject);
     procedure SelectedImage(Sender: TObject);
    protected
     procedure DoShowEditor;

    public
     procedure Edit; Override;
     function  GetValue: string;Override;
     function  GetAttributes: TPropertyAttributes; Override;
    end;


type

  { TMultiSwitch }

  TMultiSwitch = class(TCustomControl)
  private
   FFocusAlBlVal       : byte;
   FFocusedOn          : boolean;
   FFocusFrameWidth    : integer;
   FFocusFrameHeight   : integer;
   FForegroundFocusOn: boolean;
   FImages             : Array[0..39] of TCustomBitmap;
   FImgSizeFactor      : double;
   FGRoupIndex         : integer;
   FImgLeftImageIndex  : integer;
   FOnDirection        : TDirectionEvent;
   FOnLeft             : TChangeEvent;
   FOnRight            : TChangeEvent;
   FRightImage         : TCustomBitmap;
   FImgLeftImage       : TCustomBitmap;
   FFocusColor         : TColor;
   FBestTextHeight     : boolean;
   FFocusedBlendFaktor : Double;
   FDisabledColor      : TColor;
   FEnabledBlendFaktor : Double;
   FCapLeft            : integer;
   FCapTop             : integer;
   FEnabled            : boolean;
   FFont               : TFont;
   FCaption            : TCaption;
   FDirection          : TDirection;
   FAngel              : Double;
   FLeftCaption        : TCaption;
   FRightCaption       : TCaption;
   FOnChange           : TChangeEvent;
   FOnClick            : TClickEvent;
   FOnEnter            : TNotifyEvent;
   FOnExit             : TNotifyEvent;
   FOnKeyDown          : TKeyEvent;
   FOnKeyPress         : TKeyPressEvent;
   FOnKeyUp            : TKeyEvent;
   FOnMouseDown        : TMouseEvent;
   FOnMouseEnter       : TMouseEnterLeave;
   FOnMouseLeave       : TMouseEnterLeave;
   FOnMouseMove        : TMouseMoveEvent;
   FOnMouseUp          : TMouseEvent;
   FRightImageIndex    : integer;
   FRollImage          : TRollImage;
   FRotation           : Double;
   FSpeed              : integer;
   FSwitchMode         : TSwitchMode;
   FTextStyle          : TTextStyle;
   FTimer              : TTimer;
   FBorderColor        : TColor;
   FButtonColor        : TColor;
   FRightBgrdColor     : TColor;
   FHoverColor         : TColor;
   FHover              : boolean;
   FHoverBlendFaktor   : Double;
   FImgLeftBgrdColor   : TColor;
   FOldWidth           : integer;
   FOldHeight          : integer;
   FPortion            : Double;
   FMargin             : Integer;
   FRoll               : boolean;
   FRollPos            : Integer;
   FButtonSize         : Integer;
   FBackgroundImage    : TCustomBitmap;
   FButtonImage        : TCustomBitmap;
   FBorderImage        : TCustomBitmap;
   FLoadFromFile       : boolean;
   FImgLeft            : integer;
   FImgTop             : integer;
   FHotSpot            : TRect;
   FOldCursor          : TCursor;
   FMultiCursor        : TCursor;
   FSlideFirst         : boolean;
   FSlideStartX        : integer;
   FSlideEndPos        : boolean;
   FFirst              : boolean;
   FFirstRight         : boolean;
   FAbortSlide         : boolean;


   procedure DrawTheFocusframe;
   procedure SetFocusAlBlVal(AValue: byte);
   procedure SetFocusedOn(AValue: boolean);
   procedure SetFocusFrameWidth(AValue: integer);
   procedure SetForegroundFocusOn(AValue: boolean);
   function TheOnlyOne:boolean;
   procedure CheckTheGroup;
   procedure DrawAHoverEvent;
   procedure DrawForegroundFocused;
   procedure DrawNotEnabled;
   procedure DrawTheBackground;
   procedure DrawTheBorder;
   procedure DrawTheButton;
   procedure DrawTheCaption;
   procedure DrawTheRollbutton;
   procedure FontChange(Sender: TObject);
   procedure FTimerTimer({%H-}Sender: TObject);
   function CalculateTextRect:TRect;
   procedure SetAlignment(AValue: TAlignment);
   procedure SetBestTextHeight(AValue: boolean);
   procedure SetBorderColor(AValue: TColor);
   procedure SetButtonColor(AValue: TColor);
   procedure SetCapLeft(AValue: integer);
   procedure SetCapTop(AValue: integer);
   procedure SetDirection(AValue: TDirection);
   procedure SetDisabledColor(AValue: TColor);
   procedure SetEnabledBlendFaktor(AValue: Double);
   procedure SetGroupIndex(AValue: integer);
   procedure SetImgSizeFactor(AValue: double);
   procedure SetLeftImageIndex(AValue: integer);
   procedure SetRightBgrdColor(AValue: TColor);
   procedure SetRightCaption(AValue: TCaption);
   procedure SetFocusColor(AValue: TColor);
   procedure SetFont(AValue: TFont);
   procedure SetHoverColor(AValue: TColor);
   procedure SetLeftBgrdColor(AValue: TColor);
   procedure SetLeftCaption(AValue: TCaption);
   procedure SetLayout(AValue: TTextLayout);
   procedure SetRightImageIndex(AValue: integer);
   procedure SetRollImage(AValue: TRollImage);
   procedure SetSpeed(AValue: integer);
   procedure SetTextStyle(AValue: TTextStyle);
   procedure SetEnabled(aValue: boolean);reintroduce;
   procedure SlideTheButton(aX: integer);

  protected
   procedure BoundsChanged;override;
   procedure CalculateBounds;
   procedure CalculateButton;
   procedure KeyPress(var Key: char);override;
   procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
   procedure KeyUp(var Key: Word; Shift: TShiftState);  override;
   procedure CNKeyDown(var Message: TLMKeyDown); message CN_KEYDOWN;
   procedure DoExit;  override;
   procedure DoEnter; override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure Loaded; override;
   procedure ScaleFontsPPI(const AToPPI: Integer; const AProportion: Double);override;
   {$IF LCL_FullVersion >= 2010000}
   procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
   {$IFEND}
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;
   procedure LoadImagesfromFile(LeftFilename,RightFilename: string);
   procedure Paint; override;

   //How translucent is the HoverColor (1=opaque,0=transparent)
   //Wie transparent die Hoverfarbe ist (1=undurchsichtig,0=durchsichtig)
   property HoverBlendFaktor : Double read FHoverBlendFaktor write FHoverBlendFaktor;
   //How translucent is the focusColor (1=opaque,0=transparent)
   //Wie transparent die Fokusfarbe ist (1=undurchsichtig,0=durchsichtig)
   property FocusedBlendFaktor : Double read FFocusedBlendFaktor write FFocusedBlendFaktor;
   //The Startangle of the RollImage
   //Der Startwinkel des RollImages
   property Angel : Double read FAngel write FAngel;
   //The steps by rotation
   //Die Schritte beim Drehen
   property Rotation : Double read FRotation write FRotation;

   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
   //The colour of the control when enable := false
   //Die Farbe des Controlls wenn enable := false
   property DisabledColor : TColor read FDisabledColor write SetDisabledColor;
   //How translucent is the DisabledColor (1=opaque,0=transparent)
   //Wie transparent die DisabledColor ist (1=undurchsichtig,0=durchsichtig)
   property EnabledBlendFaktor : Double read FEnabledBlendFaktor write SetEnabledBlendFaktor;
   //To compensate if images with <>72px are loaded with LoadfromFile
   //Zum Ausgleich wenn mit LoadfromFile Images mit <>72px geladen werden
   property ImgSizeFactor : double read FImgSizeFactor write SetImgSizeFactor;

  published
   //The Left background colour
   //Die linke Hintergrundfarbe
   property LeftBgrdColor : TColor read FImgLeftBgrdColor write SetLeftBgrdColor default $000000C8;
   //The Right background colour
   //Die rechte Hintergrundfarbe
   property RightBgrdColor : TColor read FRightBgrdColor write SetRightBgrdColor default $0000C800;
   //The color of the button, Rollimage only visible when ButtonColor = clNone
   //Die Farbe des Buttons, Rollimages sind nur sichtbar wenn ButtonColor = clNone
   property ButtonColor : TColor read FButtonColor write SetButtonColor default clNone;
   //The color of the border (clNone = no Border)
   //Farbe des Randes (clNone = keinRand)
   property BorderColor : TColor read FBorderColor write SetBorderColor default clNone;
   //The color of a hoverevent (clNone = no hover)
   //Die Farbe eines Hoverereignisses (clNone = kein Hover)
   property HoverColor : TColor read FHoverColor write SetHoverColor default clNone;
   //Switches the focus frame on and off
   //Schaltet den Fokusrahmen ein und aus
   property FocusFrameOn : boolean read FFocusedOn write SetFocusedOn default true;
   //The color when the Control has the focus (clNone = no focus is shown)
   //Die Farbe wenn das Control den Fokus hat (clNone = keine Fokus-Anzeige)
   property FocusColor : TColor read FFocusColor write SetFocusColor default clRed;
   //How translucent the focusframe is (0=transparent, 255=opaque).
   //Wie transparent der Fokusrahmen ist (0=transparent, 255=undurchsichtig).
   property FocusAlphaBValue : byte read FFocusAlBlVal write SetFocusAlBlVal default 125;
   //The whidth of the focus-frame
   //Die Dicke des Fokus-Rahmens
   property FocusFrameWidth : integer read FFocusFrameWidth write SetFocusFrameWidth default 5;
   //Indicates when the button has focus
   //Zeigt an wenn der Button den Fokus besitzt
   property ForegroundFocusOn : boolean read FForegroundFocusOn write SetForegroundFocusOn default false;
   //Determines whether the RollButton(Image) rotates
   //Bestimmt ob sich der RollButton(Image) dreht
   property Roll : boolean read FRoll write FRoll default true;
   //Specifies whether the button is on the right or left at the start
   //Gibt an ob der Button beim Start rechts oder links ist
   property Direction        : TDirection read FDirection write SetDirection default fsLeft;
   //The speed at which the button moves
   //Die Geschwindigkeit mit der sich der Button bewegt
   property Speed            : integer read FSpeed write SetSpeed default 10;
   //The caption that is displayed when the button is on the left
   //Die Caption die angezeigt wird wenn der Button links ist
   property LeftCaption   : TCaption read FLeftCaption write SetLeftCaption;
   //The caption that is displayed when the button is on the right
   //Die Caption die angezeigt wird wenn der Button rechts ist
   property RightCaption     : TCaption read FRightCaption  write SetRightCaption;
   //The font to be used for text display in this switch.
   //Die Schrift die für die Textanzeige in diesem Schalter verwendet werden soll.
   property Font: TFont read fFont write SetFont;
   //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
   //Determines whether the control reacts on mouse or keyboard input.
   //Legt fest, ob das Steuerelement auf Maus- oder Tastatureingaben reagiert.
   property Enabled : boolean read FEnabled write SetEnabled default true;
   //Automatically adjusts the text height to the size of the control
   //Passt die Texthöhe automatisch der Größe des Controlls an
   property BestTextHeight : boolean read FBestTextHeight write SetBestTextHeight default true;
   //Starts the property editor to select loaded images
   //Startet den Eigenschaftseditor um geladene Images auszuwählen
   property NewRollImage : TRollImage read FRollImage write SetRollImage;
   //The Index of the loaded left image
   //Der Index des linken geladenen Bildes
   property LeftImageIndex : integer read FImgLeftImageIndex write SetLeftImageIndex default 0;
   //The Index of the loaded right image
   //Der Index des rechten geladenen Bildes
   property RightImageIndex : integer read FRightImageIndex write SetRightImageIndex default 1;
   //The mode with which the switch is operated, click or slide
   //Der Modus mit dem der Schalter betätigt wird, klicken oder schieben
   property SwitchMode : TSwitchMode read FSwitchMode write FSwitchMode default fsClick;
   //The Index within the group of MultiSwitches, odd index allows only 1x fsRight, even 1x fsLeft
   //Der Index der Gruppe zu der der MultiSwitch gehört, ungerader Index erlaubt nur 1x fsRight, gerade 1x fsLeft
   property GroupIndex : integer read FGroupIndex write SetGroupIndex default 0;


   property TabStop default TRUE;
   property PopupMenu;
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

   property OnClick      : TClickEvent read FOnClick     write FOnClick;
   property OnMouseMove  : TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
   property OnMouseDown  : TMouseEvent read FOnMouseDown write FOnMouseDown;
   property OnMouseUp    : TMouseEvent read FOnMouseUp write FOnMouseUp;
   property OnMouseEnter : TMouseEnterLeave read FOnMouseEnter write FOnMouseEnter;
   property OnMouseLeave : TMouseEnterLeave read FOnMouseLeave write FOnMouseLeave;
   property OnEnter      : TNotifyEvent read FOnEnter write FOnEnter;
   property OnExit       : TNotifyEvent read FOnExit write FOnExit;
   property OnKeyPress   : TKeyPressEvent read FOnKeyPress write FOnKeyPress;
   property OnKeyDown    : TKeyEvent read FOnKeyDown write FOnKeyDown;
   property OnKeyUp      : TKeyEvent read FOnKeyUp write FOnKeyUp;
   property OnChange     : TChangeEvent read FOnChange write FOnChange;
   property OnRight      : TChangeEvent read FOnRight write FOnRight;
   property OnLeft       : TChangeEvent read FOnLeft write FOnLeft;
   property OnDirection  : TDirectionEvent read FOnDirection write FOnDirection;
   property OnDragDrop;
   property OnDragOver;
   property OnEndDrag;
   property OnStartDrag;


  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiswitch_icon.lrs}
  RegisterComponents('Multi',[TMultiSwitch]);
  RegisterPropertyEditor(TypeInfo(TRollImage),nil,'NewRollImage',TPropertyImageSelector); //Hier "RollImage" muss identisch mit der Property sein
  {$R images.res}
end;

{ TMultiSwitch }

constructor TMultiSwitch.Create(AOwner: TComponent);
var lv : integer;
begin
  inherited Create(AOwner);
  Width                := 60;
  FOldWidth            := 60;
  Height               := 26;
  FOldHeight           := 26;
  FPortion             :=  0;
  FMargin              :=  3;
  FRollPos             :=  0;
  FHover               := false;
  FHoverBlendFaktor    := 0.2;
  FRoll                := true;
  FAngel               :=  0;
  FRotation            := 30;
  FDirection           := fsLeft;
  FEnabled             := true;
  FImgLeftBgrdColor    := rgb(200,0,0);
  FRightBgrdColor      := rgb(0,200,0);
  FButtonColor         := clNone;
  FBorderColor         := clNone;
  FHoverColor          := clNone;
  FEnabledBlendFaktor  := 0.7;
  FFocusedBlendFaktor  := 0.1;
  FDisabledColor       := clWhite;
  FFocusColor          := clRed;
  FBestTextHeight      := true;
  TabStop              := true;
  FLoadFromFile        := false;
  FImgSizeFactor       :=   1;
  FImgLeft             :=   0;
  FImgTop              :=   0;
  FSwitchMode          := fsClick;
  FSlideFirst          := true;
  FSlideEndPos         := true;
  FFirst               := true;
  FFirstRight          := false;
  FGRoupIndex          :=   0;
  FAbortSlide          := false;
  FFocusedOn           := true;
  FFocusAlBlVal        := 125;
  FFocusFrameWidth     :=   5;
  FFocusFrameHeight    := round(FFocusFrameWidth/(Width/Height));
  FForegroundFocusOn   := false;


  FTimer            := TTimer.Create(nil);
  FSpeed            := 10;
  FTimer.Interval   := FSpeed;
  FTimer.Enabled    := false;
  FTimer.OnTimer    :=@FTimerTimer;

  FLeftCaption      := 'OFF';
  FRightCaption     := 'ON';
  FCaption          := FLeftCaption;
  FFont             := TFont.Create;
  FFont.Color       := clWhite;
  FFont.Style       := [fsbold];
  FFont.OnChange    :=@FontChange;

  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= true;
  FTextStyle.Wordbreak := false;
  FTextStyle.Clipping  := true;

  FBackgroundImage := TPortableNetworkGraphic.Create;
  FBackgroundImage.LoadFromResourceName(HInstance,'m_backround');
  FButtonImage := TPortableNetworkGraphic.Create;
  FButtonImage.LoadFromResourceName(HInstance,'m_puk001');
  FBorderImage := TPortableNetworkGraphic.Create;
  FBorderImage.LoadFromResourceName(HInstance,'m_backroundborder');

  FImgLeftImageIndex  := 0;
  FRightImageIndex := 1;
  for lv := 0 to High(FImages) do
    FImages[lv] := TPortableNetworkGraphic.Create;
  FImages[0].LoadFromResourceName(HInstance, 'm_puk009');
  FImages[1].LoadFromResourceName(HInstance, 'm_puk008');
  FImages[2].LoadFromResourceName(HInstance, 'm_puk010');
  FImages[3].LoadFromResourceName(HInstance, 'm_puk007');
  FImages[4].LoadFromResourceName(HInstance, 'm_puk006');
  FImages[5].LoadFromResourceName(HInstance, 'm_puk005');
  FImages[6].LoadFromResourceName(HInstance, 'm_puk003');
  FImages[7].LoadFromResourceName(HInstance, 'm_puk004');
  //load more images, maximal 40   Images must have 64x64pixel!
  FImages[8].LoadFromResourceName(HInstance, 'm_puk002');
  for lv := 9 to 39 do //load the rest with dummy
    FImages[lv].Assign(FImages[8]);

  FImgLeftImage := TPortableNetworkGraphic.Create;
  FImgLeftImage.Assign(FImages[FImgLeftImageIndex]);
  FRightImage := TPortableNetworkGraphic.Create;
  FRightImage.Assign(FImages[FRightImageIndex]);



end;

destructor TMultiSwitch.Destroy;
var lv : integer;
begin

 if assigned(FRollImage) then FRollImage.Free;
 FBackgroundImage.Free;
 FButtonImage.Free;
 FBorderImage.Free;
 FImgLeftImage.Free;
 FRightImage.Free;
 for lv := 0 to High(FImages) do FImages[lv].Free;
 FTimer.Free;
 FFont.Free;
 inherited Destroy;
end;

procedure TMultiSwitch.Loaded;
begin
 inherited Loaded;
 FMultiCursor    := Cursor;
 if FFocusFrameWidth <> 0 then
   FFocusFrameHeight    := round(FFocusFrameWidth/(Width/Height))
  else
   FFocusFrameHeight    := 0;
 if (FDirection = fsRight) then
  begin
   CalculateBounds;
   CalculateButton;
   FRollPos := width - (FButtonSize + (2*FMargin)+(2*FFocusFrameWidth));
  end;
end;

procedure TMultiSwitch.MouseEnter;
begin
 if not Enabled then exit;
 inherited MouseEnter;
 FHover := true;
 FOldCursor := Cursor;
 if Assigned(OnMouseEnter) then OnMouseEnter(self);
 Invalidate;
end;

procedure TMultiSwitch.MouseLeave;
begin
 if not Enabled then exit;
 inherited MouseLeave;
 FHover := false;
 Cursor := FOldCursor;
 if Assigned(OnMouseLeave) then OnMouseLeave(self);
 if not FSlideEndPos and (FTimer.Enabled = false) then
  FTimer.Enabled:= true;
 Invalidate;
end;

procedure TMultiSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
var delta : integer;
begin
 if not Enabled then exit;
 inherited MouseMove(Shift, X, Y);
 if Assigned(OnMouseMove) then OnMouseMove(self,Shift,x,y);
 Cursor := FMultiCursor;

 if FSwitchMode = fsSlide then
  begin
   if PointInaCircle(FHotSpot,x,y) then
    begin
     Cursor:=crHandPoint;
     if ssLeft in Shift then
      begin
       if GroupIndex <> 0 then
        if TheOnlyOne then exit;
       if FSlideFirst then
        FSlideStartX := x;
       FSlideFirst := false;
       delta := x-FSlideStartX;
       if FFirstRight then
        begin
         FSlideStartX := x-FRollPos;
         delta := x-FSlideStartX;
         FFirstRight := false;
        end;
       SlideTheButton(delta);
      end;
    end;
   Invalidate;
   exit;
  end;
 FSlideFirst := true;
 Invalidate;
end;

procedure TMultiSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if not Enabled then exit;
 inherited MouseDown(Button, Shift, X, Y);
 FHover := false;
 if Assigned(OnMouseDown) then OnMouseDown(self,Button,Shift,x,y);
 Invalidate;
end;

procedure TMultiSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
 if not Enabled then exit;
 inherited MouseUp(Button, Shift, X, Y);
 if Assigned(OnMouseUp) then OnMouseUp(self,Button,Shift,x,y);
 if Assigned(OnClick) then OnClick(self);

 if FSwitchMode = fsSlide then
  if not FSlideEndPos and (FTimer.Enabled = false) then
   begin
    FAbortSlide := true;
    FTimer.Enabled:= true;
   end;



 if FSwitchMode = fsClick then
  if not FTimer.Enabled then
   begin
    if GroupIndex <> 0 then
     if TheOnlyOne then exit;

    FDirection := TDirection((ord(FDirection) + 1) mod 2);
    FTimer.Enabled:= true;
    if Assigned(OnChange) then OnChange(self);
   end;
end;

procedure TMultiSwitch.LoadImagesfromFile(LeftFilename, RightFilename: string);
var oldiniimg, oldRightimg : TCustomBitmap;
begin
 oldiniimg   :=TPortableNetworkGraphic.Create;
 oldRightimg :=TPortableNetworkGraphic.Create;
 try
  if assigned(FImgLeftImage) then
   begin
    oldiniimg.Assign(FImgLeftImage);
    FreeAndNil(FImgLeftImage);
   end;
  if assigned(FRightImage)   then
   begin
    oldRightimg.Assign(FRightImage);
    FreeAndNil(FRightImage);
   end;
 FImgLeftImage := TPortableNetworkGraphic.Create;
 FRightImage   := TPortableNetworkGraphic.Create;
  try
   if fileexists(LeftFilename) and fileexists(RightFilename) then
    begin
     FImgLeftImage.LoadFromFile(LeftFilename);
     FRightImage.LoadFromFile(RightFilename);
     if (FImgLeftImage.Width <> FRightImage.Width) or (FImgLeftImage.Height <> FRightImage.Height) then
      begin
       FImgLeftImage.Assign(oldiniimg);
       FRightImage.Assign(oldRightimg);
       showmessage('The size of the images must be the same!');
      end else FLoadFromFile := true;
    end
   else
    begin
     FImgLeftImage.Assign(oldiniimg);
     FRightImage.Assign(oldRightimg);
     showmessage('Incorrect path');
    end;
  except
   showmessage('Wrong Graphicformat, only PNG!');
  end;

 Finally
  oldiniimg.Free;
  oldRightimg.Free;
 end;
 Invalidate;
end;

function TMultiSwitch.TheOnlyOne:boolean;
var comp        : TComponent;
    CurSwitch   : TMultiSwitch;
    CurForm     : TForm;
    CurControl  : TControl;
    lv          : integer;
    exitflag    : boolean;
    allthesame  : boolean;
    first       : boolean;
    aDirection  : TDirection;
    count       : integer;
begin
 lv:=0; exitflag := false;
 CurControl := Parent;
 first := true;
 count := 1;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(lv);
 until (lv =100) or (exitflag = true);
 Result     := false;
 allthesame := true;
 CurForm := (CurControl as TForm);

 for comp in CurForm do
   begin
    if comp is TMultiSwitch then
     begin
      CurSwitch := comp as TMultiSwitch;
      if CurSwitch.GroupIndex <> 0 then
       if (CurSwitch <> self) and (CurSwitch.GroupIndex = FGroupIndex) then
        begin
         inc(count);
        end;
     end;//comp is
   end;//comp in
 if count < 3 then exit;

 for comp in CurForm do
   begin
    if comp is TMultiSwitch then
     begin
      CurSwitch := comp as TMultiSwitch;
      if CurSwitch.GroupIndex <> 0 then
       if (CurSwitch <> self) and (CurSwitch.GroupIndex = FGroupIndex) then
        begin
         if FDirection <> CurSwitch.Direction then allthesame :=false;

        end;
     end;//comp is
   end;//comp in
 if allthesame then exit;

 Result := true;
 for comp in CurForm do
   begin
    if comp is TMultiSwitch then
     begin
      CurSwitch := comp as TMultiSwitch;
      if CurSwitch.GroupIndex <> 0 then
       if (CurSwitch <> self) and (CurSwitch.GroupIndex = FGroupIndex) then
        begin
         if First then aDirection := CurSwitch.Direction;
         First := false;
         if aDirection <> CurSwitch.Direction then Result := false;

        end;
     end;//comp is
   end;//comp in

end;

procedure TMultiSwitch.CheckTheGroup;
var comp        : TComponent;
    CurSwitch   : TMultiSwitch;
    CurForm     : TForm;
    CurControl  : TControl;
    lv          : integer;
    exitflag    : boolean;

begin
 lv:=0; exitflag := false;
 CurControl := Parent;
 repeat
  if CurControl is TForm then exitflag := true
   else
    CurControl := CurControl.Parent;      //back to the Form
  inc(lv);
 until (lv =100) or (exitflag = true);

 CurForm := (CurControl as TForm);
 for comp in CurForm do
   begin
    if comp is TMultiSwitch then
     begin
      CurSwitch := comp as TMultiSwitch;
      if CurSwitch.GroupIndex <> 0 then
       if (CurSwitch <> self) and (CurSwitch.GroupIndex = FGroupIndex) then
        begin
         if CurSwitch.Direction = FDirection then
          begin
           CurSwitch.Direction := TDirection((ord(CurSwitch.Direction) + 1) mod 2);
           CurSwitch.invalidate;
          end;
        end;
     end;//comp is
   end;//comp in
end;

procedure TMultiSwitch.CalculateBounds;
var Factor : double;
begin
 if Width < 20  then
  begin
   Width  :=  10;
   Height :=   9;
  end;
 if (width > 175) or (Height > 76) then
  begin
   Width  := 175;
   Height :=  76;
  end;


 if width <> FOldWidth then
   begin
    Factor := Width / 60;
    Height := round(Factor * 26);
    FOldWidth := Width;
   end;
  if Height <> FOldHeight then
   begin
    Factor := Height / 26;
    Width := round(Factor * 60);
    FOldHeight := Height;
   end;
  if FFocusFrameWidth <> 0 then
   FFocusFrameHeight    := round(FFocusFrameWidth/(Width/Height))
  else
   FFocusFrameHeight    := 0;
end;

procedure TMultiSwitch.CalculateButton;
var Factor : double;
    i      : integer;
begin
 if FBorderColor = clNone then i:=2 else i:=3;
 Factor       := (i* 100) / 26;
 FMargin      := round((Height / 100) * Factor);

 FButtonSize := round(((Height-(FFocusFrameHeight*2) )- (2 * FMargin)) * FImgSizeFactor);

 if FLoadFromFile then
  begin
   i := (Height - FButtonSize) div 2;
   FImgLeft          :=   i-FMargin;
   FImgTop           :=   i-FMargin;
  end;
end;

procedure TMultiSwitch.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
  if Assigned(OnKeyPress) then OnKeyPress(self,Key);
end;

procedure TMultiSwitch.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Assigned(OnKeyDown) then OnKeyDown(self,Key,Shift);
end;

procedure TMultiSwitch.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Assigned(OnKeyUp) then OnKeyUp(self,Key,Shift);
end;

procedure TMultiSwitch.CNKeyDown(var Message: TLMKeyDown);
begin
 with Message do begin
    Result := 1;
    case CharCode of
        VK_RETURN  : begin
                      if not FEnabled then exit;
                       if not FTimer.Enabled then
                        begin
                         if GroupIndex <> 0 then
                          if TheOnlyOne then exit;
                         FDirection := TDirection((ord(FDirection) + 1) mod 2);
                         FTimer.Enabled:= true;
                         if Assigned(OnChange) then OnChange(self);
                        end;
                      Invalidate;
                     end

      else begin
        Result := 0;
      end;
    end;
  end;

  inherited;
end;

procedure TMultiSwitch.DoExit;
begin
  inherited DoExit;
  if Assigned(OnExit) then OnExit(self);
end;

procedure TMultiSwitch.DoEnter;
begin
 inherited DoEnter;
 if Assigned(OnEnter) then OnEnter(self);
end;

procedure TMultiSwitch.SlideTheButton(aX: integer);
var l : integer;
    f : double;
begin
 //l        := width - Height;
 l := width - (2*FFocusFrameWidth) - (2*FMargin) - FButtonSize;

 if (aX > (l-round(l*0.1))) and (FDirection=fsLeft) then
  begin
   FDirection := fsRight;
   FPortion   := 1;
   FRollPos   := width - (FButtonSize + (2*FMargin)+(2*FFocusFrameWidth));
   FCaption   := FRightCaption;
   FAngel     := 360;
   FSlideEndPos := true;
   Invalidate;
   CheckTheGroup;
   if Assigned(OnChange) then OnChange(self);
   if Assigned(OnRight) then OnRight(self);
   if Assigned(FOnDirection) then OnDirection(self,fsRight);
   exit;
  end;

 if (aX < (round(l*0.1))) and (FDirection=fsRight) then
  begin
   FDirection := fsLeft;
   FPortion   := 0;
   FRollPos   := 0;
   FCaption   := FLeftCaption;
   FAngel     := 0;
   FSlideEndPos := true;
   Invalidate;
   CheckTheGroup;
   if Assigned(OnChange) then OnChange(self);
   if Assigned(OnLeft) then OnLeft(self);
   if Assigned(FOnDirection) then OnDirection(self,fsLeft);
   exit;
  end;

 if aX >= (l-round(l*0.1)) then exit;
 if aX <= (round(l*0.1)) then exit;
 FCaption := '';
 FSlideEndPos := false;
 f         := (aX * 100) / l;
 FPortion  := (1/100)*f;
 FRollPos  := aX;
 FAngel    := (360/100)*f;
end;

procedure TMultiSwitch.FTimerTimer(Sender: TObject);
var l,l1 : integer;
    f    : double;
begin
  FCaption := '';
  //l  := width - Height;
  l := width - (2*FFocusFrameWidth) - (2*FMargin) - FButtonSize;
  l1 := round(l / (360/FRotation));
  f  := 1 / (360/FRotation);
  if FDirection = fsRight then
   begin
    FPortion := FPortion + f;
    FRollPos := FRollPos + l1;
    FAngel :=FAngel+FRotation;
    if FAngel >= 360 then
     begin
      FPortion := 1;
      FTimer.Enabled:= false;
      FRollPos := width - (FButtonSize + (2*FMargin)+(2*FFocusFrameWidth));
      FCaption := FRightCaption;
      FAngel := 360;
      FSlideEndPos := true;
      if not FAbortSlide then
       CheckTheGroup;
      FAbortSlide := false;
      if Assigned(OnRight) then OnRight(self);
      if Assigned(FOnDirection) then OnDirection(self,fsRight);
     end;
   end;
   if FDirection = fsLeft then
   begin
    FPortion := FPortion - f;
    FRollPos := FRollPos - l1;
    FAngel := FAngel - FRotation;
    f  := 1 / (360/FRotation);
    if FAngel <= 0 then
     begin
      FPortion := 0;
      FTimer.Enabled:= false;
      FRollPos := 0 ;
      FCaption := FLeftCaption;
      FAngel := 0;
      FSlideEndPos := true;
      if not FAbortSlide then
       CheckTheGroup;
      FAbortSlide := false;
      if Assigned(OnLeft) then OnLeft(self);
      if Assigned(FOnDirection) then OnDirection(self,fsLeft);
     end;
   end;
   Invalidate;
 end;

procedure TMultiSwitch.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
 inherited;
 DoScaleFontPPI(Font, AToPPI, AProportion);
end;

{$IF LCL_FullVersion >= 2010000}
procedure TMultiSwitch.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited;
  DoFixDesignFontPPI(Font, ADesignTimePPI);
end;
{$IFEND}

function TMultiSwitch.CalculateTextRect: TRect;
begin
  if FDirection = fsLeft then
  begin
   Result.Left   := FFocusFrameWidth+FMargin + FButtonSize;
   Result.Top    := FFocusFrameHeight+FMargin;
   Result.Right  := Width - (FMargin + FFocusFrameWidth);
   Result.Bottom := Height - (FMargin + FFocusFrameHeight);
  end else
  begin
   Result.Left   := FFocusFrameWidth+FMargin;
   Result.Top    := FFocusFrameHeight+FMargin;
   Result.Right  := Width - (FFocusFrameWidth+FMargin+ FButtonSize);
   Result.Bottom := Height - (FMargin + FFocusFrameHeight);
  end;
end;

procedure TMultiSwitch.DrawTheFocusframe;
var tmpBmp  : TBitmap;
begin
 if not FEnabled then exit;
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
end;

procedure TMultiSwitch.DrawTheBackground;
var TmpBmp                     : TBitmap;
    TempImg1,TempImg2,TempImg3 : TLazIntfImage;
begin
 if (FImgLeftBgrdColor <> clNone) and (FRightBgrdColor <> clNone) then
  begin
   TempImg1 := FBackgroundImage.CreateIntfImage;
   TempImg2 := FBackgroundImage.CreateIntfImage;
   TempImg3 := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp  := TBitmap.Create;
   try
    ChangeColor(TempImg1,FImgLeftBgrdColor);
    ChangeColor(TempImg2,FRightBgrdColor);
    BlendImages(TempImg1,TempImg2,FPortion);
    TempImg3.SetSize(Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    StretchDrawImgToImg(TempImg1,TempImg3,width-(FFocusFrameWidth*2),height-(FFocusFrameHeight*2));
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg3);
    Canvas.Draw(0+FFocusFrameWidth,0+FFocusFrameHeight,TmpBmp);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TempImg3.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.DrawTheBorder;
var TmpBmp            : TBitmap;
    TempImg1,TempImg2 : TLazIntfImage;
begin
 if FBorderColor <> clNone then
  begin
   TempImg1       := FBorderImage.CreateIntfImage;
   TempImg2       := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp         := TBitmap.Create;
   try
    ChangeBorderColor(TempImg1,FBorderColor);
    TempImg2.SetSize(Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    StretchDrawImgToImg(TempImg1,TempImg2,Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg2);
    Canvas.Draw(0+FFocusFrameWidth,0+FFocusFrameHeight,TmpBmp);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.DrawForegroundFocused;
var TmpBmp            : TBitmap;
    TempImg1,TempImg2 : TLazIntfImage;
begin
 if not FForegroundFocusOn then exit;
 if Focused then
  if FEnabled then
  begin
   TempImg1       := FBackgroundImage.CreateIntfImage;
   TempImg2       := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp         := TBitmap.Create;
   try
    ChangeColor(TempImg1,FFocusColor);
    TempImg2.SetSize(TempImg1.Width,TempImg1.Height);
    AlphaImages(TempImg1,FFocusedBlendFaktor);
    TempImg2.SetSize(Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    StretchDrawImgToImg(TempImg1,TempImg2,Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg2);
    Canvas.Draw(0+FFocusFrameWidth,0+FFocusFrameHeight,TmpBmp);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.DrawTheButton;
var TmpBmp            : TBitmap;
    TempImg1,TempImg2 : TLazIntfImage;
begin
 if FButtonColor <> clNone then
  begin
   TempImg1       := FButtonImage.CreateIntfImage;
   TempImg2       := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp      := TBitmap.Create;
   try
    ChangeColor(TempImg1,FButtonColor);
    TempImg2.SetSize(FButtonSize,FButtonSize);
    StretchDrawImgToImg(TempImg1,TempImg2,FButtonSize,FButtonSize);
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg2);
    Canvas.Draw(FFocusFrameWidth+FMargin+FRollPos,FFocusFrameHeight+FMargin,TmpBmp);
    FHotspot := rect(FFocusFrameWidth+FMargin+FRollPos,FFocusFrameHeight+FMargin,
                     FFocusFrameWidth+FMargin+FRollPos+FButtonSize,FFocusFrameHeight+FMargin+FButtonSize);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.DrawTheRollbutton;
var TmpBmp                     : TBitmap;
    TempImg1,TempImg2,TempImg3 : TLazIntfImage;
begin
 if FButtonColor = clNone then
 if assigned(FImgLeftImage) and assigned(FRightImage) then
  begin
   TempImg1 := FImgLeftImage.CreateIntfImage;
   TempImg2 := FRightImage.CreateIntfImage;
   TempImg3 := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp   := TBitmap.Create;
   try
    BlendImages(TempImg1,TempImg2,FPortion);
    if FRoll then
     RotateImage(TempImg1,DegToRad(FAngel));
    TmpBmp.Pixelformat := pf32Bit;
    TempImg3.SetSize(FButtonSize,FButtonSize);
    StretchDrawImgToImg(TempImg1,TempImg3,FButtonSize,FButtonSize);
    TmpBmp.Assign(TempImg3);

    if FLoadFromFile and (FDirection = fsRight) and not FTimer.Enabled then
     FImgLeft := FImgLeft * -1 else FImgLeft := FImgTop;
    Canvas.Draw(FFocusFrameWidth+FImgLeft+FMargin+FRollPos,FFocusFrameHeight+FImgTop+FMargin,TmpBmp);
    FHotspot := rect(FFocusFrameWidth+FImgLeft+FMargin+FRollPos,FFocusFrameHeight+FImgTop+FMargin,
                     FFocusFrameWidth+FImgLeft+FMargin+FRollPos+FButtonSize,FFocusFrameHeight+FImgTop+FMargin+FButtonSize);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TempImg3.Free;
    TmpBmp.Free;
   end;
 end;
end;

procedure TMultiSwitch.FontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TMultiSwitch.DrawTheCaption;
var TeRect     : TRect;
begin
 TeRect := CalculateTextRect;
 if FBestTextHeight then FFont.Height := TeRect.Height - round(TeRect.Height * 0.35);
 Canvas.Font.Assign(FFont);
 canvas.TextRect(TeRect,TeRect.Left+FCapLeft,TeRect.Top+FCapTop,
                 FCaption,FTextStyle);
end;

procedure TMultiSwitch.DrawAHoverEvent;
var TmpBmp            : TBitmap;
    TempImg1,TempImg2 : TLazIntfImage;
begin
 if FHoverColor <> clNone then
  if FHover and FEnabled then
  begin
   TempImg1       := FBackgroundImage.CreateIntfImage;
   TempImg2       := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp         := TBitmap.Create;
   try
    ChangeColor(TempImg1,FHoverColor);
    TempImg2.SetSize(TempImg1.Width,TempImg1.Height);
    AlphaImages(TempImg1,FHoverBlendFaktor);
    TempImg2.SetSize(Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    StretchDrawImgToImg(TempImg1,TempImg2,Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg2);
    Canvas.Draw(0+FFocusFrameWidth,0+FFocusFrameHeight,TmpBmp);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.DrawNotEnabled;
var TmpBmp            : TBitmap;
    TempImg1,TempImg2 : TLazIntfImage;
begin
 if not Enabled then
  begin
   TempImg1       := FBackgroundImage.CreateIntfImage;
   TempImg2       := TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
   TmpBmp         := TBitmap.Create;
   try
    ChangeColor(TempImg1,FDisabledColor);
    TempImg2.SetSize(TempImg1.Width,TempImg1.Height);
    AlphaImages(TempImg1,FEnabledBlendFaktor);
    TempImg2.SetSize(Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    StretchDrawImgToImg(TempImg1,TempImg2,Width-(FFocusFrameWidth*2),Height-(FFocusFrameHeight*2));
    TmpBmp.PixelFormat:= pf32Bit;
    TmpBmp.Assign(TempImg2);
    Canvas.Draw(0+FFocusFrameWidth,0+FFocusFrameHeight,TmpBmp);
   Finally
    TempImg1.Free;
    TempImg2.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TMultiSwitch.Paint;
begin

 DrawTheFocusframe;

 CalculateBounds;

 CalculateButton;

 DrawTheBackground;

 DrawTheBorder;

 DrawForegroundFocused;

 DrawTheButton;

 DrawTheRollbutton;

 DrawTheCaption;

 DrawAHoverEvent;

 DrawNotEnabled;

end;

{$Include multiswitch_setter.inc}
{$Include msw_imageselector.inc}
end.
