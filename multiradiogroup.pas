unit MultiRadioGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Contnrs, LResources, Forms, Controls, Graphics,
  Dialogs, infmultis, LCLProc, LCLIntf;

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
   property Items[Index: Integer]: TMRadioButton read GetRadioButton write SetRadioButton; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;


 type
  TMRadioButton = class(TCollectionItem)
   private
     FButtonColor: TColor;
     FButtonSelColor: TColor;
     FDisabledAlpBV: integer;
     FDisabledColor: TColor;
     FEnabled: boolean;
     FHoverColor: TColor;
     FRadioButtons : TCollection;
     FCaptionChange     : boolean;
     FCaptionWordbreak: boolean;
     FCapLeft: integer;
     FCaption: TCaption;
     FCapTop: integer;
     FColor: TColor;
     FFont: TFont;
     FHeight: integer;
     FParentFont: boolean;
     FSelected: Boolean;
     FTextStyle: TTextStyle;
     FVisible: Boolean;
     FWidth: integer;
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
     procedure SetLayout(AValue: TTextLayout);
     procedure SetParentFont(AValue: boolean);
     procedure SetSelected(AValue: Boolean);
     procedure SetTextStyle(AValue: TTextStyle);
     procedure SetVisible(AValue: Boolean);

   protected
     function GetOwner: TPersistent; override;
     procedure RadioButtonFontChanged(Sender : TObject);
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
   published

    property Caption  : TCaption read FCaption write SetCaption;

    property Color    : TColor read FColor write SetColor default clNone;

    property ButtonColor : TColor read FButtonColor write SetButtonColor default clWhite;

    property ButtonSelColor : TColor read FButtonSelColor write SetButtonSelColor default clBlack;

    property Selected : Boolean read FSelected write SetSelected;

    property HoverColor : TColor read FHoverColor write SetHoverColor default clSilver;
    //The font to be used for text display the caption.
    //Die Schrift die für die Textanzeige der Caption verwendet werden soll.
    property Font: TFont read FFont write SetFont;

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
   end;






type

  { TMultiRadioGroup }

  TMultiRadioGroup = class(TCustomControl)
  private
    FCaption: TCaption;
    FFont: TFont;
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

    function CreateRadioButtons: TMRadioButtons;
    function GetRadioButton: TMRadioButtons;
    function GetTextHeight(AText: String; AFont: TFont): Integer;
    function GetTextWidth(AText: String; AFont: TFont): Integer;

    function IsRadioButtonsStored: Boolean;
    procedure SetCaption(AValue: TCaption);
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
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
   procedure CalculateRadioGroup(var aRect: TRect);
   procedure DrawRadioGroup;
   procedure DrawRadioButtons;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure RadioButtonFontIsChanged(aHeight : integer);
   procedure SetAllNotSelected(aIndex : integer);
   procedure Loaded; override;
   procedure MouseEnter; override;
   procedure MouseLeave; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);override;


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

   property RadioButtons : TMRadioButtons read GetRadioButton write SetRadioButton stored IsRadioButtonsStored;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiradiogroup_icon.lrs}
  RegisterComponents('Multi',[TMultiRadioGroup]);
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
  CalculateRadioGroup(FRadioGroupBounds);
end;

procedure TMultiRadioGroup.MouseEnter;
begin
  inherited MouseEnter;
end;

procedure TMultiRadioGroup.MouseLeave;
begin
  inherited MouseLeave;
end;

procedure TMultiRadioGroup.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

end;


procedure TMultiRadioGroup.MouseMove(Shift: TShiftState; X, Y: Integer);
var lv                    : integer;
begin
  inherited MouseMove(Shift, X, Y);
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
 if parent.Visible then setfocus;
  for lv := 0 to pred(RadioButtons.Count) do
   if RadioButtons.Items[lv].FEnabled then
    if PtInRect(RadioButtons.Items[lv].FHotspot,Point(x,y)) then
     RadioButtons.Items[lv].Selected:= true;
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


procedure TMultiRadioGroup.RadioButtonFontIsChanged(aHeight: integer);
var lv : integer;
begin
 for lv := 0 to pred(RadioButtons.Count) do
  begin
   RadioButtons.Items[lv].Font.Height:=aHeight;
   if RadioButtons.Items[lv].Font.Height <> 0 then RadioButtons.Items[lv].ParentFont:= false;
  end;
end;

procedure TMultiRadioGroup.SetAllNotSelected(aIndex: integer);
var lv : integer;
begin
 for lv := 0 to pred(RadioButtons.Count) do
  if lv <> aIndex then
   RadioButtons.Items[lv].Selected:= false;
 Invalidate;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXX--- Setter MultiRadioGroup---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
procedure TMultiRadioGroup.SetFocusAlBlVal(AValue: byte);
begin
  if FFocusAlBlVal=AValue then Exit;
  FFocusAlBlVal:=AValue;
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
  Invalidate;
end;

procedure TMultiRadioGroup.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
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



//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Drawing---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TMultiRadioGroup.CalculateRadioGroup(var aRect: TRect);
begin
 aRect :=  rect(FFocusFrameWidth,FFocusFrameWidth,width-FFocusFrameWidth,height-FFocusFrameWidth);
end;

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
   if not RadioButtons.Items[lv].FParentFont then
    Canvas.Font.Assign(RadioButtons.Items[lv].FFont)
   else
    Canvas.Font.Assign(FFont);

   TRH := GetTextHeight(RadioButtons.Items[lv].FCaption,Canvas.Font);

   Space := (Height-((FocusFrameWidth*2)+CaptionHeight+(RadioButtons.Count*TRH))) div (RadioButtons.Count+1);

   TeRect:= rect(10+FocusFrameWidth +(TRH-2),CaptionHeight+(Space*(lv+1))+(lv*TRH)+FocusFrameWidth,
                 Width-FocusFrameWidth,CaptionHeight+(Space*(lv+1))+TRH+(lv*TRH)+FocusFrameWidth);


   ButRect := rect(10+FocusFrameWidth,TeRect.Top+2,
                  10+FocusFrameWidth+(TRH-4),TeRect.Bottom-2);

   SelRect := rect(ButRect.Left+round(TRH * 0.2),ButRect.Top+round(TRH * 0.2),
                   ButRect.Right-round(TRH * 0.2),ButRect.Bottom-round(TRH * 0.2));

   RadioButtons.Items[lv].FHotspot := rect(FocusFrameWidth,TeRect.Top,TeRect.Right,TeRect.Bottom);

    if RadioButtons.Items[lv].FColor <> clNone then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FColor;
      Canvas.FillRect(RadioButtons.Items[lv].FHotspot);
     end;


    if RadioButtons.Items[lv].FHover then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FHoverColor;
      Canvas.FillRect(RadioButtons.Items[lv].FHotspot);
     end;

    canvas.Brush.Color:= RadioButtons.Items[lv].FButtonColor;
    canvas.Ellipse(ButRect);
    if RadioButtons.Items[lv].Selected then
     begin
      canvas.Brush.Color:= RadioButtons.Items[lv].FButtonSelColor;
      canvas.Ellipse(SelRect);
     end;

    //ItemCaption
    if not RadioButtons.Items[lv].FCaptionChange then
     RadioButtons.Items[lv].FCaption := 'Radiobutton ' + inttostr(RadioButtons.Items[lv].Index+1);





   canvas.TextRect(TeRect,TeRect.Left+RadioButtons.Items[lv].FCapLeft,TeRect.Top+RadioButtons.Items[lv].FCapTop,
                 RadioButtons.Items[lv].FCaption,RadioButtons.Items[lv].FTextStyle);

   //Enable
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




 (*Canvas.Brush.Color:=RadioButtons.Items[0].Color;
 canvas.FillRect(5,50,RadioButtons.Items[0].Width+5,60);
 Canvas.Pen.Color:=clBlack;
 canvas.Ellipse(5,2,21,18);  *)



 //debugln('paint');
end;



{$Include mrg_radiobuttonitem.inc}

end.
