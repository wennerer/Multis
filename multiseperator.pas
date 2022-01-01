unit MultiSeperator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, InfMultis;

type
  TGradientCourse = (gcHorizontal,gcVertical,gcSpread,gcRadiant,gcAlternate); //for background color

type

  { TMultiSeperator }

  TMultiSeperator = class(TGraphicControl)
  private
    FColor: TColor;

    FColorEnd: TColor;
    FColorStart: TColor;
    FGradient: TGradientCourse;
    FLineWidth: integer;
    procedure DrawTheBackground;

    procedure SetColor(AValue: TColor); reintroduce;
    procedure SetColorEnd(AValue: TColor);
    procedure SetColorStart(AValue: TColor);
    procedure SetGradient(AValue: TGradientCourse);
    procedure SetLineWidth(AValue: integer);

  protected

  public
   constructor Create(AOwner: TComponent); override;
   destructor  Destroy; override;
   procedure   Paint; override;
  published

   property Color :TColor read FColor write SetColor default clMaroon;

   property LineWidth : integer read FLineWidth write SetLineWidth default 2;



   //The start color of the background ( for color gradient)
   //Die Startfarbe des Hintergrundes (für Farbverlauf)
   property BackgrdColorStart : TColor  read FColorStart      write SetColorStart default clGray;
   //The end color of the background ( for color gradient)
   //Die Endfarbe des Hintergrundes (für Farbverlauf)
   property BackgrdColorEnd   : TColor  read FColorEnd   write SetColorEnd default clSilver;
   //The direction of the gradient
   //Die Richtung des Farbverlaufs
   property ColorGradient : TGradientCourse read FGradient write SetGradient default gcSpread;

   //property BorderSpacing;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multiseperator_icon.lrs}
  RegisterComponents('Multi',[TMultiSeperator]);
end;

{ TSeperator }

constructor TMultiSeperator.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 Width  := 10;
 Height := 40;
 FColor          := clMaroon;
 FLineWidth      := 2;
 FColorStart     := clGray;
 FColorEnd       := clSilver;
 FGradient       := gcSpread;


end;

destructor TMultiSeperator.Destroy;
begin
 inherited Destroy;


end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Setter---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


procedure TMultiSeperator.SetColor(AValue: TColor);
begin
 if FColor=AValue then Exit;
 FColor:=AValue;
 invalidate;
end;


procedure TMultiSeperator.SetColorEnd(AValue: TColor);
begin
  if FColorEnd=AValue then Exit;
  FColorEnd:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetColorStart(AValue: TColor);
begin
  if FColorStart=AValue then Exit;
  FColorStart:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetGradient(AValue: TGradientCourse);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  invalidate;
end;

procedure TMultiSeperator.SetLineWidth(AValue: integer);
begin
  if FLineWidth=AValue then Exit;
  FLineWidth:=AValue;
  invalidate;
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

 Gradient_Bmp(bkBmp,FColorStart,FColorEnd,ord(FGradient));

 trBmp := TBitmap.Create;
 trBmp.SetSize(Width,Height);
 trBmp.TransparentColor:=clblack;
 trBmp.Transparent:= true;
 trBmp.Canvas.Brush.Color:=clwhite;
 trBmp.Canvas.FillRect(0,0,Width,Height);
 trBmp.Canvas.Brush.Color:=clBlack;
 trBmp.Canvas.Rectangle(0,0,width,height);


 mask := TBitmap.Create;
 mask.SetSize(Width,Height);
 mask.Canvas.Brush.Color:=clwhite;
 mask.Canvas.FillRect(0,0,Width,Height);
 mask.Canvas.Brush.Color:=clBlack;
 mask.Canvas.Rectangle(0,0,width,height);

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


procedure TMultiSeperator.Paint;
begin
 inherited Paint;
 DrawTheBackground;



  canvas.Pen.Width:= FLineWidth;
  canvas.Pen.Color:= FColor;
  canvas.Line(width div 2,0,width div 2,height);
end;

end.
