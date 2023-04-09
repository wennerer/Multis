unit MultiEventLine;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf;

type
  TMultiEventLine      = class; //TCustomControl , die eigentliche Komponente
  TMultiEvent           = class; //TCollectionItem, der einzelne Kreis


//xxxxxxxxxxxxxxxxxxxxxxxxxx---Die Kollektion der Kreise---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TMultiEventCollection }

  TMultiEventCollection = class(TCollection)
  private
   FDrawEvent : TMultiEventLine;
   function GetEvent(Index: Integer): TMultiEvent;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetEvent(Index: Integer; aEvent: TMultiEvent);
  protected
   function GetOwner: TPersistent; override;
  public
   constructor Create(aCollection: TMultiEventLine; aItemClass: TCollectionItemClass);
   property Items[Index: Integer]: TMultiEvent read GetEvent write SetEvent; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Der einzelne Kreis---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type
  TMultiEvent = class(TCollectionItem)
   private
    FEvents     : TCollection;
    FColor: TColor;
    FDiameter: integer;
    FEnabled: boolean;
    FVisible     : Boolean;
    FDisplayName : string;
    FLeft        : integer;
    FTop         : integer;

    procedure SetEnabled(AValue: boolean);
    procedure SetVisible(AValue: Boolean);
   protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function GetOwner: TPersistent; override;
   public
    constructor Create(ACollection: TCollection); override;
   published
    property Color    : TColor read FColor write FColor;
    property Diameter : integer read FDiameter write FDiameter;
    property Enabled  : boolean read FEnabled write SetEnabled default true;
    property Visible  : Boolean read FVisible write SetVisible default true;
    property Left     : integer read FLeft write FLeft;
   property Top       : integer read FTop write FTop;
  end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Die eigentliche Komponente---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TMultiEventLine}

  TMultiEventLine= class(TCustomControl)
  private
   FEventCollection     : TMultiEventCollection;
  protected
   function CreateEvents: TMultiEventCollection;
   function GetEvent: TMultiEventCollection;
   function IsEventStored: Boolean;
   procedure SetEvent(AEventCollection: TMultiEventCollection);
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
  published
   property Events : TMultiEventCollection read GetEvent write SetEvent stored IsEventStored;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multieventline_icon.lrs}
  RegisterComponents('Multi',[TMultiEventLine]);
end;

{ TMultiEventLine}

constructor TMultiEventLine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left  :=  20;
  Top   :=  20;
  Width := 150;
  Height:= 150;

  FEventCollection := CreateEvents;  //TCollection
  FEventCollection.Add;
end;

function TMultiEventLine.CreateEvents: TMultiEventCollection;
begin
  result := TMultiEventCollection.Create(Self, TMultiEvent);
end;

function TMultiEventLine.GetEvent: TMultiEventCollection;
begin
 result := FEventCollection;
end;

function TMultiEventLine.IsEventStored: Boolean;
begin
 result := Events.Enabled;
end;

procedure TMultiEventLine.SetEvent(AEventCollection: TMultiEventCollection);
begin
  FEventCollection.Assign(AEventCollection);
end;


destructor TMultiEventLine.Destroy;
begin
  inherited Destroy;
  FEventCollection.Free;
end;

procedure TMultiEventLine.Paint;
var lv : integer;
begin
  inherited Paint;
  canvas.Brush.Color:=clWhite;
  canvas.FillRect(0,0,width,height);


end;


{$Include Eventcollection.inc}
{$Include Eventitem.inc}
end.
