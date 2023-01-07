{ <This component provides a page that can also be made invisible at design time.>

  Copyright (C) <Version 1.0.0.2 06.01.2023> <Bernd Hübner>

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


unit MultiLayer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LMessages,
  ComponentEditors, multipanel;

type

  { TMultiLayer }

  TMultiLayer = class(TCustomControl)
  private
    FGroupIndex: integer;
    procedure SetGroupIndex(AValue: integer);

  protected
   procedure SetVisible(Value: Boolean);override;
   procedure SetToVisible;
   procedure CheckTheGroup;
   procedure CheckParent;
   procedure WMShowWindow(var Message: TLMShowWindow); message LM_SHOWWINDOW;
  public
   ParentIsMultiPanel : boolean;
   FMultiBkgrdBmp     : TBitmap;
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure InvalidateMultiPanel;
   procedure Paint; override;

  published
   property Width;
   property Height;
   //also as designtime
   property Visible;
   property Color;
   property Align;
   property Anchors;
   property BorderSpacing;
   property Constraints;
   //The index of the group to which the MultiLayer belongs
   //Der Index der Gruppe zu der der MultiLayer gehört
   property GroupIndex : integer read FGroupIndex write SetGroupIndex default 0;
  end;

procedure Register;

implementation

type
 { TSwitchLayerComponent }

  TSwitchLayerComponent = class (TComponentEditor)
  private
   FOwner : TComponent;
  protected

  public
   constructor Create(AComponent: TComponent;ADesigner: TComponentEditorDesigner); override;
   procedure Edit; Override;
   function GetVerbCount: Integer; override;
   function GetVerb(Index: Integer): string; override;
   procedure ExecuteVerb(Index: Integer); override;

  end;


procedure Register;
begin
  {$I multilayer_icon.lrs}
  RegisterComponents('Multi',[TMultiLayer]);
  RegisterComponentEditor(TMultiLayer,TSwitchLayerComponent);
end;


constructor TSwitchLayerComponent.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FOwner := AComponent;
end;

procedure TSwitchLayerComponent.Edit;
begin
  //inherited Edit;
 (FOwner as TMultiLayer).SetToVisible;
end;

function TSwitchLayerComponent.GetVerbCount: Integer;
begin
  //Result:=inherited GetVerbCount;
 result := 2;
end;

function TSwitchLayerComponent.GetVerb(Index: Integer): string;
begin
  //Result:=inherited GetVerb(Index);
 case Index of
  0: Result := 'Set this Layer to visible';
  1: Result := 'Info';

 end;
end;

procedure TSwitchLayerComponent.ExecuteVerb(Index: Integer);
begin
 // inherited ExecuteVerb(Index);
 case Index of
    0: Edit;
    1: MessageDlg ('This component provides a page'#13 +
      'that can also be made invisible'#13 +
      'at design time!', mtInformation, [mbOK], 0);
 end;
end;








//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---TMultiLayer---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

{ TMultiLayer }

procedure TMultiLayer.SetGroupIndex(AValue: integer);
begin
  if FGroupIndex=AValue then Exit;
  FGroupIndex:=AValue;
end;

procedure TMultiLayer.SetVisible(Value: Boolean);
begin
 if Value then CheckTheGroup;
 if Value then ControlStyle := ControlStyle + [csAcceptsControls]-[csNoDesignVisible] else
   ControlStyle := ControlStyle + [csAcceptsControls,csNoDesignVisible];
 inherited SetVisible(Value);
 InvalidateMultiPanel;
end;

procedure TMultiLayer.SetToVisible;
begin
 Visible:= true;
 self.SetFocus;
end;

procedure TMultiLayer.CheckTheGroup;
 var lv : integer;
 begin

   for lv :=  0 to pred(Parent.ControlCount) do
    if (Parent.Controls[lv] <> self) then
    if (Parent.Controls[lv] is TMultiLayer) then
     if TMultiLayer(Parent.Controls[lv]).FGroupIndex = FGroupIndex then
      //TMultiLayer(Parent.Controls[lv]).Visible:= false;
 end;

procedure TMultiLayer.CheckParent;
 var exitflag : boolean;
    CurControl  : TWinControl;
    i : integer;
begin
 if Parent is TMultiLayer then
  begin
   CurControl := Parent;
   i:=0;
   repeat
    if CurControl is TMultiLayer then
     begin
      exitflag := false;
      CurControl := CurControl.Parent;
     end
    else
     exitflag := true;
    inc(i);
   until (i>100) or (exitflag =true) ;
   if (CurControl is TWinControl) then Parent := CurControl;
  end;
end;


procedure TMultiLayer.WMShowWindow(var Message: TLMShowWindow);
begin
 if (csDesigning in Componentstate) then CheckParent;
end;

constructor TMultiLayer.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
  Width := 30;
  Height:= 30;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Align := alClient;
  FMultiBkgrdBmp := TBitmap.Create;
end;

destructor TMultiLayer.Destroy;
begin
  FMultiBkgrdBmp.Free;
  inherited Destroy;
end;

procedure TMultiLayer.InvalidateMultiPanel;
begin
 if Parent is TMultiPanel then (Parent as TMultiPanel).Invalidate;
end;

procedure TMultiLayer.Paint;
begin
  inherited Paint;

  if Parent is TMultiPanel then
  begin
   if assigned((Parent as TMultiPanel).FMultiBkgrdBmp) then
    begin
     canvas.CopyRect(rect(0,0,width,height),(Parent as TMultiPanel).FMultiBkgrdBmp.Canvas,rect(left,top,left+width,top+height));
     FMultiBkgrdBmp.Assign((Parent as TMultiPanel).FMultiBkgrdBmp);
     ParentIsMultiPanel := true;
    end;
  end;
end;


end.
