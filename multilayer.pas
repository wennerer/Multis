{ <This component provides a page that can also be made invisible at design time.>

  Copyright (C) <Version 1.0.0.1 29.12.2022> <Bernd Hübner>

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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, multipanel;

type

  { TMultiLayer }

  TMultiLayer = class(TCustomControl)
  private

  protected
   procedure SetVisible(Value: Boolean);override;
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
   property Visible;
   property Color;
   property Align;
   property Anchors;
   property BorderSpacing;
   property Constraints;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I multilayer_icon.lrs}
  RegisterComponents('Multi',[TMultiLayer]);
end;

{ TMultiLayer }

procedure TMultiLayer.SetVisible(Value: Boolean);
begin
 if Value then ControlStyle := ControlStyle + [csAcceptsControls]-[csNoDesignVisible] else
   ControlStyle := ControlStyle + [csAcceptsControls,csNoDesignVisible];
 inherited SetVisible(Value);
 InvalidateMultiPanel;
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
