(*This is a part of the TMultiPanel*)

{%MultiPanel ,this is a part of MultiPanel last change 24.01.2022}


//PropertyEditor
{ TCustomPanelStyle }

procedure TCustomPanelStyle.Edit;
begin
  inherited Edit;
  DoShowEditor;
end;

function TCustomPanelStyle.GetValue: string;
begin
  //Result:=inherited GetValue;
  result := 'TCustomPanelStyle';
end;

function TCustomPanelStyle.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  Result := [paMultiSelect, paDialog];
end;

procedure TCustomPanelStyle.DoShowEditor;
var lv : integer;
begin
 try
   Editor                := TForm.Create(Application);
   Editor.Width          := 715;
   Editor.Height         := 510;
   Editor.Left           := (screen.Width div 2) - (Editor.Width div 2);
   Editor.Top            := (screen.Height div 2) -(Editor.Height div 2);
   Editor.BorderStyle    := bsSingle;
   Editor.Caption        := 'CustomPanelStyleEditor';

   //FCustomValues         := TCustomStyleValues.Create;

   DrawPanel             := TPanel.Create(nil);
   DrawPanel.Parent      := Editor;
   DrawPanel.SetBounds(5,5,500,500);
   DrawPanel.Color       := clWhite;
   DrawPanel.BevelInner  := bvLowered;
   DrawPanel.BevelOuter  := bvRaised;
   DrawPanel.OnPaint     := @DrawPanelPaint;
   DrawPanel.OnMouseDown := @DrawPanelMouseDown;
   DrawPanel.OnMouseMove := @DrawPanelMouseMove;
   DrawPanel.OnMouseUp   := @DrawPanelMouseUp;

   for lv:=0 to 4 do
    begin
     Buttons[lv]           := TButton.Create(nil);
     Buttons[lv].Parent    := Editor;
     Buttons[lv].Left      := 510;
     Buttons[lv].Width     := 200;
     Buttons[lv].Height    :=  30;
     Buttons[lv].Top       := 5 + (lv*(Buttons[lv].Height+5));
     Buttons[lv].OnClick   := @ButtonsOnClick;
     Buttons[lv].Tag       := lv;
    end;
   Buttons[0].Caption:= rs_new;
   Buttons[1].Caption:= rs_apply;
   Buttons[2].Caption:= rs_cancel;
   Buttons[3].Caption:= rs_load;
   Buttons[4].Caption:= rs_save;



    Editor.ShowModal;
 finally
   //FCustomValues.Free;
   for lv:=0 to 4 do Buttons[lv].Free;
   DrawPanel.Free;
   Editor.Free;
 end;

end;

procedure TCustomPanelStyle.ButtonsOnClick(Sender: TObject);
begin
 case (Sender as TButton).Tag of
 0 : New;
 1 : Apply;
 end;
end;

procedure TCustomPanelStyle.New;
begin
 FStart := true;
 FCount := 1;
 setLength(FPolygon,0);
 DrawPanel.Invalidate;
end;

procedure TCustomPanelStyle.CurtailTheDrawing; //Beschneiden
var lv : integer;
begin
 for lv := 0 to pred(length(FPolygon)) do
  begin
   //FPolygon[lv].x
  end;
end;

procedure TCustomPanelStyle.Apply;
var  FCustomValues : TCustomStyleValues;
begin
 //CurtailTheDrawing;
 FCustomValues          := TCustomStyleValues.Create;
 try
  FCustomValues.FPolygon := copy(FPolygon);

  SetPtrValue(FCustomValues);
 finally
  FCustomValues.Free;
  Editor.Close;
 end;
end;

procedure TCustomPanelStyle.DrawPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FStart then
  begin
   FDrawing := true;
   setLength(FPolygon,FCount);
   FPolygon[FCount-1].x:=x;
   FPolygon[FCount-1].y:=y;
   FStartPoint.X:=x;
   FStartPoint.Y:=y;
   DrawPanel.Invalidate;
  end;
end;

procedure TCustomPanelStyle.DrawPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if FDrawing then
  begin
   inc(FCount);
   setLength(FPolygon,FCount);
   FPolygon[FCount-1].x:=x;
   FPolygon[FCount-1].y:=y;
   DrawPanel.Invalidate;
  end;
end;

procedure TCustomPanelStyle.DrawPanelMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if FDrawing then
  begin
   inc(FCount);
   setLength(FPolygon,FCount);
   FPolygon[FCount-1].x:=x;
   FPolygon[FCount-1].y:=y;
   inc(FCount);
   setLength(FPolygon,FCount);
   FPolygon[FCount-1].x:=FStartPoint.X;
   FPolygon[FCount-1].y:=FStartPoint.Y;
   FDrawing := false;
   FStart := false;
   DrawPanel.Invalidate;
  end;
end;

procedure TCustomPanelStyle.DrawPanelPaint(Sender: TObject);
begin
 if FDrawing then
  DrawPanel.Canvas.Polyline(FPolygon);
 if not FStart and not FDrawing then
  DrawPanel.Canvas.Polygon(FPolygon);

end;
