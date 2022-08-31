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

   setLength(FPolygon,0);

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

   for lv:=0 to 5 do
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
   Buttons[5].Caption:= rs_grid;

   FDrawing := false;
   FStart   := false;
   FShowGrid:= false;

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
 2 : Editor.Close;
 3 : LoadPolygon;
 4 : SavePolygon;
 5 : ShowAGrid;
 end;
end;

procedure TCustomPanelStyle.New;
begin
 FStart := true;   showmessage('New');
 FCount := 1;
 setLength(FPolygon,0);
 DrawPanel.Invalidate;
end;

procedure TCustomPanelStyle.VisitThePolygon;
var lv : integer;
begin
 FX_Min := 10000;
 FX_Max := 0;
 FY_Min := 10000;
 FY_Max := 0;

 for lv:= 0 to High(FPolygon) do
  begin
   if FPolygon[lv].X < FX_Min then FX_Min := FPolygon[lv].X;
   if FPolygon[lv].X > FX_Max then FX_Max := FPolygon[lv].X;
   if FPolygon[lv].Y < FY_Min then FY_Min := FPolygon[lv].Y;
   if FPolygon[lv].Y > FY_Max then FY_Max := FPolygon[lv].Y;
  end;//High
 if FX_Min < 0 then FX_Min := 0;
 if FY_Min < 0 then FY_Min := 0;

 FWidth := FX_Max - FX_Min;
 FHeight:= FY_Max - FY_Min;

end;

procedure TCustomPanelStyle.CurtailThePolygon; //Zuschneiden
var lv : integer;
begin
 for lv:= 0 to High(FPolygon) do
  begin
   FPolygon[lv].X := FPolygon[lv].X - FX_Min;
   FPolygon[lv].Y := FPolygon[lv].Y - FY_Min;
  end;
end;

procedure TCustomPanelStyle.Apply;
var  FCustomValues : TCustomStyleValues;
begin
 VisitThePolygon;      showmessage('apply');
 CurtailThePolygon;
 FCustomValues          := TCustomStyleValues.Create;
 try
  FCustomValues.FPolygon := copy(FPolygon);
  FCustomValues.FWidth   := FWidth;
  FCustomValues.FHeight  := FHeight;

  SetPtrValue(FCustomValues);
 finally
  FCustomValues.Free;
  Editor.Close;
 end;
end;

procedure TCustomPanelStyle.LoadPolygon;
var FStream: TFileStream;
    lv,i   : Integer;
    Dialog : TOpenDialog;
    AFilename : string;
    ArrayData: TArrayData;
begin
  setLength(FPolygon,0);
  Dialog  := TOpenDialog.Create(nil);
  try
   if Dialog.Execute then AFilename := Dialog.FileName;
  finally
   Dialog.Free;
  end;
  FStream := TFileStream.Create(AFilename, fmOpenRead);
   try
     FStream.Read(i, SizeOf(i));
     setlength(ArrayData,i+1);
     for lv := 0 to (High(ArrayData)) do
       FStream.Read(ArrayData[lv], SizeOf(ArrayData[lv]));
   finally
     FStream.Free;
   end;
   FPolygon := copy(ArrayData);
end;

procedure TCustomPanelStyle.SavePolygon;
var FStream: TFileStream;
    lv,i   : Integer;
    Dialog : TSaveDialog;
    AFilename : string;
begin
  Dialog  := TSaveDialog.Create(nil);
  try
   if Dialog.Execute then AFilename := Dialog.FileName;
  finally
   Dialog.Free;
  end;
  i := High(FPolygon);
  FStream := TFileStream.Create(AFilename, fmCreate);
  try
    FStream.Write(i,SizeOf (i));
    for lv := 0 to i do
      FStream.Write(FPolygon[lv], SizeOf(FPolygon[lv]));
  finally
    FStream.Free;
  end;
end;

procedure TCustomPanelStyle.ShowAGrid;
begin
  if FShowGrid then FShowGrid := false else FShowGrid := true;
  DrawPanel.Invalidate;
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
var lv : integer;
begin
 if FShowGrid then
  begin
   for Lv:=1 to 49 do
    begin
     if Frac(lv/10) = 0 then DrawPanel.Canvas.Pen.Color := clSilver else
      DrawPanel.Canvas.Pen.Color := clBackground;
     DrawPanel.Canvas.Line(lv*10,0,lv*10,DrawPanel.Height);
     DrawPanel.Canvas.Line(0,lv*10,DrawPanel.width,lv*10);
    end;
   DrawPanel.Canvas.Pen.Color := clDefault;
  end;

 if FDrawing then
  DrawPanel.Canvas.Polyline(FPolygon);
 if not FStart and not FDrawing then
  DrawPanel.Canvas.Polygon(FPolygon);

end;
