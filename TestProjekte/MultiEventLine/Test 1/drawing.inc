{%Unit1}


procedure TMainForm.MultiButtonClick(Sender: TObject);
begin
 if Sender = MultiButton1 then Choice := Line;
 if Sender = MultiButton2 then Choice := Rectangle;
 if Sender = MultiButton3 then Choice := Polygone;
end;

procedure TMainForm.SpinEdit1Change(Sender: TObject);
begin
 if Sender = SpinEdit1 then PenWidth := SpinEDit1.Value;
end;


procedure TMainForm.ColorBox1Change(Sender: TObject);
begin
 PenColor := ColorBox1.Selected;
end;

procedure TMainForm.ColorBox2Change(Sender: TObject);
begin
  BrushColor := ColorBox2.Selected;
end;




procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 draw := true;
 if (Choice = Line) or (Choice = Rectangle) then
  begin
   sx := x;
   sy := y;
   ex := x;
   ey := y;
  end;
 if choice = Polygone then
  begin
   if not start then
    begin
     count := 0; Points := nil;
     setlength(Points,count+1);
     Points[count].X:= x;
     Points[count].Y:= y;
     inc(Count);
     start := true;
    end else inc(count);
  end;
 invalidate;
end;

procedure TMainForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if not draw then exit;
 if (Choice = Line) or (Choice = Rectangle) then
  begin
   ex := x;
   ey := y;
  end;
 if choice = Polygone then
  begin
   setlength(Points,count+1);
   Points[count].X:= x;
   Points[count].Y:= y;
  end;
 invalidate;
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if choice = Polygone then exit;
 draw := false;
end;


procedure TMainForm.FormDblClick(Sender: TObject);
begin
 if choice = Polygone then
  begin
   draw := false;
   start := false;
  end;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
 Canvas.Pen.Color:= PenColor;
 Canvas.Pen.Width:= PenWidth;
 Canvas.Brush.Color:= BrushColor;

 if Choice = Line then canvas.Line(sx,sy,ex,ey);
 if Choice = Rectangle then canvas.Rectangle(sx,sy,ex,ey);
 if choice = Polygone then canvas.Polygon(Points);
end;

