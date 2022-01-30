(*This is a part of the TMultiPanel*)

{%MultiPanel ,this is a part of MultiPanel last change 25.01.2022}

{ TDDMenu }

constructor TDDMenu.create(aOwner: TCustomPanel);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TDDMenu.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TDDMenu.SetTrigger(AValue: TTrigger);
begin
  if FTrigger=AValue then Exit;
  FTrigger:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Compressed---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


{ TComp }


constructor TComp.create(aOwner: TCustomPanel);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TComp.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TComp.SetLeft(AValue: integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TComp.SetTop(AValue: integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TComp.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  (FOwner as TMultiPanel).SetDropDownMenu(self,aValue);
end;

procedure TComp.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;


//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---Stretched---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


{ TStre }


constructor TStre.create(aOwner: TCustomPanel);
begin
 FOwner:=aOwner;
 inherited create;
 if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStre.SetHeight(AValue: integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStre.SetLeft(AValue: integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStre.SetTop(AValue: integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;

procedure TStre.SetActive(AValue: boolean);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
  (FOwner as TMultiPanel).SetDropDownMenu(self,aValue);
end;

procedure TStre.SetWidth(AValue: integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if Assigned(FOwner) then FOwner.Invalidate;
end;
