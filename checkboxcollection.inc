(*This is a part of the TMultiCheckroup, last change 21.2.2023*)

{%MultiCheckGroup}

{ TMultiCheckboxCollection }


constructor TMultiCheckboxCollection.Create(aCollection: TMultiCheckGroup;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FMultiCheckGroup := aCollection;
end;

procedure TMultiCheckboxCollection.FontIsChanged(aHeight: integer);
begin
 (FMultiCheckGroup as TMultiCheckGroup).CheckBoxFontIsChanged(aHeight);
end;

procedure TMultiCheckboxCollection.TriggerAutoSize;
begin
 if not (csLoading in (FMultiCheckGroup as TMultiCheckGroup).ComponentState) then
  (FMultiCheckGroup as TMultiCheckGroup).TriggerAutoSize;
end;

function TMultiCheckboxCollection.GetCheckBox(Index: Integer): TMultiCheckBox;
begin
 result := TMultiCheckBox( inherited Items[Index] );
end;

function TMultiCheckboxCollection.GetEnabled: Boolean;
begin
 result := VisibleCount > 0;
end;

function TMultiCheckboxCollection.GetVisibleCount: Integer;
{$ifNdef newcols}
var
  i: Integer;
{$endif}
begin
  {$ifdef newcols}
  result := Count;
  {$else}
  result := 0;
  for i:=0 to Count-1 do
    if Items[i].Visible then
      inc(result);

  {$endif}
end;

procedure TMultiCheckboxCollection.SetCheckBox(Index: Integer;
  ACheckBox: TMultiCheckbox);
begin
 Items[Index].Assign(aCheckBox);
end;

function TMultiCheckboxCollection.GetOwner: TPersistent;
begin
  Result:= FMultiCheckGroup;
end;

