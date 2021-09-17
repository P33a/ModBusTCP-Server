(* Copyright (c) 2021  Paulo Costa
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in
     the documentation and/or other materials provided with the
     distribution.
   * Neither the name of the copyright holders nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.

  SPDX short identifier: BSD-3-Clause
*)

unit modbus;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Grids, Buttons, IniPropStorage,
  modbusTCP, lNet;

type

  { TFModBus }

  TFModBus = class(TForm)
    BModBusListen: TButton;
    BModBusDisconnect: TButton;
    BModbusOffsetSet: TButton;
    BModbusTest: TButton;
    CBModBusInputOverride: TCheckBox;
    CBModBusOutputOverride: TCheckBox;
    EditModBusInputOffset: TEdit;
    EditModBusOutputOffset: TEdit;
    EditModBusRegisterOffset: TEdit;
    EditModBusPort: TEdit;
    IniPropStorage: TIniPropStorage;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    LabelInBit0: TLabel;
    LabelInBit1: TLabel;
    LabelInBit10: TLabel;
    LabelInBit11: TLabel;
    LabelInBit12: TLabel;
    LabelInBit13: TLabel;
    LabelInBit14: TLabel;
    LabelInBit15: TLabel;
    LabelInBit2: TLabel;
    LabelInBit3: TLabel;
    LabelInBit4: TLabel;
    LabelInBit5: TLabel;
    LabelInBit6: TLabel;
    LabelInBit7: TLabel;
    LabelInBit8: TLabel;
    LabelInBit9: TLabel;
    LabelOutBit0: TLabel;
    LabelOutBit1: TLabel;
    LabelOutBit10: TLabel;
    LabelOutBit11: TLabel;
    LabelOutBit12: TLabel;
    LabelOutBit13: TLabel;
    LabelOutBit14: TLabel;
    LabelOutBit15: TLabel;
    LabelOutBit2: TLabel;
    LabelOutBit3: TLabel;
    LabelOutBit4: TLabel;
    LabelOutBit5: TLabel;
    LabelOutBit6: TLabel;
    LabelOutBit7: TLabel;
    LabelOutBit8: TLabel;
    LabelOutBit9: TLabel;
    MemoModBus: TMemo;
    ProgressBarModBus: TProgressBar;
    SGRegisters: TStringGrid;
    ShapeInput0: TShape;
    ShapeInput1: TShape;
    ShapeInput10: TShape;
    ShapeInput11: TShape;
    ShapeInput12: TShape;
    ShapeInput13: TShape;
    ShapeInput14: TShape;
    ShapeInput15: TShape;
    ShapeInput2: TShape;
    ShapeInput3: TShape;
    ShapeInput4: TShape;
    ShapeInput5: TShape;
    ShapeInput6: TShape;
    ShapeInput7: TShape;
    ShapeInput8: TShape;
    ShapeInput9: TShape;
    ShapeModBusState: TShape;
    ShapeOutput0: TShape;
    ShapeOutput1: TShape;
    ShapeOutput10: TShape;
    ShapeOutput11: TShape;
    ShapeOutput12: TShape;
    ShapeOutput13: TShape;
    ShapeOutput14: TShape;
    ShapeOutput15: TShape;
    ShapeOutput2: TShape;
    ShapeOutput3: TShape;
    ShapeOutput4: TShape;
    ShapeOutput5: TShape;
    ShapeOutput6: TShape;
    ShapeOutput7: TShape;
    ShapeOutput8: TShape;
    ShapeOutput9: TShape;
    TCPModBus: TLTCPComponent;
    procedure BModBusListenClick(Sender: TObject);
    procedure BModBusDisconnectClick(Sender: TObject);
    procedure BModbusOffsetSetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IniPropStorageRestoreProperties(Sender: TObject);
    procedure IniPropStorageSaveProperties(Sender: TObject);
    procedure LabelInBitDblClick(Sender: TObject);
    procedure LabelOutBitDblClick(Sender: TObject);
    procedure SGRegistersValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure ShapeInputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeOutputMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TCPModBusAccept(aSocket: TLSocket);
    procedure TCPModBusDisconnect(aSocket: TLSocket);
    procedure TCPModBusError(const msg: string; aSocket: TLSocket);
    procedure TCPModBusReceive(aSocket: TLSocket);
  private
    procedure FillLabelArray(ParentControl: TWinControl; ProtoName: string; var LabelArray: array of TLabel);
    procedure FillLEDArray(ParentControl: TWinControl; ProtoName: string; var LEDArray: array of TShape);
    procedure ModBusEvent(command: byte);

  public
    ModbusData: TModbusData;
    ModbusServer: TModbusServer;
    ModBusDisconnected, ModBusWantsToDisconnected: boolean;
    ModBusInLEDs, ModBusOutLEDs: array[0..15] of TShape;
    ModBusInLabels, ModBusOutLabels: array[0..15] of TLabel;
    ModBusInputsOffset, ModBusOutputsOffset, ModBusRegistersOffset: integer;

    InputLabels: array[0..ModBusBits - 1] of string;
    CoilLabels: array[0..ModBusBits - 1] of string;

    procedure ModbusRefreshLEDs;
    procedure ModbusRefreshRegisters;
  end;

var
  FModBus: TFModBus;

implementation

{$R *.lfm}


procedure TFModBus.ModBusEvent(command: byte);
begin
  //MemoModBus.Lines.Add(IntToStr(command));
end;

procedure TFModBus.FormShow(Sender: TObject);
begin
  BModbusOffsetSetClick(Sender);
end;

procedure TFModBus.IniPropStorageRestoreProperties(Sender: TObject);
var i: integer;
    s: string;
begin
  for i := 0 to ModBusBits - 1 do begin
    InputLabels[i] := IniPropStorage.ReadString('IL' + IntToStr(i), InputLabels[i]);
    CoilLabels[i] := IniPropStorage.ReadString('CL' + IntToStr(i), CoilLabels[i]);
  end;
end;

procedure TFModBus.IniPropStorageSaveProperties(Sender: TObject);
var i: integer;
begin
  for i := 0 to ModBusBits - 1 do begin
    if InputLabels[i] <> '' then begin
      IniPropStorage.WriteString('IL' + IntToStr(i), InputLabels[i]);
    end;
    if CoilLabels[i] <> '' then begin
      IniPropStorage.WriteString('CL' + IntToStr(i), CoilLabels[i]);
    end;
  end;

end;

procedure TFModBus.LabelInBitDblClick(Sender: TObject);
var LabelBit: TLabel;
  index: integer;
  s: string;
begin
  LabelBit := Sender as TLabel;
  index := LabelBit.Tag + ModBusInputsOffset;
  s := InputBox('New name', 'Please input new name', LabelBit.Caption);
  InputLabels[index] := s;
  BModbusOffsetSetClick(Sender);
end;

procedure TFModBus.LabelOutBitDblClick(Sender: TObject);
var LabelBit: TLabel;
  index: integer;
  s: string;
begin
  LabelBit := Sender as TLabel;
  index := LabelBit.Tag + ModBusInputsOffset;
  s := InputBox('New name', 'Please input new name', LabelBit.Caption);
  CoilLabels[index] := s;
  BModbusOffsetSetClick(Sender);
end;

procedure TFModBus.SGRegistersValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var idx: integer;
begin
  if NewValue <> OldValue then begin
    idx := ModBusRegistersOffset + aRow - 1;
    ModbusData.setHoldingRegister(idx, StrToInt(NewValue));
  end;
  ModbusRefreshRegisters();
end;

procedure TFModBus.ShapeInputMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LED: TShape;
    index: integer;
begin
  if not CBModBusInputOverride.Checked then exit;

  LED := Sender as TShape;
  index := LED.Tag + ModBusInputsOffset;

  ModbusData.SetInput(index, not ModbusData.getInput(index));

  ModbusRefreshLEDs();
end;

procedure TFModBus.ShapeOutputMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var LED: TShape;
    index: integer;
begin
  if not CBModBusOutputOverride.Checked then exit;

  LED := Sender as TShape;
  index := LED.Tag + ModBusOutputsOffset;

  ModbusData.SetCoil(index, not ModbusData.getCoil(index));

  ModbusRefreshLEDs();
end;

procedure TFModBus.TCPModBusAccept(aSocket: TLSocket);
begin
  ShapeModBusState.Brush.Color := clGreen;
end;

procedure TFModBus.TCPModBusDisconnect(aSocket: TLSocket);
begin
  ModBusDisconnected := true;
  ShapeModBusState.Brush.Color := clYellow;
end;

procedure TFModBus.TCPModBusError(const msg: string; aSocket: TLSocket);
begin
  MemoModBus.Lines.Add(msg);
end;

procedure TFModBus.TCPModBusReceive(aSocket: TLSocket);
var msg, s: String;
    i: integer;
begin
  TCPModBus.GetMessage(msg);
  if msg ='' then exit;

  ProgressBarModBus.Position := ProgressBarModBus.Position + 1;
  if ProgressBarModBus.Position >= ProgressBarModBus.Max then
    ProgressBarModBus.Position := 0;

  ModBusServer.MessageStateMachine(msg);

  if ModBusServer.response <> '' then begin
    TCPModBus.SendMessage(ModBusServer.response);
    ModBusServer.response := '';
    ModbusRefreshLEDs();
    ModbusRefreshRegisters();
  end;

  if ModBusServer.Frame.FunctionCode = $0F then begin
    ModbusRefreshLEDs();
    ModbusRefreshRegisters();
  end;

  {s := '';
  for i := 1 to length(ModBusServer.response) do begin
    s := s + IntToHex(ord(ModBusServer.response[i]), 2) + ' ';
  end;
  MemoModBus.Lines.Add('MSG:' + s);
  //MemoModBus.Lines.Add(format('Header: Len = %d Code = %d',[ModBusData.Header.LengthField, ModBusData.Header.FunctionCode]));
  ModBusServer.response := '';}
end;

procedure TFModBus.BModbusOffsetSetClick(Sender: TObject);
var i, idx: integer;
begin
  ModBusInputsOffset := strtointdef(EditModBusInputOffset.Text, 0);
  for i := low(ModBusInLabels) to high(ModBusInLabels) do begin
    idx := i + ModBusInputsOffset;
    if InputLabels[idx] = '' then begin
      ModBusInLabels[i].Caption := 'Bit ' + inttostr(idx);
    end else begin
      ModBusInLabels[i].Caption := InputLabels[idx];
    end;
  end;

  ModBusOutputsOffset := strtointdef(EditModBusOutputOffset.Text, 0);
  for i := low(ModBusOutLabels) to high(ModBusOutLabels) do begin
    idx := i + ModBusOutputsOffset;
    if CoilLabels[idx] = '' then begin
      ModBusOutLabels[i].Caption := 'Bit ' + inttostr(idx);
    end else begin
      ModBusOutLabels[i].Caption := CoilLabels[idx];
    end;
  end;

  ModBusRegistersOffset := strtointdef(EditModBusRegisterOffset.Text, 0);
  for i := 0 to SGRegisters.RowCount - 2 do begin
    SGRegisters.Cells[0, i + 1] := IntToStr(i + ModBusRegistersOffset);
  end;

  ModbusRefreshLEDs();
  ModbusRefreshRegisters();
end;

procedure TFModBus.FormCreate(Sender: TObject);
begin
  ModbusData := TModbusData.Create;
  ModbusServer := TModbusServer.Create(ModbusData, @ModBusEvent);

  FillLabelArray(FModBus, 'LabelInBit', ModBusInLabels);
  FillLabelArray(FModBus, 'LabelOutBit', ModBusOutLabels);
  FillLEDArray(FModBus, 'ShapeInput', ModBusInLEDs);
  FillLEDArray(FModBus, 'ShapeOutput', ModBusOutLEDs);
end;

procedure TFModBus.FormDestroy(Sender: TObject);
begin
  ModbusData.Free;
  ModbusServer.Free;
end;

procedure TFModBus.BModBusListenClick(Sender: TObject);
begin
  if TCPModBus.Connected then exit;
  ShapeModBusState.Brush.Color := clYellow;
  TCPModBus.Listen(StrToInt(EditModBusPort.Text));
end;

procedure TFModBus.BModBusDisconnectClick(Sender: TObject);
begin
  TCPModBus.Disconnect(false);
end;


procedure TFModBus.ModbusRefreshLEDs;
var i: integer;
    OnOffColors: array[boolean] of TCOlor;
begin
  OnOffColors[true] := clred;
  OnOffColors[false] := $000030;

  for i := low(ModBusInLEDs) to high(ModBusInLEDs) do begin
    ModBusInLEDs[i].Brush.Color := OnOffColors[ModbusData.getInput(i + ModBusInputsOffset)];
  end;
  for i := low(ModBusOutLEDs) to high(ModBusOutLEDs) do begin
    ModBusOutLEDs[i].Brush.Color := OnOffColors[ModbusData.getCoil(i + ModBusOutputsOffset)];
  end;
end;


procedure TFModBus.ModbusRefreshRegisters;
var i: integer;
begin
  for i := 0 to SGRegisters.RowCount - 2 do begin
    SGRegisters.Cells[1, i + 1] := IntToStr(ModbusData.getHoldingRegister(i + ModBusRegistersOffset));
  end;
end;


procedure TFModBus.FillLabelArray(ParentControl: TWinControl; ProtoName: string; var LabelArray: array of TLabel);
var i, cnt: integer;
begin
  cnt := low(LabelArray);
  for i := 0 to ParentControl.ControlCount-1 do begin
    if cnt > high(LabelArray) then exit;
    if LowerCase(ParentControl.Controls[i].Name) = LowerCase(ProtoName + inttostr(cnt)) then begin
      LabelArray[cnt] := TLabel(ParentControl.Controls[i]);
      inc(cnt);
    end;
  end;
end;


procedure TFModBus.FillLEDArray(ParentControl: TWinControl;  ProtoName: string; var LEDArray: array of TShape);
var i, cnt: integer;
begin
  cnt := low(LEDArray);
  for i := 0 to ParentControl.ControlCount-1 do begin
    if cnt > high(LEDArray) then exit;
    if LowerCase(ParentControl.Controls[i].Name) = LowerCase(ProtoName + inttostr(cnt)) then begin
      LEDArray[cnt] := TShape(ParentControl.Controls[i]);
      inc(cnt);
    end;
  end;
end;

end.

