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

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, IniPropStorage, modbus, config, userparams;


type

  TCoils = record
    LED_ON: boolean;
    LED_Ready: boolean;
  end;

  TInputs = record
    Go, Stop: boolean;
  end;



  { TFMain }

  TFMain = class(TForm)
    BShowConfig: TButton;
    BShowIO: TButton;
    IniPropStorage: TIniPropStorage;
    PaintBox: TPaintBox;
    StatusBar: TStatusBar;
    Timer: TTimer;
    procedure BShowConfigClick(Sender: TObject);
    procedure BShowIOClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private

  public
    Inputs: TInputs;
    Coils, PrevCoils: TCoils;

    errorMessage: string;

    miliseconds: QWord;
    count: integer;

    Go, Ready: TUserParameter;

    procedure InitScene;

    procedure SetError(err: string);
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}


{ TFMain }


procedure TFMain.TimerTimer(Sender: TObject);
begin
  FConfig.UpdateParTable;

  PrevCoils := Coils;
  // ReadModBus
  Coils.LED_ON := FModBus.ModbusServer.Data.getCoil(0);
  Coils.LED_Ready := FModBus.ModbusServer.Data.getCoil(1);

  // Process
  Inputs.go := go.value > 0.5;
  //if Coils.Stop then Inputs.LED_ON := false;

  // UpdateModBus
  if not FModBus.CBModBusInputOverride.Checked then begin
    FModBus.ModbusServer.Data.setInput(0, Inputs.Go);
    FModBus.ModbusServer.Data.setInput(1, Inputs.Stop);
  end;

  FModBus.ModbusRefreshLEDs();

  miliseconds := GetTickCount64();
  PaintBox.Repaint;
end;

procedure TFMain.SetError(err: string);
begin
  errorMessage := err;
  if errorMessage <> '' then StatusBar.SimpleText := errorMessage;
end;

procedure TFMain.InitScene;
begin
end;


procedure TFMain.FormCreate(Sender: TObject);
begin
  InitScene();
end;

procedure TFMain.BShowConfigClick(Sender: TObject);
begin
  if not FConfig.IsVisible then FConfig.show();

  //errorMessage := '';
  //StatusBar.SimpleText := '';
end;


procedure TFMain.BShowIOClick(Sender: TObject);
begin
  if not FmodBus.IsVisible then FmodBus.show();
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  FmodBus.show();
  FConfig.Show();
  go := FConfig.UserPars.CreatePar('Go', 0);
  ready := FConfig.UserPars.CreatePar('Ready', 1);
  FConfig.UserPars.CreatePar('P3', 3);

  FModBus.ModbusData.setHoldingRegister(0, 23);
  FModBus.ModbusData.setHoldingRegister(1, 256);
end;

procedure TFMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if errorMessage = '' then
    StatusBar.SimpleText := format('[%d, %d]', [X, Paintbox.Height - 1 - Y]);
end;

end.

