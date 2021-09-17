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

unit modbusTCP;

{$mode objfpc}{$H+}

interface

uses math;

const
  ModBusBits = $10000;
  ModBusHoldingRegisters = $10000;
  //ModBusBytes = $10000 div 8;
  inBufSize = 256;
  outBufSize = 256;

type
  TModbusState = (mbsIdle, mbsHeader, msbLen, msbUnitId, msbFunctionCode,
                  mbsHighAddress, mbsLowAddress, mbsHighNum, mbsLowNum,
                  msbBytesNum, msbBytes, msbError);

  TModbusEvent = procedure(command: byte) of object;

  TModbusFrame = record
    TransactionIdentifier: integer;  // 2 bytes - For synchronization between messages of server & client
    ProtocolIdentifier: integer;     // 2 bytes - Zero for Modbus/TCP
    LengthField: word;               // 2 bytes - Number of remaining bytes in this frame
    UnitIdentifier: byte;            // 1 byte - Slave Address (255 if not used)
    FunctionCode: byte;              // 1 byte - Function codes as in other variants
    Address: word;
    Num: word;
    BytesNum: byte;
  end;

  { TModbusData }

  TModbusData = class
    Inputs: array[0..ModBusBits - 1] of boolean;
    Coils: array[0..ModBusBits - 1] of boolean;
    HoldingRegisters: array[0..ModBusHoldingRegisters - 1] of word;

    function getCoil(Addr: integer): boolean;
    function getInput(Addr: integer): boolean;
    function getHoldingRegister(Addr: integer): word;

    procedure setInput(Addr: integer; newValue: boolean);
    procedure setCoil(Addr: integer; newValue: boolean);
    procedure setHoldingRegister(Addr: integer; newValue: word);

    function get8Coils(Addr: integer; numBits: integer = 8): byte;
    procedure write8Coils(Addr: integer; eightCoils: byte; numBits: integer = 8);

    function get8Inputs(Addr: integer; numBits: integer = 8): byte;
    procedure write8Inputs(Addr: integer; eightInputs: byte; numBits: integer = 8);
  private
  end;

  { TModbusServer }

  TModbusServer = class
    Data: TModbusData;
    msg: RawByteString;
    Frame: TModbusFrame;
    InBuf: array[0..InBufSize - 1] of byte;
    OutBuf: array[0..OutBufSize - 1] of byte;
    InBufCount, OutBufCount: integer;

    State: TModbusState;
    byteCount: integer;

    //curTransactionIdentifier: word;
    reqStartAddress, reqCount: word;

    ReceivedMessagesCount: integer;
    OnReceiveEvent: TModbusEvent;
  private
    procedure clearOutBuf();
    procedure clearInBuf();
    procedure addInBuf(new_data: byte);
    procedure addOutBuf(new_data: byte);
    procedure add16OutBuf(new_data: word);
    procedure ProcessInputMessage();

  public
    response: RawByteString;

    function BigEndianWord(bytes: word): RawByteString;

    procedure MessageStateMachine(mess: RawByteString);

    constructor Create(useData: TModbusData; newReceiveEvent: TModbusEvent = nil);
    destructor Destroy; override;

    function WriteMultipleCoilsAnswer(): RawByteString;
    function WriteMultipleRegistersAnswer(): RawByteString;

    function ReadMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;

    function ReadMultipleInputsAnswer(): RawByteString;
    function ReadHoldingRegistersAnswer(): RawByteString;


  end;


implementation

uses SysUtils;

{ TModbusNEW }

// Receive State Machine
//  State = (mbsIdle, mbsHeader, msbLen, msbUnitId, msbFunctionCode,
//           msbBytesNum, msbBytes, msbError);

procedure TModbusServer.MessageStateMachine(mess: RawByteString);
var b: integer;
    i, len: integer;
begin
  if mess = '' then exit;

  //msg := msg + mess;
  msg := mess;
  len := length(msg);
  //while not (msg = '') do begin

  for i := 1 to len do begin
    b := ord(msg[i]);
    case State of
      mbsIdle: begin
        //if b = $B1 then begin
          State := mbsHeader;
          byteCount := 1;
          Frame.TransactionIdentifier := b shl 8;
        //Frame.TransactionIdentifier := $B1 shl 8;
        //end;
      end;

      mbsHeader: begin     // TIH TIL 00 00
        //if (byteCount = 1) and (b = $6E) then begin
        if byteCount = 1 then begin
          Frame.TransactionIdentifier := Frame.TransactionIdentifier or b;
          inc(byteCount);
        end else if (byteCount = 2) and (b = 0) then begin
          Frame.ProtocolIdentifier := 0;
          inc(byteCount);
        end else if (byteCount = 3) and (b = 0) then begin
          Frame.ProtocolIdentifier := 0;
          State := msbLen;
          byteCount := 0;
        end else begin   // There was an error: resync
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
      end;

      msbLen: begin
        if (byteCount = 0) then begin
          Frame.LengthField := b shl 8;
          inc(byteCount);
        end else if (byteCount = 1) then begin
          Frame.LengthField := Frame.LengthField or b;
          State := msbUnitId;
        end;
      end;

      msbUnitId: begin
        Frame.UnitIdentifier := b;
        State := msbFunctionCode;
      end;

      msbFunctionCode: begin
        Frame.FunctionCode := b;
        State := mbsHighAddress;
      end;

      mbsHighAddress: begin
        Frame.Address := b shl 8;
        State := mbsLowAddress;
      end;

      mbsLowAddress: begin
        Frame.Address := Frame.Address or b;
        State := mbsHighNum;
      end;

      mbsHighNum: begin
        Frame.Num := b shl 8;
        State := mbsLowNum;
      end;

      mbsLowNum: begin
        Frame.Num := Frame.Num or b;
        clearInBuf();

        if Frame.FunctionCode in [$01, $02, $03, $04, $05, $06] then begin // These commands have no extra bytes
          // The frame is complete and can be processed
          inc(ReceivedMessagesCount);
          if assigned(OnReceiveEvent) then OnReceiveEvent(Frame.FunctionCode);
          ProcessInputMessage();
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;

        end else if Frame.FunctionCode in [$10, $0F] then begin
          // Must read the number of extra bytes
          State := msbBytesNum;

        end else begin  // Unrecognized Function Code: Resync
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
      end;

      msbBytesNum: begin
        Frame.BytesNum := b;
        State := msbBytes;
        byteCount := 0;
      end;

      msbBytes: begin
        addInBuf(b);

        if byteCount + 1 >= Frame.BytesNum  then begin  // All bytes read: Resync
          // The frame is complete
          inc(ReceivedMessagesCount);
          if assigned(OnReceiveEvent) then OnReceiveEvent(Frame.FunctionCode);
          ProcessInputMessage();
          State := mbsIdle;
          Frame.TransactionIdentifier := -1;
          continue;
        end;
        inc(byteCount);
      end;

    end;
  end;

  msg := '';
end;


constructor TModbusServer.Create(useData: TModbusData; newReceiveEvent: TModbusEvent);
begin
  msg := '';
  //curTransactionIdentifier := $B16E;
  Data := useData;

  State := mbsIdle;
  OnReceiveEvent := newReceiveEvent;

  Frame.ProtocolIdentifier := -1; //Bad Frame, for now
  Frame.LengthField := 0;
end;

destructor TModbusServer.Destroy;
begin

  inherited;
end;

procedure TModbusServer.clearOutBuf();
var i: integer;
begin
  for i := 0 to outBufSize - 1 do OutBuf[i] := 0;
  OutBufCount := 0;
end;

procedure TModbusServer.clearInBuf();
var i: integer;
begin
  for  i := 0 to inBufSize - 1 do InBuf[i] := 0;
  InBufCount := 0;
end;

procedure TModbusServer.addInBuf(new_data: byte);
begin
  if (InBufCount >= InBufSize) then exit;
  InBuf[InBufCount] := new_data;
  inc(InBufCount);
end;


procedure TModbusServer.addOutBuf(new_data: byte);
begin
  if (outBufCount >= outBufSize) then exit;
  OutBuf[OutBufCount] := new_data;
  inc(OutBufCount);
end;

procedure TModbusServer.add16OutBuf(new_data: word);
begin
  if (OutBufCount >= outBufSize - 1) then exit;
  OutBuf[OutBufCount] := (new_data shr 8) and $FF;
  inc(OutBufCount);
  OutBuf[OutBufCount] := new_data and $FF;
  inc(OutBufCount);
end;

{
TransactionIdentifier: integer;  // 2 bytes - For synchronization between messages of server & client
ProtocolIdentifier: integer;     // 2 bytes - Zero for Modbus/TCP
LengthField: word;               // 2 bytes - Number of remaining bytes in this frame
UnitIdentifier: byte;            // 1 byte - Slave Address (255 if not used)
FunctionCode: byte;              // 1 byte - Function codes as in other variants
Address: word;
Num: word;
BytesNum: byte;
}

procedure TModbusServer.ProcessInputMessage();
var i: integer;
    RegVal: word;
begin
  case Frame.FunctionCode of

    $02: begin  // Read Multiple Inputs
      response := ReadMultipleInputsAnswer();
    end;

    $03: begin  // Read Holding Registers
      response := ReadHoldingRegistersAnswer();
    end;

    $0F: begin // Write multiple coils
      for i := 0 to InBufCount - 1 do begin
        Data.write8Coils(Frame.Address + i * 8, InBuf[i]);
      end;
      response := WriteMultipleCoilsAnswer();
    end;

    $10: begin // Write multiple Registers
      for i := 0 to InBufCount div 2 - 1 do begin
        RegVal := InBuf[2 * i] * 256 + InBuf[2 * i + 1];
        Data.setHoldingRegister(Frame.Address + i, RegVal);
      end;
      response := WriteMultipleRegistersAnswer();
    end;

  end;
end;

function TModbusServer.BigEndianWord(bytes: word): RawByteString;
begin
  result := chr(bytes div 256) + chr(bytes mod 256);
end;


function TModbusServer.ReadMultipleInputsAnswer(): RawByteString;
var i, Bytes: integer;
    s: RawByteString;
begin
  Bytes := ceil(Frame.Num / 8);

  result := BigEndianWord(Frame.TransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 1 + Bytes) +
            chr(Frame.UnitIdentifier) +
            chr(02) +
            chr(Bytes);
  s := '';

  for i := 0 to Bytes - 1 do begin
    s := s + chr(Data.get8Inputs(Frame.Address + i * 8));
  end;
  result := result + s;
end;


function TModbusServer.ReadHoldingRegistersAnswer(): RawByteString;
var i, Bytes: integer;
    s: RawByteString;
    ui16: word;
begin
  Bytes := 2 * Frame.Num;  // each register has 16 bits

  result := BigEndianWord(Frame.TransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 1 + Bytes) +
            chr(Frame.UnitIdentifier) +
            chr(03) +
            chr(Bytes);

  s := '';
  for i := 0 to Bytes div 2 - 1 do begin
    ui16 := Data.getHoldingRegister(Frame.Address + i);
    s := s + chr((ui16 shr 8) and $FF) + chr(ui16 and $FF);
  end;

  result := result + s;
end;


function TModbusServer.WriteMultipleCoilsAnswer(): RawByteString;
begin
  result := BigEndianWord(Frame.TransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(6) +
            chr(Frame.UnitIdentifier) +
            chr($0F) +
            BigEndianWord(Frame.Address) +
            BigEndianWord(Frame.num);
end;


function TModbusServer.WriteMultipleRegistersAnswer(): RawByteString;
begin
  result := BigEndianWord(Frame.TransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(6) +
            chr(Frame.UnitIdentifier) +
            chr($10) +
            BigEndianWord(Frame.Address) +
            BigEndianWord(Frame.num);
end;
{
function TModbusServer.WriteMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;
var payloadCount, i, ibyte, ibit: integer;
begin
  payloadCount := (1 + count div 8);

  result := BigEndianWord(curTransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 2 + 2 + 1 + payloadCount) +
            chr(UnitId) +
            chr(15) +
            BigEndianWord(StartAddress) +
            BigEndianWord(Count) +
            chr(payloadCount);

  SetLength(coilbits, payloadCount);
  for i := 0 to payloadCount - 1 do begin
    coilbits[i] := 0;
  end;

  for i := 0 to Count - 1 do begin
    ibyte := i div 8;
    ibit := i mod 8;
    if Data.Coils[StartAddress + i] then begin
      coilbits[ibyte] := coilbits[ibyte] or (1 shl ibit);
    end;
  end;

  for i := 0 to payloadCount - 1 do begin
    result := result + chr(coilbits[i]);
  end;

end;
}

function TModbusServer.ReadMultipleCoils(UnitId: byte; StartAddress, Count: word): RawByteString;
begin
  reqStartAddress := StartAddress;
  reqCount := count;
  result := BigEndianWord(Frame.TransactionIdentifier) +
            BigEndianWord(0) +
            BigEndianWord(1 + 1 + 2 + 2) +
            chr(UnitId) +
            chr(01) +
            BigEndianWord(reqStartAddress) +
            BigEndianWord(reqCount);
end;


function TModbusData.getCoil(Addr: integer): boolean;
begin
  if (Addr > high(Coils)) or (Addr < low(Coils)) then exit;
  result := Coils[Addr];
end;

function TModbusData.getInput(Addr: integer): boolean;
begin
  if (Addr > high(Inputs)) or (Addr < low(Inputs)) then exit;
  result := Inputs[Addr];
end;

function TModbusData.getHoldingRegister(Addr: integer): word;
begin
  if (Addr > high(HoldingRegisters)) or (Addr < low(HoldingRegisters)) then exit;
  result := HoldingRegisters[Addr];
end;

procedure TModbusData.setInput(Addr: integer; newValue: boolean);
begin
  if (Addr > high(Inputs)) or (Addr < low(Inputs)) then exit;
  Inputs[Addr] := newValue;
end;

procedure TModbusData.setCoil(Addr: integer; newValue: boolean);
begin
  if (Addr > high(Coils)) or (Addr < low(Coils)) then exit;
  Coils[Addr] := newValue;
end;

procedure TModbusData.setHoldingRegister(Addr: integer; newValue: word);
begin
  if (Addr > high(HoldingRegisters)) or (Addr < low(HoldingRegisters)) then exit;
  HoldingRegisters[Addr] := newValue;
end;


function TModbusData.get8Coils(Addr: integer; numBits: integer): byte;
var i: integer;
begin
  if (Addr + numBits > high(Coils)) or (Addr < low(Coils)) then exit;

  result := 0;
  for i := 0 to min(8, numBits) - 1 do begin
    if Coils[Addr + i] then begin
      result := result or (1 shl i);
    end;
  end;
end;


procedure TModbusData.write8Coils(Addr: integer; eightCoils: byte; numBits: integer);
var i: integer;
begin
  if (Addr + numBits > high(Coils)) or (Addr < low(Coils)) then exit;

  for i := 0 to min(8, numBits) - 1 do begin
    Coils[Addr + i] := (eightCoils and (1 shl i)) <> 0;
  end;

end;


function TModbusData.get8Inputs(Addr: integer; numBits: integer): byte;
var i: integer;
begin
  if (Addr + numBits > high(Inputs)) or (Addr < low(Inputs)) then exit;

  result := 0;
  for i := 0 to min(8, numBits) - 1 do begin
    if Inputs[Addr + i] then begin
      result := result or (1 shl i);
    end;
  end;
end;


procedure TModbusData.write8Inputs(Addr: integer; eightInputs: byte; numBits: integer);
var i: integer;
begin
  if (Addr + numBits > high(Inputs)) or (Addr < low(Inputs)) then exit;

  for i := 0 to min(8, numBits) - 1 do begin
    Inputs[Addr + i] := (eightInputs and (1 shl i)) <> 0;
  end;

end;


end.


