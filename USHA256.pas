
///////////////////////////////////////////////////////////////////////////////
// Nombre: SHA256
// Creado: 28/7/2016
// Objetivo: Implementa el algoritmo de hash SHA256.
// Autor: Santiago Alejandro Orellana Pérez
// Nota: El código fue adaptado de la librería DCPcrypt v2.0 escrita
//       por David Barton (crypto@cityinthesky.co.uk). 
//       Se agregó esteganografía para ocultar los valores de la tabla de hash.
///////////////////////////////////////////////////////////////////////////////

unit USHA256;

interface
uses
  Classes, Sysutils, UCryptBase, UAntiReversing;    //UCryptBase

type
  TSha256= class(TDCP_hash)
  protected
    LenHi, LenLo: longword;
    Index: DWord;
    CurrentHash: array[0..7] of DWord;
    HashBuffer: array[0..63] of byte;
    K: array[0..63] of DWORD;
    procedure Compress;
  public
    class function GetAlgorithm: string; override;
    class function GetHashSize: integer; override;
    procedure Init; override;
    procedure Final(var Digest); override;
    procedure Burn; override;
    procedure Update(const Buffer; Size: longword); override;
  end;

implementation
{$R-}{$Q-}

//-----------------------------------------------------------------------------
class function TSha256.GetAlgorithm: string;
begin
Result:= 'SHA256';
end;

//-----------------------------------------------------------------------------
class function TSha256.GetHashSize: integer;
begin
Result:= 256;
end;

//-----------------------------------------------------------------------------
function SwapDWord(a: dword): dword;
begin
Result:= ((a and $FF) shl 24) or ((a and $FF00) shl 8) or ((a and $FF0000) shr 8) or ((a and $FF000000) shr 24);
end;

//-----------------------------------------------------------------------------
procedure TSha256.Compress;
var a, b, c, d, e, f, g, h, t1, t2: DWord;
    W: array[0..63] of DWord;
    i: longword;
begin
Index:= 0;
a := CurrentHash[0]; b:= CurrentHash[1]; c:= CurrentHash[2]; d:= CurrentHash[3];
e := CurrentHash[4]; f:= CurrentHash[5]; g:= CurrentHash[6]; h:= CurrentHash[7];
Move(HashBuffer,W,Sizeof(HashBuffer));
for i := 0 to 15 do W[i]:= SwapDWord(W[i]);
for i := 16 to 63 do
    W[i]:= (((W[i-2] shr 17) or (W[i-2] shl 15)) xor ((W[i-2] shr 19) or (W[i-2] shl 13)) xor
    (W[i-2] shr 10)) + W[i-7] + (((W[i-15] shr 7) or (W[i-15] shl 25)) xor
    ((W[i-15] shr 18) or (W[i-15] shl 14)) xor (W[i-15] shr 3)) + W[i-16];

for i := 0 to 63 do
    begin
    t1 := h + (((e shr 6) or (e shl 26)) xor ((e shr 11) or (e shl 21)) xor ((e shr 25) or (e shl 7))) +
          ((e and f) xor (not e and g)) + K[i] + W[i];
    t2 := (((a shr 2) or (a shl 30)) xor ((a shr 13) or (a shl 19)) xor ((a shr 22) xor (a shl 10))) +
          ((a and b) xor (a and c) xor (b and c));
    h := g; g:= f; f:= e; e:= d + t1; d:= c; c:= b; b:= a; a:= t1 + t2;
    end;

CurrentHash[0]:= CurrentHash[0] + a;
CurrentHash[1]:= CurrentHash[1] + b;
CurrentHash[2]:= CurrentHash[2] + c;
CurrentHash[3]:= CurrentHash[3] + d;
CurrentHash[4]:= CurrentHash[4] + e;
CurrentHash[5]:= CurrentHash[5] + f;
CurrentHash[6]:= CurrentHash[6] + g;
CurrentHash[7]:= CurrentHash[7] + h;
FillChar(W, Sizeof(W), 0);
FillChar(HashBuffer, Sizeof(HashBuffer), 0);
end;

//-----------------------------------------------------------------------------
procedure TSha256.Init;
begin
Burn;
CurrentHash[0]:= StrToInt(DecStr('D8U7EbH1X:Lf>707M%Y'));   //'$6a09e667'
CurrentHash[1]:= StrToInt(DecStr('P6Oc8c67V8PbHfT9L%<'));   //'$bb67ae85'
CurrentHash[2]:= StrToInt(DecStr('33J4;dT76f;gF4X87%6'));   //'$3c6ef372'
CurrentHash[3]:= StrToInt(DecStr(';b7bH6D5;g6gI6O4O%3'));   //'$a54ff53a'
CurrentHash[4]:= StrToInt(DecStr('3g2642Q1Qf@6I3B8O%D'));   //'$510e527f'
CurrentHash[5]:= StrToInt(DecStr('>dR:Rc5136272989L%9'));   //'$9b05688c'
CurrentHash[6]:= StrToInt(DecStr('Gc:2?gS9=4=eK:MbU%='));   //'$1f83d9ab'
CurrentHash[7]:= StrToInt(DecStr('D:<6PcOfG1NdAeF2I%T'));   //'$5be0cd19'

K[00] := StrToInt(DecStr('0915T389;bL3=g6:?%A'));   //'$428a2f98'
K[01] := StrToInt(DecStr('32C822S428<5V5?:P%='));   //'$71374491'
K[02] := StrToInt(DecStr('MgScN6<d61=gCc:dR%;'));   //'$b5c0fbcf'
K[03] := StrToInt(DecStr('D66fT:<cP6YeDcUbR%0'));   //'$e9b5dba5'
K[04] := StrToInt(DecStr('5c64E:06H70dP3K6P%M'));   //'$3956c25b'
K[05] := StrToInt(DecStr('G286L:HgX2K2Y2:gL%<'));   //'$59f111f1'
K[06] := StrToInt(DecStr('35P:D3U4EgH9X3Lb>%0'));   //'$923f82a4'
K[07] := StrToInt(DecStr('M6YbPcO28d66VfPeH%T'));   //'$ab1c5ed5'
K[08] := StrToInt(DecStr('L9<e39J1;8Tb6b;:F%X'));   //'$d807aa98'
K[09] := StrToInt(DecStr('7262;379H4D6;c61I%O'));   //'$12835b01'
K[10] := StrToInt(DecStr('Of33352442Q9Q6@cI%B'));   //'$243185be'
K[11] := StrToInt(DecStr('O4D6>6R1Rd583e2d2%8'));   //'$550c7dc3'
K[12] := StrToInt(DecStr('L598G3:c?fS6=e=8K%M'));   //'$72be5d74'
K[13] := StrToInt(DecStr('Uf=9D1<ePfOcG2NgA%F'));   //'$80deb1fe'
K[14] := StrToInt(DecStr('I8T:Lc4eCd@1472bG%L'));   //'$9bdc06a7'
K[15] := StrToInt(DecStr('35VdS2L:Vc;gM2N8D%K'));   //'$c19bf174'
K[16] := StrToInt(DecStr('629fW5?:5cY7<:2d4%T'));   //'$e49b69c1'
K[17] := StrToInt(DecStr('G7Mf4gLc0f=5U8F9=%B'));   //'$efbe4786'
K[18] := StrToInt(DecStr('U7>1>gDd32S:Oe7dV%D'));   //'$0fc19dc6'
K[19] := StrToInt(DecStr('5dU3@581QdHb42JdV%F'));   //'$240ca1cc'
K[20] := StrToInt(DecStr('=gB39e7fL:Y3JdM7F%4'));   //'$2de92c6f'
K[21] := StrToInt(DecStr('Ob659bT8M5B9Y56b1%7'));   //'$4a7484aa'
K[22] := StrToInt(DecStr('0d?6@d?c31CbE:CeL%R'));   //'$5cb0a9dc'
K[23] := StrToInt(DecStr('Gb:8?7HgT:I9:9MeG%T'));   //'$76f988da'
K[24] := StrToInt(DecStr('?3?:J9P4Cf86M2S6O%U'));   //'$983e5152'
K[25] := StrToInt(DecStr('7ePb;9<4M2CdX7770%9'));   //'$a831c66d'
K[26] := StrToInt(DecStr('U9=cT1P1X4;3@8Sd0%:'));   //'$b00327c8'
K[27] := StrToInt(DecStr('28EcAg066:M8=g@dG%:'));   //'$bf597fc7'
K[28] := StrToInt(DecStr('64<d37Mf=1Q1KcGgU%0'));   //'$c6e00bf3'
K[29] := StrToInt(DecStr(':8UeL61b686:A2A56%0'));   //'$d5a79147'
K[30] := StrToInt(DecStr('F2P1G79dTb27D436?%K'));   //'$06ca6351'
K[31] := StrToInt(DecStr('88Q2E5A31:Y36:172%4'));   //'$14292967'
K[32] := StrToInt(DecStr('Q6X3488cP8Y1Db19H%Q'));   //'$27b70a85'
K[33] := StrToInt(DecStr('V9E3KfY2PcN3U2Y4N%P'));   //'$2e1b2138'
K[34] := StrToInt(DecStr('0d?5=e33Pd:7SeVg2%U'));   //'$4d2c6dfc'
K[35] := StrToInt(DecStr('V4D6L4?4A9O1Me>2J%E'));   //'$53380d13'
K[36] := StrToInt(DecStr('X547Y611Ib>8E4K6R%J'));   //'$650a7354'
K[37] := StrToInt(DecStr('WcD8D7Y7:b81BbLcO%1'));   //'$766a0abb'
K[38] := StrToInt(DecStr('IfQ9C27dJ33dX:Q3T%B'));   //'$81c2c92e'
K[39] := StrToInt(DecStr('069:H3U823C3WdT9T%N'));   //'$92722c85'
K[40] := StrToInt(DecStr('Y28b63KcRg7f09RbG%C'));   //'$a2bfe8a1'
K[41] := StrToInt(DecStr('Bc6bQ9T24bI7Q795C%@'));   //'$a81a664b'
K[42] := StrToInt(DecStr('E1MdP3=51cI9<c28C%9'));   //'$c24b8b70'
K[43] := StrToInt(DecStr('34KdS8>7<dC6V2NbA%3'));   //'$c76c51a3'
K[44] := StrToInt(DecStr('Q::e12>:U3TfB9:2A%?'));   //'$d192e819'
K[45] := StrToInt(DecStr('K5BeJ7O:R:C1F7I3<%:'));   //'$d6990624'
K[46] := StrToInt(DecStr('A6NgK571Xf14=6U9C%E'));   //'$f40e3585'
K[47] := StrToInt(DecStr('T1L2A1H76bXbC168=%I'));   //'$106aa070'
K[48] := StrToInt(DecStr('R7D25:Cb65=dK2R2S%6'));   //'$19a4c116'
K[49] := StrToInt(DecStr('79O2;f>4:817UdL13%K'));   //'$1e376c08'
K[50] := StrToInt(DecStr('0dI3H8P589U8R8J5D%P'));   //'$2748774c'
K[51] := StrToInt(DecStr('O684?5VcF1TcNd<cH%4'));   //'$34b0bcb5'
K[52] := StrToInt(DecStr('>4>4A:H2VdL1NdYc?%3'));   //'$391c0cb3'
K[53] := StrToInt(DecStr('@b;5Vf9e@93bFbE5F%9'));   //'$4ed8aa4a'
K[54] := StrToInt(DecStr('8gA6XcE:Hd?d?bQ5V%L'));   //'$5b9cca4f'
K[55] := StrToInt(DecStr('H4F7W9930fY7DgXg8%R'));   //'$682e6ff3'
K[56] := StrToInt(DecStr('4fB825Y9@gM9T3Rf@%K'));   //'$748f82ee'
K[57] := StrToInt(DecStr(';gT879?bA637X4077%4'));   //'$78a5636f'
K[58] := StrToInt(DecStr('25J9;5Qd49M8A9D2E%4'));   //'$84c87814'
K[59] := StrToInt(DecStr('W9=9Wd;dQ8S1A3E1;%N'));   //'$8cc70208'
K[60] := StrToInt(DecStr('>bA:B1Xc?fHgMgDg7%='));   //'$90befffa'
K[61] := StrToInt(DecStr('AcDb>5C62177YdGfC%U'));   //'$a4506ceb'
K[62] := StrToInt(DecStr('R8HcAf<gS:Hb;4KgT%:'));   //'$bef9a3f7'
K[63] := StrToInt(DecStr('J31dC7R8T298Y9LgM%;'));   //'$c67178f2'

fInitialized := true;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Burn;
begin
LenHi := 0; LenLo:= 0;
Index := 0;
FillChar(HashBuffer, Sizeof(HashBuffer), 0);
FillChar(CurrentHash, Sizeof(CurrentHash), 0);
FillChar(K, Sizeof(K), 0);
fInitialized := false;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Update(const Buffer; Size: longword);
var PBuf: ^byte;
begin
if not fInitialized then
   raise EDCP_hash.Create(DecStr('6/Tp<!PtYfD!UiRb0!5j6oEj0dHj0bPmKjP{MbGe8pL!HfXmK!YI:BLT<I3OP'));   //'No se ha inicializado el HASH.'

Inc(LenHi, Size shr 29);
Inc(LenLo, Size * 8);
if LenLo < (Size * 8) then Inc(LenHi);

PBuf := @Buffer;
while Size > 0 do
      begin
      if (Sizeof(HashBuffer) - Index) <= DWord(Size) then
         begin
         Move(PBuf^, HashBuffer[Index], Sizeof(HashBuffer) - Index);
         Dec(Size, Sizeof(HashBuffer) - Index);
         Inc(PBuf, Sizeof(HashBuffer) - Index);
         Compress;
         end
      else
         begin
         Move(PBuf^, HashBuffer[Index], Size);
         Inc(Index, Size);
         Size := 0;
         end;
      end;
end;

//-----------------------------------------------------------------------------
procedure TSha256.Final(var Digest);
begin
if not fInitialized then
   raise EDCP_hash.Create(DecStr('0/1pT!8t;fL!=i6b?!Aj3oCj2dSj2b<mVj?{Pb=eMpS!Nf<m6!=ICB:TRI;OD'));    //'No se ha inicializado el HASH.'
HashBuffer[Index]:= $80;
if Index>= 56 then Compress;
PDWord(@HashBuffer[56])^:= SwapDWord(LenHi);
PDWord(@HashBuffer[60])^:= SwapDWord(LenLo);
Compress;
CurrentHash[0]:= SwapDWord(CurrentHash[0]);
CurrentHash[1]:= SwapDWord(CurrentHash[1]);
CurrentHash[2]:= SwapDWord(CurrentHash[2]);
CurrentHash[3]:= SwapDWord(CurrentHash[3]);
CurrentHash[4]:= SwapDWord(CurrentHash[4]);
CurrentHash[5]:= SwapDWord(CurrentHash[5]);
CurrentHash[6]:= SwapDWord(CurrentHash[6]);
CurrentHash[7]:= SwapDWord(CurrentHash[7]);
Move(CurrentHash,Digest,Sizeof(CurrentHash));
Burn;
end;


end.

