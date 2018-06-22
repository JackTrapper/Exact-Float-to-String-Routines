unit ExactFloatToStr_JH0;

(* *****************************************************************************

  This module includes
    (a) functions for converting a floating binary point number to its
        *exact* decimal representation in an AnsiString;
    (b) functions for parsing the floating point types into sign, exponent,
        and mantissa; and
    (c) function for analyzing a extended float number into its type (zero,
        normal, infinity, etc.)

  Its intended use is for trouble shooting problems with floating point numbers.

  This code uses dynamic arrays, overloaded calls, and optional parameters.

  These routines are not very optimized for speed or space.
		I plan to replace the individual bit-shifts and multiplies-by-ten with multiple versions of same.
		Consider making an object so that the arrays don't have to reallocated so often.
		And consider making an output buffer character array so that the Result will be allocated only once.

  Rev. 2003.01.01 by JFH to add the three ParseFloat functions.
  Rev. 12/26/2002 by JFH to bracket the DEBUG code with conditionals.
  Rev. 12/25/2002 by JFH to fix 1E20 (BinExp) problem and check for zero and
      other special values.
  Pgm. 12/24/2002 by John Herbster for Delphi programmers everywhere.

***************************************************************************** *)

{ Turn DeBUG on to make available detail debugging at expense of speed.}
{DEFINE DEBUG}

interface

uses
	SysUtils;

const
	ThinSpace:          WideChar = #$2009; //U+2009 THIN SPACE
	NarrowNoBreakSpace: WideChar = #$202F; //U+202F NARROW NO-BREAK SPACE
	FigureSpace:        WideChar = #$2007; //U+2007 FIGURE SPACE



function ExactFloatToStr(const Value: Extended): string; overload; inline;
function ExactFloatToStr(const Value: Extended; const AFormatSettings: TFormatSettings): string; overload; inline;

{ This call uses the global DecimalSeparator and ThousandSeparator.
  (It can be slow for very large or very small extended numbers.) }

function ExactFloatToStrEx(const Value: Extended; DecimalPoint: string='.'; ThousandsSep: string=''): string;
{ Use #0 for the ThousandsSep if *no* grouping breaks are desired. }

function ParseFloat(const Value: Extended): string; overload;
function ParseFloat(const Value: Double): string;   overload;
function ParseFloat(const Value: Single): string;   overload;
{ These calls parse a float value to its sign, exponent, and mantissa. }

function FloatingBinPointToDecStr(const Value; const ValNbrBits, ValBinExp: integer; Negative: Boolean;
		DecimalPoint: string='.'; ThousandsSep: string=''): string;
{ This is the basic conversion engine. }

type tTypeFloat = (tfUnknown, tfNormal, tfZero, tfDenormal, tfIndefinite, tfInfinity, tfQuietNan, tfSignalingNan);

procedure AnalyzeFloat(const Value: Extended; out NumberType: TTypeFloat; out Negative: Boolean; out Exponent: Word; out Mantissa: Int64);

var
	LogFmtX: procedure (const Fmt: AnsiString; const Data: array of const) of object;

{$IfDef DEBUG}
const Debug = True;
{$Else}
const Debug = False;
{$EndIf}

implementation

type
	TSglWord = Word; {Consider Byte or Word}
	TDblWord = LongWord; {Consider Word or LongWord}

const
//	SizeOfAryElem = SizeOf(TSglWord);
	BitsInBufElem = SizeOf(TSglWord)*8; //SizeOfAryElem*8;

const
	SPositiveSign = '+';        //LOCALE_SPOSITIVESIGN, at most 4 characters
	SNegativeSign = '-';        //LOCALE_SNEGATIVESIGN, at most 4 characters
	SPosInfinity = 'Infinity';  //LOCALE_SPOSINFINITY
	SNegInfinity = '-Infinity'; //LOCALE_SNEGINFINITY
	SNativeDigits: array[0..9] of Char = '0123456789'; //LOCALE_SNATIVEDIGITS
	INegNumber =   1; //0 = "(1.1), 1 = "-1.1", 2 = "- 1.1", 3 = "1.1-", 4 = "1.1 -" LOCALE_INEGNUMBER



{$IfDef DEBUG}
procedure LogFmt(const Fmt: AnsiString; const Data: array of const);
begin
	If Assigned(LogFmtX) then
		LogFmtX(Fmt,Data);
end;
{$EndIf}

procedure MultiplyAndAdd(Multiplican, Multiplier, CryIn: tSglWord; var CryOut, Product: tSglWord);
var
	Tmp: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
begin
	Tmp.W  := Multiplican * Multiplier + CryIn;
	CryOut  := Tmp.H;
	Product := Tmp.L;
end;

Function DivideAndRemainder
    (NumeratorHi, NumeratorLo: tSglWord; Divisor: tSglWord;
     var Quotient, Remainder: tSglWord): boolean;
var Tmp1,Tmp2: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
Begin
  Result := (Divisor <> 0);
  If Result then begin
    Tmp1.H := NumeratorHi;
    Tmp1.L := NumeratorLo;
    Tmp2.W := Tmp1.W div Divisor;
    If (Tmp2.H <> 0)
      then Result := false
      else begin
        Quotient  := Tmp2.L;
        Remainder := Tmp1.W mod Divisor;
        end;
    end;
End;

function AddSign(const s: string; IsNegative: Boolean): string;
begin
	 //0 = "(1.1), 1 = "-1.1", 2 = "- 1.1", 3 = "1.1-", 4 = "1.1 -" LOCALE_INEGNUMBER
	if IsNegative then
	begin
		case INegNumber of
		0: Result := '('+s+')';           // "(1.1)"
		1: Result := SNegativeSign+s;     // "-1.1"
		2: Result := SNegativeSign+' '+s; // "- 1.1"
		3: Result := s+SNegativeSign;     // "1.1-"
		4: Result := s+' '+SNegativeSign; // "1.1 -"
		else
			Result := SNegativeSign+s;
		end
	end
	else
	begin
		case INegNumber of
		0: Result := s;                   // "(1.1)"
		1: Result := SPositiveSign+s;     // "-1.1"
		2: Result := SPositiveSign+' '+s; // "- 1.1"
		3: Result := s+SPositiveSign;     // "1.1-"
		4: Result := s+' '+SPositiveSign; // "1.1 -"
		else
			Result := SPositiveSign+s;
		end
	end;
end;

function FloatingBinPointToDecStr(const Value; const ValNbrBits, ValBinExp: Integer; Negative: Boolean;
		DecimalPoint: string='.'; ThousandsSep: string=''): string;
{ Value = Mantissa * 2^BinExp * 10^DecExp }
var
	Man: array of tSglWord;
	CryE: TSglWord;
	Cry: TDblWord;
	NbrManElem,
	BinExp, //neg of # binary fraction bits
	DecExp, //neg of # decimal fraction bits
	NbrDecFraDigits,
	i, j, Tmp: integer;
	c: Char;
	Tmp1: packed record case byte of 0: (W: tDblWord); 1:(L,H: tSglWord); end;
	DigitGroups: Integer; //3 for most cultures, 5 is also nice to look at

{$IFDEF DEBUG}
	procedure LogManExp(const Rem: string);
	var
		s: string;
		k: integer;
	begin
		LogFmt('%s: BinExp=%d, DecExp=%d, NbrManElem=%d', [Rem,BinExp,DecExp,NbrManElem]);
		s := '';
		for k := 0 to NbrManElem - 1 do
			s := Format(' %2.2x',[Man[k]]) + s;
		LogFmt('  %s',[s]);
	end;
{$ENDIF}

begin
	if ThousandsSep = ' ' then
		DigitGroups := 5  //Space separator means group digits every 5
	else if ThousandsSep <> '' then
		DigitGroups := 3  //any other separator means group digits every 3
	else
		DigitGroups := 0;

	{ Load Mantissa and binary exponent: }
	NbrManElem := (ValNbrBits + BitsInBufElem - 1) div BitsInBufElem;
	SetLength(Man,NbrManElem);
	Move(Value,Man[0],(ValNbrBits + 7) div 8); {Assuming little endian input}

	{ Set exponents: (Value = Mantissa * 2^BinExp * 10^DecExp) }
	BinExp := ValBinExp;
	DecExp := 0;

	{ Reduce mantissa to mininum number of bits (i.e. while mantissa is odd, div by 2 and inc binary exponent): }
{$IfDef DEBUG}
	LogManExp('Before trimming');
{$ENDIF}
	while (NbrManElem > 0) and (BinExp < 0) and not Odd(Man[0]) do
	begin
		Cry := 0;
		for i := NbrManElem - 1 downto 0 do
		begin
			Tmp := (Cry shl BitsInBufElem) or Man[i];
			Man[i] := (Tmp shr 1);
			Cry := Tmp and 1;
		end;
		Inc(BinExp);
{$IFDEF DEBUG}
		LogManExp('Shifting down');
{$ENDIF}
		if Man[NbrManElem - 1] = 0 then
			Dec(NbrManElem);
	end{while};

	{ Check for zero: }
	if NbrManElem = 0 then
	begin
		Result := AddSign(Result, Negative);
		Exit;
	end;

   {
      Repeatably multiply by 10 until there is no more fraction. Decrement the DecExp at the same time.
      Note that a multiply by 10 is same as mul. by 5 and inc of BinExp exponent.
      Also note that a multiply by 5 adds two or three bits to number of mantissa bits.
   }
   NbrDecFraDigits := -BinExp; {Observe! 0.5, 0.25, 0.125, 0.0625, 0.03125, ...}
   i := NbrManElem + (3*NbrDecFraDigits + BitsInBufElem - 1) div BitsInBufElem;
   if length(Man) < i then
      SetLength(Man,i);
{$IFDEF DEBUG}
   LogManExp('Prep mul out');
{$ENDIF}
   for i := 1 to NbrDecFraDigits do
   begin
      CryE := 0;
      for j := 0 to NbrManElem - 1 do
         MultiplyAndAdd(Man[j],5,CryE,CryE,Man[j]); //MultiplyAndAdd(Multiplican, Multiplier, CryIn: tSglWord; var CryOut, Product: tSglWord);
      if CryE <> 0 then
      begin
         Inc(NbrManElem);
         Man[NbrManElem-1] := CryE;
      end;
      Inc(BinExp);
      Dec(DecExp);
{$IFDEF DEBUG}
      LogManExp('Mul out');
{$ENDIF}

   end{i-loop};

{$IFDEF DEBUG}
   LogManExp('Finished multiplies');
{$ENDIF}

   { Finish reducing BinExp to 0 by shifting mantissa up: }
   while (BinExp > 0) do
   begin
      Cry := 0;
      for i := 0 to NbrManElem - 1 do
      begin
         Tmp1.W := Man[i] shl 1;
         Man[i] := Tmp1.L + Cry;
         Cry := Tmp1.H;
      end;
      Dec(BinExp);
      if Cry <> 0 then
      begin
         Inc(NbrManElem);
         if length(Man) < NbrManElem then
            SetLength(Man,NbrManElem);
         Man[NbrManElem - 1] := Cry;
      end;
{$IFDEF DEBUG}
      LogManExp('Shifting up');
{$ENDIF}
    end{while};

   { Repeatably divide by 10 and use remainders to create decimal AnsiString: }
   Result := ''; {DEBUG}
{$IFDEF DEBUG}
   LogManExp('Before division');
{$ENDIF}

   repeat
      { If not first then place separators: }
      if Result <> '' then
		begin
         if DecExp = 0 then
            Result := DecimalPoint + Result
			else if (DigitGroups=5) and ((DecExp mod 5) = 0) then
				Result := ThousandsSep + Result
			else if (DigitGroups=3) and ((DecExp mod 3) = 0) then
				Result := ThousandsSep + Result
      end;

		{ DivideAndRemainder mantissa array by 10: }
		CryE := 0;
		for i := NbrManElem - 1 downto 0 do
			DivideAndRemainder(CryE, Man[i], 10, Man[i], CryE); //DivideAndRemainder(NumeratorHi, NumeratorLo: Byte;  Divisor: Byte; var Quotient, Remainder: Byte): boolean;

		Inc(DecExp);
		c := SNativeDigits[CryE];
		Result := c + Result;
		if (NbrManElem > 0) and (Man[NbrManElem - 1]=0) then
			Dec(NbrManElem);
   until (DecExp > 0) and (NbrManElem = 0);

	Result := AddSign(Result, Negative);
end;

procedure AnalyzeFloat(const Value: Extended;
		out NumberType: TTypeFloat; out Negative: Boolean; out Exponent: Word; out Mantissa: Int64);
var
	ValueRec: packed record Man: Int64; Exp: word end absolute Value;
begin
	Mantissa :=  ValueRec.Man;
	Negative := (ValueRec.Exp and $8000)<>0;
	Exponent := (ValueRec.Exp and $7FFF);
	if (Exponent = $7FFF) then
		if (Mantissa = 0) then
			NumberType := tfInfinity
		else
		begin
			Mantissa := (Mantissa and $3FFFFFFFFFFFFFFF);
			if ((ValueRec.Man and $4000000000000000) = 0) then
				NumberType := tfSignalingNAN
			else if (Mantissa = 0) then
				NumberType := tfIndefinite
			else NumberType := tfQuietNAN
		end
		else if (Exponent = 0) then
			if (Mantissa = 0)	then
				NumberType := tfZero
			else
				NumberType := tfDenormal
		else
			NumberType := tfNormal;
end;

function ExactFloatToStrEx(const Value: Extended; DecimalPoint: string; ThousandsSep: string): string;
var
	NumberType: TTypeFloat;
	Negative: Boolean;
	Exponent: Word;
	Mantissa: Int64;
const
	BIAS = $3FFF;
begin
{
	ThousandsSep:
			' ': group digits in groups of 5
			'', #0: no digit grouping
}
	AnalyzeFloat(Value, {out}NumberType, {out}Negative, {out}Exponent, {out}Mantissa);

	case NumberType of
	tfNormal:   Result := FloatingBinPointToDecStr(Mantissa, {NbrBits}64, {BinExp}(Exponent-BIAS)-63, Negative, DecimalPoint, ThousandsSep);
	tfDenormal: Result := FloatingBinPointToDecStr(Mantissa, {NbrBits}64, {BinExp}(-BIAS-62),         Negative, DecimalPoint, ThousandsSep);
	tfZero:
		begin
			if Negative then
				Result := SNegativeSign+'0'
			else
				Result := SPositiveSign+'0';
		end;
	tfIndefinite: Result := 'Indefinite';
	tfInfinity:
		begin
			if Negative then
				Result := SPosInfinity
			else
				Result := SNegInfinity;
      end;
	tfQuietNan: Result := Format('QNaN(%d)', [Mantissa]);
	tfSignalingNan: Result := Format('SNaN(%d)', [Mantissa]);
	else
		Result := 'UnknownNumberType';
	end;
end;

function ExactFloatToStr(const Value: Extended): string;
begin
	Result := ExactFloatToStr(Value, FormatSettings);
end;

function ExactFloatToStr(const Value: Extended; const AFormatSettings: TFormatSettings): string; overload;
begin
	Result := ExactFloatToStrEx(Value, AFormatSettings.DecimalSeparator, AFormatSettings.ThousandSeparator);
end;

Function ParseFloat(const Value: extended): string;
{ This call parses an extended value to its sign, exponent, and mantissa. }
var ValueRec: packed record Man: Int64; Exp: word end absolute Value;
const PN: array [boolean] of char = '+-';
Begin
  Result := Format('Ext(Sgn="%s",Exp=$%4.4x,Man=$%16.16x)',
                   [PN[(ValueRec.Exp and $8000) <> 0],
                       (ValueRec.Exp and $7FFF),
                        ValueRec.Man]);
End;

Function ParseFloat(const Value: double): string;
{ This call parses a double value to its sign, exponent, and mantissa. }
var ValueRec: Int64 absolute Value;
const PN: array [boolean] of char = '+-';
Begin
  Result := Format('Dbl(Sgn="%s",Exp=$%3.3x,Man=$%13.13x)',
                   [PN[(ValueRec and $8000000000000000) <> 0],
                      ((ValueRec and $7FF0000000000000) shr 52),
                       (ValueRec and $000FFFFFFFFFFFFF)]);
End;

function ParseFloat(const Value: Single): string;
var
	ValueRec: LongInt absolute Value;
const
	PN: array [boolean] of char = '+-';
begin
	{ This call parses a single value to its sign, exponent, and mantissa. }
	Result := Format('Sgl(Sgn="%s",Exp=$%2.2x,Man=$%6.6x)',
                   [PN[(ValueRec and $80000000) <> 0],
                      ((ValueRec and $7F800000) shr 23),
                       (ValueRec and $007FFFFF)]);
end;

end.

