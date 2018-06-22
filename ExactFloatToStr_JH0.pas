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

	Rev. 6/21/2018  Updated to Unicode strings and code cleanup
	Rev. 1/1/2003   by JFH to add the three ParseFloat functions.
	Rev. 12/26/2002 by JFH to bracket the DEBUG code with conditionals.
	Rev. 12/25/2002 by JFH to fix 1E20 (BinExp) problem and check for zero and other special values.
	Pgm. 12/24/2002 by John Herbster for Delphi programmers everywhere.

***************************************************************************** *)

{ Turn DEBUG on to make available detail debugging at expense of speed.}
{DEFINE DEBUG}

interface

uses
	SysUtils, Winapi.Windows;


// This call uses the global DecimalSeparator and ThousandSeparator. It can be slow for very large or very small extended numbers.)
function ExactFloatToStr(const Value: Extended): string; overload; inline;
function ExactFloatToStr(const Value: Extended; const AFormatSettings: TFormatSettings): string; overload;

function ExactFloatToStrEx(const Value: Extended; DecimalPoint: string='.'; ThousandsSep: string=''; DigitGroups: Integer=0): string;


// These calls parse a float value to its sign, exponent, and mantissa.
function ParseFloat(const Value: Extended): string; overload;
function ParseFloat(const Value: Double): string; overload;
function ParseFloat(const Value: Single): string; overload;

// This is the basic conversion engine.
function FloatingBinPointToDecStr(const Value; const ValNbrBits, ValBinExp: integer; Negative: Boolean;
		DecimalPoint: string='.'; ThousandsSep: string=''; DigitGroups: Integer=0): string;

type
	TTypeFloat = (tfUnknown, tfNormal, tfZero, tfDenormal, tfIndefinite, tfInfinity, tfQuietNan, tfSignalingNan);

procedure AnalyzeFloat(const Value: Extended; out NumberType: TTypeFloat; out Negative: Boolean; out Exponent: Word; out Mantissa: Int64);


const
	//Different spaces you can use for digit grouping. SI recommends ThinSpace
	ThinSpace: WideChar          = #$2009; // U+2009 THIN SPACE
	NarrowNoBreakSpace: WideChar = #$202F; // U+202F NARROW NO-BREAK SPACE
	FigureSpace: WideChar        = #$2007; // U+2007 FIGURE SPACE


var
	LogFmtX: procedure(const Fmt: AnsiString; const Data: array of const) of object;

implementation

type
	TSglWord = Word;     //Consider Byte or Word
	TDblWord = LongWord; //Consider Word or LongWord

	TExtendedFloat = packed record
		Man: Int64; //Mantissa
		Exp: Word; //Sign and Exponent
	end;

const
//	SizeOfAryElem = SizeOf(TSglWord);
	BitsInBufElem = SizeOf(TSglWord) * 8; // SizeOfAryElem*8;

var
	SPositiveSign: string =              '+';          // LOCALE_SPOSITIVESIGN, at most 4 characters
	SNegativeSign: string =              '-';          // LOCALE_SNEGATIVESIGN, at most 4 characters
	SPosInfinity:  string =              'Infinity';   // LOCALE_SPOSINFINITY
	SNegInfinity:  string =              '-Infinity';  // LOCALE_SNEGINFINITY
	SNativeDigits: array[0..9] of Char = '0123456789'; // LOCALE_SNATIVEDIGITS
	INegNumber:    Integer =             1;            // LOCALE_INEGNUMBER 0 = "(1.1), 1 = "-1.1", 2 = "- 1.1", 3 = "1.1-", 4 = "1.1 -"
	SGrouping:     string =              '3;0';        // LOCALE_SGROUPING

{$IFDEF DEBUG}

procedure LogFmt(const Fmt: AnsiString; const Data: array of const);
begin
	if Assigned(LogFmtX) then
		LogFmtX(Fmt, Data);
end;
{$ENDIF}

procedure MultiplyAndAdd(Multiplican, Multiplier, CarryIn: TSglWord; var CarryOut, Product: TSglWord);
var
	Tmp: packed record case byte of 0: (W: TDblWord); 1: (L, H: TSglWord); end;
begin
	Tmp.W := Multiplican * Multiplier + CarryIn;
	CarryOut := Tmp.H;
	Product := Tmp.L;
end;

function DivideAndRemainder(NumeratorHi, NumeratorLo: TSglWord; Divisor: TSglWord; var Quotient, Remainder: TSglWord): Boolean;
var
	Tmp1, Tmp2: packed record case byte of 0: (W: TDblWord); 1: (L, H: TSglWord); end;
begin
	Result := (Divisor <> 0);
	if Result then
	begin
		Tmp1.H := NumeratorHi;
		Tmp1.L := NumeratorLo;
		Tmp2.W := Tmp1.W div Divisor;
		if (Tmp2.H <> 0) then
			Result := False
		else
		begin
			Quotient := Tmp2.L;
			Remainder := Tmp1.W mod Divisor;
		end;
	end;
end;

function AddSign(const s: string; IsNegative: Boolean): string;
begin
	{
		LOCALE_INEGNUMBER
			0 = "(1.1)
			1 = "-1.1"
			2 = "- 1.1"
			3 = "1.1-"
			4 = "1.1 -"
	}
	if IsNegative then
	begin
		case INegNumber of
		0: Result := '(' + s + ')';           // "(1.1)"
		1: Result := SNegativeSign + s;       // "-1.1"
		2: Result := SNegativeSign + ' ' + s; // "- 1.1"
		3: Result := s + SNegativeSign;       // "1.1-"
		4: Result := s + ' ' + SNegativeSign; // "1.1 -"
		else
			Result := SNegativeSign + s;
		end
	end
	else
	begin
		case INegNumber of
		0: Result := s;                       // "(1.1)"
		1: Result := SPositiveSign + s;       // "-1.1"
		2: Result := SPositiveSign + ' ' + s; // "- 1.1"
		3: Result := s + SPositiveSign;       // "1.1-"
		4: Result := s + ' ' + SPositiveSign; // "1.1 -"
		else
			Result := SPositiveSign + s;
		end
	end;
end;

function FloatingBinPointToDecStr(const Value; const ValNbrBits, ValBinExp: integer; Negative: Boolean;
		DecimalPoint: string = '.'; ThousandsSep: string = ''; DigitGroups: Integer=0): string;
var
	Man: array of TSglWord;
	CryE: TSglWord;
	Cry: TDblWord;
	NbrManElem: Integer;
	BinExp: Integer; // neg of # binary fraction bits
	DecExp: Integer; // neg of # decimal fraction bits
	NbrDecFraDigits: Integer;
	i, j, Tmp: integer;
	c: Char;
	Tmp1: packed record case byte of 0: (W: TDblWord); 1: (L, H: TSglWord); end;

{$IFDEF DEBUG}
	procedure LogManExp(const Rem: string);
	var
		s: string;
		k: integer;
	begin
		LogFmt('%s: BinExp=%d, DecExp=%d, NbrManElem=%d', [Rem, BinExp, DecExp, NbrManElem]);
		s := '';
		for k := 0 to NbrManElem - 1 do
			s := Format(' %2.2x', [Man[k]]) + s;
		LogFmt('  %s', [s]);
	end;
{$ENDIF}

begin
	{
		Value = Mantissa * 2^BinExp * 10^DecExp
	}

	{ Load Mantissa and binary exponent: }
	NbrManElem := (ValNbrBits + BitsInBufElem - 1) div BitsInBufElem;
	SetLength(Man, NbrManElem);
	Move(Value, Man[0], (ValNbrBits + 7) div 8); {Assuming little endian input}

	{ Set exponents: (Value = Mantissa * 2^BinExp * 10^DecExp) }
	BinExp := ValBinExp;
	DecExp := 0;

	{ Reduce mantissa to mininum number of bits (i.e. while mantissa is odd, div by 2 and inc binary exponent): }
{$IFDEF DEBUG}
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
	i := NbrManElem + (3 * NbrDecFraDigits + BitsInBufElem - 1) div BitsInBufElem;
	if length(Man) < i then
		SetLength(Man, i);
{$IFDEF DEBUG}
	LogManExp('Prep mul out');
{$ENDIF}
	for i := 1 to NbrDecFraDigits do
	begin
		CryE := 0;
		for j := 0 to NbrManElem - 1 do
			MultiplyAndAdd(Man[j], 5, CryE, CryE, Man[j]); // MultiplyAndAdd(Multiplican, Multiplier, CryIn: tSglWord; var CryOut, Product: tSglWord);
		if CryE <> 0 then
		begin
			Inc(NbrManElem);
			Man[NbrManElem - 1] := CryE;
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
				SetLength(Man, NbrManElem);
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
			else if (DigitGroups = 5) and ((DecExp mod 5) = 0) then
				Result := ThousandsSep + Result
			else if (DigitGroups = 3) and ((DecExp mod 3) = 0) then
				Result := ThousandsSep + Result
		end;

		{ DivideAndRemainder mantissa array by 10: }
		CryE := 0;
		for i := NbrManElem - 1 downto 0 do
			DivideAndRemainder(CryE, Man[i], 10, Man[i], CryE); // DivideAndRemainder(NumeratorHi, NumeratorLo: Byte;  Divisor: Byte; var Quotient, Remainder: Byte): boolean;

		Inc(DecExp);
		c := SNativeDigits[CryE];
		Result := c + Result;
		if (NbrManElem > 0) and (Man[NbrManElem - 1] = 0) then
			Dec(NbrManElem);
	until (DecExp > 0) and (NbrManElem = 0);

	Result := AddSign(Result, Negative);
end;

procedure AnalyzeFloat(const Value: Extended; out NumberType: TTypeFloat; out Negative: Boolean; out Exponent: Word; out Mantissa: Int64);
var
	ValueRec: TExtendedFloat absolute Value;
begin
	Mantissa := ValueRec.Man;
	Negative := (ValueRec.Exp and $8000) <> 0;
	Exponent := (ValueRec.Exp and $7FFF);

	if (Exponent = $7FFF) then
	begin
		if (Mantissa = 0) then
			NumberType := tfInfinity
		else
		begin
			Mantissa := (Mantissa and $3FFFFFFFFFFFFFFF);
			if ((ValueRec.Man and $4000000000000000) = 0) then
				NumberType := tfSignalingNan
			else if (Mantissa = 0) then
				NumberType := tfIndefinite
			else
				NumberType := tfQuietNan
		end
	end
	else if (Exponent = 0) then
	begin
		if (Mantissa = 0) then
			NumberType := tfZero
		else
			NumberType := tfDenormal
	end
	else
		NumberType := tfNormal;
end;

function ExactFloatToStrEx(const Value: Extended; DecimalPoint: string; ThousandsSep: string; DigitGroups: Integer): string;
var
	NumberType: TTypeFloat;
	Negative: Boolean;
	Exponent: Word;
	Mantissa: Int64;
const
	BIAS = $3FFF;

	function IsSpace(const s: string): Boolean;
	begin
		Result := False;
		if Length(s) <> 1 then
			Exit;

		case Word(s[1]) of
		$00A0, $1680, $2000, $2001, $2002, $2003, $2004, $2005,
		$2006, $2007, $2008, $2009, $200A, $202F, $205F, $3000: Result := True;
		end;
	end;

begin
{
	ThousandsSep:
			' ': group digits in groups of 5
			'', #0: no digit grouping
}
	AnalyzeFloat(Value, {out}NumberType, {out}Negative, {out}Exponent, {out}Mantissa);

	//Convert legacy #0 char to an actual empty string.
	if ThousandsSep = #0 then
		ThousandsSep := '';

	// If a ThousandsSeparator is present, but the DigitGroups parameter is zero, then auto-guess grouping
	// (Because why else would you specify a separator if you didn't want one)
	if (DigitGroups=0) and (ThousandsSep <> '') then
	begin
		if IsSpace(ThousandsSep) then
			digitGroups := 5
		else
			digitGroups := 3;
	end;

	case NumberType of
	tfNormal:       Result := FloatingBinPointToDecStr(Mantissa, {NbrBits}64, {BinExp}(Exponent - BIAS) - 63, Negative, DecimalPoint, ThousandsSep, DigitGroups);
	tfDenormal:     Result := FloatingBinPointToDecStr(Mantissa, {NbrBits}64, {BinExp}(-BIAS - 62),           Negative, DecimalPoint, ThousandsSep, DigitGroups);
	tfQuietNan:     Result := Format('QNaN(%d)', [Mantissa]);
	tfSignalingNan: Result := Format('SNaN(%d)', [Mantissa]);
	tfZero:         Result := AddSign('0', Negative);
	tfIndefinite:   Result := 'Indefinite';
	tfInfinity:
		begin
			if Negative then
				Result := SPosInfinity
			else
				Result := SNegInfinity;
		end;
	else
		Result := 'UnknownNumberType';
	end;
end;

function ExactFloatToStr(const Value: Extended): string;
begin
	Result := ExactFloatToStr(Value, FormatSettings);
end;

function ExactFloatToStr(const Value: Extended; const AFormatSettings: TFormatSettings): string; overload;
var
	digitGroups: Integer;
begin
{
		Handling groups is fairly difficult.

			Specification  Resulting string
			3;0            3,000,000,000,000
			3;2;0          30,00,00,00,00,000
			3              3000000000,000
			3;2            30000000,00,000

		We'll just read the first digit
}
	digitGroups := 0;
	if SGrouping <> '' then
	begin
		case SGrouping[1] of
		'0'..'9': digitGroups := Ord(SGrouping[1]) - Ord('0');
		end;
	end;

	Result := ExactFloatToStrEx(Value, AFormatSettings.DecimalSeparator, AFormatSettings.ThousandSeparator, digitGroups);
end;

function ParseFloat(const Value: Extended): string;
var
	ValueRec: TExtendedFloat absolute Value;
const
	PN: array[Boolean] of Char = '+-';
begin
	// This call parses an extended value to its sign, exponent, and mantissa.
	Result := Format('Ext(Sgn="%s",Exp=$%4.4x,Man=$%16.16x)', [
			PN[(ValueRec.Exp and $8000) <> 0],
			(   ValueRec.Exp and $7FFF),
				 ValueRec.Man]);
end;

function ParseFloat(const Value: Double): string;
var
	ValueRec: Int64 absolute Value;
const
	PN: array [Boolean] of Char = '+-';
begin
	// This call parses a double value to its sign, exponent, and mantissa.
	Result := Format('Dbl(Sgn="%s",Exp=$%3.3x,Man=$%13.13x)', [
			PN[(ValueRec and $8000000000000000) <> 0],
			((  ValueRec and $7FF0000000000000) shr 52),
			(   ValueRec and $000FFFFFFFFFFFFF)]);
end;

function ParseFloat(const Value: Single): string;
var
	ValueRec: LongInt absolute Value;
const
	PN: array [Boolean] of Char = '+-';
begin
	{ This call parses a single value to its sign, exponent, and mantissa. }
	Result := Format('Sgl(Sgn="%s",Exp=$%2.2x,Man=$%6.6x)',
			[PN[(ValueRec and $80000000) <> 0],
			((   ValueRec and $7F800000) shr 23),
			(    ValueRec and $007FFFFF)]);
end;

procedure InitFormatSettings;
var
	localeID: LCID;
	s: string;
const
	//Windows Vista
	LOCALE_SPOSINFINITY = $0000006a;   // + Infinity, eg "infinity"
	LOCALE_SNEGINFINITY = $0000006b;   // - Infinity, eg "-infinity"
begin
	localeID := LOCALE_USER_DEFAULT;

	SPositiveSign := GetLocaleStr(localeID, LOCALE_SPOSITIVESIGN, '+'); // at most 4 characters
	SNegativeSign := GetLocaleStr(localeID, LOCALE_SNEGATIVESIGN, '-'); // at most 4 characters
	SPosInfinity  := GetLocaleStr(localeID, LOCALE_SPOSINFINITY,  'Infinity');   //
	SNegInfinity  := GetLocaleStr(localeID, LOCALE_SNEGINFINITY,  '-Infinity');  //
	SGrouping     := GetLocaleStr(localeID, LOCALE_SGROUPING,     '3;0');        //

	INegNumber    := StrToIntDef(GetLocaleStr(localeID, LOCALE_INEGNUMBER, '1'), 1);

	s := GetLocaleStr(localeID, LOCALE_SNATIVEDIGITS, '0123456789');
	if Length(s) = 10 then
		Move(s[1], SNativeDigits[0], 10*SizeOf(Char));
end;

initialization
	InitFormatSettings;

end.
