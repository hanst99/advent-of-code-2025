program Day2_Part1;
{$mode objfpc }
{$R+}

uses sysutils;

function FetchNextRange(var InputCursor: Cardinal; Input: AnsiString; out Left, Right: UInt64): Boolean;
var
  BeginPos, EndPos : Cardinal;
begin
  if InputCursor < Length(Input) then
  begin
    BeginPos := InputCursor;
    while Input[InputCursor] <> '-'
    do InputCursor := InputCursor+1;
    EndPos := InputCursor - 1;

    Left := StrToUInt64(Input[BeginPos..EndPos]);


    InputCursor := InputCursor + 1;
    BeginPos := InputCursor;

    while (InputCursor < Length(Input)) and (Input[InputCursor] <> ',')
    do InputCursor := InputCursor + 1;
    EndPos := InputCursor-1;
    if InputCursor = Length(Input) then EndPos := EndPos + 1;

    Right := StrToUInt64(Input[BeginPos..EndPos]);
    InputCursor := InputCursor+1;
  end
  else FetchNextRange := False;
end;

function Even(I: UInt64): Boolean;
begin
        Result := (I and 1) = 0;
end;


function SumMirrorsInRange(Left, Right: UInt64): Int64;
var
        AdjustedLeft, AdjustedRight: UInt64;
        LeftDigits, RightDigits, K: Cardinal;
        NthPowerOfTen, NthPowerOfTenRight: UInt64;
        HalfNthPowerOfTen, HalfNthPowerOfTenRight: UInt64;
        HighLeft, HighRight : UInt64;
        LeftBound, RightBound : UInt64;
        KthSum, Sum : UInt64;
        AdjustmentLeft, AdjustmentRight : AnsiString;
begin
   { Exit early for absurd input so we don't need to think about it in the rest of the function }
   if Right < Left then Exit(0);

   { Logic:
     take a mirror integer k, i.e. k has a 2n-digit decimal representation k_1,..,k_n,k_1,..,k_n.
     We can break this apart as k_low+k_high, where k_low=k_1,...,k_n = k div 10^n and k_high=k_1,...,k_n,0_1,...,0_n = k_low*10^n,
     more conveniently written as k_low + k_low*10^n.
     In other words, the 2n-digit mirror numbers M_2n are exactly [ i + i*10^(n-1) | i <- [ 10^(n-1) .. 10^n-1 ] ].
     Now since we're interesting in Sum M_2n; Rearranging the terms we can see:
             Sum M_2n
             = Sum [i | i <- 10^(n-1)..10^n-1] + Sum [i * 10^n | i <- 10^(n-1)..10^n-1 ]
             = Sum [i | i <- 10^(n-1)..10^n-1] + Sum [i | i <- 10^(n-1)..10^n-1 ] * 10^n
             = sn + sn * 10^n
    where sn = Sum [i | i <- 10^(n-1)..10^n-1]
             = Sum [i | i <- 1..10^n-1] - Sum [i | 1 <- 1..10^(n-1)-1] (note that 10^(n-1)-1 >= 0 since n>=1)
             = 1/2 * (10^n * (10^n-1) - (10^(n-1) -1)*(10^(n-1)))
             = 1/2 (10^(2n) - 10^n - (10^(2n-2) - 10^(n-1))
             = 1/2 (10^(2n) - 10^n - 10^(2n-2) + 10^(n-1))
             = 10^(n-1)/2 * (10^(n+1) - 10 - 10^(n-1) + 1)
             = 10^(n-1)/2 * (100*10^(n-1) - 10^(n-1) - 9)
             = 10^(n-1)/2 * (99*10^(n-1) - 9)
   
   Now we're actually interested in the number of 2n-digit mirror integers <= k,
   where we just need to replace the bounds
             Sum M_(<=k)
             = Sum [i | i <- 10^(n-1)..k_low] + Sum [i * 10^n | i <- 10^(n-1)..k_low ]
             = Sum [i | i <- 10^(n-1)..k_low] + Sum [i | i <- 10^(n-1)..k_low ] * 10^n
             = sn + sn * 10^n
    where sn = Sum [i | i <- 10^(n-1)..k_low]
             = (k_low - 10^(n-1))/2 * (10^(n-1)+k_low)

    
    Then, assuming left and right are both 2n-digit mirror numbers, the number want is just M_(<=right)-M_(<=left).
    Other cases we reduce to that case as such:

    a) if left is not a mirror number with an even number of digits, we round up to the next highest number that is
       1. if left has 2n-1 digits: we pick left' = 10^(2n-1)+10^(n-1), e.g. for a 1-digit number (n=1) we go to 10^1+10^0 = 11, for a 3 digit number n=2 so we go to 10^3+10^1=1010, and so on.
       2. if left is a 2n-digit number already,
             a) and left_low<=left_high: we go to left_high*10^n+left_high, i.e. if our number is 73, left_high=7, left_low=3, n=1 we go to 7*10^1 + 7 = 77
             b) and left_low>left_high: that means that left_high is *not* 999... so left_high+1 is still an n-digit number, so we can just go to (left_high+1)*10^(n-1) + (left_high+1),
                e.g. if left=123567, left_high=123, left_low=567, n=3 we go to 124*10^3 + 124 = 124124
    b) if right is not a mirror number with an even number of digits, we do more or less the same thing in the other direction:
       1. if right has 2n+1 digits: we pick right' = 10^(2n)-1, i.e. if right = 105, n=1, right' = 10^2-1 = 99, i.e. the digit sequence 9999... with 1 digit less
       2. if right is a 2n-digit number already,
          a) and right_low >= right_high: we go to right_high+10^n+right_high, i.e. if right=767911, n=3, right_high=767, right_low=911, so we go to right' = 767*10^3+767 = 767767
          b) and right_low < right_high: we'd have to check, we need to go to right_high-1 but if right_high=10^(n-1) then that will reduce the number of digits, in that case go to b.1)
             otherwise, pick right' = right_high*10^n + right_high
    c) if after adjusting left' > right', the sum is zero. This can for instance happen if left=23 and right=25, then left'=33 and right' = 22
    d) if left' is a 2n-digit number and right' 2(n+k)-digit number, then adjust the bounds with
           left'_0 = left', right'_0=10^(2n)-1,
           left'_1 = 10^n*(10^n + 1), right'_1 = 10^(2n+2)-1
           ...,
           left'_i = 10^(n+i-1)*(10^(n+i) + 1), right'_i = 10^(2(n+i))-1
           ...,
           left'_k = 10^(n+k-1)*(10^(n+k) + 1), right'_k = right'

    in total, we need to do ~n+k steps, if we reuse results between steps and
    don't be silly (in particular, don't constantly recompute 10^n... as described this is quadratic RE length of input, if we keep recomputing 10^n it will become cubic)
    I am thinking maybe there's a closed form for the whole thing but I already spent too much time for this at this point.
      }

    { Implementation note: I'm basing this off integer mul/div here. On modern PC, maybe working with log is faster? IDK }
    NthPowerOfTen := 10;
    HalfNthPowerOfTen := 1;
    LeftDigits := 1;
    while Left div NthPowerOfTen > 0 do
    begin
            LeftDigits := LeftDigits + 1;
            NthPowerOfTen := NthPowerOfTen * 10;
            if Even(LeftDigits) then HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
    end;

    HighLeft := Left div HalfNthPowerOfTen;
    if not Even(LeftDigits)
    then begin
            { a.1 }
            AdjustmentLeft := 'a.1';
            LeftDigits := LeftDigits + 1;
            AdjustedLeft := NthPowerOfTen + HalfNthPowerOfTen;
            HighLeft := NthPowerOfTen;
            NthPowerOfTen := NthPowerOfTen * 10;
            HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
    end else
    begin
            if (Left mod HalfNthPowerOfTen) > HighLeft
            then { a.2.b } begin AdjustmentLeft := 'a.2.b'; HighLeft := HighLeft + 1 end else AdjustmentLeft := 'a.2.a';
            AdjustedLeft := HighLeft * (1 + HalfNthPowerOfTen);
    end;

    { if our adjustment moved us past the right boundary, we know there can't
      be any mirrors in here, so let's leave so the rest of the logic does not need to consider this case }
    Assert(AdjustedLeft >= Left);
    Assert((AdjustedLeft div HalfNthPowerOfTen) = (AdjustedLeft mod HalfNthPowerOfTen));
    if AdjustedLeft > Right then
            { Early exit here if we adjusted so far up that we're past the bounds now }
            Exit(0);

    NthPowerOfTenRight := NthPowerOfTen;
    HalfNthPowerOfTenRight := HalfNthPowerOfTen;

    { we excluded Right < Left earlier, so we can just continue where we left off }
    RightDigits := LeftDigits;
    while Right div NthPowerOfTenRight > 0 do
    begin
            RightDigits := RightDigits + 1;
            NthPowerOfTenRight := NthPowerOfTenRight * 10;
            if Even(RightDigits) then HalfNthPowerOfTenRight := HalfNthPowerOfTenRight * 10;
    end;

    HighRight := Right div HalfNthPowerOfTenRight;
    if not Even(RightDigits)
    then begin
            { b.1 }
            RightDigits := RightDigits - 1;
            NthPowerOfTenRight := NthPowerOfTenRight div 10;
            AdjustedRight := NthPowerOfTenRight -1;
    end else if (Right mod HalfNthPowerOfTenRight) >= HighRight
    then
            { b.2.a }
            AdjustedRight := HighRight * (1 + HalfNthPowerOfTenRight)
    else begin
            { b.2.b }
            { check for special case where we go down in digits }
            HighRight := HighRight - 1;
            if HighRight + 1 = HalfNthPowerOfTenRight
            then begin
                    RightDigits := RightDigits - 2;
                    NthPowerOfTenRight := NthPowerOfTenRight div 100;
                    HalfNthPowerOfTenRight := HalfNthPowerOfTenRight div 10;
                    AdjustedRight := NthPowerOfTenRight - 1;
            end else { normal case } AdjustedRight := HighRight * (1 + HalfNthPowerOfTenRight);
    end;


    { Some sanity checks to make sure we didn't fuck up earlier }
    Assert(AdjustedRight <= Right);
    Assert((AdjustedRight div HalfNthPowerOfTenRight) = (AdjustedRight mod HalfNthPowerOfTenRight));

    if AdjustedRight < AdjustedLeft then
            { this can happen e.g. if both left and right have the same odd number of digits }
            Exit(0);

    { if you got here and understood everything congrats, the hard part is over }

    { Now it's just a matter of summing up the results }

    Assert(LeftDigits <= RightDigits);

    K := 0;
    Sum := 0;
    while LeftDigits + K <= RightDigits do
    begin
            { Left bound / right bound are 'half' of the mirror each }
            if K = 0
            then LeftBound := AdjustedLeft div HalfNthPowerOfTen
            else LeftBound := HalfNthPowerOfTen;

            if LeftDigits + K = RightDigits
            then begin
                    Assert(HalfNthPowerOfTen = HalfNthPowerOfTenRight);
                    RightBound := AdjustedRight div HalfNthPowerOfTen
            end
            else RightBound := HalfNthPowerOfTen - 1;

            KthSum := (RightBound * (RightBound +1)) div 2;
            KthSum := KthSum - (LeftBound * (LeftBound + 1)) div 2
              { we need to add in LeftBound again because otherwise we're computing the sum of (LeftBound..RightBound] }
              + LeftBound;
            KthSum := KthSum + KthSum * HalfNthPowerOfTen;

            Sum := KthSum + Sum;

            K := K + 2;
            HalfNthPowerOfTen := HalfNthPowerOfTen * 10;
    end;

    Assert(LeftDigits + K = RightDigits + 2);
    WriteLn(Left:16, LeftDigits:8, NthPowerOfTen:16, AdjustedLeft:16, AdjustmentLeft:16, Right:16, RightDigits:8, NthPowerOfTenRight:16, AdjustedRight:16, Sum:16);
    Result := Sum;
end;

var Input: AnsiString;
var Left, Right: UInt64;
InputCursor: Cardinal;
TotalMirrors: UInt64;

begin
    WriteLn('Left':16, 'nLeft':8, '10^nLeft':16, 'AdjustedLeft':16, 'AdjustmentLeft':16, 'Right':16, 'nRight':8, '10^nRight':16, 'AdjustedRight':16, 'Sum':16);
    ReadLn(Input);
    InputCursor := 1;
    TotalMirrors := 0;
    while FetchNextRange(InputCursor, Input, Left, Right) do TotalMirrors := TotalMirrors + SumMirrorsInRange(Left, Right);
    WriteLn(TotalMirrors);
end.
