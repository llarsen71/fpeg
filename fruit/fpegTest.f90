module fpegTest
!#define RUNTEST(case) run_test_case(case, #case)
  use fruit
  use fpeg
  implicit none

  integer :: realIdx, intIdx, strIdx, idx
  character*10 :: realToken, intToken, strToken
  logical :: called_realListener, called_intListener, called_strListener

contains

  !============================================================================

  subroutine fpegTestSuite
    implicit none
    call run_test_case(test_P,     "test_P")
    call run_test_case(test_Pn,    "test_Pn")
    call run_test_case(test_R,     "test_R")
    call run_test_case(test_S,     "test_S")
    call run_test_case(test_V,     "Test_V")
    call run_test_case(test_Pow,   "test_Pow")
    call run_test_case(test_Plus,  "test_Plus")
    call run_test_case(test_Minus, "test_Minus")
    call run_test_case(test_Times, "test_Times")
    call run_test_case(test_Token, "test_Token")
  end subroutine

  !============================================================================

  subroutine test_P()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => P('123')
    call assert_equals(4,match_string(ptn, "1234"), "match index is wrong for P")
    call assert_equals(NO_MATCH,match_string(ptn, " 1234"),"match should be NO_MATCH for P")
    !call assert_equals(5,match_string(ptn, " 1234", 1), "match index is wrong for P")
    call assert_equals(NO_MATCH,match_string(ptn, ""),"match should be NO_MATCH for P")
  end subroutine

  !============================================================================

  subroutine test_Pn()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => Pn(3)
    call assert_equals(NO_MATCH, match_string(ptn,"a"), "Should not match one character")
    call assert_equals(NO_MATCH, match_string(ptn,"ab"), "Should not match two characters")
    call assert_equals(4, match_string(ptn,"abc"), "Should match 3 characters")
    call assert_equals(4, match_string(ptn,"abcd"), "Should match 3+ characters")
  end subroutine

  !============================================================================

  subroutine test_R()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => R('az')
    call assert_equals(2,match_string(ptn, "zz"), "match index is wrong for R")
    call assert_equals(2,match_string(ptn, "aa"), "match index is wrong for R")
    call assert_equals(2,match_string(ptn, "ss"), "match index is wrong for R")
    call assert_equals(NO_MATCH, match_string(ptn, "Z" ), "match should be NO_MATCH for R")
  end subroutine

  !============================================================================

  subroutine test_S()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => S('123')
    call assert_equals(NO_MATCH,match_string(ptn, "0"), "match should be NO_MATCH for S")
    call assert_equals(2,match_string(ptn, "1"), "match index is wrong for S")
    call assert_equals(2,match_string(ptn, "2"), "match index is wrong for S")
    call assert_equals(2,match_string(ptn, "3"), "match index is wrong for S")
    call assert_equals(NO_MATCH,match_string(ptn, "4"), "match index should be NO_MATCH for S")
  end subroutine

  !============================================================================

  subroutine Test_V()
    implicit none
    class(VT),       pointer :: Expr
    class(PatternT), pointer :: Sum, Product, Value, IntMath, Spc

    ! Simple integer math expressions
    Spc => P(' ')**0

    Expr    => V()
    Value   => R('09')**1 + (P('(') * Spc * Expr * Spc * P(')'))
    Product => Value * Spc * (S('*/') * Spc * Value)**0
    Sum     => Spc * Product * Spc * (S('+-') * Spc * Product * Spc)**0
    call Expr%setPattern(Sum)
    IntMath => Expr * (-P(1))  ! Pattern must match whole string

    call assert_equals(12,match_string(IntMath,"1 + (2-3)*4"),"Should match integer math")
    call assert_equals(4,match_string(IntMath," 1 "),"Should match single value")
    call assert_equals(NO_MATCH,match_string(IntMath,"1 +"),"Should not match an incomplete expression")

  end subroutine

  !============================================================================

  subroutine test_Pow()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => R("az")**0
    call assert_equals(1, match_string(ptn, " "), "Should match only start of the string")
    call assert_equals(5, match_string(ptn, "this is a test"), "Should match to end of first word")
    deallocate(ptn)

    ptn => R("az")**1
    call assert_equals(NO_MATCH, match_string(ptn, " "), "match should fail (needs at least one letter)")
    call assert_equals(5, match_string(ptn, "this is a test"), "Should match to end of first word")

    ptn => R("az")**-2
    call assert_equals(1, match_string(ptn, " "), "match zero characters")
    call assert_equals(3, match_string(ptn, "this is a test"), "Should match end of first two letters")
    call assert_equals(2, match_string(ptn, "iOS"), "Should match first letter only")

  end subroutine

  !============================================================================

  subroutine test_Plus()
    implicit none
    class(PatternT), pointer :: ptn

    ptn => P('cat') + P('dog')
    call assert_equals(4,match_string(ptn, "cat"), "Should match 'cat'")
    call assert_equals(4,match_string(ptn, "dog"), "Should match 'dog'")
    call assert_equals(4,match_string(ptn, "catdog"), "Should match first 'cat'")
    call assert_equals(NO_MATCH,match_string(ptn,"cap"), "Should not match 'cap'")
  end subroutine

  !============================================================================

  subroutine test_Minus()
    implicit none
    class(PatternT), pointer :: ptn

    ! Any four (or more) character not including Bob or Fred
    ptn => (Pn(4) - P("Bob")) - P("Fred")
    call assert_equals(NO_MATCH,match_string(ptn, "Not"), "Should not match a string less that 4 characters")
    call assert_equals(NO_MATCH,match_string(ptn, "Bobo"), "Should not match a string starting with Bob")
    call assert_equals(NO_MATCH,match_string(ptn, "Fred"), "Should not match a string starting with Fred")
    call assert_equals(5,match_string(ptn, "Nuts"), "Nuts is a valid 4 character string")
    call assert_equals(5,match_string(ptn, "fred"), "fred is ok since it doesn't start with a capital F")
  end subroutine

  !============================================================================

  subroutine test_Times()
    implicit none
    class(PatternT), pointer :: ptn

    ! integer + or - integer
    ptn => R("09")**1 * P(" ")**0 * S("+-") * P(" ")**0 * R("09")**1

    call assert_equals(10,match_string(ptn, "123 + 456"), "Should be valid integer + integer")
    call assert_equals(6,match_string(ptn, "0 - 5"), "Should be valid integer - integer")
    call assert_equals(6,match_string(ptn, "0 - 5*a"), "Should be valid integer - integer at start")
    call assert_equals(NO_MATCH,match_string(ptn, "a - 5"), "First term is not an integer")
    call assert_equals(NO_MATCH,match_string(ptn, "54 + "), "Missing second integer")
  end subroutine

  !============================================================================

  subroutine test_Token()
    implicit none
    class(PatternT), pointer :: ptn
    class(PatternT), pointer :: r_, i_, s_, spc

    r_   => R('09')**1 * P(".") * R('09')**0  ! Real Value
    i_   => R('09')**1                        ! Int Value
    s_   => (R('az') + R('az') + P('_'))**1   ! String value
    spc  => P(' ')**0                         ! Spaces

    ptn => (Token(r_, 'real',   realListener) + &
            Token(i_, 'int',    intListener)  + &
            Token(s_, 'string', strListener) + spc )**1 &
            * (-P(1)) ! Match whole string

    ! All three types included
    call initListeners(1, 2, 3, "1.9", "32", "test")
    call assert_equals(12, match_string(ptn, "1.9 32 test"), "Token string should have matched")
    call verifyCalled(.true., .true., .true.)

    ! No real value included
    call initListeners(0, 2, 1, "", "1", "string")
    call assert_equals(9, match_string(ptn, "string 1"), "Token string should have matched")
    call verifyCalled(.false., .true., .true.)

    ! Not a full match since @ is not a valid part of the grammar.
    ! NOTE THAT TOKENS STILL GET CALLED EVEN IF FULL PATTERN DOES NOT MATCH!
    call initListeners(1, 2, 0, "1.5", "10", "")
    call assert_equals(NO_MATCH, match_string(ptn, "1.5 10 @"), "Token string should have matched")
    call verifyCalled(.true., .true., .false.)

  end subroutine

  !----------------------------------------------------------------------------

  subroutine initListeners(ri, ii, si, rT, iT, sT)
    integer :: ri, ii, si
    character*(*) :: rT, iT, sT

    called_realListener = .false.
    called_intListener  = .false.
    called_strListener  = .false.

    idx     = 0
    realIdx = ri
    intIdx  = ii
    strIdx  = si

    realToken = rT
    intToken  = iT
    strToken  = sT
  end subroutine

  !----------------------------------------------------------------------------

  subroutine verifyCalled(rC, iC, sC)
    logical :: rC, iC, sC

    call assert_equals(rC, called_realListener, "The realListener call incorrect")
    call assert_equals(iC, called_intListener,  "The intListener call incorrect")
    call assert_equals(sC, called_strListener,  "The strListener call incorrect")
  end subroutine

  !----------------------------------------------------------------------------

  subroutine realListener(token, i, j, src)
    character*(*)  :: token
    integer        :: i, j
    class(SourceT) :: src
    character*10   :: str

    idx = idx + 1
    called_realListener = .true.

    call assert_true(realIdx > 0, "The realListener should not have been called")
    if (.not.is_last_passed()) return

    call assert_equals(realIdx, idx, "The realListener not called in correct order")
    call assert_equals('real', token, "The token name should be real")

    call assert_true(src%getStr(i, j, str), "Real token indexes should be valid")
    if (.not.is_last_passed()) return

    call assert_equals(realToken, str, "Real token not correct")
  end subroutine

  !----------------------------------------------------------------------------

  subroutine intListener(token, i, j, src)
    character*(*)  :: token
    integer        :: i, j
    class(SourceT) :: src
    character*10   :: str

    idx = idx + 1
    called_intListener = .true.

    call assert_true(intIdx > 0, "The intListener should not have been called")
    if (.not.is_last_passed()) return

    call assert_equals(intIdx, idx, "The intListener not called in correct order")
    call assert_equals('int', token, "The token name should be int")

    call assert_true(src%getStr(i, j, str), "Int token indexes should be valid")
    if (.not.is_last_passed()) return

    call assert_equals(intToken, str, "Int token not correct")
  end subroutine

  !----------------------------------------------------------------------------

  subroutine strListener(token, i, j, src)
    character*(*)  :: token
    integer        :: i, j
    class(SourceT) :: src
    character*10   :: str

    idx = idx + 1
    called_strListener = .true.

    call assert_true(strIdx > 0, "The strListener should not have been called")
    if (.not.is_last_passed()) return

    call assert_equals(strIdx, idx, "The strListener not called in correct order")
    call assert_equals('string', token, "The token name should be str")

    call assert_true(src%getStr(i, j, str), "String token indexes should be valid")
    if (.not.is_last_passed()) return

    call assert_equals(strToken, str, "String token not correct")
  end subroutine

  !============================================================================

end module fpegTest
