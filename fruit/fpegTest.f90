module fpegTest
#define RUNTEST(case) run_test_case(case, #case)
  use fruit
  use fpeg
  implicit none
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

end module fpegTest
