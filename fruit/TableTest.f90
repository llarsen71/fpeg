module TableTest
! Not sure why this isn't working
!#define RUNTEST(case) run_test_case(case, #case)
  use fruit
  use Table
  implicit none

contains

  !============================================================================

  subroutine TableTestSuite
    call run_test_case(test_Fields, "test_Fields")
    call run_test_case(test_List,   "test_List")
  end subroutine TableTestSuite

  !============================================================================

  subroutine test_Fields()
    type(FieldsT) :: field
    class(*), pointer :: value
    character(len=:), pointer :: str
    integer :: i
    real    :: r

    nullify(str)
    ! Test ascii value
    call field%setValue("test", "bob")
    call assert_true(field%getValueA("test", str), "getValueA should return the string value")
    if (is_last_passed()) then
      call assert_true(associated(str), "getValueA should have set the string Value")
      call assert_equals("bob", str, "getValueA did not return correct value")
    end if

    ! Test real value
    call field%setValue("real", 4.5)
    call assert_true(field%getValueR("real", r), "getValueR should return the real value")
    if (is_last_passed()) then
      call assert_equals(4.5, r, "getValueR did not return correct number")
    end if

    ! Test integer value
    call field%setValue("int", 3)
    call assert_true(field%getValueI("int", i), "getValueI should return the real value")
    if (is_last_passed()) then
      call assert_equals(3, i, "getValueI did not return correct number")
    end if

  end subroutine

  !============================================================================

  subroutine test_List()
    type(ListT) :: list
    class(*), pointer :: value
    integer, pointer :: I

    allocate(I)
    I = 4
    value => I
    call assert_true(list%setValue(1, value))
    nullify(value)
    call assert_true(list%getValue(1, value))

  end subroutine

  !============================================================================

end module TableTest
