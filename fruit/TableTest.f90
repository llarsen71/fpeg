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
    type(FieldsT), target :: field
    class(*), pointer :: value, v2
    character(len=:), pointer :: str
    integer :: i
    real    :: r
    type(FieldsT), pointer :: f1

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

    ! Test setValuePtr
    v2 => field
    call field%setValuePtr("field", v2)
    call assert_true(field%getValuePtr("field", value), "getValuePtr should return the Ptr value")
    if (is_last_passed()) then
      call assert_true(associated(v2, value), "The location of value should equal v2")
      select type(value)
      type is (FieldsT)
        f1 => value
        call assert_true(associated(f1, field), "getField should return the original field")
      class default
        call add_fail("getValuePtr type returned should be FieldsT")
      end select
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
    call assert_true(list%setValuePtr(1, value))
    nullify(value)
    call assert_true(list%getValuePtr(1, value))

  end subroutine

  !============================================================================

end module TableTest
