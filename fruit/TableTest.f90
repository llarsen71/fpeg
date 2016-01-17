module TableTest
! Not sure why this isn't working
#define RUNTEST(case) run_test_case(case, #case)
  use fruit
  use Table
  implicit none

contains

  !============================================================================

  subroutine TableTestSuite
    call run_test_case(test_Fields, "test_Fields")
    call run_test_case(test_List, "test_List")
  end subroutine TableTestSuite

  !============================================================================

  subroutine test_Fields()
    type(FieldsT) :: field
    class(*), pointer :: value

    call field%setValueA("test", "bob")
    call assert_true(field%getValue("test", value))
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



  !============================================================================

end module TableTest
