program main
  use fruit
  use TableTest, only: TableTestSuite
  use fpegTest,  only: fpegTestSuite
  implicit none

  call init_fruit()

  call TableTestSuite()
  call fpegTestSuite()

  call fruit_summary()
  call fruit_finalize()
  continue
end
