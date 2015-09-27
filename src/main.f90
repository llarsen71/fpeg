program main
  use fpeg
  implicit none
  class(PatternT), pointer :: alpha, set, literal, srch
  integer :: matchi

  alpha => R('az')**0
  matchi = match_string(alpha, "this is a test of range and power")
  write(*,*) matchi

  set => S('(:-)')**0
  matchi = match_string(set, "((:--:--:)) bob")
  write(*,*) matchi

  literal => P('rep')**0
  matchi = match_string(literal, "reprep no more")
  write(*,*) matchi

  srch => (P(1)-P("test"))**0 * P("test")
  matchi = match_string(srch, "Search for test and return index after test")
  write(*,*) matchi

  continue
end
