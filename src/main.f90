program main
  use fpeg
  implicit none
  class(PatternT), pointer :: ps
  integer :: matchi

  ps => R('az')**0

  matchi = ps%match(newStringSrc("tes1testasdf t"))
  write(*,*) matchi
  continue
end
