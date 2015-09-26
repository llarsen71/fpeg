program main
  use fpeg
  implicit none
  type(PstrT), pointer :: ps
  class(MatchT), pointer :: matchi

  ps => Pstr("test")

  matchi => ps%match(newStringSrc("test 123"))
  select type(matchi)
  type is (MatchLocT)
    write(*,*) matchi%loc
  end select
  continue

end
