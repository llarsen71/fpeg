module fpeg
  implicit none

  !----------------------------------------------------------------------------

  type, abstract :: SourceT
  contains
    procedure(getLoc), deferred :: getLoc
    procedure(setLoc), deferred :: setLoc
    procedure(getChr), deferred :: getChr
    procedure(getStr), deferred :: getStr
  end type SourceT

  !----------------------------------------------------------------------------

  abstract interface
    function getLoc(this) result(loc)
      import SourceT
      class(SourceT) :: this
      integer        :: loc
    end function getLoc

    function setLoc(this, loc) result(success)
      import SourceT
      class(SourceT) :: this
      integer        :: loc
      logical        :: success
    end function setLoc

    function getChr(this, chr, loc) result(success)
      import SourceT
      class(SourceT)    :: this
      character         :: chr
      integer, optional :: loc
      logical           :: success
    end function getChr

    function getStr(this, i, j, string) result(success)
      import SourceT
      class(SourceT) :: this
      integer :: i
      integer :: j
      character(len=j-i+1) :: string
      logical :: success
    end function getStr
  end interface

  !----------------------------------------------------------------------------

  type, extends(SourceT) :: StringSrcT
    character*10000 :: String
    integer         :: sz
    integer         :: loc
  contains
    procedure :: getLoc => StringSrcT_getLoc
    procedure :: setLoc => StringSrcT_setLoc
    procedure :: getChr => StringSrcT_getChr
    procedure :: getStr => StringSrcT_getStr
  end type StringSrcT

  !----------------------------------------------------------------------------

  type :: MatchT
    logical :: isMatch = .false.
    !class(Source), pointer :: src => NULL()
  end type MatchT

  type(MatchT), target :: no_match

  !----------------------------------------------------------------------------

  type, extends(MatchT) :: MatchLocT
    integer :: loc = -1
  end type MatchLocT

  !----------------------------------------------------------------------------

  type, abstract :: PatternT
  contains
    procedure(match), deferred :: match
  end type PatternT

  !----------------------------------------------------------------------------

  abstract interface
    function match(this, src) result(match1)
      import :: PatternT, SourceT, MatchT
      class(PatternT)        :: this
      class(SourceT)         :: src
      class(MatchT), pointer :: match1
    end function match
  end interface

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PstrT
    character(len=100) :: string
    integer            :: sz
  contains
    procedure :: match => PstrT_match
  end type PstrT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PnT
    integer :: n
  contains
    procedure :: match => PnT_match
  end type PnT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PatternPlusT
    class(PatternT), pointer :: ptn1
    class(PatternT), pointer :: ptn2
  contains
    procedure :: match => PatternPlusT_match
  end type PatternPlusT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PatternTimesT
    class(PatternT), pointer :: ptn1
    class(PatternT), pointer :: ptn2
  contains
    procedure :: match => PatternTimesT_match
  end type PatternTimesT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PatternPowerT
    class(PatternT), pointer :: ptn
    integer :: n
  contains
    procedure :: match => PatternPowerT_match
  end type PatternPowerT

  !----------------------------------------------------------------------------

  !interface P
  !  module procedure Pstr, Pn
  !end interface

contains

  !============================================================================
  ! StringSrcT
  !============================================================================

  function newStringSrc(string) result(stringSrc)
    character*(*)    :: string
    type(StringSrcT) :: stringSrc

    stringSrc%string = string
    stringSrc%sz     = len(string)
    stringSrc%loc    = 1
  end function newStringSrc

  !============================================================================

  function StringSrcT_getLoc(this) result(loc)
    class(StringSrcT) :: this
    integer           :: loc

    loc = this%loc
  end function StringSrcT_getLoc

  !============================================================================

  function StringSrcT_setLoc(this, loc) result(success)
    class(StringSrcT) :: this
    integer           :: loc
    logical           :: success

    success = .false.
    if (loc < 0) return
    if (loc > this%sz+1) return
    this%loc = loc
    success = .true.
  end function StringSrcT_setLoc

  !============================================================================

  function StringSrcT_getChr(this, chr, loc) result(success)
    class(StringSrcT) :: this
    character         :: chr
    integer, optional :: loc
    logical           :: success

    chr = ' '
    success = .false.
    if (PRESENT(loc)) then
      if (0 > loc .or. loc > this%sz) return
      chr = this%string(loc:loc)
    else
      if (this%loc > this%sz) return
      chr = this%string(this%loc:this%loc)
      this%loc = this%loc + 1
    end if
    success = .true.
  end function StringSrcT_getChr

  !============================================================================

  function StringSrcT_getStr(this, i, j, string) result(success)
    class(StringSrcT) :: this
    integer :: i
    integer :: j
    character*(*) :: string
    logical :: success

    success = .false.
    if (i<j) return
    if (j > this%sz) return
    if (i < 1) return
    string = this%string(i:j)
    success = .true.
  end function StringSrcT_getStr

  !============================================================================
  ! MatchLocT
  !============================================================================

  function newMatchLoc(src) result(matchloc)
    class(SourceT)           :: src
    type(MatchLocT), pointer :: matchloc
    type(MatchLocT), target  :: matchloc1

    matchloc1%loc = src%getLoc()
    matchloc1%isMatch = .true.
    matchloc => matchloc1
    write(*,*) matchloc%loc
  end function newMatchLoc

  !============================================================================

  function noMatch(src, reset) result(match)
    class(SourceT)         :: src
    integer                :: reset
    class(MatchT), pointer :: match
    logical :: success

    success = src%setLoc(reset)
    match => no_match
  end function noMatch

  !============================================================================
  ! PstrT
  !============================================================================

  function Pstr(string) result(ptn)
!
! Create a string pattern
!
    character*(*) :: string
    type(PstrT), pointer :: ptn

    allocate(ptn)
    ptn%string = string
    ptn%sz     = len(string)
  end function Pstr

  !============================================================================

  function PstrT_match(this, src) result(match)
    class(PstrT)           :: this
    class(SourceT)         :: src
    class(MatchT), pointer :: match
    integer :: start, i
    character :: c

    start = src%getLoc()
    do i = 1, this%sz
      if (.not.src%getChr(c)) goto 10
      if (c .ne. this%string(i:i)) goto 10
    end do
    match => newMatchLoc(src)
    return
10  continue
    match => noMatch(src, start)
  end function PstrT_match

  !============================================================================
  ! PnT
  !============================================================================

  function Pn(n) result(ptn)
!
! Create a pattern to absorb n values.
!
    integer :: n
    type(PnT), pointer :: ptn

    allocate(ptn)
    ptn%n = n
  end function Pn

  !============================================================================

  function PnT_match(this, src) result(match)
    class(PnT)             :: this
    class(SourceT)         :: src
    class(MatchT), pointer :: match
    integer :: start
    logical :: reset

    start = src%getLoc()

    ! Try to set to new location. If this does work pattern fails.
    reset = src%setLoc(start+this%n)
    if (.not.reset) goto 10

    match => newMatchLoc(src)
    return
10  continue
    match => noMatch(src, start)
  end function PnT_match

  !============================================================================
  ! PatternPlusT
  !============================================================================

  function PatternPlus(ptn1, ptn2) result(ptn)
!
! Create a string pattern
!
    class(PatternT),    pointer :: ptn1, ptn2
    type(PatternPlusT), pointer :: ptn

    allocate(ptn)
    ptn%ptn1 => ptn1
    ptn%ptn2 => ptn2
  end function PatternPlus

  !============================================================================

  function PatternPlusT_match(this, src) result(match)
    class(PatternPlusT)    :: this
    class(SourceT)         :: src
    class(MatchT), pointer :: match
    integer :: start

    start = src%getLoc()
    match => this%ptn1%match(src)
    if (match%isMatch) goto 10
    match => this%ptn2%match(src)
    if (match%isMatch) goto 10

    match => noMatch(src, start)
    return

10  continue
    match => newMatchLoc(src)
  end function PatternPlusT_match

  !============================================================================
  ! PatternTimesT
  !============================================================================

  function PatternTimes(ptn1, ptn2) result(ptn)
!
! Create a string pattern
!
    class(PatternT),     pointer :: ptn1, ptn2
    type(PatternTimesT), pointer :: ptn

    allocate(ptn)
    ptn%ptn1 => ptn1
    ptn%ptn2 => ptn2
  end function PatternTimes

  !============================================================================

  function PatternTimesT_match(this, src) result(match)
    class(PatternTimesT)   :: this
    class(SourceT)         :: src
    class(MatchT), pointer :: match
    integer :: start

    start = src%getLoc()
    match => this%ptn1%match(src)
    if (.not.match%isMatch) goto 10
    match => this%ptn2%match(src)
    if (.not.match%isMatch) goto 10

    match => newMatchLoc(src)
    return
10  continue
    ! On failure reset to start
    match => noMatch(src, start)
  end function PatternTimesT_match

  !============================================================================
  ! PatternTimesT
  !============================================================================

  function PatternPower(ptn, n) result(rptn)
!
! Create a pattern for P**n
!
    class(PatternT),     pointer :: ptn
    integer                      :: n
    type(PatternPowerT), pointer :: rptn

    allocate(rptn)
    rptn%ptn => ptn
    rptn%n = n
  end function PatternPower

  !============================================================================

  function PatternPowerT_match(this, src) result(match)
    class(PatternPowerT)   :: this
    class(SourceT)         :: src
    class(MatchT), pointer :: match
    integer :: start, i

    start = src%getLoc()
    if (this%n >=0) then
      i = 0
      do
        match => this%ptn%match(src)
        if (.not.match%isMatch) then
          if (i < this%n) goto 10
          match => newMatchLoc(src)
          return
        end if
        i = i + 1
      end do
    else
      i = 0
      do
        match => this%ptn%match(src)
        if (.not.match%isMatch .or. i == abs(this%n)) then
          match => newMatchLoc(src)
          return
        end if
      end do
      i = i + 1
    end if

10  continue
    ! On failure reset to start
    match => noMatch(src, start)
  end function PatternPowerT_match

  !============================================================================
  !============================================================================

  !function S()
  !end function S

  !============================================================================


  !============================================================================
end module fpeg
