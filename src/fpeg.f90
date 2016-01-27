! Copyright (c) 2015 Lance Larsen
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is furnished
! to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
! WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
! IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

module fpeg
  implicit none

  !----------------------------------------------------------------------------
  ! A SourceT provides access to a string for performing fpeg pattern matching.
  ! The source may be a direct string, or some string source such as a file.
  ! The supported methods are:
  !
  ! getLoc - Get the current location in the string
  ! setLoc - Set the location to the given location in the string
  ! getChr - Get the next character from the string and increment the location
  ! getStr - Get a substring from the string
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
  ! The following is used when the fpeg source is a string.
  !----------------------------------------------------------------------------

  type, extends(SourceT) :: StringSrcT
    character(len=:), allocatable :: string
    integer                       :: loc
  contains
    procedure :: getLoc => StringSrcT_getLoc
    procedure :: setLoc => StringSrcT_setLoc
    procedure :: getChr => StringSrcT_getChr
    procedure :: getStr => StringSrcT_getStr
  end type StringSrcT

  !----------------------------------------------------------------------------
  ! PatternT
  !----------------------------------------------------------------------------

  type, abstract :: PatternT
  contains
    procedure(match), deferred :: match
  end type PatternT

  !----------------------------------------------------------------------------

  abstract interface
    function match(this, src) result(idxend)
      import :: PatternT, SourceT
      class(PatternT), intent(in) :: this
      class(SourceT)              :: src
      integer                     :: idxend
    end function match
  end interface

  ! Convenience value that indicates no match was found
  integer, parameter :: NO_MATCH = -1

  !----------------------------------------------------------------------------

  interface assignment(=)
    module procedure PatternAssign
  end interface assignment(=)

  !----------------------------------------------------------------------------

  interface operator(+)
    module procedure PatternPlus
  end interface operator(+)

  !----------------------------------------------------------------------------

  interface operator(-)
    module procedure PatternNeg
    module procedure PatternMinus
  end interface operator(-)

  !----------------------------------------------------------------------------

  interface operator(*)
    module procedure PatternTimes
  end interface operator(*)

  !----------------------------------------------------------------------------

  interface operator (**)
    module procedure PatternPower
  end interface operator (**)

  !----------------------------------------------------------------------------
  ! PatternT objects
  !----------------------------------------------------------------------------

  !
  ! Test for exact match of a string value. This is accessed as P('string')
  !
  type, extends(PatternT) :: PstrT
    character(len=:), allocatable :: string
  contains
    procedure :: match => PstrT_match
  end type PstrT

  !----------------------------------------------------------------------------

  !
  ! Match the next n characters. This is accessed as P(n).
  !
  type, extends(PatternT) :: PnT
    integer :: n
  contains
    procedure :: match => PnT_match
  end type PnT

  !----------------------------------------------------------------------------

  !
  ! Match character in the given set. Accessed as S('abc')
  !
  type, extends(PatternT) :: ST
    character, allocatable :: chrs(:)
  contains
    procedure :: match => ST_match
  end type ST

  !----------------------------------------------------------------------------

  !
  ! Match a character within the given range. Accessed as R('az').
  !
  type, extends(PatternT) :: RT
    character :: rng(2)
  contains
    procedure :: match => RT_match
  end type RT

  !----------------------------------------------------------------------------

  !
  ! Pattern Variable (or holder) for a pattern that will be specified and set
  ! later. This allows recursive grammars to be defined.
  !
  type, extends(PatternT) :: VT
    class(PatternT), allocatable :: ptn
  contains
    procedure :: match      => VT_match
    procedure :: setPattern => VT_setPattern
  end type VT

  !----------------------------------------------------------------------------

  !
  ! Pattern Variable (or holder) for a pattern that will be specified and set
  ! later. This allows recursive grammars to be defined.
  !
  type, extends(PatternT) :: PatternAssignT
    class(PatternT), allocatable :: ptn
  contains
    procedure :: match => PatternAssignT_match
  end type PatternAssignT

  !----------------------------------------------------------------------------

  !
  ! Try to match the first pattern. If this fails try to match the second pattern.
  !
  type, extends(PatternT) :: PatternPlusT
    class(PatternT), allocatable :: ptn1
    class(PatternT), allocatable :: ptn2
  contains
    procedure :: match => PatternPlusT_match
  end type PatternPlusT

  !----------------------------------------------------------------------------

  !
  ! Match the first pattern only if the second pattern is not a match.
  !
  type, extends(PatternT) :: PatternMinusT
    class(PatternT), allocatable :: ptn1
    class(PatternT), allocatable :: ptn2
  contains
    procedure :: match => PatternMinusT_match
  end type PatternMinusT

  !----------------------------------------------------------------------------

  !
  ! Match the first pattern followed by the second pattern
  !
  type, extends(PatternT) :: PatternTimesT
    class(PatternT), allocatable :: ptn1
    class(PatternT), allocatable :: ptn2
  contains
    procedure :: match => PatternTimesT_match
  end type PatternTimesT

  !----------------------------------------------------------------------------

  !
  ! For n>=0, P^n indicates that the pattern should match at least n times.
  ! P^-n looks for n or less matches of the pattern.
  !
  type, extends(PatternT) :: PatternPowerT
    class(PatternT), allocatable :: ptn
    integer :: n
  contains
    procedure :: match => PatternPowerT_match
  end type PatternPowerT

  !----------------------------------------------------------------------------

  !
  ! Call a listener with the token name and result when a token is found
  !
  type, extends(PatternT) :: TokenT
    class(PatternT), allocatable :: ptn
    character(len=:), allocatable :: token
    procedure(tokenListener), nopass, pointer :: listener => NULL()
  contains
    procedure :: match => TokenT_match
  end type TokenT

  !----------------------------------------------------------------------------

  interface
    subroutine tokenListener(token, i, j, src)
      import SourceT
      character*(*)  :: token
      integer        :: i, j    ! Start and end index of String
      class(SourceT) :: src     ! Input src
    end subroutine
  end interface

  !----------------------------------------------------------------------------

  interface P
    module procedure Pstr, Pn
  end interface

  !----------------------------------------------------------------------------

contains

  !============================================================================
  ! Match
  !============================================================================

  function match_string(pattern, string) result(loc)
    class(PatternT)   :: pattern
    character*(*)     :: string
    !integer, optional :: start  ! Causes string to come in bad
    integer           :: loc
    type(StringSrcT):: src

    src = newStringSrc(string)
    !if (PRESENT(start)) src%loc = start
    loc = pattern%match(src)
  end function match_string

  !============================================================================
  ! StringSrcT
  !============================================================================

  !
  ! Create a new string source
  !
  function newStringSrc(string) result(src)
    character*(*)    :: string
    type(StringSrcT) :: src

    allocate(character(len(string)) :: src%string)
    src%string = string
    src%loc    = 1
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
    if (loc > len(this%string)+1) return
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
      if (0 > loc .or. loc > len(this%string)) return
      chr = this%string(loc:loc)
    else
      if (this%loc > len(this%string)) return
      chr = this%string(this%loc:this%loc)
      this%loc = this%loc + 1
    end if
    success = .true.
  end function StringSrcT_getChr

  !============================================================================

  function StringSrcT_getStr(this, i, j, string) result(success)
    class(StringSrcT)    :: this
    integer              :: i
    integer              :: j
    character(len=j-i+1) :: string
    logical :: success

    success = .false.
    if (i>j) return
    if (j > len(this%string)) return
    if (i < 1) return
    string = this%string(i:j)
    success = .true.
  end function StringSrcT_getStr

  !============================================================================
  ! Match
  !============================================================================

  !
  ! Called if a match fails. Reset the string to the given loc and set match
  ! value to NO_MATCH.
  !
  function noMatch(src, loc) result(match)
    class(SourceT) :: src
    integer        :: loc
    integer        :: match
    logical :: success

    success = src%setLoc(loc)
    match   = NO_MATCH
  end function noMatch

  !============================================================================

  !
  ! Check if the returned match index indicated that the match was successful
  !
  function isMatch(match)
    integer :: match
    logical :: isMatch

    isMatch = (match > 0)
  end function isMatch

  !============================================================================
  ! PstrT
  !============================================================================

  !
  ! Create a new exact string match pattern.
  !
  function Pstr(string) result(ptn)
    character*(*), intent(in)    :: string
    type(PstrT)                  :: pstr_
    class(PatternT), allocatable :: ptn

    allocate(character(len(string)) :: pstr_%string)
    pstr_%string = string
    allocate(ptn, source=pstr_)
  end function Pstr

  !============================================================================

  function PstrT_match(this, src) result(match)
    class(PstrT), intent(in) :: this
    class(SourceT)           :: src
    integer                  :: match
    integer :: start, i
    character :: c

    start = src%getLoc()
    do i = 1, len(this%string)
      if (.not.src%getChr(c)) goto 10
      if (c .ne. this%string(i:i)) goto 10
    end do
    match = src%getLoc()
    return
10  continue
    match = noMatch(src, start)
  end function PstrT_match

  !============================================================================
  ! PnT
  !============================================================================

  !
  ! Create a pattern to absorb n values.
  !
  function Pn(n) result(ptn)
    integer, intent(in)          :: n
    type(PnT)                    :: pn_
    class(PatternT), allocatable :: ptn

    pn_%n = n
    allocate(ptn, source=pn_)
  end function Pn

  !============================================================================

  function PnT_match(this, src) result(match)
    class(PnT), intent(in) :: this
    class(SourceT)         :: src
    integer                :: match
    integer :: start
    logical :: reset

    start = src%getLoc()

    ! Try to set to new location. If this does work pattern fails.
    reset = src%setLoc(start+this%n)
    if (.not.reset) goto 10

    match = src%getLoc()
    return
10  continue
    match = noMatch(src, start)
  end function PnT_match

  !============================================================================
  ! S
  !============================================================================

  !
  ! Create a pattern that matches values in set
  !
  function S(set) result(ptn)
    character*(*), intent(in)    :: set
    type(ST)                     :: s_
    class(PatternT), allocatable :: ptn
    integer :: i

    allocate(s_%chrs(len(set)))
    do i = 1, len(set)
      s_%chrs(i) = set(i:i)
    end do
    allocate(ptn, source=s_)
  end function S

  !============================================================================

  function ST_match(this, src) result(match)
    class(ST),      intent(in) :: this
    class(SourceT)             :: src
    integer                    :: match
    integer :: start, i
    character :: c

    start = src%getLoc()

    ! Look for a match in set.
    if (src%getChr(c)) then
      do i = 1, size(this%chrs)
        if (c == this%chrs(i)) goto 10
      end do
    end if
    match = noMatch(src, start)
    return

    ! Match found
10  match = src%getLoc()
  end function ST_match

  !============================================================================
  ! R
  !============================================================================

  !
  ! Create a pattern that matches values in range
  !
  function R(rng) result(ptn)
    character*2, intent(in)      :: rng
    type(RT)                     :: rt_
    class(PatternT), allocatable :: ptn

    rt_%rng(1) = rng(1:1)
    rt_%rng(2) = rng(2:2)
    allocate(ptn, source=rt_)
  end function R

  !============================================================================

  function RT_match(this, src) result(match)
    class(RT), intent(in) :: this
    class(SourceT)        :: src
    integer               :: match
    integer :: start
    character :: c

    start = src%getLoc()

    ! Look for a match in set.
    if (src%getChr(c)) then
      if (this%rng(1) <= c .and. c <= this%rng(2)) goto 10
    end if
    match = noMatch(src, start)
    return

    ! Match found
10  match = src%getLoc()
  end function RT_match

  !============================================================================
  ! V
  !============================================================================

  function VT_match(this, src) result(match)
  !
  ! Try to match the associated pattern
  !
    class(VT), intent(in) :: this
    class(SourceT)        :: src
    integer               :: match

    if (allocated(this%ptn)) then
      match = this%ptn%match(src)
    else
      match = src%getLoc()
    end if
  end function VT_match

  !============================================================================

  subroutine VT_setPattern(this, ptn)
    class(VT)                   :: this
    class(PatternT), intent(in) :: ptn

    allocate(this%ptn, source = ptn)
  end subroutine VT_setPattern

  !============================================================================
  ! PatternAssignT
  !============================================================================

  subroutine PatternAssign(to, from)
    class(PatternT), allocatable, intent(out) :: to
    class(PatternT),               intent(in) :: from

    allocate(to, source=from)
  end subroutine PatternAssign

  !============================================================================

  function PatternAssignT_match(this, src) result(match)
    class(PatternAssignT), intent(in) :: this
    class(SourceT)                    :: src
    integer                           :: match

    match = this%ptn%match(src)
  end function PatternAssignT_match

  !============================================================================
  ! PatternPlusT
  !============================================================================

  !
  ! Create a string pattern
  !
  function PatternPlus(ptn1, ptn2) result(ptn)
    class(PatternT), intent(in)  :: ptn1, ptn2
    type(PatternPlusT)           :: pls
    class(PatternT), allocatable :: ptn

    allocate(pls%ptn1, source = ptn1)
    allocate(pls%ptn2, source = ptn2)
    allocate(ptn, source=pls)
  end function PatternPlus

  !============================================================================

  function PatternPlusT_match(this, src) result(match)
    class(PatternPlusT), intent(in) :: this
    class(SourceT)                  :: src
    integer                         :: match
    integer :: start

    start = src%getLoc()
    match = this%ptn1%match(src)
    if (isMatch(match)) return
    match = this%ptn2%match(src)
    if (isMatch(match)) goto 10

    match = noMatch(src, start)
    return

    ! On success set match loc
10  match = src%getLoc()
  end function PatternPlusT_match

  !============================================================================
  ! PatternMinusT
  !============================================================================

  !
  ! Create a subtraction object
  !
  function PatternMinus(ptn1, ptn2) result(ptn)
    class(PatternT), intent(in)  :: ptn1, ptn2
    type(PatternMinusT)          :: mns
    class(PatternT), allocatable :: ptn

    allocate(mns%ptn1, source=ptn1)
    allocate(mns%ptn2, source=ptn2)
    allocate(ptn, source=mns)
  end function PatternMinus

  !============================================================================

  !
  ! Create a subtraction object
  !
  function PatternNeg(ptnmns) result(ptn)
    class(PatternT), intent(in)  :: ptnmns
    type(PatternMinusT)          :: mns
    class(PatternT), allocatable :: ptn

    allocate(mns%ptn2, source=ptnmns)
    allocate(ptn, source=mns)
  end function PatternNeg

  !============================================================================

  function PatternMinusT_match(this, src) result(match)
    class(PatternMinusT) :: this
    class(SourceT)       :: src
    integer              :: match
    integer :: start

    start = src%getLoc()

    ! Check that it doesn't match pattern 2
    match = this%ptn2%match(src)
    if (isMatch(match)) goto 10 ! negate the match

    ! Check that it does match pattern 1
    ! If pattern 1 is missing assume a match
    if (allocated(this%ptn1)) then
      match = this%ptn1%match(src)
      if (.not.isMatch(match)) goto 10
    end if

    match = src%getLoc()
    return

    ! On success set match loc
10  match = noMatch(src, start)
  end function PatternMinusT_match

  !============================================================================
  ! PatternTimesT
  !============================================================================

  !
  ! Create a string pattern
  !
  function PatternTimes(ptn1, ptn2) result(ptn)
    class(PatternT), intent(in)  :: ptn1, ptn2
    type(PatternTimesT)          :: tms
    class(PatternT), allocatable :: ptn

    allocate(tms%ptn1, source=ptn1)
    allocate(tms%ptn2, source=ptn2)
    allocate(ptn, source=tms)
  end function PatternTimes

  !============================================================================

  function PatternTimesT_match(this, src) result(match)
    class(PatternTimesT), intent(in) :: this
    class(SourceT)                   :: src
    integer                          :: match
    integer :: start

    start = src%getLoc()
    match = this%ptn1%match(src)
    if (.not.isMatch(match)) goto 10
    match = this%ptn2%match(src)
    if (.not.isMatch(match)) goto 10
    match = src%getLoc()
    return

    ! On failure reset to start
10  match = noMatch(src, start)
  end function PatternTimesT_match

  !============================================================================
  ! PatternTimesT
  !============================================================================

  !
  ! Create a pattern for P**n
  !
  function PatternPower(ptn1, n) result(ptn)
    class(PatternT), intent(in)  :: ptn1
    integer,         intent(in)  :: n
    type(PatternPowerT)          :: pwr
    class(PatternT), allocatable :: ptn

    allocate(pwr%ptn, source=ptn1)
    pwr%n = n
    allocate(ptn, source=pwr)
  end function PatternPower

  !============================================================================

  function PatternPowerT_match(this, src) result(match)
    class(PatternPowerT), intent(in) :: this
    class(SourceT)                   :: src
    integer                          :: match
    integer :: start, i, matcho

    matcho = NO_MATCH
    start = src%getLoc()
    if (this%n >=0) then
      i = 0
      do
        match = this%ptn%match(src)
        if (.not.isMatch(match)) then
          ! If the match succeeded less than n time
          ! return a failed match
          if (i < this%n) goto 10
          match = src%getLoc()
          return
        else if (match == matcho) then
          ! Pattern didn't consume input. The match failed.
          match = src%getLoc()
          return
        end if
        matcho = match
        i = i + 1
      end do
    else
      i = 0
      do
        match = this%ptn%match(src)
        if (isMatch(match)) i = i + 1
        if (.not.isMatch(match) .or. i == abs(this%n) .or. match == matcho) then
          match = src%getLoc()
          return
        end if
        match = matcho
      end do
    end if
    return

    ! On failure reset to start
10  match = noMatch(src, start)
  end function PatternPowerT_match

  !============================================================================
  ! Token
  !============================================================================

  function Token(ptn1, token_, listener) result(ptn)
    class(PatternT), intent(in)  :: ptn1
    character*(*),   intent(in)  :: token_
    procedure(tokenListener)     :: listener
    type(TokenT)                 :: ptnToken
    class(PatternT), allocatable :: ptn

    allocate(character(len=len(token_)) :: ptnToken%token)
    allocate(ptnToken%ptn, source=ptn1)
    ptnToken%token    =  token_
    ptnToken%listener => listener
    allocate(ptn, source=ptnToken)
  end function Token

  !============================================================================

  function TokenT_match(this, src) result(match)
    class(TokenT)  :: this
    class(SourceT) :: src
    integer        :: match
    integer :: i

    i = src%getLoc()
    match = this%ptn%match(src)
    if (match .ne. NO_MATCH) then
      call this%listener(this%token, i, match-1, src)
    end if
  end function TokenT_match

  !============================================================================

end module fpeg
