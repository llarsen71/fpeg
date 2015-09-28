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
  use Table, only: TableT
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
      class(PatternT) :: this
      class(SourceT)  :: src
      integer         :: idxend
    end function match
  end interface

  ! Convenience value that indicates no match was found
  integer, parameter :: NO_MATCH = -1

  !----------------------------------------------------------------------------

  interface operator(+)
    procedure PatternPlus
  end interface

  !----------------------------------------------------------------------------

  interface operator(-)
    procedure PatternNeg
    procedure PatternMinus
  end interface

  !----------------------------------------------------------------------------

  interface operator(*)
    procedure PatternTimes
  end interface

  !----------------------------------------------------------------------------

  interface operator (**)
    procedure PatternPower
  end interface

  !----------------------------------------------------------------------------
  ! PatternT objects
  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PstrT
    character(len=:), allocatable :: string
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

  type, extends(PatternT) :: ST
    character, allocatable :: chrs(:)
  contains
    procedure :: match => ST_match
  end type ST

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: RT
    character :: rng(2)
  contains
    procedure :: match => RT_match
  end type RT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PatternPlusT
    class(PatternT), pointer :: ptn1
    class(PatternT), pointer :: ptn2
  contains
    procedure :: match => PatternPlusT_match
  end type PatternPlusT

  !----------------------------------------------------------------------------

  type, extends(PatternT) :: PatternMinusT
    class(PatternT), pointer :: ptn1
    class(PatternT), pointer :: ptn2
  contains
    procedure :: match => PatternMinusT_match
  end type PatternMinusT

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

  interface P
    module procedure Pstr, Pn
  end interface

  !----------------------------------------------------------------------------
  ! Captures
  !----------------------------------------------------------------------------

  type CaptureT
    class(SourceT), pointer :: src
    integer :: idx1, idx2
  contains
    procedure :: getValue => CaptureT_getValue
  end Type CaptureT

  !----------------------------------------------------------------------------

  type CapturesT
    class(TableT),  pointer :: tbl
    class(SourceT), pointer :: src
  contains
    procedure :: addCapture => CapturesT_addCapture
    procedure :: getCapture => CapturesT_getCapture
    procedure :: addField   => CapturesT_addField
    procedure :: getField   => CapturesT_getField
    generic :: getValue => getCapture, getField
  end Type CapturesT

  !----------------------------------------------------------------------------

contains

  !============================================================================
  ! Match
  !============================================================================

  function match_string(pattern, string) result(loc)
    class(PatternT) :: pattern
    character*(*)   :: string
    integer         :: loc
    type(StringSrcT):: src

    src = newStringSrc(string)
    loc = pattern%match(src)
  end function match_string

  !============================================================================
  ! StringSrcT
  !============================================================================

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
    class(StringSrcT) :: this
    integer :: i
    integer :: j
    character*(*) :: string
    logical :: success

    success = .false.
    if (i<j) return
    if (j > len(this%string)) return
    if (i < 1) return
    string = this%string(i:j)
    success = .true.
  end function StringSrcT_getStr

  !============================================================================
  ! Match
  !============================================================================

  function noMatch(src, loc) result(match)
    class(SourceT) :: src
    integer        :: loc
    integer        :: match
    logical :: success

    success = src%setLoc(loc)
    match   = NO_MATCH
  end function noMatch

  !============================================================================

  function isMatch(match)
    integer :: match
    logical :: isMatch

    isMatch = (match > 0)
  end function isMatch

  !============================================================================
  ! PstrT
  !============================================================================

  function Pstr(string) result(ptn)
!
! Create a string pattern
!
    character*(*)            :: string
    class(PatternT), pointer :: ptn
    type(PstrT), pointer :: pstr1

    allocate(pstr1)
    allocate(character(len(string)) :: pstr1%string)
    pstr1%string = string
    ptn => pstr1
  end function Pstr

  !============================================================================

  function PstrT_match(this, src) result(match)
    class(PstrT)   :: this
    class(SourceT) :: src
    integer        :: match
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

  function Pn(n) result(ptn)
!
! Create a pattern to absorb n values.
!
    integer :: n
    class(PatternT), pointer :: ptn
    type(PnT), pointer :: pn1

    allocate(pn1)
    pn1%n = n
    ptn => pn1
  end function Pn

  !============================================================================

  function PnT_match(this, src) result(match)
    class(PnT)     :: this
    class(SourceT) :: src
    integer        :: match
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

  function S(set) result(ptn)
!
! Create a pattern that matches values in set
!
    character*(*)            :: set
    class(PatternT), pointer :: ptn
    type(ST), pointer :: pset
    integer :: i

    allocate(pset)
    allocate(pset%chrs(len(set)))
    do i = 1, len(set)
      pset%chrs(i) = set(i:i)
    end do
    ptn => pset
  end function S

  !============================================================================

  function ST_match(this, src) result(match)
    class(ST)      :: this
    class(SourceT) :: src
    integer        :: match
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

  function R(rng) result(ptn)
!
! Create a pattern that matches values in range
!
    character*2              :: rng
    class(PatternT), pointer :: ptn
    type(RT), pointer :: prng

    allocate(prng)
    prng%rng(1) = rng(1:1)
    prng%rng(2) = rng(2:2)
    ptn => prng
  end function R

  !============================================================================

  function RT_match(this, src) result(match)
    class(RT)      :: this
    class(SourceT) :: src
    integer        :: match
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
  ! PatternPlusT
  !============================================================================

  function PatternPlus(ptn1, ptn2) result(ptn)
!
! Create a string pattern
!
    class(PatternT), pointer, intent(in) :: ptn1, ptn2
    class(PatternT), pointer             :: ptn
    type(PatternPlusT), pointer :: pls

    allocate(pls)
    pls%ptn1 => ptn1
    pls%ptn2 => ptn2
    ptn => pls
  end function PatternPlus

  !============================================================================

  function PatternPlusT_match(this, src) result(match)
    class(PatternPlusT) :: this
    class(SourceT)      :: src
    integer             :: match
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

  function PatternMinus(ptn1, ptn2) result(ptn)
!
! Create a subtraction object
!
    class(PatternT), pointer, intent(in) :: ptn1, ptn2
    class(PatternT), pointer             :: ptn
    type(PatternMinusT), pointer :: mns

    allocate(mns)
    mns%ptn1 => ptn1
    mns%ptn2 => ptn2
    ptn => mns
  end function PatternMinus

  !============================================================================

  function PatternNeg(ptnmns) result(ptn)
!
! Create a subtraction object
!
    class(PatternT), pointer, intent(in) :: ptnmns
    class(PatternT), pointer             :: ptn
    type(PatternMinusT), pointer :: mns

    allocate(mns)
    nullify(mns%ptn1)
    mns%ptn2 => ptnmns
    ptn => mns
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
    if (associated(this%ptn1)) then
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

  function PatternTimes(ptn1, ptn2) result(ptn)
!
! Create a string pattern
!
    class(PatternT), pointer, intent(in) :: ptn1, ptn2
    class(PatternT), pointer             :: ptn
    type(PatternTimesT), pointer :: tms

    allocate(tms)
    tms%ptn1 => ptn1
    tms%ptn2 => ptn2
    ptn => tms
  end function PatternTimes

  !============================================================================

  function PatternTimesT_match(this, src) result(match)
    class(PatternTimesT) :: this
    class(SourceT)       :: src
    integer              :: match
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

  function PatternPower(ptn, n) result(rptn)
!
! Create a pattern for P**n
!
    class(PatternT), pointer, intent(in) :: ptn
    integer,                  intent(in) :: n
    class(PatternT), pointer             :: rptn
    type(PatternPowerT), pointer :: pwr

    allocate(pwr)
    pwr%ptn => ptn
    pwr%n = n
    rptn => pwr
  end function PatternPower

  !============================================================================

  function PatternPowerT_match(this, src) result(match)
    class(PatternPowerT) :: this
    class(SourceT)       :: src
    integer              :: match
    integer :: start, i, matcho

    matcho = NO_MATCH
    start = src%getLoc()
    if (this%n >=0) then
      i = 0
      do
        match = this%ptn%match(src)
        if (.not.isMatch(match)) then
          if (i < this%n) goto 10
          match = src%getLoc()
          return
        else if (match == matcho) then
          ! Pattern didn't consume input
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
        if (.not.isMatch(match) .or. i == abs(this%n) .or. match == matcho) then
          match = src%getLoc()
          return
        end if
        match = matcho
        i = i + 1
      end do
    end if
    return

    ! On failure reset to start
10  match = noMatch(src, start)
  end function PatternPowerT_match

  !============================================================================
  ! CaptureT
  !============================================================================

  subroutine CaptureT_getValue(this, string)
  !
  ! Get a capture string from the SourceT.
  !
    class(CaptureT)               :: this
    character(len=:), allocatable :: string
    integer :: sz
    logical :: success

    sz = this%idx2 - this%idx1 + 1
    if (sz < 0) return
    allocate(character(this%idx2 - this%idx1 + 1) :: string)
    success = this%src%getStr(this%idx1, this%idx2, string)
  end subroutine CaptureT_getValue

  !============================================================================
  ! CapturesT
  !============================================================================

  subroutine CapturesT_addCapture(this, idx1, idx2)
    class(CapturesT) :: this
    integer          :: idx1, idx2
    class(CaptureT), pointer :: capt
    class(*), pointer :: value
    logical :: success

    allocate(capt)
    capt%idx1 = idx1
    capt%idx2 = idx2
    capt%src => this%src

    value => capt
    success = this%tbl%setItemValue(0, value)
  end subroutine CapturesT_addCapture

  !============================================================================

  function CapturesT_getCapture(this, idx, string) result(success)
    class(CapturesT)              :: this
    integer                       :: idx
    character(len=:), allocatable :: string
    logical                       :: success
    class(*), pointer :: value

    success = this%tbl%getItemValue(idx, value)
    if (.not.success) return

    select type(value)
    type is (CaptureT)
      allocate(character(value%idx2-value%idx1+1) :: string)
      success = value%src%getStr(value%idx1, value%idx2, string)
      return
    end select
    success = .false.
  end function CapturesT_getCapture

  !============================================================================

  subroutine CapturesT_addField(this, name, value)
    class(CapturesT)  :: this
    character*(*)     :: name
    class(*), pointer :: value

    call this%tbl%setFieldValue(name, value)
  end subroutine CapturesT_addField

  !============================================================================

  function CapturesT_getField(this, name, value) result(success)
    class(CapturesT)  :: this
    character*(*)     :: name
    class(*), pointer :: value
    logical           :: success

    success = this%tbl%getFieldValue(name, value)
  end function CapturesT_getField

  !============================================================================

end module fpeg
