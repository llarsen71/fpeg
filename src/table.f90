module Table

  !----------------------------------------------------------------------------
  ! FieldsT - An associative array (dictionary, hashtable)
  !----------------------------------------------------------------------------

  ! Store a set of name value pairs. The value can be of any type supported by class(*)
  type FieldsT
    character(len=:), allocatable :: name
    class(*), pointer             :: value
    logical                       :: autodealloc = .false.
    type(FieldsT), pointer        :: next => NULL()
  contains
    procedure :: getField => FieldsT_getField
    procedure :: getValuePtr => FieldsT_getValuePtr
    procedure :: getValueA => FieldsT_getValueA
    procedure :: getValueI => FieldsT_getValueI
    procedure :: getValueR => FieldsT_getValueR
    procedure :: setValuePtr => FieldsT_setValuePtr
    procedure :: setValueA => FieldsT_setValueA
    procedure :: setValueI => FieldsT_setValueI
    procedure :: setValueR => FieldsT_setValueR
    generic :: setValue => setValueA, setValueI, setValueR
  end type FieldsT

  !----------------------------------------------------------------------------
  ! ListT - A dynamic array
  !----------------------------------------------------------------------------

  ! Store a list of values. Value can be any value supported by class(*)
  type ListT
    logical               :: autodealloc = .false.
    class(*), pointer     :: value
    type(ListT), pointer  :: next => NULL()
  contains
    procedure :: getSize  => ListT_getSize
    procedure :: getItem  => ListT_getItem
    procedure :: getValuePtr => ListT_getValuePtr
    procedure :: setValuePtr => ListT_setValuePtr
    procedure :: addValuePtr => ListT_addValuePtr
  end type ListT

  !----------------------------------------------------------------------------
  ! TableT - A combination of associative array and list.
  !----------------------------------------------------------------------------

  type TableT
    type(FieldsT), pointer :: fields
    type(ListT),   pointer :: items
  contains
    procedure :: setFieldValue => TableT_setFieldValue
    procedure :: setItemValue  => TableT_setItemValue
    procedure :: getItemValue  => TableT_getItemValue
    procedure :: getFieldValue => TableT_getFieldValue
    generic :: getValue => getItemValue, getFieldValue
  end type TableT

  !----------------------------------------------------------------------------

contains

  !============================================================================
  ! FieldsT
  !============================================================================

  function FieldsT_getField(this, name, field) result(success)
    class(FieldsT), target  :: this
    character*(*)           :: name
    class(FieldsT), pointer :: field
    logical                 :: success

    field => this
    do
      if (.not.associated(field)) exit
      if (field%name == name) then
        success = .true.
        return
      end if
      field => field%next
    end do

    ! Failed to find field with the given name
    nullify(field)
    success = .false.
  end function FieldsT_getField

  !============================================================================

  function FieldsT_getValuePtr(this, name, value) result(success)
    class(FieldsT), target :: this
    character*(*)          :: name
    class(*), pointer      :: value
    logical                :: success
    class(FieldsT), pointer :: field

    success = .false.
    if (.not.this%getField(name, field)) return

    value => field%value
    success = .true.
  end function FieldsT_getValuePtr

  !============================================================================

  function FieldsT_getValueA(this, name, value) result(success)
    class(FieldsT), target    :: this
    character*(*)             :: name
    character(len=:), pointer :: value
    logical                   :: success
    class(FieldsT), pointer :: field
    class(*), pointer :: value1

    success = .false.
    if (.not.this%getValuePtr(name, value1)) return

    select type(value1)
    type is (character(len=*))
      value => value1
      success = .true.
    end select
  end function FieldsT_getValueA

  !============================================================================

  function FieldsT_getValueI(this, name, value) result(success)
    class(FieldsT), target :: this
    character*(*)          :: name
    integer                :: value
    logical                :: success
    class(FieldsT), pointer :: field
    class(*), pointer :: value1

    success = .false.
    if (.not.this%getValuePtr(name, value1)) return

    select type(value1)
    type is (integer)
      value = value1
      success = .true.
    end select
  end function FieldsT_getValueI

  !============================================================================

  function FieldsT_getValueR(this, name, value) result(success)
    class(FieldsT), target :: this
    character*(*)          :: name
    real                   :: value
    logical                :: success
    class(FieldsT), pointer :: field
    class(*), pointer :: value1

    success = .false.
    if (.not.this%getValuePtr(name, value1)) return

    select type(value1)
    type is (real)
      value   = value1
      success = .true.
    end select
  end function FieldsT_getValueR

  !============================================================================

  subroutine FieldsT_setValuePtr(this, name, value)
    class(FieldsT), target :: this
    character*(*)          :: name
    class(*), pointer      :: value
    class(FieldsT), pointer :: field

    field => this
    do
      if (field%name == name) exit
      if (.not.associated(field%next)) then
        allocate(field%next)
        field => field%next
        allocate(character(len(name)) :: field%name)
        field%name = name
        exit
      end if
      field => field%next
    end do
    field%value => value
  end subroutine FieldsT_setValuePtr

  !============================================================================

  subroutine FieldsT_setValueA(this, name, value)
    class(FieldsT), target :: this
    character*(*)          :: name
    character*(*)          :: value
    character(len=:), pointer :: A
    class(*), pointer :: cls

    allocate(character(len(value)) :: A)
    A = value
    cls => A
    call this%setValuePtr(name, cls)

  end subroutine FieldsT_setValueA

  !============================================================================

  subroutine FieldsT_setValueI(this, name, value)
    class(FieldsT), target :: this
    character*(*)          :: name
    integer                :: value
    integer, pointer :: I
    class(*), pointer :: cls

    allocate(I)
    I = value
    cls => I
    call this%setValuePtr(name, cls)

  end subroutine FieldsT_setValueI

  !============================================================================

  subroutine FieldsT_setValueR(this, name, value)
    class(FieldsT), target :: this
    character*(*)          :: name
    real, target           :: value
    real, pointer :: R
    class(*), pointer :: cls

    allocate(R)
    R = value
    cls => R
    call this%setValuePtr(name, cls)

  end subroutine FieldsT_setValueR

  !============================================================================
  ! ListT
  !============================================================================

  function ListT_getSize(this) result(sz)
!
! Get the size of the list.
!
    class(ListT), target  :: this
    integer               :: sz
    class(ListT), pointer :: item
    integer :: i

    sz = 1
    item => this
    do while(associated(item%next))
      sz = sz + 1
      item => item%next
    end do
  end function ListT_getSize

  !============================================================================

  function ListT_getItem(this, idx, item) result(success)
!
! Get list item at index. Negative numbers start from the end.
!
    class(ListT), target  :: this
    integer               :: idx
    class(ListT), pointer :: item
    logical               :: success
    integer :: i, idx2

    nullify(item)
    success = .false.
    if (idx == 0) return

    idx2 = idx
    if (idx < 0) idx2 = this%getSize() + 1 - idx
    if (idx2 < 1) return  ! Invalid index

    ! Loop through items till we reach the index
    item => this
    i = 1
    do while(idx2 > i)
      i = i + 1
      item => item%next
      if (.not.associated(item)) return ! Failed to find index
    end do
    success = .true.
  end function ListT_getItem

  !============================================================================

  function ListT_getValuePtr(this, idx, value) result(success)
    class(ListT), target :: this
    integer              :: idx
    class(*), pointer    :: value
    logical              :: success
    class(ListT), pointer :: item

    nullify(value)
    success = this%getItem(idx, item)
    if (.not.success) return
    value => item%value
  end function ListT_getValuePtr

  !============================================================================

  function ListT_setValuePtr(this, idx, value) result(success)
    class(ListT)      :: this
    integer           :: idx
    class(*), pointer :: value
    logical           :: success
    class(ListT), pointer :: item

    success = this%getItem(idx, item)
    if (.not.success) return

    ! Add item to list if needed
    if (.not.associated(item%next)) then
      allocate(item%next)
      item => item%next
    end if
    item%value => value
  end function ListT_setValuePtr

  !============================================================================

  subroutine ListT_addValuePtr(this, value)
    class(ListT)      :: this
    class(*), pointer :: value
    class(ListT), pointer :: item

    ! Get the last item in the list
    success = this%getItem(-1, item)

    ! Add item to list
    allocate(item%next)
    item => item%next
    item%value => value
  end subroutine ListT_addValuePtr

  !============================================================================
  ! TableT
  !============================================================================

  function TableT_getItemValue(this, idx, value) result(success)
    class(TableT)     :: this
    integer           :: idx
    class(*), pointer :: value
    logical           :: success

    success = this%items%getValuePtr(idx, value)
  end function TableT_getItemValue

  !============================================================================

  function TableT_getFieldValue(this, name, value) result(success)
    class(TableT)     :: this
    character*(*)     :: name
    class(*), pointer :: value
    logical           :: success

    success = this%fields%getValuePtr(name, value)
  end function TableT_getFieldValue

  !============================================================================

  subroutine TableT_setFieldValue(this, name, value)
    class(TableT)     :: this
    character*(*)     :: name
    class(*), pointer :: value

    call this%fields%setValuePtr(name, value)
  end subroutine TableT_setFieldValue

  !============================================================================

  function TableT_setItemValue(this, idx, value) result(success)
    class(TableT)     :: this
    integer           :: idx
    class(*), pointer :: value
    logical           :: success

    success = this%items%setValuePtr(idx, value)
  end function TableT_setItemValue

  !============================================================================

end module Table
