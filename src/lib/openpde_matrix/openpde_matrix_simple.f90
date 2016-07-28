!< Concrete class of (square) matrix, naive.
module openpde_matrix_simple
    !< Concrete class of (square) matrix, naive.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_matrix_abstract
    use openpde_kinds

    implicit none
    private
    public :: associate_matrix_simple, matrix_simple

    type, extends(matrix) :: matrix_simple
        !< Naive matrix (not sparse)
        real(R_P), dimension(:,:), allocatable :: val  !< values
        contains
            ! deferred public methods
            procedure :: init 
            procedure :: output
            procedure :: set
            ! deferred private methods
            procedure, private :: assign_matrix
            procedure, private :: add
            procedure, private :: sub
            procedure, pass(rhs) :: realmul
    endtype matrix_simple
contains
    ! public, non TBP
    function associate_matrix_simple(matrix_input, emsg) result(matrix_pointer)
        !< Check the type of the matrix passed as input and return a matrix pointer with type matrix_simple
        class(matrix),      intent(in), target   :: matrix_input   !< Input matrix.
        character(*),       intent(in), optional :: emsg           !< Auxiliary error message.
        class(matrix_simple), pointer            :: matrix_pointer !< Simple matrix pointer.

        select type(matrix_input)
            type is(matrix_simple)
                matrix_pointer => matrix_input
            class default
               write(stderr, '(A)')'error: cast matrix to matrix_simple'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
    end function associate_matrix_simple

    function add(lhs, rhs) result(opr)
        !< Add matrix.
        class(matrix_simple), intent(in)      :: lhs     !< Left hand side.
        class(matrix), intent(in), target     :: rhs     !< Right hand side.
        class(matrix), allocatable, target    :: opr     !< Operator result.
        class(matrix_simple), pointer         :: opr_cur !< Dummy pointer for result.
        class(matrix_simple), pointer         :: rhs_cur !< Dummy pointer for rhs.

        rhs_cur => associate_matrix_simple(matrix_input=rhs, emsg='calling procedure matrix_simple%add')
        allocate(matrix_simple :: opr)
        opr_cur => associate_matrix_simple(matrix_input=opr, emsg='calling procedure matrix_simple%add')
        call opr_cur%init(lhs%n)
        opr_cur%val = lhs%val + rhs_cur%val
    end function add

    function sub(lhs, rhs) result(opr)
        !< Subtract matrix.
        class(matrix_simple), intent(in)      :: lhs     !< Left hand side.
        class(matrix), intent(in), target     :: rhs     !< Right hand side.
        class(matrix), allocatable, target    :: opr     !< Operator result.
        class(matrix_simple), pointer         :: opr_cur !< Dummy pointer for result.
        class(matrix_simple), pointer         :: rhs_cur !< Dummy pointer for rhs.

        rhs_cur => associate_matrix_simple(matrix_input=rhs, emsg='calling procedure matrix_simple%sub')
        allocate(matrix_simple :: opr)
        opr_cur => associate_matrix_simple(matrix_input=opr, emsg='calling procedure matrix_simple%sub')
        call opr_cur%init(lhs%n)
        opr_cur%val = lhs%val - rhs_cur%val
    end function sub
    function realmul(lhs, rhs) result(opr)
        !< Real matrix multiply.
        real(R_P), intent(in)                :: lhs     !< Left hand side.
        class(matrix_simple), intent(in)     :: rhs     !< Right hand side.
        class(matrix), allocatable, target   :: opr     !< Operator result.
        class(matrix_simple), pointer        :: opr_cur !< Dummy pointer for result.

        allocate(matrix_simple :: opr)
        opr_cur => associate_matrix_simple(matrix_input=opr, emsg='calling procedure matrix_simple%realmul')
        call opr_cur%init(rhs%n)
        opr_cur%val = lhs * rhs%val
    end function realmul
    
    subroutine assign_matrix(lhs, rhs)
        !< Assignment overloading.
        class(matrix_simple), intent(inout)  :: lhs     !< Left hand side.
        class(matrix), intent(in), target    :: rhs     !< Right hand side.
        class(matrix_simple), pointer        :: rhs_cur !< Dummy pointer for rhs.

        rhs_cur => associate_matrix_simple(matrix_input=rhs, emsg='calling procedure matrix_simple%assign')
        lhs%n   = rhs_cur%n
        lhs%val = rhs_cur%val
    end subroutine assign_matrix
  
    subroutine init(this, n, description, error)
        !< Initialize matrix
        class(matrix_simple),  intent(inout) :: this        !< The matrix.
        integer(I_P)                         :: n           !< Matrix size.
        character(*), intent(in),  optional  :: description !< Matrix description.
        integer(I_P), intent(out), optional  :: error       !< Error status.

        this%n = n
        allocate(this%val(n, n))
        !TODO maybe allocation and zeroification should be separed
        this%val(:,:) = 0._R_P
    end subroutine init

    subroutine output(this, filename, error)
        !< Print matrix
        class(matrix_simple),  intent(inout) :: this        !< The matrix.
        character(*), intent(in)             :: filename    !< Initialization file name.
        integer(I_P), intent(out), optional  :: error       !< Error status.
        integer(I_P)                         :: i           !< Loop index i
        integer(I_P)                         :: j           !< Loop index j

        open(unit=11,file=filename)
            do i =1,this%n
                do j =1,this%n
                    write(11,"(G15.8)", advance="no") this%val(i,j)
                enddo
                write(11,*)
            enddo
        close(11)
    end subroutine output

    subroutine set(this, i, j, val)
        !< Set matrix value
        class(matrix_simple),  intent(inout) :: this        !< The matrix.
        real(R_P), intent(in)                :: val         !< Value to be inserted
        integer(I_P), intent(in)             :: i           !< Index i where set matrix.
        integer(I_P), intent(in)             :: j           !< Index i where set matrix.

        this%val(i,j) = val

    end subroutine set
end module openpde_matrix_simple
