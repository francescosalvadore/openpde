!< Concrete class of vector, naive.
module openpde_vector_simple
    !< Concrete class of vector, naive.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_vector_abstract
    use openpde_kinds

    implicit none
    private
    public :: associate_vector_simple, vector_simple

    type, extends(vector) :: vector_simple
        !< Naive vector (not sparse)
        real(R_P), dimension(:), allocatable :: val  !< Vector values
        contains
            ! deferred public methods
            procedure :: init 
            procedure :: output
            procedure :: set
            procedure :: get
            ! deferred private methods
            procedure, private :: assign_vector
    endtype vector_simple
contains
    ! public, non TBP
    function associate_vector_simple(vector_input, emsg) result(vector_pointer)
        !< Check the type of the vector passed as input and return a vector pointer with type vector_simple
        class(vector),      intent(in), target   :: vector_input   !< Input vector.
        character(*),       intent(in), optional :: emsg           !< Auxiliary error message.
        class(vector_simple), pointer            :: vector_pointer !< Simple vector pointer.

        select type(vector_input)
            type is(vector_simple)
                vector_pointer => vector_input
            class default
               write(stderr, '(A)')'error: cast vector to vector_simple'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
    end function associate_vector_simple

    subroutine assign_vector(lhs, rhs)
        !< Assignment overloading.
        class(vector_simple), intent(inout)  :: lhs      !< Left hand side.
        class(vector), intent(in), target    :: rhs      !< Right hand side.
        class(vector_simple), pointer        :: rhs_cur  !< Dummy pointer for rhs.

        rhs_cur => associate_vector_simple(vector_input=rhs, emsg='calling procedure vector_simple%assign_vector')
        lhs%val = rhs_cur%val                            !TODO automatic lhs%val allocation can occur here, is it ok?
    end subroutine assign_vector

    subroutine init(this, n, description, error)
        !< Initialize vector.
        class(vector_simple),  intent(inout) :: this        !< The vector.
        integer(I_P), intent(in)             :: n           !< Vector number of elements.
        character(*), intent(in),  optional  :: description !< Vector description.
        integer(I_P), intent(out), optional  :: error       !< Error status.

        this%n = n
        allocate(this%val(n))
        this%val(:) = 0._R_P                                !TODO might be avoided here
    end subroutine init

    subroutine output(this, filename, error)
        !< Print (naively) vector
        class(vector_simple),  intent(inout) :: this        !< The vector.
        character(*), intent(in)             :: filename    !< Output file name.
        integer(I_P), intent(out), optional  :: error       !< Error status.
        integer(I_P)                         :: i           !< Loop index

        open(unit=11,file=filename)
            do i =1,this%n
                write(11,"(G15.8)") this%val(i)
            enddo
        close(11)
    end subroutine output

    subroutine set(this, i, val)
        !< Set vector value
        class(vector_simple),  intent(inout) :: this        !< The vector.
        real(R_P), intent(in)                :: val         !< Value to assign.
        integer(I_P), intent(in)             :: i           !< Vector index filled.

        this%val(i) = val

    end subroutine set

    function get(this, i) result(val)
        !< Get vector value
        class(vector_simple),  intent(in)    :: this        !< The vector.
        integer(I_P), intent(in)             :: i           !< Vector index filled.
        real(R_P)                            :: val         !< Value to assign.

        val = this%val(i)

    end function get
end module openpde_vector_simple
