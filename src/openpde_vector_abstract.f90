!< Abstract class of vector
module openpde_vector_abstract
    !< Abstract class of vector
    use openpde_kinds

    implicit none
    private
    public :: vector

    type, abstract :: vector
        !< Abstract class of vector
        character(len=:), allocatable :: description  !< Vector description.
        integer(I_P) :: n                             !< Vector size.
        contains
            ! deferred public methods
            procedure(abstract_vectorinit),    deferred :: init   !< Initilize vector.
            procedure(abstract_vectoroutput) , deferred :: output !< Output vector.
            procedure(abstract_vectorset)    , deferred :: set    !< Set vector element.
            procedure(abstract_vectorget)    , deferred :: get    !< Get vector element.
            ! deferred private methods
            procedure(abstract_assign)       , private, deferred :: assign_vector !< Assign vector.
            ! public operators
            generic, public :: assignment(=) => assign_vector    !< Operator `= overloading.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype vector

    abstract interface
        !< Initialize vector.
        subroutine abstract_vectorinit(this, n, description, error)
            !< Initialize vector.
            import :: I_P, vector
            class(vector),  intent(inout)        :: this        !< The vector.
            integer(I_P), intent(in)             :: n           !< Vector number of elements.
            character(*), intent(in),  optional  :: description !< Vector description.
            integer(I_P), intent(out), optional  :: error       !< Error status.
        end subroutine abstract_vectorinit
    endinterface

    abstract interface
        !< Output vector.
        subroutine abstract_vectoroutput(this, filename, error)
            !< Output vector.
            import :: I_P, vector
            class(vector),  intent(inout)       :: this        !< The vector.
            character(*), intent(in)            :: filename    !< Output filename.
            integer(I_P), intent(out), optional :: error       !< Error status.
        end subroutine abstract_vectoroutput
    endinterface

    abstract interface
        !< Set vector element.
        subroutine abstract_vectorset(this, i, val)
            !< Set vector element.
            import :: R_P, I_P, vector
            class(vector),  intent(inout)       :: this        !< The vector.
            real(R_P), intent(in)               :: val         !< Value to be assigned
            integer(I_P), intent(in)            :: i           !< Index to set
        end subroutine abstract_vectorset
    endinterface

    abstract interface
        !< Get vector element.
        function abstract_vectorget(this, i) result(val)
            !< Set vector element.
            import :: R_P, I_P, vector
            class(vector),  intent(in)          :: this        !< The vector.
            integer(I_P), intent(in)            :: i           !< Index to get
            real(R_P)                           :: val         !< Value to be extracted
        end function abstract_vectorget
    endinterface

    abstract interface
        !< Assignment overloading.
        subroutine abstract_assign(lhs, rhs)
            !< Assignment overloading.
            import :: vector
            class(vector), intent(inout)      :: lhs           !< Left hand side.
            class(vector), intent(in), target :: rhs           !< Right hand side.
        end subroutine abstract_assign
    endinterface

contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(vector), intent(inout) :: this !< The vector.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_vector_abstract
