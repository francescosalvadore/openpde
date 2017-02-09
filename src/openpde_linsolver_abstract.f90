!< Abstract class of linsolver A*x=b.
module openpde_linsolver_abstract
    !< Abstract class of linsolver.
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_kinds

    implicit none
    private
    public :: linsolver

    type, abstract :: linsolver
        !< Abstract class of linsolver.
        character(len=:), allocatable :: description !< Description.
        class(matrix), allocatable    :: mat         !< Matrix A
        class(vector), allocatable    :: vec         !< Vector b
        class(vector), allocatable    :: sol         !< Solution x
        contains
            ! deferred public methods
            procedure(abstract_init),       deferred :: init       !< Initilize field.
            procedure(abstract_set_matrix), deferred :: set_matrix !< Associate matrix
            procedure(abstract_set_vector), deferred :: set_vector !< Associate vector
            procedure(abstract_solve)     , deferred :: solve      !< Solve system.
    endtype linsolver

    ! deferred public methods interfaces
    abstract interface
        subroutine abstract_init(this, n)
            !< Init linsolver.
            import :: I_P, linsolver
            class(linsolver), intent(inout)         :: this 
            integer(I_P) :: n
        end subroutine abstract_init
    endinterface

    abstract interface
        subroutine abstract_set_matrix(this, mat)
            !< Set matrix.
            import :: linsolver, matrix
            class(linsolver), intent(inout)     :: this !< The field.
            class(matrix),  intent(in), target  :: mat  !< The mesh.
        end subroutine abstract_set_matrix
    endinterface

    abstract interface
        subroutine abstract_set_vector(this, vec)
            !< Set vector.
            import :: linsolver, vector
            class(linsolver), intent(inout)     :: this !< The field.
            class(vector),  intent(in), target  :: vec  !< The mesh.
        end subroutine abstract_set_vector
    endinterface

    abstract interface
        !< Initialize the field.
        subroutine abstract_solve(this)
            !< Solve system.
            import :: linsolver
            class(linsolver), intent(inout), target     :: this !< The field.
        end subroutine abstract_solve
    endinterface

contains
    ! public methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(linsolver), intent(inout) :: this !< The linsolver.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_linsolver_abstract
