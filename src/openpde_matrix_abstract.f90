!< Abstract class of matrix.
module openpde_matrix_abstract
    !< Abstract class of matrix.
    use openpde_kinds

    implicit none
    private
    public :: matrix

    type, abstract :: matrix
        !< Abstract class of matrix.
        character(len=:), allocatable :: description !< Matrix description.
        integer(I_P) :: n
        contains
            ! deferred public methods
            procedure(abstract_matrixinit),    deferred :: init   !< Initilize matrix.
            procedure(abstract_matrixoutput) , deferred :: output !< Output matrix.
            procedure(abstract_matrixset)    , deferred :: set    !< Set matrix value.
            ! deferred private methods
            procedure(abstract_simmetric_operator)      , private, deferred :: add           !< Add matrix.
            procedure(abstract_simmetric_operator)      , private, deferred :: sub           !< Sub matrix.
            procedure(abstract_real_op_field), pass(rhs), private, deferred :: realmul       !< Real*matrix.
            procedure(abstract_assign)                  , private, deferred :: assign_matrix !< Assign matrix. 
            ! public operators
            generic, public :: assignment(=) => assign_matrix  !< Operator `=` overloading. 
            generic, public :: operator(+)   => add            !< Operator `+` overloading.
            generic, public :: operator(-)   => sub            !< Operator `- overloading.
            generic, public :: operator(*)   => realmul        !< Operator `*` overloading.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype matrix

    abstract interface
        !< Assignment overloading.
        function abstract_simmetric_operator(lhs, rhs) result(opr)
            !< Assignment overloading.
            import :: matrix
            class(matrix), intent(in)          :: lhs  !< Left hand side.
            class(matrix), intent(in), target  :: rhs  !< Right hand side.
            class(matrix), allocatable, target :: opr  !< Result
        end function abstract_simmetric_operator
    endinterface

    abstract interface
        function abstract_real_op_field(lhs, rhs) result(opr)
            !< Non symmetric operator real.op.field.
            import :: matrix, R_P
            real(R_P),    intent(in)  :: lhs !< Left hand side.
            class(matrix), intent(in)  :: rhs !< Right hand side.
            class(matrix), allocatable, target :: opr !< Operator result.
        end function abstract_real_op_field
    endinterface

    abstract interface
        !< Assignment overloading.
        subroutine abstract_assign(lhs, rhs)
            !< Assignment overloading.
            import :: matrix
            class(matrix), intent(inout)      :: lhs !< Left hand side.
            class(matrix), intent(in), target :: rhs !< Right hand side.
        end subroutine abstract_assign
    endinterface

    abstract interface
        !< Initialize mesh.
        subroutine abstract_matrixinit(this, n, description, error)
            !< Initialize mesh.
            import :: I_P, matrix
            class(matrix),  intent(inout)         :: this         !< The matrix.
            integer(I_P)                          :: n            !< Initialization file name.
            character(*), intent(in),  optional   :: description  !< Mesh description.
            integer(I_P), intent(out), optional   :: error        !< Error status.
        end subroutine abstract_matrixinit
    endinterface

    abstract interface
        !< Initialize mesh.
        subroutine abstract_matrixoutput(this, filename, error)
            !< Initialize mesh.
            import :: I_P, matrix
            class(matrix),  intent(inout)       :: this      !< The matrix.
            character(*), intent(in)            :: filename    !< Initialization file name.
            integer(I_P), intent(out), optional :: error       !< Error status.
        end subroutine abstract_matrixoutput
    endinterface

    abstract interface
        !< Initialize mesh.
        subroutine abstract_matrixset(this, i, j, val)
            !< Initialize mesh.
            import :: R_P, I_P, matrix
            class(matrix),  intent(inout)       :: this     
            real(R_P), intent(in)               :: val
            integer(I_P), intent(in)            :: i, j
        end subroutine abstract_matrixset
    endinterface

contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(matrix), intent(inout) :: this !< The integrator.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_matrix_abstract
