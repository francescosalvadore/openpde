!< Simil-abstract class of equation advanced.
module openpde_equation_adv
    !< Simil-abstract class of equation advanced.
    use openpde_field_abstract
    use openpde_v2f_abstract
    use openpde_f2v_abstract
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_mesh_abstract
    use openpde_multigrid_abstract
    use openpde_linsolver_abstract
    use openpde_kinds

    implicit none
    private
    public :: equation_adv

    type :: equation_adv
        !< Simil-abstract class of equation_adv.
        !<
        !< The really-concrete types are implemented at application level (by the user).
        character(len=:), allocatable :: description              !< Equation description.
        integer(I_P)                  :: n_equ=0_I_P              !< Number of equations
        class(mesh), pointer          :: m                        !< Mesh pointer
        logical                       :: enable_explicit=.false.  !< Activate explicit solver.
        logical                       :: enable_implicit=.false.  !< Activate implicit solver.
        logical                       :: enable_multigrid=.false. !< Activate multigrid solver.
        ! Explicit section
        class(field), allocatable, dimension(:) :: resvar_e !< Residuals field for explicit solver.
        ! Implicit section
        integer(I_P)                  :: n_size   !< Number of elements (vector size for linear solvers).
        class(matrix), allocatable    :: resvar_i !< Residuals field for implicit solver.
        class(linsolver), allocatable :: solver   !< Linear solver.
        class(f2v), allocatable       :: f2v_opr  !< Field to vector converter.
        class(v2f), allocatable       :: v2f_opr  !< Vector to Field converter.
        ! Multigrid section
        class(multigrid), allocatable           :: mg        !< Multigrid solver.
        class(field), allocatable, dimension(:) :: resvar_mg !< Residuals field for multigrid solver.
        class(field), allocatable, dimension(:) :: source_mg !< Sources field for multigrid solver.
        contains
            ! not deferred but to be implemented by concrete
            procedure :: bc_e      !< Equation boundary conditions explicit.
            procedure :: bc_i      !< Equation boundary conditions implicit.
            procedure :: init      !< Initialize the equation.
            procedure :: jacobian  !< Jacobian for multigrid solution
            procedure :: resid_e   !< Residual explicit
            procedure :: resid_emg !< Residual for multigrid
            procedure :: resid_i   !< Residual implicit
            ! public methods
            procedure :: free !< Free dynamic memory.
    endtype equation_adv
contains
    subroutine init(this, n_equ, field_mesh, inp, description, filename, error)
        !< Initialize equation.
        class(equation_adv), intent(inout)         :: this        !< The equation.
        integer(I_P),        intent(in)            :: n_equ       !< Number of equations
        class(mesh),         intent(in), target    :: field_mesh  !< Mesh of the field.
        class(field),        intent(inout), target :: inp(:)      !< Input field.
        character(*),        intent(in),  optional :: description !< Equation description.
        character(*),        intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),        intent(out), optional :: error       !< Error status.
        STOP 'init to be implemented by your equation and depending on your integrator'
    end subroutine init

    subroutine bc_e(this, inp, t)
        !< Equation boundary condition.
        class(equation_adv), intent(in)            :: this   !< The equation.
        class(field),        intent(inout), target :: inp(:) !< Input field.
        real(R_P),           intent(in)            :: t      !< Time.
        STOP 'bc_e to be implemented by your equation and depending on your integrator'
    end subroutine bc_e

    subroutine bc_i(this, matA, vecB, t)
        !< Equation boundary condition.
        class(equation_adv), intent(in)            :: this !< The equation.
        class(matrix),       intent(inout), target :: matA !< Input field.
        class(vector),       intent(inout), target :: vecB !< Input field.
        real(R_P),           intent(in)            :: t    !< Time.
        STOP 'bc_i to be implemented by your equation and depending on your integrator'
    end subroutine bc_i

    subroutine resid_e(this, inp, t)
        !< Return the field after forcing the equation.
        class(equation_adv), intent(inout)      :: this   !< The equation.
        class(field),        intent(in), target :: inp(:) !< Input field.
        real(R_P),           intent(in)         :: t      !< Time.
        STOP 'resid_e to be implemented by your equation and depending on your integrator'
    end subroutine resid_e

    subroutine resid_emg(this, inp, t, output)
        !< Return the field after forcing the equation.
        class(equation_adv), intent(inout)       :: this      !< The equation.
        class(field),        intent(in),  target :: inp(:)    !< Input field.
        class(field),        intent(out), target :: output(:) !< Input field.
        real(R_P),           intent(in)          :: t         !< Time.
        STOP 'resid_emg to be implemented by your equation and depending on your integrator'
    end subroutine resid_emg

    subroutine resid_i(this, inp, t)
        !< Return the field after forcing the equation.
        class(equation_adv), intent(inout)      :: this   !< The equation.
        class(field),        intent(in), target :: inp(:) !< Input field.
        real(R_P),           intent(in)         :: t      !< Time.
        STOP 'resid_i to be implemented by your equation and depending on your integrator'
    end subroutine resid_i

    function jacobian(this, inp, t) result(opr)
        !< Return the jacobian, useful for multigrid
        class(equation_adv), intent(in)         :: this !< The equation.
        class(field),        intent(in), target :: inp  !< Input field.
        real(R_P),           intent(in)         :: t    !< Time.
        class(matrix), allocatable              :: opr  !< Field computed.
        STOP 'jacobian to be implemented by your equation and depending on your integrator'
    end function jacobian

    elemental subroutine free(this)
        !< Free dynamic memory.
        !<
        !< To be completed.
        class(equation_adv), intent(inout) :: this !< The equation.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_equation_adv
