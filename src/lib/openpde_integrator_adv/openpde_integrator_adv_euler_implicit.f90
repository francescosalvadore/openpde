!< Concrete class of integrator, Euler explicit scheme.
module openpde_integrator_adv_euler_implicit
    !< Concrete class of integrator, Euler explicit scheme.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_equation_adv
    use openpde_field_abstract
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_integrator_adv_abstract
    use openpde_kinds

    implicit none
    private
    public :: integrator_adv_euler_implicit

    type, extends(integrator_adv) :: integrator_adv_euler_implicit
        !< Concrete class of integrator, Euler implicit scheme.
        real(R_P)                         :: alpha
        class(matrix), allocatable        :: matA
        class(vector), allocatable        :: vecB, vecS
        class(matrix), allocatable        :: mat_identity
        contains
            ! deferred public methods
            procedure, pass(this) :: init      !< Initilize integrator.
            procedure, pass(this) :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            generic :: load => load_from_json !< Load integrator definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load integrator definition from jSON file.
    endtype integrator_adv_euler_implicit
contains
    ! deferred public methods
    subroutine init(this, equ, description, filename, error)
        !< Initialize integrator.
        class(integrator_adv_euler_implicit), intent(inout)     :: this        !< The integrator.
        class(equation_adv),  intent(inout),    target :: equ   !< The equation.
        character(*),        intent(in),  optional :: description !< Integrator description.
        character(*),       intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),       intent(out), optional :: error       !< Error status.
        integer(I_P) :: n
        integer(I_P) :: i

        n = equ%n_size

        call this%free
        if (present(description)) this%description = description
        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%dt = 0.001_R_P
            if (present(error)) error = 0
        endif

        this%alpha = 0._R_P

        allocate(this%matA, mold=equ%solver%mat)
        allocate(this%vecB, mold=equ%solver%vec)
        allocate(this%vecS, mold=equ%solver%sol)
        call this%matA%init(n)
        call this%vecB%init(n)
        call this%vecS%init(n)

        allocate(this%mat_identity, mold=equ%solver%mat)
        call this%mat_identity%init(n)

        do i=1,this%mat_identity%n
            call this%mat_identity%set(i, i, 1._R_P)
        enddo
        !call this%mat_identity%output("identity.dat")

    end subroutine init

    function integrate(this, equ, t, inp) result(error)
        !< Integrate the field accordingly the to equation by means of the Euler explicit scheme.
        use openpde_field_FD_1D
        class(integrator_adv_euler_implicit), intent(inout)            :: this  !< The integrator.
        class(equation_adv),                  intent(inout),    target :: equ   !< The equation.
        real(R_P),                        intent(in)            :: t     !< Time.
        class(field), intent(inout), target, dimension(:) :: inp   !< Input field.
        integer(I_P)                                            :: error !< Error status.
        class(field), allocatable, dimension(:)                               :: for   !< Temporary
        integer(I_P)                                            :: ie !< Count equation
       
        !--------------------------------------------------------------------------------------------
        ! (1) Explicit section
        !--------------------------------------------------------------------------------------------
        print*,'equ%enable_explicit :',equ%enable_explicit
        print*,'equ%enable_implicit :',equ%enable_implicit
        if(equ%enable_explicit) then
            print*,'Explicit solver enabled'
            ! (1a) Imposes boundary conditions: modify "inp" field array
            call equ%bc_e(inp=inp, t=t)

            ! (1b) Computes the residual term: modify "equ%resvar_e" 
            call equ%resid_e(inp=inp, t=t)

            ! (1c) Updates the inp field according to Euler scheme
            do ie=1,size(inp)
                inp(ie) = inp(ie) + this%dt * equ%resvar_e(ie) 
            enddo
        endif
        !--------------------------------------------------------------------------------------------
        ! (2) Implicit section
        !--------------------------------------------------------------------------------------------
        if(equ%enable_implicit) then
            print*,'Implicit solver enabled'
            ! (2a) Computes the residual term: modify "equ%resvar_i" 
            call equ%resid_i(inp=inp, t=t)

            ! (2b) Compute the linear solver matrix and vector
            this%matA = this%mat_identity - this%dt * equ%resvar_i
            !this%matA = this%mat_identity - ((1._R_P-this%alpha)*this%dt) * equ%resvar_i
            this%vecB = equ%f2v_opr%operate(inp)

            ! (2c) Impose boundary conditions
            call equ%bc_i(matA=this%matA, vecB=this%vecB, t=t)
!            call this%matA%output("matA.dat")
!            call this%vecB%output("vecB.dat")

            ! (2d) Assign solver vector and matrix (A and b of A*x=b)
            call equ%solver%set_vector(this%vecB)
            call equ%solver%set_matrix(this%matA)

            ! (2e) Solve linear system
            call equ%solver%solve()

            ! (2f) Assign the solution to the input field
            this%vecS = equ%solver%sol
!            call this%vecS%output("vecS.dat")
            call equ%v2f_opr%operate(this%vecS, inp)
        endif

!        STOP

        error = 0
    end function integrate

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load integrator definition from JSON file.
        class(integrator_adv_euler_implicit), intent(inout)     :: this            !< The integrator.
        character(*),                     intent(in)            :: filename        !< File name of JSON file.
        integer(I_P),                     intent(out), optional :: error           !< Error status.
        character(len=:), allocatable                           :: integrator_type !< Integrator type.
        type(json_file)                                         :: json            !< JSON file handler.
        logical                                                 :: found           !< Flag inquiring the result json parsing.

        call json%initialize()
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%load_file(filename=filename)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        call json%get('integrator.type', integrator_type, found)
        if (json%failed()) then
            call json%print_error_message(stderr) ; stop
        end if
        if (.not.found) then
            write(stderr, "(A)")' error: integrator definition of "'//filename//'" incomplete!'
            write(stderr, "(A)")'   "type" missing'
            stop
        endif
        if (integrator_type=="euler explicit") then
            call json%get('integrator.dt', this%dt, found)
            if (json%failed()) then
                call json%print_error_message(stderr) ; stop
            end if
            if (.not.found) then
                write(stderr, "(A)")' error: integrator definition of "'//filename//'" incomplete!'
                write(stderr, "(A)")'   "dt" missing'
                stop
            endif
        else
            write(stderr, "(A)")' error: integrator definition of "'//filename//'" is not "euler explicit"!'
            stop
        endif
    endsubroutine load_from_json
end module openpde_integrator_adv_euler_implicit
