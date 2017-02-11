!< Concrete class of integrator, Euler explicit scheme.
module openpde_integrator_rk_explicit
    !< Concrete class of integrator, Euler explicit scheme.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_equation_abstract
    use openpde_field_abstract
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_integrator_abstract
    use openpde_kinds
    use openpde_field_FD_1D

    implicit none
    private
    public :: integrator_rk_explicit

    type, extends(integrator) :: integrator_rk_explicit
        !< Concrete class of integrator, Euler implicit scheme.
        real(R_P)                  :: alpha
        class(matrix), allocatable :: matA
        class(vector), allocatable :: vecB, vecS
        class(matrix), allocatable :: mat_identity
        integer(I_P)               :: n_stages
        real(R_P), allocatable     :: rk_alph(:,:)
        real(R_P), allocatable     :: rk_beta(:)
        real(R_P), allocatable     :: rk_gamm(:)
        class(field), allocatable  :: stages(:,:)
        contains
            ! deferred public methods
            procedure, pass(this) :: init      !< Initilize integrator.
            procedure, pass(this) :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            generic :: load => load_from_json !< Load integrator definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load integrator definition from jSON file.
    endtype integrator_rk_explicit
contains
    ! deferred public methods
    subroutine init(this, equ, description, filename, error)
        !< Initialize integrator.
        class(integrator_rk_explicit), intent(inout)         :: this        !< The integrator.
        class(equation),               intent(inout), target :: equ         !< The equation.
        character(*),                  intent(in),  optional :: description !< Integrator description.
        character(*),                  intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),                  intent(out), optional :: error       !< Error status.
        integer(I_P)                                         :: n           !< Counter.
        integer(I_P)                                         :: i           !< Counter.

        if (equ%enable_explicit) then
            this%n_stages = 5
            allocate(this%rk_alph(this%n_stages,this%n_stages))
            allocate(this%rk_beta(this%n_stages))
            allocate(this%rk_gamm(this%n_stages))
            this%rk_alph(:,:) = 0._R_P
            this%rk_beta(:)   = 0._R_P
            this%rk_gamm(:)   = 0._R_P
            allocate(this%stages(this%n_stages, equ%n_equ), mold=equ%resvar_e(1))

            select case(this%n_stages)
                case(1)
                    ! RK(1,1) Forward-Euler
                    this%rk_beta(1) = 1._R_P
                case(2)
                    ! SSPRK(2,2)
                    this%rk_beta(1) = 0.5_R_P
                    this%rk_beta(2) = 0.5_R_P

                    this%rk_alph(2, 1) = 1._R_P

                    this%rk_gamm(2) = 1._R_P
                case(3)
                    ! SSPRK(3,3)
                    this%rk_beta(1) = 1._R_P/6._R_P
                    this%rk_beta(2) = 1._R_P/6._R_P
                    this%rk_beta(3) = 2._R_P/3._R_P

                    this%rk_alph(2, 1) = 1._R_P
                    this%rk_alph(3, 1) = 0.25_R_P
                    this%rk_alph(3, 2) = 0.25_R_P

                    this%rk_gamm(2) = 1._R_P
                    this%rk_gamm(3) = 0.5_R_P
                case(5)
                    ! SSPRK(5,4)
                    this%rk_beta(1) = 0.14681187618661_R_P
                    this%rk_beta(2) = 0.24848290924556_R_P
                    this%rk_beta(3) = 0.10425883036650_R_P
                    this%rk_beta(4) = 0.27443890091960_R_P
                    this%rk_beta(5) = 0.22600748319395_R_P

                    this%rk_alph(2, 1)=0.39175222700392_R_P
                    this%rk_alph(3, 1)=0.21766909633821_R_P
                    this%rk_alph(3, 2)=0.36841059262959_R_P
                    this%rk_alph(4, 1)=0.08269208670950_R_P
                    this%rk_alph(4, 2)=0.13995850206999_R_P
                    this%rk_alph(4, 3)=0.25189177424738_R_P
                    this%rk_alph(5, 1)=0.06796628370320_R_P
                    this%rk_alph(5, 2)=0.11503469844438_R_P
                    this%rk_alph(5, 3)=0.20703489864929_R_P
                    this%rk_alph(5, 4)=0.54497475021237_R_P

                    this%rk_gamm(2) = 0.39175222700392_R_P
                    this%rk_gamm(3) = 0.58607968896780_R_P
                    this%rk_gamm(4) = 0.47454236302687_R_P
                    this%rk_gamm(5) = 0.93501063100924_R_P
            endselect

            n = equ%n_size

            call this%free
            if (present(description)) this%description = description
            if (present(filename)) then
                call this%load(filename=filename, error=error)
            else
                this%dt = 0.0001_R_P
                if (present(error)) error = 0
            endif

            this%alpha = 0._R_P
        elseif (equ%enable_implicit) then
            error stop "rk implicit to be implemented"
        elseif (equ%enable_multigrid) then
            error stop "rk multigrid to be implemented"
        endif

    end subroutine init

   function integrate(this, equ, t, inp) result(error)
      !< Integrate the field accordingly the to equation definition.
      class(integrator_rk_explicit), intent(inout)         :: this           !< The integrator.
      class(equation),               intent(inout), target :: equ            !< The equation.
      real(R_P),                     intent(in)            :: t              !< Time.
      class(field),                  intent(inout), target :: inp(:)         !< Input field.
      integer(I_P)                                         :: error          !< Error status.
      class(field), allocatable                            :: for(:)         !< Temporary.
      class(field_FD_1D), pointer                          :: inp_cur(:)     !< Field input pointer.
      class(field_FD_1D), pointer                          :: fields(:,:)    !< Fields pointer to MG fields.
      class(field_FD_1D), pointer                          :: fields0(:,:)   !< Initial fields pointer to MG fields.
      class(field_FD_1D), pointer                          :: residuals(:,:) !< Residual field pointer to MG fields.
      class(field_FD_1D), pointer                          :: sources(:,:)   !< Sources field pointer to MG fields.
      class(field_FD_1D), pointer                          :: tau(:,:)       !< Field input pointer.
      integer(I_P)                                         :: ie             !< Counter.
      integer(I_P)                                         :: iv             !< Counter.
      integer(I_P)                                         :: n_levels       !< Counter.
      integer(I_P)                                         :: i_mg           !< Counter.
      integer(I_P)                                         :: i_up           !< Counter.
      integer(I_P)                                         :: i_down         !< Counter.
      integer(I_P)                                         :: s
      integer(I_P)                                         :: ss

      if (equ%enable_explicit) then
          do s=1, this%n_stages
              do ie=1,size(inp)
                  this%stages(s,ie) = inp(ie)
              enddo
              do ie=1,size(inp)
                  do ss=1, s - 1
                      this%stages(s,ie) = this%stages(s,ie) + this%stages(ss,ie) * (this%dt * this%rk_alph(s, ss))
                  enddo
              enddo
              call equ%bc_e(inp=this%stages(s,:), t=t)
              call equ%resid_e(inp=this%stages(s,:), t=t)
              do ie=1,size(inp)
                  this%stages(s,ie) = equ%resvar_e(ie)
              enddo
          enddo
          ! computing new time step
          do ie=1,size(inp)
              do s=1, this%n_stages
                  inp(ie) = inp(ie) + this%stages(s,ie) * (this%dt * this%rk_beta(s))
              enddo
          enddo
          call equ%bc_e(inp=inp, t=t)
      elseif (equ%enable_implicit) then
          error stop "rk implicit to be implemented"
      elseif (equ%enable_multigrid) then
          error stop "rk mg to be implemented"
      endif

      error = 0
   end function integrate

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load integrator definition from JSON file.
        class(integrator_rk_explicit), intent(inout)         :: this            !< The integrator.
        character(*),                  intent(in)            :: filename        !< File name of JSON file.
        integer(I_P),                  intent(out), optional :: error           !< Error status.
        character(len=:), allocatable                        :: integrator_type !< Integrator type.
        type(json_file)                                      :: json            !< JSON file handler.
        logical                                              :: found           !< Flag inquiring the result json parsing.

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
        if (integrator_type=="rk implicit") then
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
            write(stderr, "(A)")' error: integrator definition of "'//filename//'" is not "rk implicit"!'
            stop
        endif
    endsubroutine load_from_json
end module openpde_integrator_rk_explicit
