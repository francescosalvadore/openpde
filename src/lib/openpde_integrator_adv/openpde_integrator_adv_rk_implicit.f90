!< Concrete class of integrator, Euler explicit scheme.
module openpde_integrator_adv_rk_implicit
    !< Concrete class of integrator, Euler explicit scheme.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use json_module
    use openpde_equation_adv
    use openpde_field_abstract
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_integrator_adv_abstract
    use openpde_kinds
    use openpde_field_FD_1D

    implicit none
    private
    public :: integrator_adv_rk_implicit

    type, extends(integrator_adv) :: integrator_adv_rk_implicit
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
    endtype integrator_adv_rk_implicit
contains
    ! deferred public methods
    subroutine init(this, equ, description, filename, error)
        !< Initialize integrator.
        class(integrator_adv_rk_implicit), intent(inout)     :: this        !< The integrator.
        class(equation_adv),  intent(inout),    target :: equ   !< The equation.
        character(*),        intent(in),  optional :: description !< Integrator description.
        character(*),       intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),       intent(out), optional :: error       !< Error status.
        integer(I_P) :: n
        integer(I_P) :: i

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
            !case(4)
            !    print*,"FOUR STAGES RK INITIALIZATION....."
            !    this%rk_alph(:) = (/0.,0.5,0.5,1./)
            !    this%rk_beta(:) = (/1./6.,2./6.,2./6.,1./6./)
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
      !< Integrate the field accordingly the to equation definition.
      class(integrator_adv_rk_implicit), intent(inout)         :: this           !< The integrator.
      class(equation_adv),                  intent(inout), target :: equ            !< The equation.
      real(R_P),                            intent(in)            :: t              !< Time.
      class(field),                         intent(inout), target :: inp(:)         !< Input field.
      integer(I_P)                                                :: error          !< Error status.
      class(field), allocatable, dimension(:)                     :: for            !< Temporary

      class(field_FD_1D), pointer                                 :: inp_cur(:)     !< Field input pointer.
      class(field_FD_1D), pointer                                 :: fields(:,:)    !< Fields pointer to MG fields.
      class(field_FD_1D), pointer                                 :: fields0(:,:)   !< Initial fields pointer to MG fields.
      class(field_FD_1D), pointer                                 :: residuals(:,:) !< Residual field pointer to MG fields.
      class(field_FD_1D), pointer                                 :: sources(:,:)   !< Sources field pointer to MG fields.
      class(field_FD_1D), pointer                                 :: tau(:,:)       !< Field input pointer.

      integer(I_P)                                                :: ie             !< Counter.
      integer(I_P)                                                :: iv             !< Counter.
      integer(I_P)                                                :: n_levels       !< Counter.
      integer(I_P)                                                :: i_mg           !< Counter.
      integer(I_P)                                                :: i_up           !< Counter.
      integer(I_P)                                                :: i_down         !< Counter.
      integer(I_P)                                                :: s
      integer(I_P)                                                :: ss

      if(equ%enable_explicit) then
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
      endif

      if(equ%enable_implicit) then
          STOP "rk implicit to be implemented"
          !print*,'Implicit solver enabled'
          ! (2a) Computes the residual term: modify "equ%resvar_i"
          call equ%resid_i(inp=inp, t=t)

          ! (2b) Compute the linear solver matrix and vector
          this%matA = this%mat_identity - this%dt * equ%resvar_i
          !this%matA = this%mat_identity - ((1._R_P-this%alpha)*this%dt) * equ%resvar_i
          this%vecB = equ%f2v_opr%operate(inp)

          ! (2c) Impose boundary conditions
          call equ%bc_i(matA=this%matA, vecB=this%vecB, t=t)
!          call this%matA%output("matA.dat")
!          call this%vecB%output("vecB.dat")

          ! (2d) Assign solver vector and matrix (A and b of A*x=b)
          call equ%solver%set_vector(this%vecB)
          call equ%solver%set_matrix(this%matA)

          ! (2e) Solve linear system
          call equ%solver%solve()

          ! (2f) Assign the solution to the input field
          this%vecS = equ%solver%sol
!          call this%vecS%output("vecS.dat")
          call equ%v2f_opr%operate(this%vecS, inp)
      endif

      if(equ%enable_multigrid) then
          STOP "rk mg to be implemented"
         n_levels = equ%mg%levels_number

         inp_cur => associate_field_FD_1D(field_input=inp, emsg='calling procedure integrator_adv_euler_implicit%integrate')
         tau => associate_field_FD_1D(field_input=equ%mg%tau, emsg='calling procedure integrator_adv_euler_implicit%integrate')
         fields => associate_field_FD_1D(field_input=equ%mg%fields, &
                                         emsg='calling procedure integrator_adv_euler_implicit%integrate')
         fields0 => associate_field_FD_1D(field_input=equ%mg%fields0, &
                                          emsg='calling procedure integrator_adv_euler_implicit%integrate')
         residuals => associate_field_FD_1D(field_input=equ%mg%residuals, &
                                            emsg='calling procedure integrator_adv_euler_implicit%integrate')
         sources => associate_field_FD_1D(field_input=equ%mg%sources, &
                                          emsg='calling procedure integrator_adv_euler_implicit%integrate')
         do ie=1, equ%n_equ
            fields(ie, 1) = inp_cur(ie)
         enddo

         !  du
         ! ---- + R(u) = 0
         !  dt

         ! Start multigrid v-cycle
         v_cycle: do iv=1,equ%mg%max_iterations
             ! Iterations for actual grid (fine grid) - Smoothing iteration
             i_mg = 1
             do i_up=1, equ%mg%n_it_up(i_mg)
                ! R(u) evaluation
                call equ%resid_emg(inp=fields(:,i_mg), t=t, output=residuals(:,i_mg))
                ! Summing du/dt (it results 0 only when converged)
                ! call this%temp_sum(residuals(:,i_mg)) ! TODO implement temp_sum
                ! Check convergence
                ! equ%mg%norm = equ%mg%compute_norm(residuals(:,i_mg)) ! TODO implement norm
                print*,"iv, Multigrid convergence norm: ", iv, equ%mg%norm
                if(equ%mg%norm <= equ%mg%tolerance) then
                   print*,"iv, Convergence reached: ", iv
                   exit v_cycle
                endif
                ! Update of current estimate
                do ie=1, equ%n_equ
                   fields(ie, i_mg) = fields(ie, i_mg) + residuals(ie, i_mg) * tau(ie, i_mg)
                enddo
             enddo

                ! Iterations for nested grids (coarse grids) - downward
             do i_mg=2, n_levels

                ! Tasks:
                ! (1) Approximation of coarse grid solution
                ! (2) Source term evaluation
                ! Restriction of solution fine => coarse
                ! fields(:,i_mg) = equ%mg%restriction(fields(:,i_mg-1)) ! TODO implement resistriction
                ! Save restriction
                do ie=1, equ%n_equ
                   fields0(ie, i_mg) = fields(ie, i_mg)
                enddo
                ! Collect residuals fine => coarse
                ! sources(:,i_mg) = equ%mg%collect(residuals(:,i_mg-1)) ! TODO implement collect
                ! Compute R(u) on coarse grid (same result as normal explicit case)
                call equ%resid_emg(inp=fields(:,i_mg), t=t, output=residuals(:,i_mg))
                ! Summing du/dt (it results 0 only when converged)
                ! call this%temp_sum(residuals(:,i_mg)) ! TODO implement temp_sum
                ! Compute source term for coarse grid
                do ie=1, equ%n_equ
                   sources(ie, i_mg) = sources(ie, i_mg) - residuals(ie, i_mg)
                enddo

                ! Tasks:
                ! (1) Smoothing iteration: new estimate computation
                !---------------------------------------------------------------------------
                do i_up=1, equ%mg%n_it_up(i_mg)
                   ! Compute R(u) on coarse grid (same result as normal explicit case)
                   call equ%resid_emg(inp=fields(:,i_mg), t=t, output=residuals(:,i_mg))
                   ! Summing du/dt (it results 0 only when converged)
                   ! call this%temp_sum(residuals(:,i_mg)) ! TODO implement temp_sum
                   ! Add source term
                   do ie=1, equ%n_equ
                      residuals(ie, i_mg) = residuals(ie, i_mg) + sources(ie, i_mg)
                   enddo
                   ! Update of current estimate
                   do ie=1, equ%n_equ
                      fields(ie, i_mg) = fields(ie, i_mg) + residuals(ie, i_mg) * tau(ie, i_mg)
                   enddo
                enddo
             enddo

             do i_mg=n_levels-1,2,-1
                ! Tasks:
                ! (1) Correction estimation
                ! (2) Prolungation
                ! Compute correction at coarser grid
                do ie=1, equ%n_equ
                   fields(ie, i_mg+1) = fields(ie, i_mg+1) - fields0(ie, i_mg+1) ! now fields is the correction
                enddo
                ! Prolongation coarse => fine
                ! fields(:,i_mg) = fields(:,i_mg) + equ%mg%prolongation(fields(:,i_mg+1)) ! TODO implement prolongation

                ! Tasks:
                ! (1) Smoothing iteration: new estimate computation
                do i_down=1, equ%mg%n_it_down(i_mg)
                    ! Compute R(u) on coarse grid (same result as normal explicit case)
                    call equ%resid_emg(inp=fields(:,i_mg), t=t, output=residuals(:,i_mg))
                    ! Summing du/dt (it results 0 only when converged)
                    ! call this%temp_sum(residuals(:,i_mg)) ! TODO implement temp_sum
                    ! Add source term
                    do ie=1, equ%n_equ
                       residuals(ie, i_mg) = residuals(ie, i_mg) + sources(ie, i_mg)
                    enddo
                    ! Update of current estimate
                    do ie=1, equ%n_equ
                       fields(ie, i_mg) = fields(ie, i_mg) + residuals(ie, i_mg) * tau(ie, i_mg)
                    enddo
                enddo

            enddo
            i_mg = 1
            ! Compute correction at coarser grid
            do ie=1, equ%n_equ
               fields(ie, i_mg+1) = fields(ie, i_mg+1) - fields0(ie, i_mg+1) ! now fields correction
            enddo
            ! Prolungation to finest grid
            ! fields(:,i_mg) = fields(:,i_mg) + equ%mg%prolongation(fields(:,i_mg+1)) ! TODO implement prolongation
         enddo v_cycle

         do ie=1, size(inp, dim=1)
             inp_cur(ie) =  fields(ie, 1)
         enddo

      endif
      error = 0
   end function integrate

    ! private methods
    subroutine load_from_json(this, filename, error)
        !< Load integrator definition from JSON file.
        class(integrator_adv_rk_implicit), intent(inout)     :: this            !< The integrator.
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
end module openpde_integrator_adv_rk_implicit
