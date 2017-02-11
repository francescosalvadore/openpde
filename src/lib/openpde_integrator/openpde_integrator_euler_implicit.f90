!< Concrete class of integrator, Euler implicit scheme.
module openpde_integrator_euler_implicit
    !< Concrete class of integrator, Euler implicit scheme.
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
    public :: integrator_euler_implicit

    type, extends(integrator) :: integrator_euler_implicit
        !< Concrete class of integrator, Euler implicit scheme.
        real(R_P)                  :: alpha
        class(matrix), allocatable :: matA
        class(vector), allocatable :: vecB, vecS
        class(matrix), allocatable :: mat_identity
        contains
            ! deferred public methods
            procedure, pass(this) :: init      !< Initilize integrator.
            procedure, pass(this) :: integrate !< Integrate the field accordingly to the equation.
            ! public methods
            generic :: load => load_from_json !< Load integrator definition from file.
            ! private methods
            procedure, pass(this), private :: load_from_json !< Load integrator definition from jSON file.
    endtype integrator_euler_implicit
contains
    ! deferred public methods
    subroutine init(this, equ, description, filename, error)
        !< Initialize integrator.
        class(integrator_euler_implicit), intent(inout)         :: this        !< The integrator.
        class(equation),                  intent(inout), target :: equ         !< The equation.
        character(*),                     intent(in),  optional :: description !< Integrator description.
        character(*),                     intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),                     intent(out), optional :: error       !< Error status.
        integer(I_P) :: n
        integer(I_P) :: i

        n = equ%n_size

        call this%free
        if (present(description)) this%description = description
        if (present(filename)) then
            call this%load(filename=filename, error=error)
        else
            this%dt = 0.005_R_P
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
      class(integrator_euler_implicit), intent(inout)         :: this           !< The integrator.
      class(equation),                  intent(inout), target :: equ            !< The equation.
      real(R_P),                        intent(in)            :: t              !< Time.
      class(field),                     intent(inout), target :: inp(:)         !< Input field.
      integer(I_P)                                            :: error          !< Error status.
      class(field), allocatable, dimension(:)                 :: for            !< Temporary
      class(field_FD_1D), pointer                             :: inp_cur(:)     !< Field input pointer.
      class(field_FD_1D), pointer                             :: fields(:,:)    !< Fields pointer to MG fields.
      class(field_FD_1D), pointer                             :: fields0(:,:)   !< Initial fields pointer to MG fields.
      class(field_FD_1D), pointer                             :: residuals(:,:) !< Residual field pointer to MG fields.
      class(field_FD_1D), pointer                             :: sources(:,:)   !< Sources field pointer to MG fields.
      class(field_FD_1D), pointer                             :: tau(:,:)       !< Field input pointer.
      integer(I_P)                                            :: ie             !< Counter.
      integer(I_P)                                            :: iv             !< Counter.
      integer(I_P)                                            :: n_levels       !< Counter.
      integer(I_P)                                            :: i_mg           !< Counter.
      integer(I_P)                                            :: i_up           !< Counter.
      integer(I_P)                                            :: i_down         !< Counter.

      if(equ%enable_explicit) then
          !print*,'Explicit solver enabled'
          ! (1a) Imposes boundary conditions: modify "inp" field array
          call equ%bc_e(inp=inp, t=t)

          ! (1b) Computes the residual term: modify "equ%resvar_e"
          call equ%resid_e(inp=inp, t=t)

          ! (1c) Updates the inp field according to Euler scheme
          do ie=1,size(inp)
              inp(ie) = inp(ie) + this%dt * equ%resvar_e(ie)
          enddo
      endif

      if(equ%enable_implicit) then
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
         n_levels = equ%mg%levels_number

         ! inp_cur => associate_field_FD_1D(field_input=inp, emsg='calling procedure integrator_euler_implicit%integrate')
         ! tau => associate_field_FD_1D(field_input=equ%mg%tau, emsg='calling procedure integrator_euler_implicit%integrate')
         ! fields => associate_field_FD_1D(field_input=equ%mg%fields, &
         !                                 emsg='calling procedure integrator_euler_implicit%integrate')
         ! fields0 => associate_field_FD_1D(field_input=equ%mg%fields0, &
         !                                  emsg='calling procedure integrator_euler_implicit%integrate')
         ! residuals => associate_field_FD_1D(field_input=equ%mg%residuals, &
         !                                    emsg='calling procedure integrator_euler_implicit%integrate')
         ! sources => associate_field_FD_1D(field_input=equ%mg%sources, &
         !                                  emsg='calling procedure integrator_euler_implicit%integrate')
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
        class(integrator_euler_implicit), intent(inout)         :: this            !< The integrator.
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
end module openpde_integrator_euler_implicit
