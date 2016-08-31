!< Abstract class of multigrid solver.
module openpde_multigrid_abstract
    !< Abstract class of multigrid solver.
    use openpde_field_abstract
    use openpde_mesh_abstract
    use openpde_kinds

    implicit none
    private
    public :: multigrid

    type, abstract :: multigrid
        !< Abstract class of multigrid solver.
        character(len=:), allocatable :: description        !< Description.
        class(field), allocatable     :: fields(:,:)        !< Field on each MG level.
        class(mesh), allocatable      :: meshes(:)          !< Meshes on each MG level.
        class(field), allocatable     :: fields0(:,:)       !< Initial field on each MG level.
        class(field), allocatable     :: residuals(:,:)     !< Residual field on each MG level.
        class(field), allocatable     :: sources(:,:)       !< Source field on each MG level.
        integer(I_P)                  :: levels_number      !< MG levels number.
        integer(I_P)                  :: max_iterations     !< Maximum number of iterations.
        integer(I_P), allocatable     :: n_it_up(:)         !< Number of iteration in "up" cycle for each MG level.
        integer(I_P), allocatable     :: n_it_down(:)       !< Number of iteration in "up" cycle for each MG level.
        real(R_P), allocatable        :: stability(:)       !< Stability coefficient for each MG level.
        class(field), allocatable     :: tau(:,:)           !< Local pseudo-dt for each MG level.
        real(R_P)                     :: tolerance          !< Tolerance on iterative convergence.
        real(R_P)                     :: norm               !< Norm of residuals.
        contains
            ! deferred public methods
            procedure(create_subgrids_field_interface), deferred :: create_subgrids_field !< Create subgrid-levels field.
            procedure(init_interface),                  deferred :: init                  !< Init multigrid solver.
            ! procedure(abstract_prolungation), deferred :: prolongation
            ! procedure(abstract_restriction), deferred :: restriction
            ! procedure(abstract_collect), deferred :: collect
            ! procedure(abstract_smoother), deferred :: smoother
    endtype multigrid

    ! deferred public methods interfaces
    abstract interface
        !< Create subgrid-levels field.
        subroutine create_subgrids_field_interface(this, inp, subgrids)
            !< Create subgrid-levels field.
            import :: I_P, multigrid, field
            class(multigrid), intent(in), target :: this          !< The solver.
            class(field),     intent(in)         :: inp(:)        !< Input field on finest mesh.
            class(field),     intent(inout)      :: subgrids(:,:) !< Subgrid-levels field.
        end subroutine create_subgrids_field_interface
    endinterface

     abstract interface
        !< Init multigrid solver.
        subroutine init_interface(this, inp, levels_number)
            !< Init multigrid solver.
            import :: I_P, multigrid, field
            class(multigrid), intent(inout)      :: this          !< The solver.
            class(field),     intent(in), target :: inp           !< Input field on finest mesh.
            integer(I_P),     intent(in)         :: levels_number !< Levels number.
        end subroutine init_interface
    endinterface
contains
    ! public methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(multigrid), intent(inout) :: this !< The linsolver.
        if (allocated(this%description)) deallocate(this%description)
        if (allocated(this%fields     )) deallocate(this%fields     )
        if (allocated(this%meshes     )) deallocate(this%meshes     )
        if (allocated(this%fields0    )) deallocate(this%fields0    )
        if (allocated(this%residuals  )) deallocate(this%residuals  )
        if (allocated(this%sources    )) deallocate(this%sources    )
        if (allocated(this%n_it_up    )) deallocate(this%n_it_up    )
        if (allocated(this%n_it_down  )) deallocate(this%n_it_down  )
        if (allocated(this%stability  )) deallocate(this%stability  )
        if (allocated(this%tau        )) deallocate(this%tau        )
    end subroutine free
end module openpde_multigrid_abstract
