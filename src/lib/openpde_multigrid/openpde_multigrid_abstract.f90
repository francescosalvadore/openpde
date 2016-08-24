!< Abstract class of linsolver A*x=b.
module openpde_multigrid_abstract
    !< Abstract class of linsolver.
    use openpde_field_abstract
    use openpde_kinds

    implicit none
    private
    public :: multigrid

!    type :: pointer_container 
!        class(field), pointer :: p
!    endtype     
   
    type, abstract :: multigrid
        !< Abstract class of multigrid
        character(len=:), allocatable :: description !< Description.

!        type(pointer_container), allocatable :: inp_levels(:)
!        type(pointer_container), allocatable :: inp0_levels(:)
!        type(pointer_container), allocatable :: resvar_mg_levels(:)
!        type(pointer_container), allocatable :: source_mg_levels(:)

        class(field), allocatable :: mesh_levels(:)
        class(field), allocatable :: inp_levels(:,:)
        class(field), allocatable :: inp0_levels(:,:)
        class(field), allocatable :: resvar_mg_levels(:,:)
        class(field), allocatable :: source_mg_levels(:,:)

        integer :: n_levels
        integer :: max_nv
        integer, allocatable :: n_it_up(:), n_it_down(:)
        real, allocatable :: coeff_stab(:)

        !type(pointer_container), allocatable :: tau(:) ! local pseudo-dt
        class(fields), allocatable :: tau(:) ! local pseudo-dt

        class(mesh), allocatable :: m(:)
        real :: tolerance
        real :: norm_var
        contains
            ! deferred public methods
            procedure(abstract_init), deferred :: init  !< Initilize field.
            procedure(abstract_prolungation), deferred :: prolongation
            procedure(abstract_restriction), deferred :: restriction
            procedure(abstract_collect), deferred :: collect
            procedure(abstract_smoother), deferred :: smoother
            procedure(abstract_subgrids), deferred :: create_subgrids
    endtype multigrid

    ! deferred public methods interfaces
    abstract interface
        subroutine abstract_init(this, inp, n_levels)
            !< Init linsolver.
            import :: I_P, multigrid, field
            class(multigrid), intent(inout) :: this 
            class(field), intent(in), target :: inp
            integer(I_P) :: n_levels
        end subroutine abstract_init
    endinterface

contains
    ! public methods
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(linsolver), intent(inout) :: this !< The linsolver.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_multigrid_abstract
