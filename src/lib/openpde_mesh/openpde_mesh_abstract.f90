!< Abstract class of mesh.
module openpde_mesh_abstract
    !< Abstract class of mesh.
    use openpde_kinds

    implicit none
    private
    public :: mesh

    type, abstract :: mesh
        !< Abstract class of mesh.
        character(len=:), allocatable :: description !< Mesh description.
        contains
            ! deferred public methods
            procedure(abstract_meshinit),    pass(this), deferred :: init   !< Initilize mesh.
            procedure(abstract_meshoutput) , pass(this), deferred :: output !< Output mesh.
            ! public methods
            procedure, pass(this) :: free !< Free dynamic memory.
    endtype mesh

    abstract interface
        !< Initialize mesh.
        subroutine abstract_meshinit(this, description, filename, error)
            !< Initialize mesh.
            import :: I_P, mesh
            class(mesh),  intent(inout)         :: this        !< The mesh.
            character(*), intent(in),  optional :: description !< Mesh description.
            character(*), intent(in),  optional :: filename    !< Initialization file name.
            integer(I_P), intent(out), optional :: error       !< Error status.
        end subroutine abstract_meshinit
    endinterface

    abstract interface
        !< Output mesh.
        subroutine abstract_meshoutput(this, error)
            !< Output mesh.
            import :: I_P, mesh
            class(mesh),  intent(in)            :: this  !< The mesh.
            integer(I_P), intent(out), optional :: error !< Error status.
        end subroutine abstract_meshoutput
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(mesh), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module openpde_mesh_abstract
