!< Abstract class of mesh.
module opendiff_adt_mesh
    !< Abstract class of mesh.
    use opendiff_kinds

    implicit none
    private
    public :: mesh

    type, abstract :: mesh
        !< Abstract class for *mesh* handling.
        character(len=:), allocatable :: description !< Mesh description.
        contains
            procedure                                 :: free   !< Free dynamic memory.
            procedure(abstract_meshinit),    deferred :: init   !< Initilize mesh.
            procedure(abstract_meshoutput) , deferred :: output !< Output mesh data.
    endtype mesh

    abstract interface
        subroutine abstract_meshinit(this, description, error)
            import :: I4P, mesh
            class(mesh),  intent(inout)         :: this
            character(*), intent(in),  optional :: description
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_meshinit
    endinterface

    abstract interface
        subroutine abstract_meshoutput(this, error)
            import :: I4P, mesh
            class(mesh),  intent(in)            :: this
            integer(I4P), intent(out), optional :: error
        end subroutine abstract_meshoutput
    endinterface
contains
    elemental subroutine free(this)
        !< Free dynamic memory.
        class(mesh), intent(inout) :: this !< The mesh.
        if (allocated(this%description)) deallocate(this%description)
    end subroutine free
end module opendiff_adt_mesh
