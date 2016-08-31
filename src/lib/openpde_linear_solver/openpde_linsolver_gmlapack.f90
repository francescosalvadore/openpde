!< Concrete class of linsolver using LAPACK with general matrix.
module openpde_linsolver_gmlapack
    !< Concrete class of linsolver using LAPACK with general matrix.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_linsolver_abstract
    use openpde_matrix_abstract
    use openpde_vector_abstract
    use openpde_matrix_simple
    use openpde_vector_simple
    use openpde_kinds

    implicit none
    private
    public :: linsolver_gmlapack

    type, extends(linsolver) :: linsolver_gmlapack
        !< Concrete class of linsolver using LAPACK with general matrix.
        integer(I_P), allocatable :: ipiv(:)
        integer(I_P) :: info
        integer(I_P) :: lda
        contains
            ! deferred public methods
            procedure :: init         
            procedure :: set_matrix   
            procedure :: set_vector   
            procedure :: solve        
    endtype linsolver_gmlapack
contains
    subroutine init(this, n)
        !< Init linsolver
        class(linsolver_gmlapack), intent(inout)   :: this 
        integer(I_P) :: n

        allocate(matrix_simple :: this%mat)
        allocate(vector_simple :: this%vec)
        allocate(vector_simple :: this%sol)

        call this%mat%init(n)
        call this%vec%init(n)
        call this%sol%init(n)

        allocate(this%ipiv(n))
        this%lda = n
       
    end subroutine init

    subroutine set_matrix(this, mat)
        !< Set the solver matrix A.
        class(linsolver_gmlapack), intent(inout)     :: this
        class(matrix),  intent(in), target  :: mat 

        !debug select type(mat)
        !debug     type is(matrix_simple)
        !debug         print*,"SET_MATRIX allocated(mat%val)      : ",allocated(mat%val),      size(mat%val)
        !debug end select 
        !debug associate(m => this%mat)
        !debug select type(m)
        !debug     type is(matrix_simple)
        !debug        print*,"SET_MATRIX allocated(this%mat%val) : ",allocated(m%val), size(m%val)
        !debug end select 
        !debug end associate
        this%mat = mat
   
    end subroutine set_matrix

    subroutine set_vector(this, vec)
        !< Set the solver vector b.
        class(linsolver_gmlapack), intent(inout)     :: this
        class(vector),  intent(in), target  :: vec 

        this%vec = vec

    end subroutine set_vector

    subroutine solve(this)
        !< Solve the linear system.
        class(linsolver_gmlapack), intent(inout), target :: this
        integer(I_P)                              :: n
        class(matrix_simple), pointer             :: mat_cur
        class(vector_simple), pointer             :: vec_cur

        n = this%mat%n

        !debug print*,"allocated(this%mat): ",allocated(this%mat)
        !debug associate(v => this%mat)
        !debug select type(v)
        !debug     type is(matrix_simple)
        !debug         mat_cur => v
        !debug         print*,"allocated(this%mat%val): ",allocated(v%val)
        !debug         print*,"this%mat%val: ",v%val(:,:)
        !debug end select
        !debug end associate
        !debug associate(v => this%vec)
        !debug select type(v)
        !debug     type is(vector_simple)
        !debug         print*,"this%vec%val: ",v%val(:)
        !debug         vec_cur => v
        !debug end select
        !debug end associate

        vec_cur => associate_vector_simple(vector_input=this%vec)
        mat_cur => associate_matrix_simple(matrix_input=this%mat)

        ! Factorize matrix
        call dgetrf( n, n, mat_cur%val, this%lda, this%ipiv, this%info )

        ! Solver linear system
        call dgetrs( 'n', n, 1, mat_cur%val, this%lda, this%ipiv, vec_cur%val, n, this%info )

        ! Assign solution to sol vector
        this%sol = this%vec

        !debug associate(v => this%sol)
        !debug select type(v)
        !debug     type is(vector_simple)
        !debug         print*,"this%sol%val: ",v%val(:)
        !debug end select
        !debug end associate
      
    end subroutine solve
end module openpde_linsolver_gmlapack
