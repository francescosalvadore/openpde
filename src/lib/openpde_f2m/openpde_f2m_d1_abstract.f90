!< Abstract class of matrix operator of first derivative.
module openpde_f2m_d1_abstract
   !< Abstract class of matrix operator of first derivative.
    use openpde_matrix_abstract
    use openpde_f2m_abstract

    implicit none
    private
    public :: f2m_d1

    type, abstract, extends(f2m) :: f2m_d1
        !< Abstract class of matrix operator of first derivative.
    endtype f2m_d1

end module openpde_f2m_d1_abstract
