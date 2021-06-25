program main
  implicit none
  integer :: n, nbase
  real(8) :: vdc_res

  nbase = 3

  do n = 1,10
     call vdCorput_sequence(n,nbase,vdc_res)
     write(*,*)n,vdc_res
  end do
  
end program main
!-----------------------------------------
subroutine vdCorput_sequence(n_in,nbase,vdc_res)
  implicit none
  integer,intent(in) :: n_in, nbase
  real(8),intent(out) :: vdc_res
  integer :: n
  real(8) :: bk

  n = n_in
  bk = 1d0/nbase
  vdc_res = 0d0

  do while(n>0)
     vdc_res = vdc_res + mod(n,nbase)*bk
     n = n/nbase
     bk = bk/nbase
     
  end do
  
end subroutine vdCorput_sequence
