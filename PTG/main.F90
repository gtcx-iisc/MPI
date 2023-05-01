!================================================
! Date: 07/09/2022
!
! Program to test:
! 1) MPI initialization
! 2) MPI broadcast operation
! 3) MPI partitioning
! 4) MPI gather-scatter operation
! 5) File writing
!
! In particular, access to multiple nodes.
!
!================================================

module GlobalVariables
  implicit none

  integer :: Value1, Value2, MyPE, ierror
  integer :: GatheredValue1, GatheredValue2
end module GlobalVariables

!-----------------------------------------------

program main
  use MPI
  use GlobalVariables

  implicit none

  call MPI_Initialize

  if( MyPE==0 ) then
    write(*,*)' '
    write(*,*)' '
    write(*,*)' '
    write(*,*)'!====== Running MPI test ======!'
    write(*,*)' '
  endif

  if( MyPE == 0 )  write(*,*)'MPI_Initialization done....!'

  call ReadFile
  if( MyPE == 0 )  write(*,*)'MPI_ReadFile done....!'

  call MPI_Broadcast
  if( MyPE == 0 )  write(*,*)'MPI_Broadcast done....!'

  call MPI_Partition
  if( MyPE == 0 )  write(*,*)'MPI_Partition done....!'

  call MPI_Gather_Test
  if( MyPE == 0 )  write(*,*)'MPI_Gather done....!'

  call MPI_Scatter_Test
  if( MyPE == 0 )  write(*,*)'MPI_Scatter done....!'

  call MPI_WriteFiles
  if( MyPE == 0 )  write(*,*)'MPI_WriteFiles done....!'

  call MPI_Final

  if( MyPE==0 ) then
    write(*,*)' '
    write(*,*)' '
    write(*,*)'!===== MPI test successful =====!'
    write(*,*)' '
    write(*,*)' '
  endif

end program main

!----------------------------------------------

subroutine MPI_Initialize
  use MPI
  use GlobalVariables

  implicit none

  call mpi_init(ierror)
  call mpi_comm_rank(MPI_COMM_WORLD,MyPE,ierror)

end subroutine MPI_Initialize

!----------------------------------------------

subroutine ReadFile
  use MPI
  use GlobalVariables

  implicit none
  logical :: file_exist
  namelist /input_parameters/ Value1, Value2

  if(MyPE==0)then

    ! check whether the input file exists
    inquire(file='Data.in',exist=file_exist)
    if(file_exist) then
      open(10,file='Data.in',status='old')
      read(10,nml=input_parameters,iostat=ierror)
      if(ierror>0)write(*,*)'WARNING: check input_parameters'
        close(10)
      else
        write(*,*)'Cannot find file Data.in !!!'
        write(*,*)' '
        call MPI_Final
        stop
    endif

    write(*,*)' '
    write(*,*)'----- Data from file ------'
    write(*,*)'   Value1 = ',Value1,'Value2 = ',Value2
    write(*,*)'---------------------------'
    write(*,*)' '

  endif

end subroutine ReadFile

!----------------------------------------------

subroutine MPI_Broadcast
  use GlobalVariables
  use MPI  

  implicit none

  integer :: IntegerData(2)

  if( MyPE==0 ) then
    IntegerData(1) = Value1
    IntegerData(2) = Value2
  endif

  call MPI_BCAST(IntegerData,2,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)

  if( MyPE/=0 ) then
    Value1 = IntegerData(1)
    Value2 = IntegerData(2)
  endif
    
end subroutine MPI_Broadcast

!----------------------------------------------

subroutine MPI_Partition

end subroutine MPI_Partition

!----------------------------------------------

subroutine MPI_Gather_Test
  use GlobalVariables
  use MPI

  implicit none
  integer :: SendData(2), ReceiveData(2)

  SendData(1) = Value1
  SendData(2) = Value2

  call MPI_ALLREDUCE( SendData, ReceiveData, 2, MPI_INT, MPI_SUM,&
         MPI_COMM_WORLD, ierror)

  GatheredValue1 = ReceiveData(1)
  GatheredValue2 = ReceiveData(2)

end subroutine MPI_Gather_Test

!----------------------------------------------

subroutine MPI_Scatter_Test

end subroutine MPI_Scatter_Test

!----------------------------------------------

subroutine MPI_WriteFiles
  use GlobalVariables
  use MPI

  implicit none

  integer :: FileID
  character*25 FileName

  FileID = MyPE

  write(FileName,'("Output",i6,".out")')(FileID)
!  write(FileName,*)"Output",FileID,".out"
  open(FileID,file=FileName,status='replace')

  write(FileID,*)'Value1 = ',Value1
  write(FileID,*)'Value2 = ',Value2
  write(FileID,*)'GatheredValue1 = ',GatheredValue1
  write(FileID,*)'GatheredValue2 = ',GatheredValue2

  close(FileID)

end subroutine MPI_WriteFiles

!----------------------------------------------

subroutine MPI_Final
 
  implicit none
  integer :: ierror

  call mpi_finalize(ierror)

end subroutine MPI_Final

!----------------------------------------------
