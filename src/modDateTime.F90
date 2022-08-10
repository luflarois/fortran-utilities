module modDateTime
   !! Módulo com diversas funções relativas a manipulação de datas.
   !!
   !! @note
   !!
   !! **Project**: fortran-utilities
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  10Agosto2022 08:18
   !!
   !! **Full description**:
   !! Módulo com diversas funções relativas a manipulação de datas.
   !!
   !! @endnote
   !!
   !! @warning
   !!
   !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
   !!
   !!     This program is free software: you can redistribute it and/or modify
   !!     it under the terms of the GNU General Public License as published by
   !!     the  Free  Software  Foundation, either version 3 of the License, or
   !!     (at your option) any later version.
   !!
   !!     This program is distributed in the hope that it  will be useful, but
   !!     WITHOUT  ANY  WARRANTY;  without  even  the   implied   warranty  of
   !!     MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See  the, GNU
   !!     GNU General Public License for more details.
   !!
   !!     You should have received a copy  of the GNU General  Public  License
   !!     along with this program.  If not, see <https://www.gnu.org/licenses/>.
   !!
   !! @endwarning

   use dump
   implicit none
   include 'constants.h'
   character(len=*), parameter :: sourceName = 'modDateTime.F90' ! Nome do arquivo fonte
   character(len=*), parameter :: moduleName = 'modDateTime' ! Nome do módulo

   type dt 
      integer :: year
      integer :: month
      integer :: day
      integer :: hour
      integer :: minute
      integer :: second
      integer :: milisecond
      character :: zone ! U,L
      integer :: timezone
   end type dt 

   private
   public :: dt

contains

   function now() result()
      !! Retorna a data e hora do sistema no tipo dt
      !!
      !! @note
      !!
      !! **Project**: fortran utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  10Agosto2022 08:35
      !!
      !! **Full description**:
      !! Retorna a data e hora do sistema no tipo dt
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'now' ! Nome da função
   
      !Variables (output):
      type(dt) :: dateNow
      !! Date do instante montada no tipo
   
      !Local variables:
      integer :: dtn(8)
      !! Variável local com a data obtida
   
      !Code:
      call date_and_time(values=dtn)
      dateNow%year = dtn(1)
      dateNow%month = dtn(2)
      dateNow%day = dtn(3)
      dateNow%hour = dtn(5)
      dateNow%minute = dtn(6)
      dateNow%second = dtn(7)
      dateNow%milisecond = dtn(8)
      dateNow%timezone = dtn(4)/60
   
   end function now

   function date2String(dtin, separador) result(sdts)
      !! Retorna um string com a data e tempo
      !!
      !! @note
      !!
      !! **Project**: fortran utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  10Agosto2022 08:43
      !!
      !! **Full description**:
      !! Retorna um string com a data e tempo
      !!
      !! @endnote
      !!
      !! @warning
      !!
      !!  [](https://www.gnu.org/graphics/gplv3-127x51.png'')
      !!
      !!     Under the terms of the GNU General Public version 3
      !!
      !! @endwarning
   
      implicit none
      !Parameters:
      character(len=*), parameter :: procedureName = 'date2String' ! Nome da função
   
      !Variables (input):
      type(dt) :: dtin
      !! Tipo com a data a ser convertida
      character, intent(in), optional :: separador
      !! Separador de ano, mês e dia
   
      !Local variables:
      character(len=23), intent(in) :: sdts 
      !! Saída no formato "AAAA/MM/DDTHH:MM:SS:MMM"
      

      if(present(separador)) then
         sep = separador
      else
         sep = "/"
      endif
         
   
      !Code:
      write(sdts,fmt='(I4.4,separador,'// &
                     ' I2.2,separador,'// &
                     ' I2.2,separador,'// &
                     '"T",'            // &
                     ' I2.2,":",'// &
                     ' I2.2,":",'// &
                     ' I3.3)') &
                      dtin%year,dtin%month,dtin%day &
                     ,dtin%hour,dtin%minute &
                     ,dtin%second,dtin%milisecond 
   
   end function date2String

end module modDateTime