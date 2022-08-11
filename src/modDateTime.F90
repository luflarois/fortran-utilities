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
   integer, parameter, dimension(12) :: daysByMonth = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   type t_dt 
      integer :: year
      integer :: month
      integer :: day
      integer :: hour
      integer :: minute
      integer :: second
      integer :: milisecond
      integer :: timezone
      character(len=5) :: zone 
      integer :: julDay
   end type t_dt 
   !! Tipo de data/tempo

   private
   public :: t_dt, now, date2String, int2DateTime

contains

   function now() result(dateNow)
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
      type(t_dt) :: dateNow
      !! Date do instante montada no tipo
   
      !Local variables:
      integer :: dtn(8)
      character(len=5) :: zone
      !! Variável local com a data obtida
   
      !Code:
      call date_and_time(values=dtn,zone=zone)
      dateNow%year = dtn(1)
      dateNow%month = dtn(2)
      dateNow%day = dtn(3)
      dateNow%hour = dtn(5)
      dateNow%minute = dtn(6)
      dateNow%second = dtn(7)
      dateNow%milisecond = dtn(8)
      dateNow%zone=zone
      read(zone(1:3),*) dateNow%timezone
      dateNow%julDay = julday(dateNow)
   
   end function now

   function date2String(dtin,splitter)
      !! Retorna um string  de tempo
      !!
      !! @note
      !!
      !! **Project**: fortran utilities
      !! **Author(s)**: Rodrigues, L.F. [LFR]
      !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
      !! **Date**:  10Agosto2022 08:35
      !!
      !! **Full description**:
      !! Retorna um string  de tempo ISO 8601
      !! https://www.iso.org/iso-8601-date-and-time-format.html
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
   
      type(t_dt), intent(in) :: dtin
      !! Tempo a ser convertido
      character,intent(in), optional :: splitter
      !! Caracter aser usado na separação de ano, mês e dia
      character(len=29) :: date2String
      !! Função
      
      character(len=5)  :: zone
      !! Zona horária
      integer           :: dti(8)
      !! Valores locais de data
      character(len=65) :: isofmt
      !! Formato de escrita da ISO 8601
      character :: sep

      sep='-'
      if(present(splitter)) sep=splitter
      isofmt = '(i4, 2("'//sep//'", i2.2), "T", 2(i0.2, ":"), ' // &
               'i0.2, ".", i0.3, a, ":", a)'
      
      dti(1) = dtin%year
      dti(2) = dtin%month
      dti(3) = dtin%day
      dti(5) = dtin%hour
      dti(6) = dtin%minute
      dti(7) = dtin%second
      dti(8) = dtin%milisecond
      zone = dtin%zone

      write (date2String, isofmt) dti(1), dti(2), dti(3), dti(5), dti(6), &
                                   dti(7), dti(8), zone(1:3), zone(4:5)
  end function date2String

  !=============================================================================================
  integer function julday(dtin)
  !! returns which day of the year is the input date
  !!
  !! @note
  !! ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
  !!
  !! **Brief**: returns which day of the year is the input date
  !!
  !! **Documentation**: <http://brams.cptec.inpe.br/documentation/>
  !!
  !! **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
  !!
  !! **Date**: 26 August 2020 (Wednesday)
  !! @endnote
  !!
  !! @changes
  !! &#9744; <br/>
  !! @endchanges
  !! @bug
  !!
  !!@endbug
  !!
  !!@todo
  !!  &#9744; <br/>
  !! @endtodo
  !!
  !! @warning
  !! Now is under CC-GPL License, please see
  !! &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
  !! @endwarning
  !!
    
  ! 

  type(t_dt), intent(in) :: dtin
  !! Tempo a ser convertido

  integer :: iYear
  integer :: iMonth
  integer :: iDay

  iYear = dtin%year
  iMonth = dtin%month
  iDay = dtin%day

  julday= iday  &
       + min(1,max(0,imonth-1))*31  &
       + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4))))  &
       + min(1,max(0,imonth-3))*31  &
       + min(1,max(0,imonth-4))*30  &
       + min(1,max(0,imonth-5))*31  &
       + min(1,max(0,imonth-6))*30  &
       + min(1,max(0,imonth-7))*31  &
       + min(1,max(0,imonth-8))*31  &
       + min(1,max(0,imonth-9))*30  &
       + min(1,max(0,imonth-10))*31  &
       + min(1,max(0,imonth-11))*30  &
       + min(1,max(0,imonth-12))*31

end function julday

function int2DateTime(Year,Month,Day,Hour,Minute,Second,Milisecond,TimeZone) result(dtOut)
   !! Convert integer values of year, month, day, etc to dateTime
   !!
   !! @note
   !!
   !! **Project**: fortran-utilities
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  11Agosto2022 16:54
   !!
   !! **Full description**:
   !! Convert integer values of year, month, day, etc to dateTime
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
   character(len=*), parameter :: procedureName = 'int2DateTime' ! Nome da função

   !Variables (input):
   integer, intent(in) :: Year
   integer, intent(in) :: Month
   integer, intent(in) :: Day
   integer, intent(in), optional :: Hour
   integer, intent(in), optional :: Minute
   integer, intent(in), optional :: Second
   integer, intent(in), optional :: Milisecond
   integer, intent(in), optional :: timezone

   !Local variables:
   type(t_dt) :: dtOut

   !Code:
   dtOut%year = Year
   dtOut%month = Month
   dtOut%day = Day
   dtOut%hour = 0
   dtOut%minute = 0
   dtOut%second = 0
   dtOut%milisecond = 0
   dtOut%timezone = 0

   if(.not. dayExist(dtOut)) iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
      ," input date invalid: ",(/Year,Day,Month/),"I8")

   if(present(Hour)) then
      if(Hour<0 .or. Hour>23)  iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
         ," input hour invalid: ",Hour,"I8")
      dtOut%hour = Hour
   endif
   if(present(Minute)) then
      if(Minute<0 .or. Minute>59)  iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
         ," input minute invalid: ",Minute,"I8")
      dtOut%minute = Minute
   endif
   if(present(Second)) then
      if(Second<0 .or. Second>59)  iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
         ," input second invalid: ",Second,"I8")      
      dtOut%second = Second
   endif
   if(present(Milisecond)) then
      if(MiliSecond<0 .or. MiliSecond>999)  iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
         ," input milisecond invalid: ",MiliSecond,"I8")       
      dtOut%milisecond = MiliSecond
   endif
   if(present(TimeZone)) then
      if(TimeZone<-12 .or. TimeZone>12)  iErrNumber = dumpMessage(c_tty,c_yes,"","",c_fatal &
         ," input timezone invalid: ",TimeZone,"I8")         
      dtOut%timezone = timezone
   endif
   if(present(TimeZone)) then
      if(timezone>0) then
         write(dtOut%zone,fmt='("+",I2.2,I2.2)') timezone,0
      else
         write(dtOut%zone,fmt='("-",I2.2,I2.2)') abs(timezone),0
      endif
   else 
      dtOut%zone="+0000"
   endif
   dtOut%julDay = julday(dtOut)

end function int2DateTime

function dayExist(dtIn) result(dExist)
   !! Check if a day exist
   !!
   !! @note
   !!
   !! **Project**: fortran-utilities
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  11Agosto2022 17:21
   !!
   !! **Full description**:
   !! Check if a day exist
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
   character(len=*), parameter :: procedureName = 'dayExist' ! Nome da função

   !Variables (input):
   type(t_dt), intent(in) :: dtIn

   !Local variables:
   logical :: dExist
   !! Return logical exist

   !Code:

   dExist = .false.
   if(dtIn%Month<1 .and. dtIn%Month>12) return
   if(dtIn%day<1) return
   if(dtIn%month == 2 .and. isLeapYear(dtIn%year) .and. dtIn%day>29) return
   if(dtIn%month == 2 .and. .not. isLeapYear(dtIn%year) .and. dtIn%day>28) return
   if(dtIn%day>daysByMonth(dtIn%month)) return
   dExist = .true.

end function dayExist

function isLeapYear(year) result(isLY)
   !! Testa se um ano é bissexto
   !!
   !! @note
   !!
   !! **Project**: fortran-utilities
   !! **Author(s)**: Rodrigues, L.F. [LFR]
   !! **e-mail**: <mailto:luiz.rodrigues@inpe.br>
   !! **Date**:  11Agosto2022 17:30
   !!
   !! **Full description**:
   !! Testa se um ano é bissexto
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
   character(len=*), parameter :: procedureName = 'isLeapYear' ! Nome da função

   !Variables (input):
   integer, intent(in) :: year
   
   !Local variables:
   logical :: isLY

   !Code:
   isLY = .false.
   if (mod(year, 400) == 0 .or. (mod(year, 4) == 0 .and. mod(year, 100) /= 0)) then
		isLY = .true.
	endif

end function isLeapYear

end module modDateTime