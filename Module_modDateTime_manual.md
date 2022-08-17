# Manual do Módulo modDateTime

## Introdução:

Esse manual descreve tipos e funções pertencentes ao módulo modDateTime que agrupa funções específicas para uso em programas fortran. Faz parte de um esforço para manter bibliotecas importantes e agrupadas para uso em programação Fortran.

## Tipos:

**t_dt :** Tipo com uma estrutura de dados de informações de DateTime. Essa estrutura é composta de:

```fortran
  integer :: year
  !! Ano no formato de até 4 dígitos
  integer :: month
  !! Mê de 1 a 12 - 1=Janeiro 
  integer :: day
  !! Dia do mês (1 a 28,29,30 ou 31 dias sengundo o mês/ano)
  integer :: hour
  !! Hora do dia de 0 a 23
  integer :: minute
  !! Minuto de 0 a 59
  integer :: second
  !! Segundo de 0 a 59
  integer :: milisecond
  !! Milisegundos de 0 a 999 (inteiro) 
  integer :: timezone
  !! Fuso horário de -12 até 12 (Não considera horário de verão)
  character(len=5) :: zone 
  !! Fuso no formato texto de "-1200" até "+1200"
  integer :: julDay
  !! O dia em dias julianos (1 a 365/366)
```

O acesso aos membros da estrutura deve ser feito pelo padrão Fortran. Uma variável pode ser definida por essa tipo e acessado conforme exemplo abaixo.

```fortran
use modDateTime, only: t_dt
type(t_dt) :: novaData
novaData%year = 2022
novaData%month = 08
novaData%day = 10
print *, novaData%Year, novaData%month, novaData%day
```

## Funções:

### 1. date2Seconds(dtIn,baseYear)

##### Descrição:

 Computa o total de segundos transcorridos desde o primeiro dia de um ano base (01/01) até uma data dtIn fornecida. Se o ano  base (baseYear) não for fornecido a função assume o dia 01/01/1900 como referência.

##### Parâmetros de entrada:

- type(t_dt) :: **dtIn** - Data para se calcular o total d esegundos transcorridos 

- integer,intent(in),optional :: **baseYear** - Ano base para o cálculo

##### Variável de saída:

- real(kind=kind_rb) :: **seconds** - Segundos desde o primeiro dia do ano base

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,date2Seconds
real(kind=kind_rb) :: sec
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
sec = date2Seconds(agora)
sec = date2Seconds(agora,2000)
```

---

### 2. date2String(dtin,splitter)

##### Descrição:

Transforma a data no tipo dateTime em um string ISO 8601 contendo as informações da data. Essa string tem formato AAAA-MM-DDTHH:MM:SS.mmm-ZZ:00, por exemplo: 1997-03-13T22:02:00.000+03:00. SE for fornecido um separador de data esse será usado. Caso contrário o seprador será o padrão "-".

##### Parâmetros de entrada:

- type(t_dt) :: **dtIn** - Data para ser transformada em string

- character,intent(in), optional :: **splitter** - Caracter separador de datas

##### Variável de saída:

- character(len=29) :: **date2String** - string que será retornada.

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,date2String
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
print *, date2String(agora) !imprime 2022-08-13T22:02:00.000+03:00
print *, date2String(agora,'/') !imprime 2022/08/13T22:02:00.000+03:00
```

---

### 3. dayExist(dtin)

##### Descrição:

Retorna o lógico .true. se a data existe. 

##### Parâmetros de entrada:

- type(t_dt) :: **dtIn** - Data para ser verificada

##### Variável de saída:

- logical :: **dExist** - logical .true. se a data é válida.

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,dayExist
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
print *, dayExist(agora) !imprime um .true. (T)
```

---

### 4. dayOfWeek(dtin)

##### Descrição:

Retorna o número do dia da semana. Considera o domingo como sendo 1. 

##### Parâmetros de entrada:

- type(t_dt) :: **dtIn** - Data para se obter o número do dia da semana

##### Variável de saída:

- integer :: **dowNumber** - Número do dia da semana

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,dayOfWeek
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
print *, dayOfWeek(agora) !imprime um valor de 1 a 7 (depende do dia)
```

---

### 5. incDateTime(dtin,miliseconds,seconds,minutes,hours,days)

##### Descrição:

Incrementa uma data em uma quantidade de milisegundos e/ou segundos e/ou horas e/ou minutos e/ou dias.

##### Parâmetros de entrada:

- type(t_dt) :: **dtIn** - Data para ser incrementada

- real(kind=kind_rb), intent(in), optional :: **miliseconds** - Incremento em miliseconds

- real(kind=kind_rb), intent(in), optional :: **seconds** - Incremento em segundos

- real(kind=kind_rb), intent(in), optional :: **minutes** - Incremento em minutos

- real(kind=kind_rb), intent(in), optional :: **hours** - Incremento em horas

- real(kind=kind_rb), intent(in), optional :: **days** - Incremento em dias

##### Variável de saída:

- type(t_dt) :: **dtOut** - Data incrementada pela função

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,incDateTime
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
print *,'Agora+800=',incDateTime(agora,seconds=dble(800.))
print *,'Agora+800+2min=',incDateTime(agora,seconds=dble(800.),minutes=dble(2.))
```

---

### 6. int2DateTime(Year,Month,Day,Hour,Minute,Second,Milisecond,TZ)

##### Descrição:

Transforma valores inteiros de data para uma data no formato dateTime

##### Parâmetros de entrada:

- integer, intent(in) :: **Year** - Ano da data

- integer, intent(in) :: **Month** - Mês da data

- integer, intent(in) :: **Day** - Dia da data

- integer, intent(in), optional :: **Hour** - Hora da data

- integer, intent(in), optional :: **Minute** - Minuto da data

- integer, intent(in), optional :: **Second** - Segundo da data

- integer, intent(in), optional :: **Milisecond** - Milesegundo da data

- integer, intent(in), optional :: **TZ** - Fuso da data

##### Variável de saída:

- type(t_dt) :: **dtOut** - Dataobtida pela função

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,int2DateTime
type(t_dt) :: ndt
ndt = int2DateTime(2200,03,01,second=34,timezone=-3,milisecond=0,hour=13)
stemp = date2String(ndt)  
print *, stemp !Imprime a data 2200-03-01T13:00:34.000-0300
```

---

### 7. isLeapYear(year)

##### Descrição:

Retorna .true. se o ano é bissexto.

##### Parâmetros de entrada:

- integer, intent(in) :: **year** - Ano a ser verificado

##### Variável de saída:

- logical :: **isLY** - retorna o resultado 

##### Chamadas exemplos:

```fortran
use modDateTime, only: isLeapYear
print *,'Bissexto?',isLeapYear(2000) !Retorna .true.
print *,'Bissexto?',isLeapYear(2003) !Retorna .false.
```

---

### 8. monthName(monthIn,size,lang)

##### Descrição:

Retorna o nome do mês. Esse nome pode ser curto (3 letras) ou longo (nome completo) e estar em inglês ou português.

##### Parâmetros de entrada:

- integer, intent(in) :: **monthIn** - Número do mês de 1 a 12 (1=jan)
- character, intent(in), optional :: **size** - Tamanho do string (l=long e s=short)
- character(len=2), intent(in), optional :: **lang** - Linguagem (pt=port. e en=inglês)

##### Variável de saída:

- character(len=9) :: **month** - Nome do mês

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,monthName
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
print *,monthName(agora%month,size='l',lang='pt') !Retorna em port. e longo
print *,monthName(agora%month) !Padrão: retorna curto e em inglês
print *,monthName(10) !Imprime 'oct'
print *,monthName(10,size='l') !imprime 'october'
```

---

### 9. now()

##### Descrição:

Retorna a data e tempo correntes.

##### Parâmetros de entrada:

- Não há

##### Variável de saída:

- type(t_dt) :: **dateNow** - retorna a data/hora corrente

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now
type(t_dt) :: agora
agora = now() !Retorna a data do momento no formato t_dt
```

---

### 10. seconds2DateTime(seconds)

##### Descrição:

Retorna a data/hota no formato dateTime à partir dos segundos transcorridos desde 1900

##### Parâmetros de entrada:

- real(kind=kind_rb), intent(in) :: **seconds** - Segundos desde 1900 

##### Variável de saída:

- type(t_dt) :: **dtOut** - Data correspondente ao número de segundos

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,now,seconds2DateTime,date2Seconds
real(kind=kind_rb) :: sec
sec = date2Seconds(string2DateTime("2022-08-12T17:08:06.315-0300"))
print *, seconds2DateTime(sec) !Imprime o valor 3869399286.315
```

---

### 11. string2DateTime(dateIn)

##### Descrição:

Retorna a data/hota no formato dateTime à partir de um string no formato ISO 8601 ou conforme um substring da entrada.

##### Parâmetros de entrada:

- character(len=*), intent(in) :: **dateIn** - string com a data na entrada

##### Variável de saída:

- type(t_dt) :: **dtOut** - Data/hora no formato dateTime na saída

##### Chamadas exemplos:

```fortran
use modDateTime, only: t_dt,string2DateTime
type(t_dt) :: parcial
print *, string2DateTime("2022-08-12T17:08:06.315-0300")
print *, string2DateTime("2022-08-12")
parcial = string2DateTime("2022/08/12T18:20")
```

---

### 12. weekName(nWDay,size,lang)

##### Descrição:

Retorna o nome ddo dia da semana. Esse nome pode ser curto (3 letras) ou longo (nome completo) e estar em inglês ou português. O dia vai de 1 a 7 e 1 é o domingo.

##### Parâmetros de entrada:

- integer, intent(in) :: **nWDay** - Número do mês de 1 a 12 (1=jan)
- character, intent(in), optional :: **size** - Tamanho do string (l=long e s=short)
- character(len=2), intent(in), optional :: **lang** - Linguagem (pt=port. e en=inglês)

##### Variável de saída:

- character(len=9) :: **wName** - Nome do dia da semana

##### Chamadas exemplos:

```fortran
use modDateTime, only: weekName
integer :: i
do i=1,7
   print *, i,weekName(i,size='s',lang='en'),weekName(i,size='s',lang='pt')
   print *, i,trim(weekName(i,size='L',lang='EN')),' ',trim(weekName(i,size='l',lang='pt'))
end do
```

---
