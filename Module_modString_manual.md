# Manual do Módulo modString

## Introdução:

Esse manual descreve tipos e funções pertencentes ao módulo modString que agrupa funções específicas para uso em programas fortran. Faz parte de um esforço para manter bibliotecas importantes e agrupadas para uso em programação Fortran.

## ## Funções:

### 1. parseLine(linha,separador)

##### Descrição:

 Divide uma linha fornecida em campos. Os 'N' campos são strings de tamanho máximo de 256 caracteres. Todos os campos em uma linha são separados por um caracter específico. Se o Caracter separador não for fornecido o código usa uma vírgula. 

##### Parâmetros de entrada:

- character(len=*), intent(in) :: **linha** - Linha contendo os string s separados pelo separador

- character, intent(in),optional :: **separador** - Caracter que separa os campos

##### Variável de saída:

- character(len=p_campSize), allocatable :: **campos**(:) - Vetor com N campos de tamanho máximo = 256 caracteres

##### Chamadas exemplos:

```fortran
use modString, only: parseLine 
character(len=256) :: campos(8)
campos = parseLine("Luiz,Pedro,Carlos,Jose")
print *,trim(campos(3)) !Imprime o nome carlos
```

---

### 2. lower(strIn)

##### Descrição:

Retorna o string passado na chamada da função em caracteres minúsculos.

##### Parâmetros de entrada:

- character(*), intent(in) :: **strIn** - String a ser convertido

##### Variável de saída:

- character(len=len(strIn)) :: **strOut** - string em minúsculo que será retornada.

##### Chamadas exemplos:

```fortran
use modString, only: lower

print *, lower("JULia, DATA: 05/12") !imprime "julia, data: 05/12""
```

---

### 3. upper(strIn)

##### Descrição:

Retorna o string passado na chamada da função em caracteres maiúsculos.

##### Parâmetros de entrada:

- character(*), intent(in) :: **strIn** - String a ser convertido

##### Variável de saída:

- character(len=len(strIn)) :: **strOut** - string em maiúsculo que será retornada.

##### Chamadas exemplos:

```fortran
use modString, only: upper

print *, upper("JULia, DATA: 05/12") !imprime "JULIA, DATA: 05/12""
```

---

### 4. string2Real(strVal)

##### Descrição:

Converte uma string para um valor REAL

##### Parâmetros de entrada:

- character(len=*), intent(in) :: **strVal** - String para ser convertido

##### Variável de saída:

- real :: **realVal** - Valor REAL retornado

##### Chamadas exemplos:

```fortran
use modString, only: string2Real

write(*,fmt='(F12.6)') string2Real("-23.00987")
write(*,fmt='(F12.6)') string2Real("+23.00987")
write(*,fmt='(F12.6)') string2Real("23.00987")
write(*,fmt='(F12.6)') string2Real(".00987")
write(*,fmt='(F12.6)') string2Real("-0.00987")
write(*,fmt='(F12.6)') string2Real("-.00987")
write(*,fmt='(F12.6)') string2Real("-4.")
write(*,fmt='(F12.6)') string2Real("-4")
write(*,fmt='(F12.6)') string2Real("2345")
write(*,fmt='(F12.6)') string2Real("+2345") 
write(*,fmt='(F12.6)') string2Real("+23") 
write(*,fmt='(F12.6)') string2Real("-23") 
write(*,fmt='(E12.6)') string2Real("+2.345e-23")
write(*,fmt='(E12.6)') string2Real("-5.67e+23")
```

---

### 5. string2Int(iInt)

##### Descrição:

Retorna um número inteiro de uma string fornecida 

##### Parâmetros de entrada:

- character(len=*), intent(in) :: **iInt** - String representando um número inteiro

##### Variável de saída:

- integer :: **intVal** - Valor inteiro obtido do string

##### Chamadas exemplos:

```fortran
use modString, only: string2Int

write(*,fmt='(I8.8)') string2Int("58")
write(*,fmt='(I8.8)') string2Int("   58")
```

---
