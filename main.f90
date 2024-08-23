module StockModule
    implicit none

    type :: Inventario
    character(len=256) :: nombre
    integer :: cantidad
    real :: precio
    character(len=256) :: ubicacion
    contains
        procedure :: crear
        procedure :: agregarStock
        procedure :: eliminarStock

    end type Inventario
    
    contains
    subroutine crear(this, nombre, cantidad, precio, ubicacion)
        class(Inventario), intent(inout) :: this
        character(len=256), intent(in) :: nombre
        integer, intent(in) :: cantidad
        real, intent(in) :: precio
        character(len=256), intent(in) :: ubicacion

        this%nombre = nombre
        this%cantidad = cantidad
        this%precio = precio
        this%ubicacion = ubicacion
    end subroutine crear

    subroutine agregarStock(this, cantidad)
        class(Inventario), intent(inout) :: this
        integer, intent(in) :: cantidad

        this%cantidad = this%cantidad + cantidad

    end subroutine agregarStock

    subroutine eliminarStock(this, cantidad)
        class(Inventario), intent(inout) :: this
        integer, intent(in) :: cantidad

        integer :: resultante
        resultante = this%cantidad - cantidad

        if(resultante >= 0) then
        this%cantidad = this%cantidad - cantidad
        else
            call system ("cls")
            print *, "No se puede eliminar esa cantidad de equipos, resultado negativo"
            read *,
        end if 

    end subroutine eliminarStock

end module StockModule

module global_vars
    use StockModule
    integer :: num = 1
    integer :: i
    type(Inventario), dimension(100) :: articulos

end module global_vars

program main
    use StockModule
    use global_vars
    implicit none
    
    integer :: opcion
    
    do 

        print *, "---------------------------------------------------------------"
        print *, "Practica 1 - Lenguajes Formales y de Programacion - Seccion: A-"
        print *, "---------------------------------------------------------------"
        print *, "# Sistema de Inventario"
        print *, " "
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar instrucciones de movimiento"
        print *, "3. Crear Informe de Inventario"
        print *, "4. Salir"
        print *, " "
        print *, "Ingrese una opcion: "
        read *, opcion
        select case (opcion)
            case(1)
                call cargarInventario()

            case(2)
                call cargarInstrucciones()

            case(3)
                call crearInforme()

            case(4)
                print *, "Saliendo del sistema..."
                print *, "Presione Enter para salir del programa"
                read *
                call system ("cls")
                stop

            case default
                print *, "Opcion no valida"
                print *, "Presione Enter para volver al menu principal" 
                read *

        end select
        call system ("cls")
    end do

    

end program main

subroutine cargarInventario()
    integer :: iunit, ios, pos, cantidad_int
    real :: precio_real
    character(len=256) :: nombre, cantidad, precio, ubicacion, linea, comando
    character(len=512) :: ruta_archivo

    print *, "Ingrese la ruta del archivo"
    read(*, '(A)') ruta_archivo
    
    iunit = 10

    open(unit = iunit, file = trim(ruta_archivo), status = 'old', action = 'read', iostat = ios)

    if ( ios /= 0 ) then
        print *, "Error al abrir el archivo"
        stop
    end if

    do
        read(iunit, '(A)', iostat=ios)linea
        if ( ios /= 0 ) exit
            linea = trim(linea)

            print *, linea



            pos = index(linea, ' ')
        if(pos > 0)then
            comando = linea(1: pos-1)
            linea = trim(linea(pos+1:))
            pos = index(linea, ';')
            if ( pos > 0 ) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                pos = index(linea, ';')
                if ( pos > 0 ) then
                    cantidad = linea(1:pos-1)
                    read(cantidad, '(I10)', iostat=ios) cantidad_int
                    linea = trim(linea(pos+1:))
                    pos = index(linea, ';')
                if ( pos > 0 ) then
                    precio = linea(1:pos-1)
                    read(precio, '(F10.2)', iostat=ios) precio_real
                    ubicacion = trim(linea(pos+1:))

                    if (comando=="crear_equipo")then
                        call crearInventario(nombre, cantidad_int, precio_real, ubicacion)
                        endif
                    endif
                endif
            endif
        endif
    end do   
    close(unit=iunit) 


end subroutine cargarInventario

subroutine crearInventario(nombre, cantidad, precio, ubicacion)
    use StockModule
    use global_vars

    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    real, intent (in) :: precio
    character(len=256), intent (in) :: ubicacion

    type(Inventario) :: nuevoArticulo
    call nuevoArticulo%crear(nombre,cantidad,precio,ubicacion)
    articulos(num) = nuevoArticulo
    num = num+1
    
end subroutine crearInventario

subroutine cargarInstrucciones()
    use StockModule
    use global_vars
    integer :: iunit, ios, pos, cantidad_int
    character(len=256) :: nombre, cantidad, ubicacion, linea, comando
    character(len=512) :: ruta_archivo

    print *, "Ingrese la ruta del archivo"
    read(*, '(A)') ruta_archivo

    iunit = 11

    open(unit = iunit, file = trim(ruta_archivo), status = 'old', action = 'read', iostat = ios)

    if ( ios /= 0 ) then
        print *, "Error al abrir el archivo"
        stop
    end if

    do
        read(iunit, '(A)', iostat=ios)linea
        if ( ios /= 0 ) exit
            linea = trim(linea)

            print *, linea

            pos = index(linea, ' ')
        if(pos > 0)then
            comando = linea(1: pos-1)
            linea = trim(linea(pos+1:))
            pos = index(linea, ';')
            if ( pos > 0 ) then
                nombre = linea(1:pos-1)
                linea = trim(linea(pos+1:))
                pos = index(linea, ';')
                if ( pos > 0 ) then
                    cantidad = linea(1:pos-1)
                    read(cantidad, '(I10)', iostat=ios) cantidad_int
                    ubicacion = trim(linea(pos+1:))

                    if (comando == "agregar_stock")then
                        call agregarEquipo(nombre, cantidad_int, ubicacion)
                    else if(comando == "eliminar_equipo")then
                        call eliminarEquipo(nombre, cantidad_int, ubicacion)
                        endif
                    endif
                endif
            endif
    end do   
    close(unit=iunit)

end subroutine cargarInstrucciones

subroutine agregarEquipo(nombre, cantidad, ubicacion)
    use StockModule
    use global_vars

    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    character(len=256), intent (in) :: ubicacion

    do i=1, num-1
        if(articulos(i)%nombre == nombre .and. articulos(i)%ubicacion == ubicacion) then
        call articulos(i)%agregarStock(cantidad)
        end if
    end do


end subroutine agregarEquipo


subroutine eliminarEquipo(nombre, cantidad, ubicacion)
    use StockModule
    use global_vars

    character(len=256), intent (in) :: nombre
    integer, intent (in) :: cantidad
    character(len=256), intent (in) :: ubicacion

    do i=1, num-1
        if(articulos(i)%nombre == nombre .and. articulos(i)%ubicacion == ubicacion)then
        call articulos(i)%eliminarStock(cantidad)
        end if
    end do

end subroutine eliminarEquipo

subroutine crearInforme()
    use StockModule
    use global_vars

    integer iunit, ios

    iunit = 20

    open(unit = iunit, file = "Informe.txt", status = 'replace', action = 'write', iostat = ios)

    if ( ios /= 0 ) then
        print *, "Error al abrir el archivo"
        stop
    end if

    write(iunit, '(A30, A30, A30, A30)')"Nombre", "Cantidad", "Precio", "Ubicacion"

    do i=1, num-1
        write(iunit, '(A30, I30, F30.2, A30)')trim(articulos(i)%nombre), articulos(i)%cantidad, articulos(i)%precio, trim(articulos(i)%ubicacion)
    end do

    print *, "Informe creado con exito"
    print *, "Presione Enter o cualquier tecla para continuar"

    close(unit = iunit)


end subroutine crearInforme